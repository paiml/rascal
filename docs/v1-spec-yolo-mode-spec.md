## Rascal-Light: Technical Specification & Implementation Roadmap

### Architecture Overview

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────┐
│ Liquid Haskell  │────▶│ Rascal HIR   │────▶│ Rust + SMT  │
│   Frontend      │     │ (Typed Core) │     │   Backend   │
└─────────────────┘     └──────────────┘     └─────────────┘
         │                      │                     │
         ▼                      ▼                     ▼
    Parse Tree            Verified IR          Verified Rust
    + Refinements         + Ownership          + Creusot/Prusti
```

### Core Data Structures

```rust
// HIR representation preserving verification info
#[derive(Clone, Debug)]
pub enum HIR {
    // Verified function with refinement types
    Function {
        name: Symbol,
        params: Vec<(Symbol, RefinedType)>,
        body: Box<HIR>,
        pre: Vec<Predicate>,
        post: Vec<Predicate>,
        decreases: Option<Measure>,
    },
    // Pattern match with exhaustiveness proof
    Match {
        scrutinee: Box<HIR>,
        arms: Vec<(Pattern, Box<HIR>)>,
        exhaustive: ExhaustivenessProof,
    },
    // Let binding with linearity annotation
    Let {
        name: Symbol,
        ty: RefinedType,
        value: Box<HIR>,
        body: Box<HIR>,
        linear: bool,
    },
}

#[derive(Clone, Debug)]
pub struct RefinedType {
    base: BaseType,
    refinement: Option<SMTExpr>,
    ownership: OwnershipKind,
}
```

## Implementation Checkpoints

### Checkpoint 1: Minimal Parser & HIR (Week 1-2)

**Goal**: Parse subset of Liquid Haskell into HIR with refinement preservation.

```haskell
-- Test input: examples/nat.rhl
{-@ type Nat = {v:Int | 0 <= v} @-}
{-@ add :: Nat -> Nat -> Nat @-}
add :: Int -> Int -> Int
add x y = x + y
```

**Implementation**:
```rust
// src/parser.rs
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, multispace0},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, preceded, tuple},
};

pub fn parse_refinement(input: &str) -> IResult<&str, RefinedType> {
    let (input, _) = tag("{-@")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ty) = parse_type_sig(input)?;
    let (input, _) = tag("@-}")(input)?;
    Ok((input, ty))
}

// Ownership inference based on usage patterns
pub fn infer_ownership(hir: &HIR) -> HIR {
    match hir {
        HIR::Function { body, .. } => {
            let usage_map = collect_usage(body);
            annotate_ownership(hir, &usage_map)
        }
        _ => hir.clone(),
    }
}
```

**Verification criteria**:
- Parse 10 core Liquid Haskell examples
- Preserve all refinement type information in HIR
- Round-trip property: `parse(pretty(hir)) == hir`

**Command**:
```bash
cargo test --test parser_tests -- --nocapture
# Expected: 10/10 tests pass, <100ms per test
```

### Checkpoint 2: SMT Encoding & Verification (Week 3-4)

**Goal**: Translate refinements to SMT-LIB2 format, verify via Z3.

```rust
// src/smt.rs
use z3::{ast::*, Config, Context, Solver};

pub struct SMTContext<'ctx> {
    ctx: &'ctx Context,
    solver: Solver<'ctx>,
    vars: HashMap<Symbol, Dynamic<'ctx>>,
}

impl<'ctx> SMTContext<'ctx> {
    pub fn encode_refinement(&mut self, ty: &RefinedType) -> Bool<'ctx> {
        match &ty.refinement {
            Some(SMTExpr::Comparison { op, lhs, rhs }) => {
                let l = self.encode_expr(lhs);
                let r = self.encode_expr(rhs);
                match op {
                    CompOp::Leq => l.le(&r),
                    CompOp::Lt => l.lt(&r),
                    CompOp::Eq => l._eq(&r),
                }
            }
            _ => Bool::from_bool(self.ctx, true),
        }
    }
    
    pub fn verify_function(&mut self, func: &HIR) -> VerificationResult {
        // Encode preconditions
        for pre in &func.pre {
            self.solver.assert(&self.encode_predicate(pre));
        }
        
        // Encode function body
        let body_constraint = self.encode_hir(&func.body);
        
        // Check postconditions
        for post in &func.post {
            let post_encoded = self.encode_predicate(post);
            self.solver.push();
            self.solver.assert(&post_encoded.not());
            
            match self.solver.check() {
                SatResult::Unsat => continue,  // Post condition holds
                SatResult::Sat => {
                    let model = self.solver.get_model().unwrap();
                    return VerificationResult::CounterExample(model);
                }
                SatResult::Unknown => {
                    return VerificationResult::Timeout;
                }
            }
            self.solver.pop(1);
        }
        
        VerificationResult::Verified
    }
}
```

**Benchmark target**:
```rust
// benches/smt_bench.rs
#[bench]
fn bench_bst_insert_verification(b: &mut Bencher) {
    let hir = parse_file("examples/bst.rhl").unwrap();
    b.iter(|| {
        let ctx = Context::new(&Config::new());
        let mut smt = SMTContext::new(&ctx);
        smt.verify_function(&hir)
    });
}
// Target: <50ms for 100-line function
```

### Checkpoint 3: Ownership Analysis & Rust Codegen (Week 5-6)

**Goal**: Infer ownership patterns and generate safe Rust code.

```rust
// src/ownership.rs
#[derive(Debug, Clone, Copy)]
pub enum OwnershipKind {
    Owned,           // T
    Borrowed,        // &T
    MutBorrowed,     // &mut T
    Shared(usize),   // Rc<T> with refcount hint
}

pub struct OwnershipAnalysis {
    usage_graph: DiGraph<Symbol, UsageEdge>,
    dominators: HashMap<Symbol, HashSet<Symbol>>,
}

impl OwnershipAnalysis {
    pub fn analyze(&mut self, hir: &HIR) -> Result<OwnershipMap, OwnershipError> {
        // Build usage graph
        self.collect_usages(hir);
        
        // Compute dominance frontier for lifetime inference
        self.compute_dominators();
        
        // Apply ownership rules
        let mut ownership = HashMap::new();
        for node in self.usage_graph.node_indices() {
            let symbol = &self.usage_graph[node];
            ownership.insert(
                symbol.clone(),
                self.infer_ownership_kind(symbol)
            );
        }
        
        // Verify no cycles in mutable borrows
        self.verify_acyclic_borrows(&ownership)?;
        
        Ok(ownership)
    }
    
    fn infer_ownership_kind(&self, sym: &Symbol) -> OwnershipKind {
        let usages = self.get_usages(sym);
        
        match (usages.reads, usages.writes, usages.moves) {
            (_, 0, 0) => OwnershipKind::Borrowed,
            (_, n, 0) if n > 0 => OwnershipKind::MutBorrowed,
            (_, _, n) if n > 0 => OwnershipKind::Owned,
            (n, 0, 0) if n > 2 => OwnershipKind::Shared(n),
            _ => OwnershipKind::Owned, // Conservative default
        }
    }
}
```

**Codegen with lifetime inference**:
```rust
// src/codegen.rs
pub fn generate_rust(hir: &HIR, ownership: &OwnershipMap) -> String {
    match hir {
        HIR::Function { name, params, body, .. } => {
            let mut out = String::new();
            
            // Generate signature with lifetimes
            let lifetimes = infer_lifetimes(params, body, ownership);
            if !lifetimes.is_empty() {
                write!(out, "<{}>", lifetimes.join(", "));
            }
            
            write!(out, "fn {}", name);
            write!(out, "(");
            for (i, (param, ty)) in params.iter().enumerate() {
                if i > 0 { write!(out, ", "); }
                let ownership_kind = ownership.get(param).unwrap();
                write!(out, "{}: {}", param, ty.to_rust(ownership_kind));
            }
            write!(out, ") -> {}", return_type.to_rust());
            
            // Generate body
            writeln!(out, " {{");
            writeln!(out, "{}", generate_body(body, ownership, 1));
            writeln!(out, "}}");
            
            out
        }
        _ => todo!(),
    }
}
```

### Checkpoint 4: Verification Backend Integration (Week 7-8)

**Goal**: Generate Prusti/Creusot annotations from refinements.

```rust
// src/verification_backend.rs
pub trait VerificationBackend {
    fn generate_spec(&self, hir: &HIR) -> String;
    fn generate_proof_obligations(&self, hir: &HIR) -> Vec<ProofObligation>;
}

pub struct PrustiBackend;

impl VerificationBackend for PrustiBackend {
    fn generate_spec(&self, hir: &HIR) -> String {
        match hir {
            HIR::Function { pre, post, decreases, .. } => {
                let mut spec = String::new();
                
                // Preconditions
                for p in pre {
                    writeln!(spec, "#[requires({})]", self.pred_to_prusti(p));
                }
                
                // Postconditions
                for p in post {
                    writeln!(spec, "#[ensures({})]", self.pred_to_prusti(p));
                }
                
                // Termination
                if let Some(measure) = decreases {
                    writeln!(spec, "#[decreases({})]", measure);
                }
                
                spec
            }
            _ => String::new(),
        }
    }
}

// Integration test
#[test]
fn test_full_pipeline() {
    let input = r#"
        {-@ insert :: Ord a => a -> BST a -> BST a @-}
        {-@ measure size @-}
        {-@ size :: BST a -> Nat @-}
        insert x Empty = Node x Empty Empty
        insert x (Node y l r)
          | x < y = Node y (insert x l) r
          | otherwise = Node y l (insert x r)
    "#;
    
    let hir = parse(input).unwrap();
    let ownership = OwnershipAnalysis::new().analyze(&hir).unwrap();
    let rust_code = generate_rust(&hir, &ownership);
    let verification = PrustiBackend.generate_spec(&hir);
    
    // Should produce verified Rust
    assert!(rust_code.contains("fn insert<T: Ord>"));
    assert!(verification.contains("#[ensures(size(&result)"));
}
```

### Checkpoint 5: Optimization & Performance (Week 9-10)

**Goal**: Achieve <10% overhead vs hand-written Rust.

```rust
// src/optimizer.rs
pub struct OptimizationPass {
    fusion_rules: Vec<FusionRule>,
    inline_threshold: usize,
}

impl OptimizationPass {
    pub fn optimize(&self, hir: HIR) -> HIR {
        let hir = self.inline_small_functions(hir);
        let hir = self.fuse_list_operations(hir);
        let hir = self.eliminate_unnecessary_clones(hir);
        let hir = self.tail_call_optimization(hir);
        hir
    }
    
    fn fuse_list_operations(&self, hir: HIR) -> HIR {
        // map f . map g = map (f . g)
        // filter p . map f = mapMaybe (λx → if p (f x) then Just (f x) else Nothing)
        match hir {
            HIR::Compose(
                HIR::Map(f),
                HIR::Map(g)
            ) => HIR::Map(HIR::Compose(f, g)),
            _ => hir,
        }
    }
}

// Performance regression test
#[bench]
fn bench_rbt_insert_vs_handwritten(b: &mut Bencher) {
    let n = 10_000;
    let mut rng = thread_rng();
    let data: Vec<i32> = (0..n).map(|_| rng.gen()).collect();
    
    // Rascal-generated version
    let rascal_rbt = compile_and_load("examples/rbt.rhl");
    
    b.iter(|| {
        let mut tree = RBT::Empty;
        for &x in &data {
            tree = rascal_rbt.insert(x, tree);
        }
        tree
    });
}
// Target: Within 10% of hand-written performance
```

### Checkpoint 6: Error Handling & Diagnostics (Week 11-12)

**Goal**: Compiler-quality error messages with verification feedback.

```rust
// src/diagnostics.rs
pub struct Diagnostic {
    level: Level,
    span: Span,
    message: String,
    notes: Vec<Note>,
    verification_trace: Option<VerificationTrace>,
}

impl Diagnostic {
    pub fn render(&self, source: &str) -> String {
        let mut output = String::new();
        
        // Rust-style error formatting
        writeln!(output, "{}: {}", self.level, self.message);
        writeln!(output, "  --> {}:{}:{}", 
                 self.span.file, self.span.line, self.span.col);
        
        // Source snippet with highlighting
        let snippet = self.extract_snippet(source);
        writeln!(output, "   |");
        writeln!(output, "{} | {}", self.span.line, snippet);
        writeln!(output, "   | {}{}", 
                 " ".repeat(self.span.col), 
                 "^".repeat(self.span.len));
        
        // Verification counterexample if present
        if let Some(trace) = &self.verification_trace {
            writeln!(output, "   |");
            writeln!(output, "   = note: counterexample found:");
            for (var, val) in &trace.model {
                writeln!(output, "           {} = {}", var, val);
            }
        }
        
        output
    }
}
```

## Final Integration Test Suite

```bash
#!/bin/bash
# test_suite.sh

# Correctness tests
echo "=== Correctness Tests ==="
for test in examples/*.rhl; do
    echo -n "Testing $test... "
    if rascal-light compile "$test" -o /tmp/out.rs && \
       rustc --edition 2021 /tmp/out.rs && \
       rascal-light verify "$test"; then
        echo "PASS"
    else
        echo "FAIL"
        exit 1
    fi
done

# Performance benchmarks
echo "=== Performance Benchmarks ==="
cargo bench --bench comparison -- --save-baseline rascal

# Memory safety verification
echo "=== Miri Tests ==="
for test in target/debug/examples/*; do
    MIRIFLAGS="-Zmiri-strict-provenance" cargo miri run --bin "$test"
done

# Integration with Prusti
echo "=== Prusti Verification ==="
cargo prusti --features verification
```

## Deliverables Checklist

- [ ] **Core compiler** (<5K LOC)
    - [ ] Parser: 800 LOC
    - [ ] HIR + Types: 1200 LOC
    - [ ] Ownership analysis: 1000 LOC
    - [ ] Codegen: 1500 LOC
    - [ ] Verification: 500 LOC

- [ ] **Test suite**
    - [ ] 50 unit tests
    - [ ] 20 integration tests
    - [ ] 5 performance benchmarks
    - [ ] Property tests with QuickCheck

- [ ] **Documentation**
    - [ ] Language reference (20 pages)
    - [ ] Tutorial with 10 examples
    - [ ] Architecture guide
    - [ ] Verification theory appendix

- [ ] **Benchmarks**
    - [ ] BST operations: <5% overhead
    - [ ] RBT operations: <10% overhead
    - [ ] List processing: 0% overhead (fusion)
    - [ ] Compilation speed: >10K LOC/sec

This specification provides a concrete path from theory to implementation, with measurable checkpoints and realistic performance targets. Each phase builds on proven technology (Liquid Haskell's refinements, Rust's ownership system, SMT-based verification) while introducing genuine innovation in their integration.