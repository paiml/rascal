# CLAUDE.md - Rascal-Light Development Guidelines

## Project Context
Rascal-Light is a verification-oriented Haskell-to-Rust transpiler focusing on proof-preserving translation of Liquid Haskell refinements to Prusti/Creusot specifications. The system must bridge the semantic gap between lazy pure functional code and eager imperative Rust while maintaining mathematical correctness guarantees.

## Development Principles

### 自働化 (Jidoka) - Build Quality In
- **Never ship unverified translations**: Every HIR transformation must preserve refinement type semantics through SMT encoding
- **Soundness-first development**: New translation rules require formal proof of semantic preservation
- **Example**: When implementing pattern match translation:
  ```rust
  // CORRECT: Complete exhaustiveness proof preservation
  match hir {
      HIR::Match { scrutinee, arms, exhaustive } => {
          let rust_match = translate_scrutinee(scrutinee)?;
          let rust_arms = arms.iter()
              .map(|(pat, expr)| translate_arm(pat, expr))
              .collect::<Result<Vec<_>, _>>()?;
          
          // Preserve exhaustiveness proof as Prusti annotation
          let proof = encode_exhaustiveness_smt(&exhaustive)?;
          emit_verified_match(rust_match, rust_arms, proof)
      }
      _ => Err(TranspileError::PreservationFailure(hir.clone()))
  }
  // NEVER: Drop verification constraints during translation
  ```

### 現地現物 (Genchi Genbutsu) - Direct Observation
- **Verify against actual SMT solvers**: Test refinement preservation through Z3/CVC5, not just syntactic translation
- **Profile verification overhead**: Measure proof checking time on realistic algebraic structures (BST, RBT, finger trees)
- **Debug at three levels**:
    1. Liquid Haskell source refinements
    2. SMT-LIB2 encoding
    3. Generated Prusti/Creusot specifications

### 反省 (Hansei) - Fix Before Adding
- **Current semantic gaps to address**:
    1. Lazy evaluation semantics lost in strict translation (thunk elimination incomplete)
    2. Higher-rank polymorphism creates unsound Rust lifetime bounds
    3. Refinement weakening during ownership inference drops critical invariants
- **Do not add**: GADTs, type families, or effect systems until core ADT + refinement translation achieves 100% soundness

### 改善 (Kaizen) - Continuous Improvement
- **Verification levels**:
    - `--verify basic`: Type safety only
    - `--verify refinement`: Include Liquid Haskell predicates
    - `--verify total`: Full functional correctness with termination
- **Performance targets**:
    - Translation: >50K LOC/sec
    - Verification: <100ms per function
    - Runtime overhead: <5% vs hand-written Rust
- **Proof quality metrics**: Generated proofs should discharge in Prusti without manual hints

## Critical Invariants
1. **Refinement preservation**: ∀ e:τ{ν:φ} in Haskell, [[e]]ᴿᵘˢᵗ : [[τ]]ᴿᵘˢᵗ with equivalent SMT constraint [[φ]]ˢᴹᵀ
2. **Termination preservation**: Liquid Haskell termination metrics must map to Prusti decreases clauses
3. **No semantic drift**: Operational semantics of source and target must be bisimilar modulo evaluation strategy

## Build Commands
```bash
# Run verification test suite with QuickCheck properties
cargo test --workspace --features "quickcheck smt-backend"

# Transpile with full verification pipeline
cargo run -- transpile examples/verified/rbt.lhs --verify total --backend prusti

# Benchmark against hand-written implementations
cargo bench -- --baseline handwritten

# Verify semantic preservation properties
cargo run -- verify-bisimulation examples/verified/bst.lhs
```

## Verification Pipeline
```bash
# Stage 1: Parse and validate Liquid Haskell
cargo test -p rascal-parser -- --test liquid_parser

# Stage 2: Verify refinement encoding
cargo test -p rascal-smt -- --test smt_encoding

# Stage 3: Test ownership inference soundness  
cargo test -p rascal-ownership -- --test ownership_properties

# Stage 4: Validate Prusti/Creusot output
cargo test -p rascal-codegen -- --test verification_backend
```

## Performance Validation
```rust
// Benchmark harness for semantic overhead measurement
#[bench]
fn bench_verified_rbt_vs_native(b: &mut Bencher) {
    let ops = 10_000;
    let rascal_impl = transpile_and_load("benchmarks/rbt.lhs");
    let native_impl = load_baseline("benchmarks/rbt_handwritten.rs");
    
    b.iter(|| {
        assert_observational_equivalence(&rascal_impl, &native_impl, ops);
    });
}
```

## Known Semantic Challenges
1. **Sharing vs Linearity**: Haskell's implicit sharing requires Rc<T> but breaks Rust's move semantics
2. **Infinite structures**: Coinductive types need careful μ-type encoding
3. **Non-strict let**: Requires thunk introduction incompatible with verification

## Quality Gates
- Generated Rust must satisfy `#![deny(unsafe_code)]`
- All functions must have machine-checkable termination proofs
- Memory allocation patterns must match hand-optimization (zero unnecessary clones)