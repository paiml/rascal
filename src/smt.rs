use crate::hir::*;
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use z3::{ast::*, Config, Context, Model, SatResult, Solver};

pub struct SMTContext<'ctx> {
    ctx: &'ctx Context,
    solver: Solver<'ctx>,
    vars: HashMap<Symbol, Dynamic<'ctx>>,
}

#[derive(Debug, Clone)]
pub enum VerificationResult {
    Verified,
    CounterExample(CounterExample),
    Timeout,
    Unknown(String),
}

#[derive(Debug, Clone)]
pub struct CounterExample {
    pub model: HashMap<String, String>,
    pub failing_condition: String,
}

#[derive(Debug, Clone)]
pub struct VerificationTrace {
    pub model: HashMap<String, String>,
}

impl<'ctx> SMTContext<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        SMTContext {
            ctx,
            solver: Solver::new(ctx),
            vars: HashMap::new(),
        }
    }

    pub fn encode_refinement(&mut self, ty: &RefinedType) -> Bool<'ctx> {
        match &ty.refinement {
            Some(expr) => self.encode_smt_expr(expr),
            None => Bool::from_bool(self.ctx, true),
        }
    }

    pub fn encode_smt_expr(&mut self, expr: &SMTExpr) -> Bool<'ctx> {
        match expr {
            SMTExpr::Comparison { op, lhs, rhs } => {
                let l = self.encode_smt_value(lhs);
                let r = self.encode_smt_value(rhs);
                self.encode_comparison(op, l, r)
            }
            SMTExpr::And(exprs) => {
                let encoded: Vec<_> = exprs.iter().map(|e| self.encode_smt_expr(e)).collect();
                Bool::and(self.ctx, &encoded.iter().collect::<Vec<_>>())
            }
            SMTExpr::Or(exprs) => {
                let encoded: Vec<_> = exprs.iter().map(|e| self.encode_smt_expr(e)).collect();
                Bool::or(self.ctx, &encoded.iter().collect::<Vec<_>>())
            }
            SMTExpr::Not(expr) => self.encode_smt_expr(expr).not(),
            SMTExpr::Implies(p, q) => {
                let p_enc = self.encode_smt_expr(p);
                let q_enc = self.encode_smt_expr(q);
                p_enc.implies(&q_enc)
            }
            SMTExpr::Forall(vars, body) => {
                // Create bound variables
                let bound_vars: Vec<Dynamic> = vars
                    .iter()
                    .map(|(name, sort)| self.create_var_of_sort(name, sort))
                    .collect();

                // Save current variable bindings
                let saved_vars = self.vars.clone();

                // Add bound variables to context
                for ((name, _), var) in vars.iter().zip(&bound_vars) {
                    self.vars.insert(name.clone(), var.clone());
                }

                // Encode body
                let body_enc = self.encode_smt_expr(body);

                // Restore variable bindings
                self.vars = saved_vars;

                // Create quantified formula
                let ast_vars: Vec<&dyn Ast> = bound_vars.iter()
                    .map(|v| v as &dyn Ast)
                    .collect();
                forall_const(
                    self.ctx,
                    &ast_vars,
                    &[],
                    &body_enc,
                )
            }
            SMTExpr::Exists(vars, body) => {
                // Similar to Forall but with exists
                let bound_vars: Vec<Dynamic> = vars
                    .iter()
                    .map(|(name, sort)| self.create_var_of_sort(name, sort))
                    .collect();

                let saved_vars = self.vars.clone();

                for ((name, _), var) in vars.iter().zip(&bound_vars) {
                    self.vars.insert(name.clone(), var.clone());
                }

                let body_enc = self.encode_smt_expr(body);
                self.vars = saved_vars;

                let ast_vars: Vec<&dyn Ast> = bound_vars.iter()
                    .map(|v| v as &dyn Ast)
                    .collect();
                exists_const(
                    self.ctx,
                    &ast_vars,
                    &[],
                    &body_enc,
                )
            }
            _ => Bool::from_bool(self.ctx, true), // TODO: Handle other cases
        }
    }

    fn encode_smt_value(&mut self, expr: &SMTExpr) -> Dynamic<'ctx> {
        match expr {
            SMTExpr::Var(sym) => {
                self.vars.get(sym).cloned().unwrap_or_else(|| {
                    // Create new variable if not exists
                    let var = Int::new_const(self.ctx, sym.0.as_str());
                    let dyn_var = Dynamic::from_ast(&var);
                    self.vars.insert(sym.clone(), dyn_var.clone());
                    dyn_var
                })
            }
            SMTExpr::Const(lit) => match lit {
                Literal::Int(n) => Dynamic::from_ast(&Int::from_i64(self.ctx, *n)),
                Literal::Bool(b) => Dynamic::from_ast(&Bool::from_bool(self.ctx, *b)),
                _ => panic!("Unsupported literal type in SMT"),
            },
            SMTExpr::App(func, args) => {
                // Handle function application
                match func.0.as_str() {
                    "+" => {
                        let args: Vec<_> = args
                            .iter()
                            .map(|a| self.encode_smt_value(a).as_int().unwrap())
                            .collect();
                        Dynamic::from_ast(&Int::add(self.ctx, &args.iter().collect::<Vec<_>>()))
                    }
                    "-" => {
                        let args: Vec<_> = args
                            .iter()
                            .map(|a| self.encode_smt_value(a).as_int().unwrap())
                            .collect();
                        if args.len() == 2 {
                            Dynamic::from_ast(&Int::sub(self.ctx, &[&args[0], &args[1]]))
                        } else {
                            panic!("Subtraction requires exactly 2 arguments")
                        }
                    }
                    "*" => {
                        let args: Vec<_> = args
                            .iter()
                            .map(|a| self.encode_smt_value(a).as_int().unwrap())
                            .collect();
                        Dynamic::from_ast(&Int::mul(self.ctx, &args.iter().collect::<Vec<_>>()))
                    }
                    _ => panic!("Unknown function: {}", func.0),
                }
            }
            _ => panic!("Cannot encode {:?} as SMT value", expr),
        }
    }

    fn encode_comparison(&self, op: &CompOp, lhs: Dynamic<'ctx>, rhs: Dynamic<'ctx>) -> Bool<'ctx> {
        // Assume integer comparison for now
        let l = lhs.as_int().expect("Expected integer in comparison");
        let r = rhs.as_int().expect("Expected integer in comparison");

        match op {
            CompOp::Eq => l._eq(&r),
            CompOp::Neq => l._eq(&r).not(),
            CompOp::Lt => l.lt(&r),
            CompOp::Leq => l.le(&r),
            CompOp::Gt => l.gt(&r),
            CompOp::Geq => l.ge(&r),
        }
    }

    fn create_var_of_sort(&self, name: &Symbol, sort: &Sort) -> Dynamic<'ctx> {
        match sort {
            Sort::Int => Dynamic::from_ast(&Int::new_const(self.ctx, name.0.as_str())),
            Sort::Bool => Dynamic::from_ast(&Bool::new_const(self.ctx, name.0.as_str())),
            Sort::Array(_, _) => {
                // TODO: Implement array sorts
                panic!("Array sorts not yet implemented")
            }
            Sort::Datatype(_) => {
                // TODO: Implement datatype sorts
                panic!("Datatype sorts not yet implemented")
            }
        }
    }

    pub fn encode_predicate(&mut self, pred: &Predicate) -> Bool<'ctx> {
        // For now, treat predicates as function applications
        match pred.name.0.as_str() {
            "sorted" => {
                // Example: sorted predicate for lists
                Bool::from_bool(self.ctx, true) // TODO: Implement actual encoding
            }
            _ => {
                // Generic predicate encoding
                Bool::from_bool(self.ctx, true) // TODO: Implement
            }
        }
    }

    pub fn encode_hir(&mut self, hir: &HIR) -> Bool<'ctx> {
        match hir {
            HIR::Lit(Literal::Bool(b)) => Bool::from_bool(self.ctx, *b),
            HIR::Var(sym) => {
                if let Some(var) = self.vars.get(sym) {
                    if let Some(b) = var.as_bool() {
                        b
                    } else {
                        Bool::from_bool(self.ctx, true)
                    }
                } else {
                    Bool::from_bool(self.ctx, true)
                }
            }
            HIR::BinOp { op, lhs, rhs } => {
                match op {
                    BinOp::And => {
                        let l = self.encode_hir(lhs);
                        let r = self.encode_hir(rhs);
                        Bool::and(self.ctx, &[&l, &r])
                    }
                    BinOp::Or => {
                        let l = self.encode_hir(lhs);
                        let r = self.encode_hir(rhs);
                        Bool::or(self.ctx, &[&l, &r])
                    }
                    BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq | BinOp::Eq | BinOp::Neq => {
                        // Convert to SMTExpr for comparison
                        let smt_expr = self.hir_to_smt_comparison(op, lhs, rhs);
                        self.encode_smt_expr(&smt_expr)
                    }
                    _ => Bool::from_bool(self.ctx, true), // TODO: Handle arithmetic ops
                }
            }
            HIR::Let {
                body, ..
            } => {
                // Evaluate value and bind to name
                // For now, just process body
                self.encode_hir(body)
            }
            _ => Bool::from_bool(self.ctx, true), // Conservative default
        }
    }

    fn hir_to_smt_comparison(&self, op: &BinOp, lhs: &HIR, rhs: &HIR) -> SMTExpr {
        let comp_op = match op {
            BinOp::Lt => CompOp::Lt,
            BinOp::Leq => CompOp::Leq,
            BinOp::Gt => CompOp::Gt,
            BinOp::Geq => CompOp::Geq,
            BinOp::Eq => CompOp::Eq,
            BinOp::Neq => CompOp::Neq,
            _ => panic!("Not a comparison operator"),
        };

        SMTExpr::Comparison {
            op: comp_op,
            lhs: Box::new(self.hir_to_smt_expr(lhs)),
            rhs: Box::new(self.hir_to_smt_expr(rhs)),
        }
    }

    fn hir_to_smt_expr(&self, hir: &HIR) -> SMTExpr {
        match hir {
            HIR::Var(sym) => SMTExpr::Var(sym.clone()),
            HIR::Lit(lit) => SMTExpr::Const(lit.clone()),
            _ => panic!("Cannot convert HIR to SMT expression: {:?}", hir),
        }
    }

    pub fn verify_function(&mut self, func: &HIR) -> VerificationResult {
        match func {
            HIR::Function {
                pre, post, body, ..
            } => {
                // Encode preconditions
                for pred in pre {
                    let enc = self.encode_predicate(pred);
                    self.solver.assert(&enc);
                }

                // Encode function body (simplified for now)
                let _body_constraint = self.encode_hir(body);

                // Check postconditions
                for pred in post {
                    let post_encoded = self.encode_predicate(pred);
                    self.solver.push();
                    self.solver.assert(&post_encoded.not());

                    match self.solver.check() {
                        SatResult::Unsat => {
                            self.solver.pop(1);
                            continue; // Post condition holds
                        }
                        SatResult::Sat => {
                            let model = self.solver.get_model().unwrap();
                            self.solver.pop(1);
                            return VerificationResult::CounterExample(
                                self.extract_counter_example(&model, &pred.name.0),
                            );
                        }
                        SatResult::Unknown => {
                            self.solver.pop(1);
                            return VerificationResult::Unknown(
                                "SMT solver returned unknown".to_string(),
                            );
                        }
                    }
                }

                VerificationResult::Verified
            }
            _ => VerificationResult::Unknown("Not a function".to_string()),
        }
    }

    fn extract_counter_example(&self, model: &Model, failing_pred: &str) -> CounterExample {
        let mut model_map = HashMap::new();

        // Extract variable assignments from model
        for (sym, var) in &self.vars {
            if let Some(interp) = model.get_const_interp(var) {
                model_map.insert(sym.0.clone(), format!("{}", interp));
            }
        }

        CounterExample {
            model: model_map,
            failing_condition: failing_pred.to_string(),
        }
    }

    pub fn check_satisfiability(&mut self) -> SatResult {
        self.solver.check()
    }

    pub fn add_assertion(&mut self, assertion: Bool<'ctx>) {
        self.solver.assert(&assertion);
    }

    pub fn push(&mut self) {
        self.solver.push();
    }

    pub fn pop(&mut self, n: u32) {
        self.solver.pop(n);
    }
}

// Public verification API
pub fn verify_hir(hir: &HIR) -> Result<VerificationResult> {
    let cfg = Config::new();
    let ctx = Context::new(&cfg);
    let mut smt = SMTContext::new(&ctx);

    Ok(smt.verify_function(hir))
}

pub fn check_refinement_satisfiability(ty: &RefinedType) -> Result<bool> {
    let cfg = Config::new();
    let ctx = Context::new(&cfg);
    let mut smt = SMTContext::new(&ctx);

    let constraint = smt.encode_refinement(ty);
    smt.add_assertion(constraint);

    match smt.check_satisfiability() {
        SatResult::Sat => Ok(true),
        SatResult::Unsat => Ok(false),
        SatResult::Unknown => Err(anyhow!("SMT solver returned unknown")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_refinement() {
        let ty = RefinedType::new(BaseType::Int).with_refinement(SMTExpr::Comparison {
            op: CompOp::Geq,
            lhs: Box::new(SMTExpr::Var(Symbol("v".to_string()))),
            rhs: Box::new(SMTExpr::Const(Literal::Int(0))),
        });

        let result = check_refinement_satisfiability(&ty).unwrap();
        assert!(result); // Should be satisfiable (there exist non-negative integers)
    }
}
