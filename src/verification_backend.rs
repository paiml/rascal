use std::fmt::Write;
use crate::hir::*;
use crate::smt::VerificationResult;
use anyhow::{Result, anyhow};

pub trait VerificationBackend {
    fn generate_spec(&self, hir: &HIR) -> String;
    fn generate_proof_obligations(&self, hir: &HIR) -> Vec<ProofObligation>;
    fn format_verification_result(&self, result: &VerificationResult) -> String;
}

#[derive(Debug, Clone)]
pub struct ProofObligation {
    pub name: String,
    pub kind: ProofKind,
    pub condition: String,
}

#[derive(Debug, Clone)]
pub enum ProofKind {
    Precondition,
    Postcondition,
    Invariant,
    Termination,
    Assertion,
}

pub struct PrustiBackend;

impl VerificationBackend for PrustiBackend {
    fn generate_spec(&self, hir: &HIR) -> String {
        match hir {
            HIR::Function { pre, post, decreases, .. } => {
                let mut spec = String::new();
                
                // Preconditions
                for p in pre {
                    writeln!(spec, "#[requires({})]", self.pred_to_prusti(p)).unwrap();
                }
                
                // Postconditions
                for p in post {
                    writeln!(spec, "#[ensures({})]", self.pred_to_prusti(p)).unwrap();
                }
                
                // Termination
                if let Some(measure) = decreases {
                    writeln!(spec, "#[decreases({})]", self.measure_to_prusti(measure)).unwrap();
                }
                
                spec
            }
            _ => String::new(),
        }
    }

    fn generate_proof_obligations(&self, hir: &HIR) -> Vec<ProofObligation> {
        let mut obligations = Vec::new();
        
        match hir {
            HIR::Function { name, pre, post, decreases, .. } => {
                // Precondition obligations
                for (i, pred) in pre.iter().enumerate() {
                    obligations.push(ProofObligation {
                        name: format!("{}_pre_{}", name.0, i),
                        kind: ProofKind::Precondition,
                        condition: self.pred_to_prusti(pred),
                    });
                }
                
                // Postcondition obligations
                for (i, pred) in post.iter().enumerate() {
                    obligations.push(ProofObligation {
                        name: format!("{}_post_{}", name.0, i),
                        kind: ProofKind::Postcondition,
                        condition: self.pred_to_prusti(pred),
                    });
                }
                
                // Termination obligation
                if let Some(measure) = decreases {
                    obligations.push(ProofObligation {
                        name: format!("{}_terminates", name.0),
                        kind: ProofKind::Termination,
                        condition: self.measure_to_prusti(measure),
                    });
                }
            }
            _ => {}
        }
        
        obligations
    }

    fn format_verification_result(&self, result: &VerificationResult) -> String {
        match result {
            VerificationResult::Verified => "✓ Verified".to_string(),
            VerificationResult::CounterExample(ce) => {
                format!("✗ Counterexample found:\n  Failing condition: {}\n  Model: {:?}",
                    ce.failing_condition, ce.model)
            }
            VerificationResult::Timeout => "⏱ Verification timeout".to_string(),
            VerificationResult::Unknown(msg) => format!("? Unknown: {}", msg),
        }
    }
}

impl PrustiBackend {
    fn pred_to_prusti(&self, pred: &Predicate) -> String {
        match pred.name.0.as_str() {
            "result_eq" => {
                // Special handling for result equality
                if let Some(arg) = pred.args.first() {
                    format!("result == {}", self.smt_to_prusti(arg))
                } else {
                    "true".to_string()
                }
            }
            "forall" => {
                // Handle quantified formulas
                "forall(|_| true)".to_string() // TODO: Proper implementation
            }
            _ => {
                // Generic predicate
                format!("{}({})", 
                    pred.name.0,
                    pred.args.iter()
                        .map(|arg| self.smt_to_prusti(arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }

    fn measure_to_prusti(&self, measure: &Measure) -> String {
        self.smt_to_prusti(&measure.expr)
    }

    fn smt_to_prusti(&self, expr: &SMTExpr) -> String {
        match expr {
            SMTExpr::Var(sym) => sym.0.clone(),
            SMTExpr::Const(lit) => match lit {
                Literal::Int(n) => n.to_string(),
                Literal::Bool(b) => b.to_string(),
                Literal::String(s) => format!("\"{}\"", s),
                Literal::Unit => "()".to_string(),
            },
            SMTExpr::App(func, args) => {
                format!("{}({})",
                    func.0,
                    args.iter()
                        .map(|arg| self.smt_to_prusti(arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            SMTExpr::Comparison { op, lhs, rhs } => {
                format!("{} {} {}",
                    self.smt_to_prusti(lhs),
                    match op {
                        CompOp::Eq => "==",
                        CompOp::Neq => "!=",
                        CompOp::Lt => "<",
                        CompOp::Leq => "<=",
                        CompOp::Gt => ">",
                        CompOp::Geq => ">=",
                    },
                    self.smt_to_prusti(rhs)
                )
            }
            SMTExpr::And(exprs) => {
                if exprs.is_empty() {
                    "true".to_string()
                } else {
                    format!("({})",
                        exprs.iter()
                            .map(|e| self.smt_to_prusti(e))
                            .collect::<Vec<_>>()
                            .join(" && ")
                    )
                }
            }
            SMTExpr::Or(exprs) => {
                if exprs.is_empty() {
                    "false".to_string()
                } else {
                    format!("({})",
                        exprs.iter()
                            .map(|e| self.smt_to_prusti(e))
                            .collect::<Vec<_>>()
                            .join(" || ")
                    )
                }
            }
            SMTExpr::Not(expr) => {
                format!("!({})", self.smt_to_prusti(expr))
            }
            SMTExpr::Implies(p, q) => {
                format!("({}) ==> ({})", self.smt_to_prusti(p), self.smt_to_prusti(q))
            }
            SMTExpr::Forall(vars, body) => {
                let var_list = vars.iter()
                    .map(|(name, _)| name.0.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("forall(|{}| {})", var_list, self.smt_to_prusti(body))
            }
            SMTExpr::Exists(vars, body) => {
                let var_list = vars.iter()
                    .map(|(name, _)| name.0.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("exists(|{}| {})", var_list, self.smt_to_prusti(body))
            }
        }
    }
}

pub struct CreusotBackend;

impl VerificationBackend for CreusotBackend {
    fn generate_spec(&self, hir: &HIR) -> String {
        match hir {
            HIR::Function { pre, post, decreases, .. } => {
                let mut spec = String::new();
                
                // Use Creusot's annotation style
                writeln!(spec, "#[logic]").unwrap();
                
                // Preconditions
                for p in pre {
                    writeln!(spec, "#[requires({})]", self.pred_to_creusot(p)).unwrap();
                }
                
                // Postconditions
                for p in post {
                    writeln!(spec, "#[ensures({})]", self.pred_to_creusot(p)).unwrap();
                }
                
                // Variant for termination
                if let Some(measure) = decreases {
                    writeln!(spec, "#[variant({})]", self.measure_to_creusot(measure)).unwrap();
                }
                
                spec
            }
            _ => String::new(),
        }
    }

    fn generate_proof_obligations(&self, hir: &HIR) -> Vec<ProofObligation> {
        // Similar to Prusti but with Creusot-specific formatting
        PrustiBackend.generate_proof_obligations(hir)
    }

    fn format_verification_result(&self, result: &VerificationResult) -> String {
        // Use same formatting as Prusti for now
        PrustiBackend.format_verification_result(result)
    }
}

impl CreusotBackend {
    fn pred_to_creusot(&self, pred: &Predicate) -> String {
        // Creusot uses similar syntax to Prusti
        PrustiBackend.pred_to_prusti(pred)
    }

    fn measure_to_creusot(&self, measure: &Measure) -> String {
        PrustiBackend.measure_to_prusti(measure)
    }
}

// Factory function to create verification backend
pub fn create_backend(backend_type: &str) -> Result<Box<dyn VerificationBackend>> {
    match backend_type {
        "prusti" => Ok(Box::new(PrustiBackend)),
        "creusot" => Ok(Box::new(CreusotBackend)),
        _ => Err(anyhow!("Unknown verification backend: {}", backend_type)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prusti_spec_generation() {
        let func = HIR::Function {
            name: Symbol("add".to_string()),
            params: vec![
                (Symbol("x".to_string()), RefinedType::new(BaseType::Int)),
                (Symbol("y".to_string()), RefinedType::new(BaseType::Int)),
            ],
            body: Box::new(HIR::BinOp {
                op: BinOp::Add,
                lhs: Box::new(HIR::Var(Symbol("x".to_string()))),
                rhs: Box::new(HIR::Var(Symbol("y".to_string()))),
            }),
            pre: vec![
                Predicate {
                    name: Symbol("geq".to_string()),
                    args: vec![
                        SMTExpr::Var(Symbol("x".to_string())),
                        SMTExpr::Const(Literal::Int(0)),
                    ],
                },
            ],
            post: vec![
                Predicate {
                    name: Symbol("result_eq".to_string()),
                    args: vec![
                        SMTExpr::App(
                            Symbol("+".to_string()),
                            vec![
                                SMTExpr::Var(Symbol("x".to_string())),
                                SMTExpr::Var(Symbol("y".to_string())),
                            ],
                        ),
                    ],
                },
            ],
            decreases: None,
        };

        let backend = PrustiBackend;
        let spec = backend.generate_spec(&func);
        
        assert!(spec.contains("#[requires("));
        assert!(spec.contains("#[ensures("));
    }
}