use crate::hir::*;
use std::collections::HashMap;

pub struct OptimizationPass {
    fusion_rules: Vec<FusionRule>,
    inline_threshold: usize,
    function_sizes: HashMap<Symbol, usize>,
}

#[derive(Clone)]
pub struct FusionRule {
    #[allow(dead_code)]
    name: String,
    matcher: fn(&HIR) -> Option<HIR>,
}

impl OptimizationPass {
    pub fn new() -> Self {
        let mut pass = OptimizationPass {
            fusion_rules: Vec::new(),
            inline_threshold: 20,
            function_sizes: HashMap::new(),
        };

        // Register fusion rules
        pass.register_fusion_rules();
        pass
    }

    fn register_fusion_rules(&mut self) {
        // map f . map g = map (f . g)
        self.fusion_rules.push(FusionRule {
            name: "map-map-fusion".to_string(),
            matcher: |hir| match hir {
                HIR::Compose(f, g) => {
                    if let (HIR::Map(inner_f), HIR::Map(inner_g)) = (f.as_ref(), g.as_ref()) {
                        Some(HIR::Map(Box::new(HIR::Compose(
                            inner_f.clone(),
                            inner_g.clone(),
                        ))))
                    } else {
                        None
                    }
                }
                _ => None,
            },
        });

        // TODO: Add more fusion rules
    }

    pub fn optimize(&mut self, hir: HIR) -> HIR {
        // Multiple optimization passes
        let hir = self.inline_small_functions(hir);
        let hir = self.fuse_list_operations(hir);
        let hir = self.eliminate_unnecessary_clones(hir);
        let hir = self.tail_call_optimization(hir);
        let hir = self.common_subexpression_elimination(hir);
        hir
    }

    pub fn inline_small_functions(&mut self, hir: HIR) -> HIR {
        // First pass: collect function sizes
        self.collect_function_sizes(&hir);

        // Second pass: inline small functions
        self.inline_functions_recursive(hir)
    }

    fn collect_function_sizes(&mut self, hir: &HIR) {
        match hir {
            HIR::Function { name, body, .. } => {
                let size = self.calculate_hir_size(body);
                self.function_sizes.insert(name.clone(), size);
            }
            _ => {}
        }
    }

    fn calculate_hir_size(&self, hir: &HIR) -> usize {
        match hir {
            HIR::Var(_) | HIR::Lit(_) => 1,
            HIR::Let { value, body, .. } => {
                1 + self.calculate_hir_size(value) + self.calculate_hir_size(body)
            }
            HIR::App { func, args } => {
                1 + self.calculate_hir_size(func)
                    + args
                        .iter()
                        .map(|a| self.calculate_hir_size(a))
                        .sum::<usize>()
            }
            HIR::Lambda { body, .. } => 1 + self.calculate_hir_size(body),
            HIR::Match {
                scrutinee, arms, ..
            } => {
                1 + self.calculate_hir_size(scrutinee)
                    + arms
                        .iter()
                        .map(|(_, e)| self.calculate_hir_size(e))
                        .sum::<usize>()
            }
            HIR::BinOp { lhs, rhs, .. } => {
                1 + self.calculate_hir_size(lhs) + self.calculate_hir_size(rhs)
            }
            HIR::Constructor { args, .. } => {
                1 + args
                    .iter()
                    .map(|a| self.calculate_hir_size(a))
                    .sum::<usize>()
            }
            HIR::Annot { expr, .. } => self.calculate_hir_size(expr),
            HIR::Compose(f, g) => 1 + self.calculate_hir_size(f) + self.calculate_hir_size(g),
            HIR::Map(f) => 1 + self.calculate_hir_size(f),
            HIR::Function { body, .. } => self.calculate_hir_size(body),
        }
    }

    fn inline_functions_recursive(&self, hir: HIR) -> HIR {
        match hir {
            HIR::App { func, args } if matches!(func.as_ref(), HIR::Var(_)) => {
                let name = if let HIR::Var(name) = func.as_ref() {
                    name
                } else {
                    unreachable!()
                };
                // Check if function should be inlined
                if let Some(&size) = self.function_sizes.get(&name) {
                    if size <= self.inline_threshold {
                        // TODO: Perform actual inlining
                        // For now, just return the original
                        HIR::App {
                            func: Box::new(HIR::Var(name.clone())),
                            args: args
                                .into_iter()
                                .map(|a| self.inline_functions_recursive(a))
                                .collect(),
                        }
                    } else {
                        HIR::App {
                            func: Box::new(HIR::Var(name.clone())),
                            args: args
                                .into_iter()
                                .map(|a| self.inline_functions_recursive(a))
                                .collect(),
                        }
                    }
                } else {
                    HIR::App {
                        func: Box::new(HIR::Var(name.clone())),
                        args: args
                            .into_iter()
                            .map(|a| self.inline_functions_recursive(a))
                            .collect(),
                    }
                }
            }
            HIR::Let {
                name,
                ty,
                value,
                body,
                linear,
            } => HIR::Let {
                name,
                ty,
                value: Box::new(self.inline_functions_recursive(*value)),
                body: Box::new(self.inline_functions_recursive(*body)),
                linear,
            },
            HIR::Match {
                scrutinee,
                arms,
                exhaustive,
            } => HIR::Match {
                scrutinee: Box::new(self.inline_functions_recursive(*scrutinee)),
                arms: arms
                    .into_iter()
                    .map(|(p, e)| (p, Box::new(self.inline_functions_recursive(*e))))
                    .collect(),
                exhaustive,
            },
            HIR::Lambda { params, body } => HIR::Lambda {
                params,
                body: Box::new(self.inline_functions_recursive(*body)),
            },
            HIR::BinOp { op, lhs, rhs } => HIR::BinOp {
                op,
                lhs: Box::new(self.inline_functions_recursive(*lhs)),
                rhs: Box::new(self.inline_functions_recursive(*rhs)),
            },
            HIR::Constructor { name, args } => HIR::Constructor {
                name,
                args: args
                    .into_iter()
                    .map(|a| self.inline_functions_recursive(a))
                    .collect(),
            },
            HIR::Annot { expr, ty } => HIR::Annot {
                expr: Box::new(self.inline_functions_recursive(*expr)),
                ty,
            },
            HIR::Compose(f, g) => HIR::Compose(
                Box::new(self.inline_functions_recursive(*f)),
                Box::new(self.inline_functions_recursive(*g)),
            ),
            HIR::Map(f) => HIR::Map(Box::new(self.inline_functions_recursive(*f))),
            HIR::Function {
                name,
                params,
                body,
                pre,
                post,
                decreases,
            } => HIR::Function {
                name,
                params,
                body: Box::new(self.inline_functions_recursive(*body)),
                pre,
                post,
                decreases,
            },
            _ => hir,
        }
    }

    pub fn fuse_list_operations(&self, hir: HIR) -> HIR {
        // Apply fusion rules
        for rule in &self.fusion_rules {
            if let Some(fused) = (rule.matcher)(&hir) {
                // Recursively apply fusion to the result
                return self.fuse_list_operations(fused);
            }
        }

        // If no rule matches, recursively process children
        match hir {
            HIR::Let {
                name,
                ty,
                value,
                body,
                linear,
            } => HIR::Let {
                name,
                ty,
                value: Box::new(self.fuse_list_operations(*value)),
                body: Box::new(self.fuse_list_operations(*body)),
                linear,
            },
            HIR::App { func, args } => HIR::App {
                func: Box::new(self.fuse_list_operations(*func)),
                args: args
                    .into_iter()
                    .map(|a| self.fuse_list_operations(a))
                    .collect(),
            },
            HIR::Match {
                scrutinee,
                arms,
                exhaustive,
            } => HIR::Match {
                scrutinee: Box::new(self.fuse_list_operations(*scrutinee)),
                arms: arms
                    .into_iter()
                    .map(|(p, e)| (p, Box::new(self.fuse_list_operations(*e))))
                    .collect(),
                exhaustive,
            },
            HIR::Lambda { params, body } => HIR::Lambda {
                params,
                body: Box::new(self.fuse_list_operations(*body)),
            },
            HIR::BinOp { op, lhs, rhs } => HIR::BinOp {
                op,
                lhs: Box::new(self.fuse_list_operations(*lhs)),
                rhs: Box::new(self.fuse_list_operations(*rhs)),
            },
            HIR::Constructor { name, args } => HIR::Constructor {
                name,
                args: args
                    .into_iter()
                    .map(|a| self.fuse_list_operations(a))
                    .collect(),
            },
            HIR::Annot { expr, ty } => HIR::Annot {
                expr: Box::new(self.fuse_list_operations(*expr)),
                ty,
            },
            HIR::Compose(f, g) => HIR::Compose(
                Box::new(self.fuse_list_operations(*f)),
                Box::new(self.fuse_list_operations(*g)),
            ),
            HIR::Map(f) => HIR::Map(Box::new(self.fuse_list_operations(*f))),
            HIR::Function {
                name,
                params,
                body,
                pre,
                post,
                decreases,
            } => HIR::Function {
                name,
                params,
                body: Box::new(self.fuse_list_operations(*body)),
                pre,
                post,
                decreases,
            },
            HIR::Var(_) | HIR::Lit(_) => hir,
        }
    }

    pub fn eliminate_unnecessary_clones(&self, hir: HIR) -> HIR {
        // TODO: Implement clone elimination based on ownership analysis
        // For now, just traverse the HIR
        match hir {
            HIR::Let {
                name,
                ty,
                value,
                body,
                linear,
            } => HIR::Let {
                name,
                ty,
                value: Box::new(self.eliminate_unnecessary_clones(*value)),
                body: Box::new(self.eliminate_unnecessary_clones(*body)),
                linear,
            },
            HIR::App { func, args } => HIR::App {
                func: Box::new(self.eliminate_unnecessary_clones(*func)),
                args: args
                    .into_iter()
                    .map(|a| self.eliminate_unnecessary_clones(a))
                    .collect(),
            },
            HIR::Match {
                scrutinee,
                arms,
                exhaustive,
            } => HIR::Match {
                scrutinee: Box::new(self.eliminate_unnecessary_clones(*scrutinee)),
                arms: arms
                    .into_iter()
                    .map(|(p, e)| (p, Box::new(self.eliminate_unnecessary_clones(*e))))
                    .collect(),
                exhaustive,
            },
            HIR::Lambda { params, body } => HIR::Lambda {
                params,
                body: Box::new(self.eliminate_unnecessary_clones(*body)),
            },
            HIR::BinOp { op, lhs, rhs } => HIR::BinOp {
                op,
                lhs: Box::new(self.eliminate_unnecessary_clones(*lhs)),
                rhs: Box::new(self.eliminate_unnecessary_clones(*rhs)),
            },
            HIR::Constructor { name, args } => HIR::Constructor {
                name,
                args: args
                    .into_iter()
                    .map(|a| self.eliminate_unnecessary_clones(a))
                    .collect(),
            },
            HIR::Annot { expr, ty } => HIR::Annot {
                expr: Box::new(self.eliminate_unnecessary_clones(*expr)),
                ty,
            },
            HIR::Compose(f, g) => HIR::Compose(
                Box::new(self.eliminate_unnecessary_clones(*f)),
                Box::new(self.eliminate_unnecessary_clones(*g)),
            ),
            HIR::Map(f) => HIR::Map(Box::new(self.eliminate_unnecessary_clones(*f))),
            HIR::Function {
                name,
                params,
                body,
                pre,
                post,
                decreases,
            } => HIR::Function {
                name,
                params,
                body: Box::new(self.eliminate_unnecessary_clones(*body)),
                pre,
                post,
                decreases,
            },
            HIR::Var(_) | HIR::Lit(_) => hir,
        }
    }

    pub fn tail_call_optimization(&self, hir: HIR) -> HIR {
        match hir {
            HIR::Function {
                name,
                params,
                body,
                pre,
                post,
                decreases,
            } => HIR::Function {
                name: name.clone(),
                params,
                body: Box::new(self.optimize_tail_calls(*body, &name)),
                pre,
                post,
                decreases,
            },
            _ => hir,
        }
    }

    fn optimize_tail_calls(&self, hir: HIR, func_name: &Symbol) -> HIR {
        match hir {
            // Direct tail call
            HIR::App { func, args } if matches!(func.as_ref(), HIR::Var(name) if name == func_name) =>
            {
                // TODO: Transform to loop
                HIR::App { func, args }
            }
            // Tail call in match arm
            HIR::Match {
                scrutinee,
                arms,
                exhaustive,
            } => HIR::Match {
                scrutinee,
                arms: arms
                    .into_iter()
                    .map(|(p, e)| (p, Box::new(self.optimize_tail_calls(*e, func_name))))
                    .collect(),
                exhaustive,
            },
            // Tail call in let body
            HIR::Let {
                name,
                ty,
                value,
                body,
                linear,
            } => HIR::Let {
                name,
                ty,
                value,
                body: Box::new(self.optimize_tail_calls(*body, func_name)),
                linear,
            },
            _ => hir,
        }
    }

    pub fn common_subexpression_elimination(&self, hir: HIR) -> HIR {
        let mut cse_map: HashMap<String, Symbol> = HashMap::new();
        let mut counter = 0;
        self.cse_recursive(hir, &mut cse_map, &mut counter)
    }

    fn cse_recursive(
        &self,
        hir: HIR,
        cse_map: &mut HashMap<String, Symbol>,
        counter: &mut usize,
    ) -> HIR {
        // Hash the expression
        let expr_hash = format!("{:?}", hir); // Simple hash for now

        // Check if we've seen this expression before
        if let Some(var_name) = cse_map.get(&expr_hash) {
            if matches!(hir, HIR::BinOp { .. } | HIR::App { .. }) {
                return HIR::Var(var_name.clone());
            }
        }

        match hir {
            HIR::Let {
                name,
                ty,
                value,
                body,
                linear,
            } => {
                let opt_value = self.cse_recursive(*value, cse_map, counter);
                let opt_body = self.cse_recursive(*body, cse_map, counter);
                HIR::Let {
                    name,
                    ty,
                    value: Box::new(opt_value),
                    body: Box::new(opt_body),
                    linear,
                }
            }
            HIR::BinOp { op, lhs, rhs } => {
                let opt_lhs = self.cse_recursive(*lhs, cse_map, counter);
                let opt_rhs = self.cse_recursive(*rhs, cse_map, counter);

                let expr = HIR::BinOp {
                    op,
                    lhs: Box::new(opt_lhs.clone()),
                    rhs: Box::new(opt_rhs.clone()),
                };

                // If this is a complex expression, consider extracting it
                if self.is_complex_expr(&opt_lhs) || self.is_complex_expr(&opt_rhs) {
                    *counter += 1;
                    let var_name = Symbol(format!("_cse{}", counter));
                    cse_map.insert(format!("{:?}", expr), var_name.clone());
                }

                expr
            }
            HIR::App { func, args } => HIR::App {
                func: Box::new(self.cse_recursive(*func, cse_map, counter)),
                args: args
                    .into_iter()
                    .map(|a| self.cse_recursive(a, cse_map, counter))
                    .collect(),
            },
            HIR::Match {
                scrutinee,
                arms,
                exhaustive,
            } => HIR::Match {
                scrutinee: Box::new(self.cse_recursive(*scrutinee, cse_map, counter)),
                arms: arms
                    .into_iter()
                    .map(|(p, e)| (p, Box::new(self.cse_recursive(*e, cse_map, counter))))
                    .collect(),
                exhaustive,
            },
            HIR::Lambda { params, body } => HIR::Lambda {
                params,
                body: Box::new(self.cse_recursive(*body, cse_map, counter)),
            },
            HIR::Constructor { name, args } => HIR::Constructor {
                name,
                args: args
                    .into_iter()
                    .map(|a| self.cse_recursive(a, cse_map, counter))
                    .collect(),
            },
            HIR::Annot { expr, ty } => HIR::Annot {
                expr: Box::new(self.cse_recursive(*expr, cse_map, counter)),
                ty,
            },
            HIR::Compose(f, g) => HIR::Compose(
                Box::new(self.cse_recursive(*f, cse_map, counter)),
                Box::new(self.cse_recursive(*g, cse_map, counter)),
            ),
            HIR::Map(f) => HIR::Map(Box::new(self.cse_recursive(*f, cse_map, counter))),
            HIR::Function {
                name,
                params,
                body,
                pre,
                post,
                decreases,
            } => {
                // New scope for function
                let mut local_cse = HashMap::new();
                HIR::Function {
                    name,
                    params,
                    body: Box::new(self.cse_recursive(*body, &mut local_cse, counter)),
                    pre,
                    post,
                    decreases,
                }
            }
            HIR::Var(_) | HIR::Lit(_) => hir,
        }
    }

    fn is_complex_expr(&self, hir: &HIR) -> bool {
        match hir {
            HIR::BinOp { .. } | HIR::App { .. } => true,
            _ => false,
        }
    }
}

// Public API
pub fn optimize(hir: HIR) -> HIR {
    let mut optimizer = OptimizationPass::new();
    optimizer.optimize(hir)
}
