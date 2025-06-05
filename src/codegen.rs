use crate::hir::*;
use crate::ownership::OwnershipMap;
use anyhow::{anyhow, Result};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

pub struct CodeGenerator {
    indent_level: usize,
    lifetime_counter: usize,
    generated_lifetimes: HashMap<String, String>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            indent_level: 0,
            lifetime_counter: 0,
            generated_lifetimes: HashMap::new(),
        }
    }

    fn indent(&self) -> String {
        "    ".repeat(self.indent_level)
    }

    fn fresh_lifetime(&mut self) -> String {
        self.lifetime_counter += 1;
        format!("'a{}", self.lifetime_counter)
    }

    pub fn generate_rust(&mut self, hir: &HIR, ownership: &OwnershipMap) -> Result<String> {
        match hir {
            HIR::Function {
                name,
                params,
                body,
                pre,
                post,
                decreases,
            } => self.generate_function(name, params, body, pre, post, decreases, ownership),
            _ => Err(anyhow!("Can only generate code for functions at top level")),
        }
    }

    fn generate_function(
        &mut self,
        name: &Symbol,
        params: &[(Symbol, RefinedType)],
        body: &HIR,
        pre: &[Predicate],
        post: &[Predicate],
        decreases: &Option<Measure>,
        ownership: &OwnershipMap,
    ) -> Result<String> {
        let mut out = String::new();

        // Generate verification annotations if present
        if !pre.is_empty() || !post.is_empty() || decreases.is_some() {
            writeln!(out, "{}// Verification conditions:", self.indent())?;
            for pred in pre {
                writeln!(
                    out,
                    "{}// @requires {}",
                    self.indent(),
                    self.format_predicate(pred)
                )?;
            }
            for pred in post {
                writeln!(
                    out,
                    "{}// @ensures {}",
                    self.indent(),
                    self.format_predicate(pred)
                )?;
            }
            if let Some(measure) = decreases {
                writeln!(
                    out,
                    "{}// @decreases {}",
                    self.indent(),
                    self.format_measure(measure)
                )?;
            }
            writeln!(out)?;
        }

        // Generate function signature
        write!(out, "{}pub fn {}", self.indent(), name.0)?;

        // Generate lifetime parameters
        let lifetimes = self.infer_lifetimes(params, body, ownership);
        if !lifetimes.is_empty() {
            write!(out, "<{}>", lifetimes.join(", "))?;
        }

        // Generate parameters
        write!(out, "(")?;
        for (i, (param, ty)) in params.iter().enumerate() {
            if i > 0 {
                write!(out, ", ")?;
            }
            let ownership_kind = ownership
                .get(param)
                .cloned()
                .unwrap_or(OwnershipKind::Owned);
            write!(out, "{}: {}", param.0, ty.to_rust(&ownership_kind))?;
        }
        write!(out, ")")?;

        // Generate return type
        let return_type = self.infer_return_type(body);
        write!(out, " -> {}", return_type)?;

        // Generate body
        writeln!(out, " {{")?;
        self.indent_level += 1;
        let body_code = self.generate_expr(body, ownership)?;
        write!(out, "{}", body_code)?;
        self.indent_level -= 1;
        writeln!(out, "{}}}", self.indent())?;

        Ok(out)
    }

    fn generate_expr(&mut self, expr: &HIR, ownership: &OwnershipMap) -> Result<String> {
        match expr {
            HIR::Var(sym) => Ok(format!("{}{}\n", self.indent(), sym.0)),

            HIR::Lit(lit) => Ok(format!("{}{}\n", self.indent(), self.format_literal(lit))),

            HIR::Let {
                name,
                ty,
                value,
                body,
                linear,
            } => {
                let mut out = String::new();
                let ownership_kind = ownership.get(name).cloned().unwrap_or(OwnershipKind::Owned);

                write!(out, "{}let ", self.indent())?;
                if !matches!(ownership_kind, OwnershipKind::MutBorrowed) {
                    write!(out, "mut ")?;
                }
                write!(out, "{}: {} = ", name.0, ty.to_rust(&ownership_kind))?;

                // Generate value inline if simple, otherwise on new line
                let value_str = self.generate_expr_inline(value, ownership)?;
                if value_str.lines().count() == 1 && value_str.len() < 60 {
                    writeln!(out, "{};", value_str.trim())?;
                } else {
                    writeln!(out)?;
                    self.indent_level += 1;
                    write!(out, "{}", self.generate_expr(value, ownership)?)?;
                    self.indent_level -= 1;
                    writeln!(out, "{};", self.indent())?;
                }

                write!(out, "{}", self.generate_expr(body, ownership)?)?;
                Ok(out)
            }

            HIR::App { func, args } => {
                let mut out = String::new();
                let func_str = self.generate_expr_inline(func, ownership)?;

                write!(out, "{}{}(", self.indent(), func_str.trim())?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    let arg_str = self.generate_expr_inline(arg, ownership)?;
                    write!(out, "{}", arg_str.trim())?;
                }
                writeln!(out, ")")?;
                Ok(out)
            }

            HIR::Lambda { params, body } => {
                let mut out = String::new();
                write!(out, "{}|", self.indent())?;

                for (i, (param, ty)) in params.iter().enumerate() {
                    if i > 0 {
                        write!(out, ", ")?;
                    }
                    let ownership_kind = ownership
                        .get(param)
                        .cloned()
                        .unwrap_or(OwnershipKind::Owned);
                    write!(out, "{}: {}", param.0, ty.to_rust(&ownership_kind))?;
                }

                writeln!(out, "| {{")?;
                self.indent_level += 1;
                write!(out, "{}", self.generate_expr(body, ownership)?)?;
                self.indent_level -= 1;
                writeln!(out, "{}}}", self.indent())?;
                Ok(out)
            }

            HIR::Match {
                scrutinee, arms, ..
            } => {
                let mut out = String::new();
                let scrutinee_str = self.generate_expr_inline(scrutinee, ownership)?;
                writeln!(out, "{}match {} {{", self.indent(), scrutinee_str.trim())?;

                self.indent_level += 1;
                for (pattern, expr) in arms {
                    write!(out, "{}{} => ", self.indent(), self.format_pattern(pattern))?;

                    // Check if expression is simple enough for inline
                    let expr_str = self.generate_expr_inline(expr, ownership)?;
                    if expr_str.lines().count() == 1 && expr_str.len() < 40 {
                        writeln!(out, "{},", expr_str.trim())?;
                    } else {
                        writeln!(out, "{{")?;
                        self.indent_level += 1;
                        write!(out, "{}", self.generate_expr(expr, ownership)?)?;
                        self.indent_level -= 1;
                        writeln!(out, "{}}},", self.indent())?;
                    }
                }
                self.indent_level -= 1;

                writeln!(out, "{}}}", self.indent())?;
                Ok(out)
            }

            HIR::BinOp { op, lhs, rhs } => {
                let lhs_str = self.generate_expr_inline(lhs, ownership)?;
                let rhs_str = self.generate_expr_inline(rhs, ownership)?;
                Ok(format!(
                    "{}{} {} {}\n",
                    self.indent(),
                    lhs_str.trim(),
                    self.format_binop(op),
                    rhs_str.trim()
                ))
            }

            HIR::Constructor { name, args } => {
                let mut out = String::new();
                write!(out, "{}{}", self.indent(), name.0)?;

                if !args.is_empty() {
                    write!(out, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(out, ", ")?;
                        }
                        let arg_str = self.generate_expr_inline(arg, ownership)?;
                        write!(out, "{}", arg_str.trim())?;
                    }
                    write!(out, ")")?;
                }
                writeln!(out)?;
                Ok(out)
            }

            HIR::Annot { expr, ty } => {
                let mut out = String::new();
                write!(out, "{}(", self.indent())?;
                let expr_str = self.generate_expr_inline(expr, ownership)?;
                write!(out, "{}", expr_str.trim())?;
                writeln!(out, " as {})", ty.base.to_rust_str())?;
                Ok(out)
            }

            _ => Err(anyhow!("Code generation not implemented for: {:?}", expr)),
        }
    }

    fn generate_expr_inline(&mut self, expr: &HIR, ownership: &OwnershipMap) -> Result<String> {
        // Generate expression without indentation for inline use
        let saved_indent = self.indent_level;
        self.indent_level = 0;
        let result = self.generate_expr(expr, ownership)?;
        self.indent_level = saved_indent;
        Ok(result.trim().to_string())
    }

    fn format_literal(&self, lit: &Literal) -> String {
        match lit {
            Literal::Int(n) => n.to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Unit => "()".to_string(),
        }
    }

    fn format_pattern(&self, pattern: &Pattern) -> String {
        match pattern {
            Pattern::Var(sym) => sym.0.clone(),
            Pattern::Lit(lit) => self.format_literal(lit),
            Pattern::Constructor(name, patterns) => {
                if patterns.is_empty() {
                    name.0.clone()
                } else {
                    format!(
                        "{}({})",
                        name.0,
                        patterns
                            .iter()
                            .map(|p| self.format_pattern(p))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            Pattern::Wildcard => "_".to_string(),
            Pattern::As(sym, pattern) => {
                format!("{} @ {}", sym.0, self.format_pattern(pattern))
            }
        }
    }

    fn format_binop(&self, op: &BinOp) -> &'static str {
        match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::Lt => "<",
            BinOp::Leq => "<=",
            BinOp::Gt => ">",
            BinOp::Geq => ">=",
            BinOp::Eq => "==",
            BinOp::Neq => "!=",
        }
    }

    fn format_predicate(&self, pred: &Predicate) -> String {
        format!(
            "{}({})",
            pred.name.0,
            pred.args
                .iter()
                .map(|arg| self.format_smt_expr(arg))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    fn format_smt_expr(&self, expr: &SMTExpr) -> String {
        match expr {
            SMTExpr::Var(sym) => sym.0.clone(),
            SMTExpr::Const(lit) => self.format_literal(lit),
            SMTExpr::Comparison { op, lhs, rhs } => {
                format!(
                    "{} {} {}",
                    self.format_smt_expr(lhs),
                    self.format_comp_op(op),
                    self.format_smt_expr(rhs)
                )
            }
            _ => "...".to_string(), // TODO: Implement other cases
        }
    }

    fn format_comp_op(&self, op: &CompOp) -> &'static str {
        match op {
            CompOp::Eq => "==",
            CompOp::Neq => "!=",
            CompOp::Lt => "<",
            CompOp::Leq => "<=",
            CompOp::Gt => ">",
            CompOp::Geq => ">=",
        }
    }

    fn format_measure(&self, measure: &Measure) -> String {
        self.format_smt_expr(&measure.expr)
    }

    fn infer_lifetimes(
        &mut self,
        params: &[(Symbol, RefinedType)],
        _body: &HIR,
        ownership: &OwnershipMap,
    ) -> Vec<String> {
        let mut lifetimes = Vec::new();
        let mut seen = HashSet::new();

        for (param, _) in params {
            if let Some(kind) = ownership.get(param) {
                match kind {
                    OwnershipKind::Borrowed | OwnershipKind::MutBorrowed => {
                        let lifetime = self.fresh_lifetime();
                        if seen.insert(lifetime.clone()) {
                            lifetimes.push(lifetime);
                        }
                    }
                    _ => {}
                }
            }
        }

        lifetimes
    }

    fn infer_return_type(&self, body: &HIR) -> String {
        // Simple type inference based on body
        match body {
            HIR::Lit(Literal::Int(_)) => "i32".to_string(),
            HIR::Lit(Literal::Bool(_)) => "bool".to_string(),
            HIR::Lit(Literal::String(_)) => "String".to_string(),
            HIR::Lit(Literal::Unit) => "()".to_string(),
            HIR::BinOp { op, .. } => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => "i32".to_string(),
                BinOp::And
                | BinOp::Or
                | BinOp::Lt
                | BinOp::Leq
                | BinOp::Gt
                | BinOp::Geq
                | BinOp::Eq
                | BinOp::Neq => "bool".to_string(),
            },
            _ => "()".to_string(), // TODO: Proper type inference
        }
    }
}

// Public API
pub fn generate_rust(hir: &HIR, ownership: &OwnershipMap) -> Result<String> {
    let mut generator = CodeGenerator::new();
    generator.generate_rust(hir, ownership)
}

pub fn generate_module(functions: &[HIR], ownership: &OwnershipMap) -> Result<String> {
    let mut generator = CodeGenerator::new();
    let mut output = String::new();

    // Add module header
    writeln!(output, "// Generated by Rascal-Light")?;
    writeln!(output, "#![allow(dead_code)]")?;
    writeln!(output, "#![allow(unused_variables)]")?;
    writeln!(output)?;

    // Generate each function
    for func in functions {
        let func_code = generator.generate_rust(func, ownership)?;
        writeln!(output, "{}", func_code)?;
    }

    Ok(output)
}
