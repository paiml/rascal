use std::collections::{HashMap, HashSet};
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::algo::dominators::{Dominators, simple_fast};
use petgraph::Direction;
use petgraph::visit::EdgeRef;
use crate::hir::*;
use anyhow::{Result, anyhow};

#[derive(Debug, Clone)]
pub struct UsageEdge {
    pub kind: UsageKind,
    pub count: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UsageKind {
    Read,
    Write,
    Move,
}

#[derive(Debug, Default)]
pub struct UsageStats {
    pub reads: usize,
    pub writes: usize,
    pub moves: usize,
}

pub type OwnershipMap = HashMap<Symbol, OwnershipKind>;

pub struct OwnershipAnalysis {
    usage_graph: DiGraph<Symbol, UsageEdge>,
    symbol_to_node: HashMap<Symbol, NodeIndex>,
    dominators: Option<Dominators<NodeIndex>>,
}

#[derive(Debug, Clone)]
pub enum OwnershipError {
    CyclicMutableBorrow(Vec<Symbol>),
    ConflictingUsage(Symbol, UsageKind, UsageKind),
    UnresolvedOwnership(Symbol),
}

impl OwnershipAnalysis {
    pub fn new() -> Self {
        OwnershipAnalysis {
            usage_graph: DiGraph::new(),
            symbol_to_node: HashMap::new(),
            dominators: None,
        }
    }

    pub fn analyze(&mut self, hir: &HIR) -> Result<OwnershipMap> {
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

    pub fn collect_usages(&mut self, hir: &HIR) {
        match hir {
            HIR::Function { params, body, .. } => {
                // Add parameters to graph
                for (param, _) in params {
                    self.add_symbol(param.clone());
                }
                self.collect_expr_usages(body, UsageKind::Read);
            }
            _ => {}
        }
    }

    fn collect_expr_usages(&mut self, expr: &HIR, context: UsageKind) {
        match expr {
            HIR::Var(sym) => {
                self.add_usage(sym.clone(), context);
            }
            HIR::Let { name, value, body, linear, .. } => {
                self.add_symbol(name.clone());
                self.collect_expr_usages(value, UsageKind::Move);
                if *linear {
                    self.mark_linear(name.clone());
                }
                self.collect_expr_usages(body, context);
            }
            HIR::App { func, args } => {
                self.collect_expr_usages(func, UsageKind::Read);
                for arg in args {
                    self.collect_expr_usages(arg, UsageKind::Read);
                }
            }
            HIR::Lambda { params, body } => {
                for (param, _) in params {
                    self.add_symbol(param.clone());
                }
                self.collect_expr_usages(body, context);
            }
            HIR::Match { scrutinee, arms, .. } => {
                self.collect_expr_usages(scrutinee, UsageKind::Read);
                for (pattern, arm_expr) in arms {
                    self.collect_pattern_bindings(pattern);
                    self.collect_expr_usages(arm_expr, context);
                }
            }
            HIR::BinOp { lhs, rhs, .. } => {
                self.collect_expr_usages(lhs, UsageKind::Read);
                self.collect_expr_usages(rhs, UsageKind::Read);
            }
            HIR::Constructor { args, .. } => {
                for arg in args {
                    self.collect_expr_usages(arg, UsageKind::Move);
                }
            }
            HIR::Annot { expr, .. } => {
                self.collect_expr_usages(expr, context);
            }
            HIR::Compose(f, g) => {
                self.collect_expr_usages(f, UsageKind::Read);
                self.collect_expr_usages(g, UsageKind::Read);
            }
            HIR::Map(f) => {
                self.collect_expr_usages(f, UsageKind::Read);
            }
            HIR::Lit(_) => {}
            HIR::Function { .. } => {} // Nested functions handled separately
        }
    }

    fn collect_pattern_bindings(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Var(sym) => {
                self.add_symbol(sym.clone());
            }
            Pattern::As(sym, inner) => {
                self.add_symbol(sym.clone());
                self.collect_pattern_bindings(inner);
            }
            Pattern::Constructor(_, patterns) => {
                for p in patterns {
                    self.collect_pattern_bindings(p);
                }
            }
            Pattern::Lit(_) | Pattern::Wildcard => {}
        }
    }

    fn add_symbol(&mut self, symbol: Symbol) {
        if !self.symbol_to_node.contains_key(&symbol) {
            let node = self.usage_graph.add_node(symbol.clone());
            self.symbol_to_node.insert(symbol, node);
        }
    }

    fn add_usage(&mut self, symbol: Symbol, kind: UsageKind) {
        if let Some(&node) = self.symbol_to_node.get(&symbol) {
            // Find or create edge representing this usage
            let edges: Vec<_> = self.usage_graph
                .edges_directed(node, Direction::Outgoing)
                .collect();
            
            for edge in edges {
                if edge.weight().kind == kind {
                    // Increment existing usage count
                    if let Some(weight) = self.usage_graph.edge_weight_mut(edge.id()) {
                        weight.count += 1;
                    }
                    return;
                }
            }
            
            // Add new usage edge (self-edge for tracking)
            self.usage_graph.add_edge(node, node, UsageEdge { kind, count: 1 });
        }
    }

    fn mark_linear(&mut self, symbol: Symbol) {
        // Linear variables must be used exactly once
        if let Some(&node) = self.symbol_to_node.get(&symbol) {
            self.usage_graph.add_edge(
                node, 
                node, 
                UsageEdge { kind: UsageKind::Move, count: 1 }
            );
        }
    }

    pub fn compute_dominators(&mut self) {
        if let Some(root) = self.usage_graph.node_indices().next() {
            self.dominators = Some(simple_fast(&self.usage_graph, root));
        }
    }

    pub fn get_usages(&self, sym: &Symbol) -> UsageStats {
        let mut stats = UsageStats::default();
        
        if let Some(&node) = self.symbol_to_node.get(sym) {
            for edge in self.usage_graph.edges(node) {
                match edge.weight().kind {
                    UsageKind::Read => stats.reads += edge.weight().count,
                    UsageKind::Write => stats.writes += edge.weight().count,
                    UsageKind::Move => stats.moves += edge.weight().count,
                }
            }
        }
        
        stats
    }

    pub fn infer_ownership_kind(&self, sym: &Symbol) -> OwnershipKind {
        let usages = self.get_usages(sym);
        
        match (usages.reads, usages.writes, usages.moves) {
            (n, 0, 0) if n > 2 => OwnershipKind::Shared(n),
            (_, 0, 0) => OwnershipKind::Borrowed,
            (_, n, 0) if n > 0 => OwnershipKind::MutBorrowed,
            (_, _, n) if n > 0 => OwnershipKind::Owned,
            _ => OwnershipKind::Owned, // Conservative default
        }
    }

    pub fn verify_acyclic_borrows(&self, ownership: &OwnershipMap) -> Result<()> {
        // Build a graph of mutable borrows
        let mut borrow_graph = DiGraph::<Symbol, ()>::new();
        let mut sym_to_node = HashMap::new();
        
        // Add nodes for mutable borrows
        for (sym, kind) in ownership {
            if matches!(kind, OwnershipKind::MutBorrowed) {
                let node = borrow_graph.add_node(sym.clone());
                sym_to_node.insert(sym.clone(), node);
            }
        }
        
        // Add edges based on usage dependencies
        for (sym, &node) in &sym_to_node {
            if let Some(&usage_node) = self.symbol_to_node.get(sym) {
                for neighbor in self.usage_graph.neighbors(usage_node) {
                    let neighbor_sym = &self.usage_graph[neighbor];
                    if let Some(&neighbor_borrow_node) = sym_to_node.get(neighbor_sym) {
                        borrow_graph.add_edge(node, neighbor_borrow_node, ());
                    }
                }
            }
        }
        
        // Check for cycles
        if petgraph::algo::is_cyclic_directed(&borrow_graph) {
            // Find a cycle for error reporting
            let cycle = self.find_cycle_in_borrows(&borrow_graph, &sym_to_node);
            return Err(anyhow!(
                "Cyclic mutable borrow detected: {:?}", 
                OwnershipError::CyclicMutableBorrow(cycle)
            ));
        }
        
        Ok(())
    }

    fn find_cycle_in_borrows(
        &self,
        graph: &DiGraph<Symbol, ()>,
        _sym_to_node: &HashMap<Symbol, NodeIndex>
    ) -> Vec<Symbol> {
        // Simple DFS to find a cycle
        let mut visited = HashSet::new();
        let mut path = Vec::new();
        
        for node in graph.node_indices() {
            if visited.contains(&node) {
                continue;
            }
            
            if let Some(cycle) = self.dfs_find_cycle(graph, node, &mut visited, &mut path) {
                return cycle;
            }
        }
        
        vec![]
    }

    fn dfs_find_cycle(
        &self,
        graph: &DiGraph<Symbol, ()>,
        node: NodeIndex,
        visited: &mut HashSet<NodeIndex>,
        path: &mut Vec<NodeIndex>
    ) -> Option<Vec<Symbol>> {
        if path.contains(&node) {
            // Found a cycle
            let cycle_start = path.iter().position(|&n| n == node).unwrap();
            return Some(
                path[cycle_start..]
                    .iter()
                    .map(|&n| graph[n].clone())
                    .collect()
            );
        }
        
        if visited.contains(&node) {
            return None;
        }
        
        visited.insert(node);
        path.push(node);
        
        for neighbor in graph.neighbors(node) {
            if let Some(cycle) = self.dfs_find_cycle(graph, neighbor, visited, path) {
                return Some(cycle);
            }
        }
        
        path.pop();
        None
    }
}

// Public API functions
pub fn infer_ownership(hir: &HIR) -> Result<HIR> {
    let mut analysis = OwnershipAnalysis::new();
    let ownership_map = analysis.analyze(hir)?;
    Ok(annotate_ownership(hir, &ownership_map))
}

pub fn collect_usage(hir: &HIR) -> HashMap<Symbol, UsageStats> {
    let mut analysis = OwnershipAnalysis::new();
    analysis.collect_usages(hir);
    
    let mut usage_map = HashMap::new();
    for (sym, _) in &analysis.symbol_to_node {
        usage_map.insert(sym.clone(), analysis.get_usages(sym));
    }
    
    usage_map
}

pub fn annotate_ownership(hir: &HIR, ownership_map: &OwnershipMap) -> HIR {
    match hir {
        HIR::Function { name, params, body, pre, post, decreases } => {
            let annotated_params = params.iter()
                .map(|(sym, ty)| {
                    let ownership = ownership_map.get(sym)
                        .cloned()
                        .unwrap_or(OwnershipKind::Owned);
                    (sym.clone(), ty.clone().with_ownership(ownership))
                })
                .collect();
            
            HIR::Function {
                name: name.clone(),
                params: annotated_params,
                body: Box::new(annotate_ownership(body, ownership_map)),
                pre: pre.clone(),
                post: post.clone(),
                decreases: decreases.clone(),
            }
        }
        HIR::Let { name, ty, value, body, linear } => {
            let ownership = ownership_map.get(name)
                .cloned()
                .unwrap_or(OwnershipKind::Owned);
            
            HIR::Let {
                name: name.clone(),
                ty: ty.clone().with_ownership(ownership),
                value: Box::new(annotate_ownership(value, ownership_map)),
                body: Box::new(annotate_ownership(body, ownership_map)),
                linear: *linear,
            }
        }
        HIR::Match { scrutinee, arms, exhaustive } => {
            HIR::Match {
                scrutinee: Box::new(annotate_ownership(scrutinee, ownership_map)),
                arms: arms.iter()
                    .map(|(pat, expr)| {
                        (pat.clone(), Box::new(annotate_ownership(expr, ownership_map)))
                    })
                    .collect(),
                exhaustive: exhaustive.clone(),
            }
        }
        HIR::App { func, args } => {
            HIR::App {
                func: Box::new(annotate_ownership(func, ownership_map)),
                args: args.iter()
                    .map(|arg| annotate_ownership(arg, ownership_map))
                    .collect(),
            }
        }
        HIR::Lambda { params, body } => {
            let annotated_params = params.iter()
                .map(|(sym, ty)| {
                    let ownership = ownership_map.get(sym)
                        .cloned()
                        .unwrap_or(OwnershipKind::Owned);
                    (sym.clone(), ty.clone().with_ownership(ownership))
                })
                .collect();
            
            HIR::Lambda {
                params: annotated_params,
                body: Box::new(annotate_ownership(body, ownership_map)),
            }
        }
        HIR::BinOp { op, lhs, rhs } => {
            HIR::BinOp {
                op: *op,
                lhs: Box::new(annotate_ownership(lhs, ownership_map)),
                rhs: Box::new(annotate_ownership(rhs, ownership_map)),
            }
        }
        HIR::Constructor { name, args } => {
            HIR::Constructor {
                name: name.clone(),
                args: args.iter()
                    .map(|arg| annotate_ownership(arg, ownership_map))
                    .collect(),
            }
        }
        HIR::Annot { expr, ty } => {
            HIR::Annot {
                expr: Box::new(annotate_ownership(expr, ownership_map)),
                ty: ty.clone(),
            }
        }
        HIR::Compose(f, g) => {
            HIR::Compose(
                Box::new(annotate_ownership(f, ownership_map)),
                Box::new(annotate_ownership(g, ownership_map))
            )
        }
        HIR::Map(f) => {
            HIR::Map(Box::new(annotate_ownership(f, ownership_map)))
        }
        HIR::Var(_) | HIR::Lit(_) => hir.clone(),
    }
}