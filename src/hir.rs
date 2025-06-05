use derive_more::{Display, From};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display, From, Serialize, Deserialize)]
pub struct Symbol(pub String);

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
    // Variable reference
    Var(Symbol),
    // Literal values
    Lit(Literal),
    // Function application
    App {
        func: Box<HIR>,
        args: Vec<HIR>,
    },
    // Lambda abstraction
    Lambda {
        params: Vec<(Symbol, RefinedType)>,
        body: Box<HIR>,
    },
    // Binary operation
    BinOp {
        op: BinOp,
        lhs: Box<HIR>,
        rhs: Box<HIR>,
    },
    // Constructor application
    Constructor {
        name: Symbol,
        args: Vec<HIR>,
    },
    // Type annotation
    Annot {
        expr: Box<HIR>,
        ty: RefinedType,
    },
    // Composition for optimization
    Compose(Box<HIR>, Box<HIR>),
    // Map operation for fusion
    Map(Box<HIR>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct RefinedType {
    pub base: BaseType,
    pub refinement: Option<SMTExpr>,
    pub ownership: OwnershipKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BaseType {
    Int,
    Bool,
    String,
    Unit,
    Function(Box<BaseType>, Box<BaseType>),
    Constructor(Symbol, Vec<BaseType>),
    TypeVar(Symbol),
    List(Box<BaseType>),
    Tuple(Vec<BaseType>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum SMTExpr {
    Var(Symbol),
    Const(Literal),
    App(Symbol, Vec<SMTExpr>),
    Comparison {
        op: CompOp,
        lhs: Box<SMTExpr>,
        rhs: Box<SMTExpr>,
    },
    And(Vec<SMTExpr>),
    Or(Vec<SMTExpr>),
    Not(Box<SMTExpr>),
    Implies(Box<SMTExpr>, Box<SMTExpr>),
    Forall(Vec<(Symbol, Sort)>, Box<SMTExpr>),
    Exists(Vec<(Symbol, Sort)>, Box<SMTExpr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompOp {
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Sort {
    Int,
    Bool,
    Array(Box<Sort>, Box<Sort>),
    Datatype(Symbol),
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum OwnershipKind {
    Owned,           // T
    Borrowed,        // &T
    MutBorrowed,     // &mut T
    Shared(usize),   // Rc<T> with refcount hint
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(String),
    Unit,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Pattern {
    Var(Symbol),
    Lit(Literal),
    Constructor(Symbol, Vec<Pattern>),
    Wildcard,
    As(Symbol, Box<Pattern>),
}

#[derive(Clone, Debug)]
pub struct Predicate {
    pub name: Symbol,
    pub args: Vec<SMTExpr>,
}

#[derive(Clone, Debug)]
pub struct Measure {
    pub expr: SMTExpr,
    pub decreasing: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExhaustivenessProof {
    pub patterns: Vec<Pattern>,
    pub coverage: CoverageProof,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CoverageProof {
    Complete,
    Partial(Vec<Pattern>), // Missing patterns
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
}

impl RefinedType {
    pub fn new(base: BaseType) -> Self {
        RefinedType {
            base,
            refinement: None,
            ownership: OwnershipKind::Owned,
        }
    }

    pub fn with_refinement(mut self, refinement: SMTExpr) -> Self {
        self.refinement = Some(refinement);
        self
    }

    pub fn with_ownership(mut self, ownership: OwnershipKind) -> Self {
        self.ownership = ownership;
        self
    }

    pub fn to_rust(&self, ownership: &OwnershipKind) -> String {
        let base_str = match &self.base {
            BaseType::Int => "i32".to_string(),
            BaseType::Bool => "bool".to_string(),
            BaseType::String => "String".to_string(),
            BaseType::Unit => "()".to_string(),
            BaseType::Function(from, to) => {
                format!("Box<dyn Fn({}) -> {}>", 
                    from.to_rust_str(), to.to_rust_str())
            }
            BaseType::Constructor(name, args) => {
                if args.is_empty() {
                    name.0.clone()
                } else {
                    format!("{}<{}>", name.0, 
                        args.iter()
                            .map(|t| t.to_rust_str())
                            .collect::<Vec<_>>()
                            .join(", "))
                }
            }
            BaseType::TypeVar(name) => name.0.clone(),
            BaseType::List(inner) => format!("Vec<{}>", inner.to_rust_str()),
            BaseType::Tuple(types) => {
                format!("({})", 
                    types.iter()
                        .map(|t| t.to_rust_str())
                        .collect::<Vec<_>>()
                        .join(", "))
            }
        };

        match ownership {
            OwnershipKind::Owned => base_str,
            OwnershipKind::Borrowed => format!("&{}", base_str),
            OwnershipKind::MutBorrowed => format!("&mut {}", base_str),
            OwnershipKind::Shared(_) => format!("Rc<{}>", base_str),
        }
    }
}

impl BaseType {
    pub fn to_rust_str(&self) -> String {
        match self {
            BaseType::Int => "i32".to_string(),
            BaseType::Bool => "bool".to_string(),
            BaseType::String => "String".to_string(),
            BaseType::Unit => "()".to_string(),
            BaseType::Function(_, _) => "Box<dyn Fn>".to_string(), // Simplified
            BaseType::Constructor(name, _) => name.0.clone(),
            BaseType::TypeVar(name) => name.0.clone(),
            BaseType::List(inner) => format!("Vec<{}>", inner.to_rust_str()),
            BaseType::Tuple(types) => {
                format!("({})", 
                    types.iter()
                        .map(|t| t.to_rust_str())
                        .collect::<Vec<_>>()
                        .join(", "))
            }
        }
    }
}