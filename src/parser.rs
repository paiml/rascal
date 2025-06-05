use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1, digit1, char},
    combinator::{map, opt, recognize, value},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, tuple, pair},
};
use std::collections::HashMap;
use crate::hir::*;
use anyhow::{Result, anyhow};

pub struct Parser {
    type_aliases: HashMap<String, RefinedType>,
    measures: HashMap<String, Measure>,
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            type_aliases: HashMap::new(),
            measures: HashMap::new(),
        }
    }

    pub fn parse_file(&mut self, input: &str) -> Result<Vec<HIR>> {
        let mut declarations = Vec::new();
        let mut remaining = input;

        while !remaining.trim().is_empty() {
            match self.parse_declaration(remaining) {
                Ok((rest, Some(decl))) => {
                    declarations.push(decl);
                    remaining = rest;
                }
                Ok((rest, None)) => {
                    remaining = rest;
                }
                Err(e) => {
                    return Err(anyhow!("Parse error: {:?}", e));
                }
            }
        }

        Ok(declarations)
    }

    fn parse_declaration<'a>(&mut self, input: &'a str) -> IResult<&'a str, Option<HIR>> {
        if let Ok((rest, _)) = self.parse_refinement_type(input) {
            return Ok((rest, None));
        }
        if let Ok((rest, _)) = self.parse_measure(input) {
            return Ok((rest, None));
        }
        if let Ok((rest, func)) = self.parse_function(input) {
            return Ok((rest, Some(func)));
        }
        Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
    }

    // Parse refinement type annotation: {-@ type Nat = {v:Int | 0 <= v} @-}
    fn parse_refinement_type<'a>(&mut self, input: &'a str) -> IResult<&'a str, ()> {
        let (input, _) = multispace0(input)?;
        let (input, _) = tag("{-@")(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = tag("type")(input)?;
        let (input, _) = multispace1(input)?;
        let (input, name) = parse_identifier(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = char('=')(input)?;
        let (input, _) = multispace0(input)?;
        let (input, ty) = self.parse_type_with_refinement(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = tag("@-}")(input)?;
        let (input, _) = multispace0(input)?;

        self.type_aliases.insert(name.to_string(), ty);
        Ok((input, ()))
    }

    // Parse measure declaration: {-@ measure size @-}
    fn parse_measure<'a>(&mut self, input: &'a str) -> IResult<&'a str, ()> {
        let (input, _) = multispace0(input)?;
        let (input, _) = tag("{-@")(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = tag("measure")(input)?;
        let (input, _) = multispace1(input)?;
        let (input, name) = parse_identifier(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = tag("@-}")(input)?;
        let (input, _) = multispace0(input)?;

        self.measures.insert(
            name.to_string(),
            Measure {
                expr: SMTExpr::Var(Symbol(name.to_string())),
                decreasing: true,
            }
        );
        Ok((input, ()))
    }

    // Parse function with optional refinement type
    fn parse_function<'a>(&mut self, input: &'a str) -> IResult<&'a str, HIR> {
        let (input, _) = multispace0(input)?;
        
        // Optional refinement type annotation
        let (input, refinement_sig) = opt(|i| self.parse_refinement_signature(i))(input)?;
        
        // Function name
        let (input, name) = parse_identifier(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = tag("::")(input)?;
        let (input, _) = multispace0(input)?;
        
        // Type signature
        let (input, type_sig) = self.parse_type_signature(input)?;
        let (input, _) = multispace0(input)?;
        
        // Function definition
        let (input, _) = tag(name)(input)?;
        let (input, _) = multispace1(input)?;
        let (input, params) = separated_list1(multispace1, parse_identifier)(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = char('=')(input)?;
        let (input, _) = multispace0(input)?;
        let (input, body) = self.parse_expr(input)?;
        let (input, _) = multispace0(input)?;

        // Build parameter list with types
        let param_types = self.extract_param_types(&type_sig, params.len());
        let params_with_types: Vec<(Symbol, RefinedType)> = params.iter()
            .zip(param_types.iter())
            .map(|(name, ty)| (Symbol(name.to_string()), ty.clone()))
            .collect();

        // Extract pre/post conditions from refinement signature
        let (pre, post) = if let Some(ref_sig) = refinement_sig {
            self.extract_conditions(&ref_sig)
        } else {
            (vec![], vec![])
        };

        Ok((input, HIR::Function {
            name: Symbol(name.to_string()),
            params: params_with_types,
            body: Box::new(body),
            pre,
            post,
            decreases: None,
        }))
    }

    // Parse refinement signature: {-@ add :: Nat -> Nat -> Nat @-}
    fn parse_refinement_signature<'a>(&mut self, input: &'a str) -> IResult<&'a str, String> {
        let (input, _) = multispace0(input)?;
        let (input, _) = tag("{-@")(input)?;
        let (input, content) = take_until("@-}")(input)?;
        let (input, _) = tag("@-}")(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, content.to_string()))
    }

    // Parse type signature: Int -> Int -> Int
    fn parse_type_signature<'a>(&mut self, input: &'a str) -> IResult<&'a str, Vec<RefinedType>> {
        separated_list1(
            tuple((multispace0, tag("->"), multispace0)),
            |i| self.parse_base_type(i)
        )(input)
    }

    // Parse base type
    fn parse_base_type<'a>(&mut self, input: &'a str) -> IResult<&'a str, RefinedType> {
        alt((
            map(tag("Int"), |_| RefinedType::new(BaseType::Int)),
            map(tag("Bool"), |_| RefinedType::new(BaseType::Bool)),
            map(tag("String"), |_| RefinedType::new(BaseType::String)),
            map(tag("()"), |_| RefinedType::new(BaseType::Unit)),
            map(parse_identifier, |name| {
                // Check if it's a type alias
                if let Some(ty) = self.type_aliases.get(name) {
                    ty.clone()
                } else {
                    RefinedType::new(BaseType::TypeVar(Symbol(name.to_string())))
                }
            }),
        ))(input)
    }

    // Parse type with refinement: {v:Int | 0 <= v}
    fn parse_type_with_refinement<'a>(&mut self, input: &'a str) -> IResult<&'a str, RefinedType> {
        let (input, _) = char('{')(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _var) = parse_identifier(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = char(':')(input)?;
        let (input, _) = multispace0(input)?;
        let (input, base_ty) = self.parse_base_type(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = char('|')(input)?;
        let (input, _) = multispace0(input)?;
        let (input, refinement) = self.parse_smt_expr(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = char('}')(input)?;

        Ok((input, base_ty.with_refinement(refinement)))
    }

    // Parse SMT expression
    fn parse_smt_expr<'a>(&mut self, input: &'a str) -> IResult<&'a str, SMTExpr> {
        self.parse_smt_comparison(input)
    }

    // Parse comparison: 0 <= v
    fn parse_smt_comparison<'a>(&mut self, input: &'a str) -> IResult<&'a str, SMTExpr> {
        let (input, lhs) = self.parse_smt_atom(input)?;
        let (input, _) = multispace0(input)?;
        let (input, op) = alt((
            value(CompOp::Leq, tag("<=")),
            value(CompOp::Lt, tag("<")),
            value(CompOp::Geq, tag(">=")),
            value(CompOp::Gt, tag(">")),
            value(CompOp::Eq, tag("==")),
            value(CompOp::Neq, tag("/=")),
        ))(input)?;
        let (input, _) = multispace0(input)?;
        let (input, rhs) = self.parse_smt_atom(input)?;

        Ok((input, SMTExpr::Comparison {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }))
    }

    // Parse SMT atom
    fn parse_smt_atom<'a>(&mut self, input: &'a str) -> IResult<&'a str, SMTExpr> {
        alt((
            map(parse_integer, |n| SMTExpr::Const(Literal::Int(n))),
            map(parse_identifier, |name| SMTExpr::Var(Symbol(name.to_string()))),
        ))(input)
    }

    // Parse expression with operator precedence
    fn parse_expr<'a>(&mut self, input: &'a str) -> IResult<&'a str, HIR> {
        self.parse_additive(input)
    }
    
    // Parse additive operators (+, -)
    fn parse_additive<'a>(&mut self, input: &'a str) -> IResult<&'a str, HIR> {
        let (mut input, mut left) = self.parse_primary(input)?;
        
        loop {
            let (rest, _) = multispace0(input)?;
            if let Ok((rest2, op)) = alt::<_, _, nom::error::Error<&str>, _>((
                value(BinOp::Add, char::<_, nom::error::Error<&str>>('+')),
                value(BinOp::Sub, char::<_, nom::error::Error<&str>>('-')),
            ))(rest) {
                let (rest3, _) = multispace0(rest2)?;
                let (rest4, right) = self.parse_primary(rest3)?;
                left = HIR::BinOp {
                    op,
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                };
                input = rest4;
            } else {
                break;
            }
        }
        
        Ok((input, left))
    }
    
    // Parse primary expressions
    fn parse_primary<'a>(&mut self, input: &'a str) -> IResult<&'a str, HIR> {
        self.parse_match_or_app(input)
    }

    // Parse match expression or application
    fn parse_match_or_app<'a>(&mut self, input: &'a str) -> IResult<&'a str, HIR> {
        if let Ok((rest, match_expr)) = self.parse_match(input) {
            return Ok((rest, match_expr));
        }
        self.parse_application(input)
    }

    // Parse match expression
    fn parse_match<'a>(&mut self, input: &'a str) -> IResult<&'a str, HIR> {
        let (input, _) = tag("case")(input)?;
        let (input, _) = multispace1(input)?;
        let (input, scrutinee) = self.parse_expr(input)?;
        let (input, _) = multispace1(input)?;
        let (input, _) = tag("of")(input)?;
        let (input, _) = multispace0(input)?;
        let (input, arms) = many1(|i| self.parse_match_arm(i))(input)?;

        let patterns = arms.iter().map(|(p, _)| p.clone()).collect();
        Ok((input, HIR::Match {
            scrutinee: Box::new(scrutinee),
            arms,
            exhaustive: ExhaustivenessProof {
                patterns,
                coverage: CoverageProof::Complete, // TODO: Actual analysis
            },
        }))
    }

    // Parse match arm
    fn parse_match_arm<'a>(&mut self, input: &'a str) -> IResult<&'a str, (Pattern, Box<HIR>)> {
        let (input, _) = multispace0(input)?;
        let (input, pattern) = self.parse_pattern(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = tag("->")(input)?;
        let (input, _) = multispace0(input)?;
        let (input, expr) = self.parse_expr(input)?;
        Ok((input, (pattern, Box::new(expr))))
    }

    // Parse pattern
    fn parse_pattern<'a>(&mut self, input: &'a str) -> IResult<&'a str, Pattern> {
        alt((
            map(tag("_"), |_| Pattern::Wildcard),
            map(parse_identifier, |name| {
                if name.chars().next().unwrap().is_uppercase() {
                    Pattern::Constructor(Symbol(name.to_string()), vec![])
                } else {
                    Pattern::Var(Symbol(name.to_string()))
                }
            }),
        ))(input)
    }

    // Parse application
    fn parse_application<'a>(&mut self, input: &'a str) -> IResult<&'a str, HIR> {
        let (input, atoms) = separated_list1(multispace1, |i| self.parse_atom(i))(input)?;
        
        if atoms.len() == 1 {
            Ok((input, atoms.into_iter().next().unwrap()))
        } else {
            let mut iter = atoms.into_iter();
            let func = iter.next().unwrap();
            let args = iter.collect();
            Ok((input, HIR::App {
                func: Box::new(func),
                args,
            }))
        }
    }

    // Parse atom
    fn parse_atom<'a>(&mut self, input: &'a str) -> IResult<&'a str, HIR> {
        if let Ok((rest, n)) = parse_integer(input) {
            return Ok((rest, HIR::Lit(Literal::Int(n))));
        }
        if let Ok((rest, _)) = tag::<_, _, nom::error::Error<_>>("True")(input) {
            return Ok((rest, HIR::Lit(Literal::Bool(true))));
        }
        if let Ok((rest, _)) = tag::<_, _, nom::error::Error<_>>("False")(input) {
            return Ok((rest, HIR::Lit(Literal::Bool(false))));
        }
        if let Ok((rest, name)) = parse_identifier(input) {
            let hir = if name.chars().next().unwrap().is_uppercase() {
                HIR::Constructor {
                    name: Symbol(name.to_string()),
                    args: vec![],
                }
            } else {
                HIR::Var(Symbol(name.to_string()))
            };
            return Ok((rest, hir));
        }
        if let Ok((rest, expr)) = delimited(char('('), |i| self.parse_expr(i), char(')'))(input) {
            return Ok((rest, expr));
        }
        Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Alt)))
    }

    // Parse binary operation
    fn parse_binary_op<'a>(&mut self, input: &'a str) -> IResult<&'a str, HIR> {
        let (input, lhs) = self.parse_atom(input)?;
        let (input, _) = multispace0(input)?;
        let (input, op) = alt((
            value(BinOp::Add, char('+')),
            value(BinOp::Sub, char('-')),
            value(BinOp::Mul, char('*')),
            value(BinOp::Div, char('/')),
            value(BinOp::Lt, char('<')),
        ))(input)?;
        let (input, _) = multispace0(input)?;
        let (input, rhs) = self.parse_atom(input)?;

        Ok((input, HIR::BinOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }))
    }

    // Helper methods
    fn extract_param_types(&self, type_sig: &[RefinedType], param_count: usize) -> Vec<RefinedType> {
        if type_sig.len() > param_count {
            type_sig[..param_count].to_vec()
        } else {
            type_sig.to_vec()
        }
    }

    fn extract_conditions(&self, _refinement_sig: &str) -> (Vec<Predicate>, Vec<Predicate>) {
        // TODO: Parse actual pre/post conditions from refinement signature
        (vec![], vec![])
    }
}

// Helper parsers
fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )
    )(input)
}

fn parse_integer(input: &str) -> IResult<&str, i64> {
    map(digit1, |s: &str| s.parse::<i64>().unwrap())(input)
}

// Public API
pub fn parse(input: &str) -> Result<Vec<HIR>> {
    let mut parser = Parser::new();
    parser.parse_file(input)
}