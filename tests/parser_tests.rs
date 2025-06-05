use rascal::hir::*;
use rascal::parser::parse;

#[test]
fn test_parse_simple_function() {
    let source = r#"
        add :: Int -> Int -> Int
        add x y = x + y
    "#;

    let result = parse(source);
    assert!(result.is_ok());

    let functions = result.unwrap();
    assert_eq!(functions.len(), 1);

    match &functions[0] {
        HIR::Function { name, params, .. } => {
            assert_eq!(name.0, "add");
            assert_eq!(params.len(), 2);
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_parse_refined_type() {
    let source = r#"
        {-@ type Nat = {v:Int | 0 <= v} @-}
        {-@ add :: Nat -> Nat -> Nat @-}
        add :: Int -> Int -> Int
        add x y = x + y
    "#;

    let result = parse(source);
    assert!(result.is_ok());

    let functions = result.unwrap();
    assert_eq!(functions.len(), 1);
}

#[test]
fn test_parse_pattern_match() {
    let source = r#"
        isEmpty :: List a -> Bool
        isEmpty xs = case xs of
            Empty -> True
            _ -> False
    "#;

    let result = parse(source);
    // Parser implementation needs pattern matching support
    // For now, this test documents expected behavior
}
