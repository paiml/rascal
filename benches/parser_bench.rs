use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use rascal_light::*;

fn benchmark_parsing(c: &mut Criterion) {
    let small_source = r#"
        add :: Int -> Int -> Int
        add x y = x + y
    "#;
    
    let medium_source = (0..50).map(|i| {
        format!("func{} :: Int -> Int\nfunc{} x = x + {}\n", i, i, i)
    }).collect::<Vec<_>>().join("\n");
    
    let large_source = (0..500).map(|i| {
        format!("func{} :: Int -> Int -> Int\nfunc{} x y = x + y + {}\n", i, i, i)
    }).collect::<Vec<_>>().join("\n");
    
    let mut group = c.benchmark_group("parsing");
    
    group.bench_with_input(
        BenchmarkId::new("small", "3_lines"),
        &small_source,
        |b, source| {
            b.iter(|| parse(black_box(source)))
        },
    );
    
    group.bench_with_input(
        BenchmarkId::new("medium", "100_lines"),
        &medium_source,
        |b, source| {
            b.iter(|| parse(black_box(source)))
        },
    );
    
    group.bench_with_input(
        BenchmarkId::new("large", "1000_lines"),
        &large_source,
        |b, source| {
            b.iter(|| parse(black_box(source)))
        },
    );
    
    group.finish();
}

fn benchmark_transpilation(c: &mut Criterion) {
    let test_cases = vec![
        ("simple", r#"
            id :: Int -> Int
            id x = x
        "#),
        ("with_refinements", r#"
            {-@ type Nat = {v:Int | 0 <= v} @-}
            {-@ add :: Nat -> Nat -> Nat @-}
            add :: Int -> Int -> Int
            add x y = x + y
        "#),
        ("multiple_functions", r#"
            add :: Int -> Int -> Int
            add x y = x + y
            
            sub :: Int -> Int -> Int
            sub x y = x - y
            
            mul :: Int -> Int -> Int
            mul x y = x + y
        "#),
    ];
    
    let mut group = c.benchmark_group("transpilation");
    
    for (name, source) in test_cases {
        group.bench_with_input(
            BenchmarkId::new("transpile", name),
            source,
            |b, source| {
                b.iter(|| transpile(black_box(source)))
            },
        );
    }
    
    group.finish();
}

fn benchmark_optimization(c: &mut Criterion) {
    let source = r#"
        compose :: (Int -> Int) -> (Int -> Int) -> Int -> Int
        compose f g x = f (g x)
        
        pipeline :: Int -> Int
        pipeline x = compose (\y -> y + 1) (\z -> z * 2) x
    "#;
    
    c.bench_function("optimization", |b| {
        b.iter(|| {
            let functions = parse(black_box(source)).unwrap();
            for func in functions {
                black_box(optimize(func));
            }
        })
    });
}

fn benchmark_ownership_analysis(c: &mut Criterion) {
    let source = r#"
        complex :: Int -> Int -> Int -> Int
        complex a b c = 
            let x = a + b in
            let y = b + c in
            x + y
    "#;
    
    c.bench_function("ownership_analysis", |b| {
        b.iter(|| {
            let functions = parse(black_box(source)).unwrap();
            for func in &functions {
                black_box(infer_ownership(func));
            }
        })
    });
}

fn benchmark_memory_usage(c: &mut Criterion) {
    let large_source = (0..1000).map(|i| {
        format!(r#"
            func{0} :: Int -> Int -> Int
            func{0} x y = 
                let a{0} = x + {0} in
                let b{0} = y + {0} in
                a{0} + b{0}
        "#, i)
    }).collect::<Vec<_>>().join("\n");
    
    c.bench_function("memory_large_file", |b| {
        b.iter(|| {
            let result = parse(black_box(&large_source));
            black_box(result)
        })
    });
}

criterion_group!(
    benches,
    benchmark_parsing,
    benchmark_transpilation,
    benchmark_optimization,
    benchmark_ownership_analysis,
    benchmark_memory_usage
);
criterion_main!(benches);