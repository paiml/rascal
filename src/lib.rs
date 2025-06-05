pub mod hir;
pub mod parser;
pub mod ownership;
#[cfg(feature = "smt-backend")]
pub mod smt;
pub mod codegen;
pub mod verification_backend;
pub mod optimizer;
pub mod diagnostics;

use anyhow::{Result, Context};
use std::path::Path;
use std::fs;

pub use hir::*;
pub use parser::parse;
pub use ownership::infer_ownership;
pub use smt::verify_hir;
pub use codegen::generate_rust;
pub use verification_backend::{create_backend, VerificationBackend};
pub use optimizer::optimize;
pub use diagnostics::{Diagnostic, DiagnosticBuilder, CompileError};

pub struct CompilerOptions {
    pub verify: VerificationLevel,
    pub optimize: bool,
    pub backend: String,
    pub output_path: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VerificationLevel {
    None,
    Basic,
    Refinement,
    Total,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions {
            verify: VerificationLevel::Basic,
            optimize: true,
            backend: "prusti".to_string(),
            output_path: None,
        }
    }
}

pub struct Compiler {
    options: CompilerOptions,
    diagnostics: DiagnosticBuilder,
}

impl Compiler {
    pub fn new(options: CompilerOptions) -> Self {
        Compiler {
            options,
            diagnostics: DiagnosticBuilder::new(),
        }
    }

    pub fn compile_file(&mut self, path: &Path) -> Result<String> {
        let source = fs::read_to_string(path)
            .context("Failed to read source file")?;
        
        self.compile_string(&source, path.to_string_lossy().to_string())
    }

    pub fn compile_string(&mut self, source: &str, filename: String) -> Result<String> {
        // Parse
        let hir_functions = parse(source)
            .context("Failed to parse Liquid Haskell")?;
        
        if hir_functions.is_empty() {
            return Err(anyhow::anyhow!("No functions found in source"));
        }
        
        let mut processed_functions = Vec::new();
        let mut ownership_map = HashMap::new();
        
        for mut func in hir_functions {
            // Ownership analysis
            let mut ownership_analysis = crate::ownership::OwnershipAnalysis::new();
            let inferred_ownership = ownership_analysis.analyze(&func)?;
            ownership_map.extend(inferred_ownership);
            func = infer_ownership(&func)?;
            
            // Optimization
            if self.options.optimize {
                func = optimize(func);
            }
            
            // Verification
            if self.options.verify != VerificationLevel::None {
                self.verify_function(&func, &filename)?;
            }
            
            processed_functions.push(func);
        }
        
        // Code generation
        let rust_code = codegen::generate_module(&processed_functions, &ownership_map)?;
        
        // Add verification annotations if requested
        let final_code = if self.options.verify != VerificationLevel::None {
            self.add_verification_annotations(rust_code, &processed_functions)?
        } else {
            rust_code
        };
        
        Ok(final_code)
    }

    fn verify_function(&mut self, func: &HIR, filename: &str) -> Result<()> {
        match self.options.verify {
            VerificationLevel::None => Ok(()),
            VerificationLevel::Basic => {
                // Type checking only (already done during parsing)
                Ok(())
            }
            VerificationLevel::Refinement | VerificationLevel::Total => {
                let result = verify_hir(func)?;
                match result {
                    smt::VerificationResult::Verified => Ok(()),
                    smt::VerificationResult::CounterExample(ce) => {
                        let diag = Diagnostic::error(
                            diagnostics::Span {
                                file: filename.to_string(),
                                line: 1, // TODO: Get actual line number
                                col: 1,
                                len: 10,
                            },
                            format!("Verification failed: {}", ce.failing_condition)
                        )
                        .with_verification_trace(smt::VerificationTrace {
                            model: ce.model,
                        });
                        
                        self.diagnostics.add(diag);
                        Err(anyhow::anyhow!("Verification failed"))
                    }
                    smt::VerificationResult::Timeout => {
                        let diag = Diagnostic::warning(
                            diagnostics::Span {
                                file: filename.to_string(),
                                line: 1,
                                col: 1,
                                len: 10,
                            },
                            "Verification timed out"
                        );
                        self.diagnostics.add(diag);
                        Ok(()) // Continue with warning
                    }
                    smt::VerificationResult::Unknown(msg) => {
                        let diag = Diagnostic::warning(
                            diagnostics::Span {
                                file: filename.to_string(),
                                line: 1,
                                col: 1,
                                len: 10,
                            },
                            format!("Verification unknown: {}", msg)
                        );
                        self.diagnostics.add(diag);
                        Ok(())
                    }
                }
            }
        }
    }

    fn add_verification_annotations(&self, mut rust_code: String, functions: &[HIR]) -> Result<String> {
        let backend = create_backend(&self.options.backend)?;
        
        // Insert verification annotations before each function
        for func in functions {
            if let HIR::Function { name, .. } = func {
                let spec = backend.generate_spec(func);
                if !spec.is_empty() {
                    // Find function definition and insert spec before it
                    let fn_pattern = format!("pub fn {}", name.0);
                    if let Some(pos) = rust_code.find(&fn_pattern) {
                        rust_code.insert_str(pos, &spec);
                    }
                }
            }
        }
        
        Ok(rust_code)
    }

    pub fn get_diagnostics(&self) -> &DiagnosticBuilder {
        &self.diagnostics
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }
}

// Convenience functions for common operations
pub fn transpile(source: &str) -> Result<String> {
    let mut compiler = Compiler::new(CompilerOptions::default());
    compiler.compile_string(source, "<input>".to_string())
}

pub fn transpile_with_verification(source: &str, level: VerificationLevel) -> Result<String> {
    let mut compiler = Compiler::new(CompilerOptions {
        verify: level,
        ..Default::default()
    });
    compiler.compile_string(source, "<input>".to_string())
}

#[cfg(not(feature = "smt-backend"))]
pub mod smt {
    use crate::hir::HIR;
    use anyhow::Result;
    use std::collections::HashMap;
    
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
    
    pub fn verify_hir(_hir: &HIR) -> Result<VerificationResult> {
        Ok(VerificationResult::Unknown("SMT backend not available".to_string()))
    }
}

use std::collections::HashMap;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_transpilation() {
        let source = r#"
            add :: Int -> Int -> Int
            add x y = x + y
        "#;
        
        let result = transpile(source);
        match result {
            Ok(rust_code) => {
                assert!(rust_code.contains("pub fn add"));
                assert!(rust_code.contains("x + y"));
            }
            Err(e) => {
                panic!("Transpilation failed: {:?}", e);
            }
        }
    }
}