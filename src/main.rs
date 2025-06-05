use anyhow::{Context, Result};
use clap::{Parser, Subcommand, ValueEnum};
use colored::Colorize;
use rascal_light::smt::VerificationResult;
use rascal_light::*;
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "rascal-light")]
#[command(about = "A verification-oriented Haskell-to-Rust transpiler", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Transpile a Liquid Haskell file to Rust
    Transpile {
        /// Input Liquid Haskell file
        input: PathBuf,

        /// Output Rust file (defaults to input.rs)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Verification level
        #[arg(short, long, value_enum, default_value = "basic")]
        verify: VerifyLevel,

        /// Verification backend
        #[arg(short, long, default_value = "prusti")]
        backend: String,

        /// Disable optimizations
        #[arg(long)]
        no_optimize: bool,
    },

    /// Verify a Liquid Haskell file without generating code
    Verify {
        /// Input Liquid Haskell file
        input: PathBuf,

        /// Verification level
        #[arg(short, long, value_enum, default_value = "refinement")]
        level: VerifyLevel,
    },

    /// Check syntax without generating code
    Check {
        /// Input Liquid Haskell file
        input: PathBuf,
    },
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum VerifyLevel {
    /// No verification
    None,
    /// Basic type safety only
    Basic,
    /// Include Liquid Haskell refinements
    Refinement,
    /// Full functional correctness with termination
    Total,
}

impl From<VerifyLevel> for VerificationLevel {
    fn from(level: VerifyLevel) -> Self {
        match level {
            VerifyLevel::None => VerificationLevel::None,
            VerifyLevel::Basic => VerificationLevel::Basic,
            VerifyLevel::Refinement => VerificationLevel::Refinement,
            VerifyLevel::Total => VerificationLevel::Total,
        }
    }
}

fn main() -> Result<()> {
    env_logger::init();

    let cli = Cli::parse();

    match cli.command {
        Commands::Transpile {
            input,
            output,
            verify,
            backend,
            no_optimize,
        } => {
            transpile_command(input, output, verify.into(), backend, !no_optimize)?;
        }
        Commands::Verify { input, level } => {
            verify_command(input, level.into())?;
        }
        Commands::Check { input } => {
            check_command(input)?;
        }
    }

    Ok(())
}

fn transpile_command(
    input: PathBuf,
    output: Option<PathBuf>,
    verify: VerificationLevel,
    backend: String,
    optimize: bool,
) -> Result<()> {
    println!("{} {}", "Transpiling".green().bold(), input.display());

    let options = CompilerOptions {
        verify,
        optimize,
        backend,
        output_path: output.as_ref().map(|p| p.to_string_lossy().to_string()),
    };

    let mut compiler = Compiler::new(options);
    let source = fs::read_to_string(&input).context("Failed to read input file")?;

    match compiler.compile_string(&source, input.to_string_lossy().to_string()) {
        Ok(rust_code) => {
            // Determine output path
            let output_path = output.unwrap_or_else(|| {
                let mut path = input.clone();
                path.set_extension("rs");
                path
            });

            // Write output
            fs::write(&output_path, rust_code).context("Failed to write output file")?;

            println!(
                "{} Written to {}",
                "Success!".green().bold(),
                output_path.display()
            );

            // Print any warnings
            if !compiler.get_diagnostics().has_errors() {
                let diagnostics = compiler.get_diagnostics().render_all(&source);
                if !diagnostics.is_empty() {
                    println!("\n{}", diagnostics);
                }
            }

            Ok(())
        }
        Err(e) => {
            eprintln!("{} {}", "Error:".red().bold(), e);

            // Print detailed diagnostics
            let diagnostics = compiler.get_diagnostics().render_all(&source);
            if !diagnostics.is_empty() {
                eprintln!("\n{}", diagnostics);
            }

            std::process::exit(1);
        }
    }
}

fn verify_command(input: PathBuf, level: VerificationLevel) -> Result<()> {
    println!(
        "{} {} with level {:?}",
        "Verifying".blue().bold(),
        input.display(),
        level
    );

    let options = CompilerOptions {
        verify: level,
        optimize: false,
        backend: "prusti".to_string(),
        output_path: None,
    };

    let mut compiler = Compiler::new(options);
    let source = fs::read_to_string(&input).context("Failed to read input file")?;

    // Parse and verify without generating code
    let functions = parse(&source).context("Failed to parse Liquid Haskell")?;

    let mut all_verified = true;

    for func in functions {
        match verify_hir(&func) {
            Ok(VerificationResult::Verified) => {
                if let HIR::Function { name, .. } = &func {
                    println!("  {} {}", "✓".green(), name.0);
                }
            }
            Ok(VerificationResult::CounterExample(ce)) => {
                if let HIR::Function { name, .. } = &func {
                    println!("  {} {} - counterexample found", "✗".red(), name.0);
                    println!("    Failing: {}", ce.failing_condition);
                    for (var, val) in &ce.model {
                        println!("    {} = {}", var, val);
                    }
                }
                all_verified = false;
            }
            Ok(VerificationResult::Timeout) => {
                if let HIR::Function { name, .. } = &func {
                    println!("  {} {} - timeout", "⏱".yellow(), name.0);
                }
            }
            Ok(VerificationResult::Unknown(msg)) => {
                if let HIR::Function { name, .. } = &func {
                    println!("  {} {} - unknown: {}", "?".yellow(), name.0, msg);
                }
            }
            Err(e) => {
                if let HIR::Function { name, .. } = &func {
                    println!("  {} {} - error: {}", "!".red(), name.0, e);
                }
                all_verified = false;
            }
        }
    }

    if all_verified {
        println!("\n{} All functions verified!", "Success!".green().bold());
    } else {
        println!(
            "\n{} Some functions failed verification",
            "Failed!".red().bold()
        );
        std::process::exit(1);
    }

    Ok(())
}

fn check_command(input: PathBuf) -> Result<()> {
    println!("{} {}", "Checking".cyan().bold(), input.display());

    let source = fs::read_to_string(&input).context("Failed to read input file")?;

    match parse(&source) {
        Ok(functions) => {
            println!("{} Syntax is valid", "✓".green());
            println!("  Found {} function(s)", functions.len());

            for func in functions {
                if let HIR::Function { name, params, .. } = func {
                    println!("  - {} ({} params)", name.0, params.len());
                }
            }

            Ok(())
        }
        Err(e) => {
            eprintln!("{} Parse error: {}", "✗".red().bold(), e);
            std::process::exit(1);
        }
    }
}
