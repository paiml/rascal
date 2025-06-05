use crate::smt::VerificationTrace;
use colored::Colorize;
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq)]
pub enum Level {
    Error,
    Warning,
    Info,
    Help,
}

#[derive(Debug, Clone)]
pub struct Span {
    pub file: String,
    pub line: usize,
    pub col: usize,
    pub len: usize,
}

#[derive(Debug, Clone)]
pub struct Note {
    pub message: String,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: Level,
    pub span: Span,
    pub message: String,
    pub notes: Vec<Note>,
    pub verification_trace: Option<VerificationTrace>,
}

impl Diagnostic {
    pub fn error(span: Span, message: impl Into<String>) -> Self {
        Diagnostic {
            level: Level::Error,
            span,
            message: message.into(),
            notes: Vec::new(),
            verification_trace: None,
        }
    }

    pub fn warning(span: Span, message: impl Into<String>) -> Self {
        Diagnostic {
            level: Level::Warning,
            span,
            message: message.into(),
            notes: Vec::new(),
            verification_trace: None,
        }
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(Note {
            message: note.into(),
            span: None,
        });
        self
    }

    pub fn with_span_note(mut self, span: Span, note: impl Into<String>) -> Self {
        self.notes.push(Note {
            message: note.into(),
            span: Some(span),
        });
        self
    }

    pub fn with_verification_trace(mut self, trace: VerificationTrace) -> Self {
        self.verification_trace = Some(trace);
        self
    }

    pub fn render(&self, source: &str) -> String {
        let mut output = String::new();

        // Main error message
        let level_str = match self.level {
            Level::Error => "error".red().bold(),
            Level::Warning => "warning".yellow().bold(),
            Level::Info => "info".blue().bold(),
            Level::Help => "help".green().bold(),
        };

        writeln!(output, "{}: {}", level_str, self.message.bold()).unwrap();

        // Location
        writeln!(
            output,
            "  {} {}:{}:{}",
            "-->".blue().bold(),
            self.span.file,
            self.span.line,
            self.span.col
        )
        .unwrap();

        // Source snippet with highlighting
        self.render_snippet(&mut output, source, &self.span);

        // Notes
        for note in &self.notes {
            writeln!(output, "  {} note: {}", "=".blue().bold(), note.message).unwrap();
            if let Some(span) = &note.span {
                self.render_snippet(&mut output, source, span);
            }
        }

        // Verification counterexample if present
        if let Some(trace) = &self.verification_trace {
            writeln!(output).unwrap();
            writeln!(output, "  {} counterexample found:", "=".blue().bold()).unwrap();
            for (var, val) in &trace.model {
                writeln!(output, "           {} = {}", var.cyan(), val).unwrap();
            }
        }

        output
    }

    fn render_snippet(&self, output: &mut String, source: &str, span: &Span) {
        let lines: Vec<&str> = source.lines().collect();

        // Calculate line number width
        let line_no_width = span.line.to_string().len();

        writeln!(output, "   {}", " ".repeat(line_no_width + 1).blue().bold()).unwrap();

        // Show context (line before if available)
        if span.line > 1 && span.line <= lines.len() {
            writeln!(
                output,
                "{:>width$} {} {}",
                (span.line - 1).to_string().blue().bold(),
                "|".blue().bold(),
                lines[span.line - 2],
                width = line_no_width
            )
            .unwrap();
        }

        // Show the main line
        if span.line > 0 && span.line <= lines.len() {
            let line = lines[span.line - 1];
            writeln!(
                output,
                "{:>width$} {} {}",
                span.line.to_string().blue().bold(),
                "|".blue().bold(),
                line,
                width = line_no_width
            )
            .unwrap();

            // Show the error underline
            let prefix_len = line_no_width + 3 + span.col - 1;
            let underline = "^".repeat(span.len.min(line.len() - span.col + 1));
            writeln!(
                output,
                "{}{}",
                " ".repeat(prefix_len),
                underline.red().bold()
            )
            .unwrap();
        }
    }

    pub fn extract_snippet(&self, source: &str) -> String {
        let lines: Vec<&str> = source.lines().collect();
        if self.span.line > 0 && self.span.line <= lines.len() {
            lines[self.span.line - 1].to_string()
        } else {
            String::new()
        }
    }
}

pub struct DiagnosticBuilder {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBuilder {
    pub fn new() -> Self {
        DiagnosticBuilder {
            diagnostics: Vec::new(),
        }
    }

    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.level == Level::Error)
    }

    pub fn render_all(&self, source: &str) -> String {
        let mut output = String::new();

        for (i, diagnostic) in self.diagnostics.iter().enumerate() {
            if i > 0 {
                writeln!(output).unwrap();
            }
            write!(output, "{}", diagnostic.render(source)).unwrap();
        }

        // Summary
        let error_count = self
            .diagnostics
            .iter()
            .filter(|d| d.level == Level::Error)
            .count();
        let warning_count = self
            .diagnostics
            .iter()
            .filter(|d| d.level == Level::Warning)
            .count();

        if error_count > 0 || warning_count > 0 {
            writeln!(output).unwrap();
            write!(output, "{}: ", "summary".bold()).unwrap();

            if error_count > 0 {
                write!(
                    output,
                    "{} {}",
                    error_count.to_string().red().bold(),
                    if error_count == 1 { "error" } else { "errors" }
                )
                .unwrap();
            }

            if error_count > 0 && warning_count > 0 {
                write!(output, ", ").unwrap();
            }

            if warning_count > 0 {
                write!(
                    output,
                    "{} {}",
                    warning_count.to_string().yellow().bold(),
                    if warning_count == 1 {
                        "warning"
                    } else {
                        "warnings"
                    }
                )
                .unwrap();
            }

            writeln!(output).unwrap();
        }

        output
    }
}

// Error types for different compilation phases
#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Ownership error: {0}")]
    OwnershipError(String),

    #[error("Verification error: {0}")]
    VerificationError(String),

    #[error("Code generation error: {0}")]
    CodeGenError(String),
}

impl CompileError {
    pub fn to_diagnostic(&self, span: Span) -> Diagnostic {
        match self {
            CompileError::ParseError(msg) => {
                Diagnostic::error(span, format!("parse error: {}", msg))
                    .with_note("expected valid Liquid Haskell syntax")
            }
            CompileError::TypeError(msg) => Diagnostic::error(span, format!("type error: {}", msg))
                .with_note("types must match exactly"),
            CompileError::OwnershipError(msg) => {
                Diagnostic::error(span, format!("ownership error: {}", msg))
                    .with_note("ensure proper ownership and borrowing")
            }
            CompileError::VerificationError(msg) => {
                Diagnostic::error(span, format!("verification failed: {}", msg))
                    .with_note("refinement types could not be verified")
            }
            CompileError::CodeGenError(msg) => {
                Diagnostic::error(span, format!("code generation failed: {}", msg))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagnostic_rendering() {
        let source = r#"add :: Int -> Int -> Int
add x y = x + y

main = add 1 "hello""#;

        let diag = Diagnostic::error(
            Span {
                file: "test.rhl".to_string(),
                line: 4,
                col: 14,
                len: 7,
            },
            "type mismatch: expected Int, found String",
        )
        .with_note("strings cannot be used where integers are expected")
        .with_span_note(
            Span {
                file: "test.rhl".to_string(),
                line: 1,
                col: 15,
                len: 3,
            },
            "function expects Int here",
        );

        let rendered = diag.render(source);

        assert!(rendered.contains("type mismatch"));
        assert!(rendered.contains("test.rhl:4:14"));
        assert!(rendered.contains("^^^^^^^")); // Error underline
    }
}
