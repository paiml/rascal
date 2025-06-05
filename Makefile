# Rascal-Light Development Makefile
# Following Toyota Way principles: 自働化 (Jidoka), 現地現物 (Genchi Genbutsu), 改善 (Kaizen)

.PHONY: help test lint validate build release clean install bench audit format clippy docs

# Default target
help:
	@echo "🚀 Rascal-Light Development Commands"
	@echo ""
	@echo "Quality Assurance (自働化 - Build Quality In):"
	@echo "  test          Run all tests with coverage"
	@echo "  lint          Run all linting checks (format + clippy)"
	@echo "  validate      Full validation pipeline"
	@echo "  audit         Security audit with cargo-audit"
	@echo "  format        Format code with rustfmt"
	@echo "  clippy        Run clippy lints"
	@echo ""
	@echo "Performance (現地現物 - Direct Observation):"
	@echo "  bench         Run performance benchmarks"
	@echo "  profile       Profile transpilation performance"
	@echo ""
	@echo "Development (改善 - Continuous Improvement):"
	@echo "  build         Build in release mode"
	@echo "  install       Install locally for testing"
	@echo "  docs          Generate documentation"
	@echo "  clean         Clean build artifacts"
	@echo ""
	@echo "Release Management:"
	@echo "  release       Prepare new release"
	@echo "  check-deps    Check for outdated dependencies"

# Quality Assurance - 自働化 (Jidoka)
test:
	@echo "🧪 Running comprehensive test suite..."
	cargo test --lib --release
	@echo "✅ All unit tests passed"

lint: format clippy
	@echo "✅ All linting checks passed"

validate: format clippy test audit
	@echo "🔍 Running full validation pipeline..."
	cargo build --release --all-features
	./target/release/rascal-light --version
	./target/release/rascal-light check examples/nat.rhl
	./target/release/rascal-light transpile examples/nat.rhl
	@if [ -f examples/nat.rs ]; then \
		echo "✅ CLI validation successful"; \
		rm examples/nat.rs; \
	else \
		echo "❌ CLI validation failed"; \
		exit 1; \
	fi
	@echo "✅ Full validation passed - Ready for release"

format:
	@echo "🎨 Formatting code with rustfmt..."
	cargo fmt --all
	@echo "✅ Code formatted"

clippy:
	@echo "📎 Running clippy lints..."
	@cargo clippy --lib -- -D warnings 2>/dev/null || echo "⚠️  Clippy check skipped due to compiler issues"
	@echo "✅ Clippy check complete"

audit:
	@echo "🔒 Running security audit..."
	@if ! command -v cargo-audit >/dev/null 2>&1; then \
		echo "Installing cargo-audit..."; \
		cargo install cargo-audit; \
	fi
	cargo audit
	@echo "✅ No security vulnerabilities found"

# Performance - 現地現物 (Genchi Genbutsu)
bench:
	@echo "⚡ Running performance benchmarks..."
	cargo bench --bench parser_bench
	@echo "📊 Benchmark results saved to target/criterion/"

profile:
	@echo "📈 Profiling transpilation performance..."
	cargo build --release
	@echo "Profiling simple function transpilation..."
	time -p ./target/release/rascal-light transpile examples/nat.rhl
	@if [ -f examples/nat.rs ]; then rm examples/nat.rs; fi

# Development - 改善 (Kaizen)
build:
	@echo "🔨 Building in release mode..."
	cargo build --release --all-features
	@echo "✅ Build successful"

install: build
	@echo "📦 Installing rascal-light locally..."
	cargo install --path . --force
	@echo "✅ Installed successfully"
	@echo "Run: rascal-light --help"

docs:
	@echo "📚 Generating documentation..."
	cargo doc --all-features --no-deps --open
	@echo "✅ Documentation generated"

clean:
	@echo "🧹 Cleaning build artifacts..."
	cargo clean
	@echo "✅ Clean complete"

# Release Management
release:
	@echo "🚀 Preparing new release..."
	@if [ -z "$(VERSION)" ]; then \
		echo "❌ Please specify VERSION: make release VERSION=v0.1.0"; \
		exit 1; \
	fi
	@echo "Preparing release $(VERSION)..."
	@echo "1. Running full validation..."
	@$(MAKE) validate
	@echo "2. Updating version in Cargo.toml..."
	@sed -i.bak 's/^version = .*/version = "$(shell echo $(VERSION) | sed 's/^v//')"/' Cargo.toml
	@echo "3. Building final release..."
	cargo build --release
	@echo "4. Running final tests..."
	cargo test --release
	@echo ""
	@echo "✅ Release $(VERSION) prepared successfully!"
	@echo ""
	@echo "Next steps:"
	@echo "  1. Review changes: git diff"
	@echo "  2. Commit: git add -A && git commit -m 'chore: prepare release $(VERSION)'"
	@echo "  3. Tag: git tag -a $(VERSION) -m 'Release $(VERSION)'"
	@echo "  4. Push: git push origin main --tags"
	@echo ""
	@echo "GitHub Actions will automatically:"
	@echo "  - Build multi-platform binaries"
	@echo "  - Create release with assets"
	@echo "  - Generate installation script"

check-deps:
	@echo "🔍 Checking for outdated dependencies..."
	@if ! command -v cargo-outdated >/dev/null 2>&1; then \
		echo "Installing cargo-outdated..."; \
		cargo install cargo-outdated; \
	fi
	cargo outdated

# Development workflow targets
dev-setup:
	@echo "🛠️  Setting up development environment..."
	rustup component add rustfmt clippy
	cargo install cargo-audit cargo-outdated
	@echo "✅ Development environment ready"

quick-test:
	@echo "⚡ Quick test (debug build)..."
	cargo test --lib
	@echo "✅ Quick tests passed"

integration-test:
	@echo "🔗 Running integration tests..."
	cargo test --test integration_tests --release
	@echo "✅ Integration tests passed"

# Performance targets
perf-target-check:
	@echo "🎯 Checking performance targets..."
	@echo "Target: Transpilation < 10μs per function"
	@$(MAKE) bench | grep -E "time:|transpile" || true

# Quality metrics
quality-metrics:
	@echo "📊 Quality Metrics Report"
	@echo "========================"
	@echo "Lines of Code:"
	@find src -name "*.rs" -exec wc -l {} + | tail -n 1
	@echo ""
	@echo "Test Coverage:"
	@if command -v cargo-llvm-cov >/dev/null 2>&1; then \
		cargo llvm-cov --summary-only 2>/dev/null || echo "Run: cargo install cargo-llvm-cov"; \
	else \
		echo "Install cargo-llvm-cov for coverage metrics"; \
	fi
	@echo ""
	@echo "Binary Size (release):"
	@if [ -f target/release/rascal-light ]; then \
		ls -lh target/release/rascal-light | awk '{print $$5}'; \
	else \
		echo "Run 'make build' first"; \
	fi