#!/bin/bash
set -e

# Rascal-Light Release Preparation Script
# Following Toyota Way: 自働化 (Jidoka) - Build Quality In

VERSION="$1"
if [ -z "$VERSION" ]; then
    echo "❌ Usage: $0 <version>"
    echo "   Example: $0 v0.1.0"
    exit 1
fi

# Remove 'v' prefix for Cargo.toml
CARGO_VERSION=$(echo "$VERSION" | sed 's/^v//')

echo "🚀 Preparing Rascal-Light release $VERSION"
echo "========================================"

# 1. Validate current state
echo "📋 Step 1: Validating current state..."
if [ -n "$(git status --porcelain)" ]; then
    echo "❌ Working directory not clean. Please commit or stash changes."
    git status --short
    exit 1
fi

if [ "$(git branch --show-current)" != "main" ]; then
    echo "❌ Not on main branch. Please switch to main."
    exit 1
fi

echo "✅ Working directory clean and on main branch"

# 2. Run full validation pipeline (自働化 - Build Quality In)
echo ""
echo "🔍 Step 2: Running full validation pipeline..."
echo "Following 自働化 (Jidoka) - Never ship unverified code"

# Format check
echo "  Checking code formatting..."
if ! cargo +stable fmt --all -- --check; then
    echo "❌ Code not formatted. Run: cargo fmt --all"
    exit 1
fi

# Clippy check
echo "  Running clippy..."
if ! cargo +stable clippy --all-targets --all-features -- -D warnings; then
    echo "❌ Clippy warnings found. Please fix."
    exit 1
fi

# Security audit
echo "  Running security audit..."
if ! command -v cargo-audit >/dev/null 2>&1; then
    echo "Installing cargo-audit..."
    cargo install cargo-audit
fi
if ! cargo +stable audit; then
    echo "❌ Security vulnerabilities found. Please address."
    exit 1
fi

# Build check
echo "  Building release..."
if ! cargo +stable build --release --all-features; then
    echo "❌ Release build failed."
    exit 1
fi

# Test suite
echo "  Running test suite..."
if ! cargo +stable test --release --all-features; then
    echo "❌ Tests failed."
    exit 1
fi

# Integration tests
echo "  Running integration tests..."
if ! cargo +stable test --test integration_tests --release; then
    echo "❌ Integration tests failed."
    exit 1
fi

# CLI validation
echo "  Validating CLI functionality..."
if ! ./target/release/rascal-light --version; then
    echo "❌ CLI version check failed."
    exit 1
fi

if ! ./target/release/rascal-light check examples/nat.rhl; then
    echo "❌ CLI check command failed."
    exit 1
fi

if ! ./target/release/rascal-light transpile examples/nat.rhl; then
    echo "❌ CLI transpile command failed."
    exit 1
fi

if [ ! -f examples/nat.rs ]; then
    echo "❌ Transpilation did not generate output file."
    exit 1
fi

# Clean up test output
rm -f examples/nat.rs

echo "✅ Full validation pipeline passed"

# 3. Performance benchmarks (現地現物 - Direct Observation)
echo ""
echo "⚡ Step 3: Running performance benchmarks..."
echo "Following 現地現物 (Genchi Genbutsu) - Direct observation of performance"

echo "  Running benchmarks..."
cargo +stable bench --bench parser_bench > benchmark_results.txt 2>&1 || true

# Check if transpilation is under 10μs target
if grep -q "transpilation.*time:" benchmark_results.txt; then
    BENCH_TIME=$(grep "transpilation.*time:" benchmark_results.txt | head -1)
    echo "  📊 $BENCH_TIME"
    
    # Extract time value and check if under 10μs
    if echo "$BENCH_TIME" | grep -q "µs"; then
        TIME_VALUE=$(echo "$BENCH_TIME" | grep -oE '[0-9]+\.[0-9]+' | head -1)
        if [ "$(echo "$TIME_VALUE < 10.0" | bc 2>/dev/null || echo "0")" = "1" ]; then
            echo "  ✅ Performance target met: ${TIME_VALUE}μs < 10μs"
        else
            echo "  ⚠️  Performance target missed: ${TIME_VALUE}μs >= 10μs"
        fi
    fi
else
    echo "  ℹ️  Benchmark results available in benchmark_results.txt"
fi

# 4. Update version in Cargo.toml
echo ""
echo "📝 Step 4: Updating version information..."

# Backup current Cargo.toml
cp Cargo.toml Cargo.toml.backup

# Update version
sed -i.tmp "s/^version = .*/version = \"$CARGO_VERSION\"/" Cargo.toml
rm -f Cargo.toml.tmp

echo "  Updated Cargo.toml version to $CARGO_VERSION"

# Verify version update
if ! grep -q "version = \"$CARGO_VERSION\"" Cargo.toml; then
    echo "❌ Failed to update version in Cargo.toml"
    mv Cargo.toml.backup Cargo.toml
    exit 1
fi

# 5. Final build with new version
echo ""
echo "🔨 Step 5: Final build with updated version..."
if ! cargo +stable build --release; then
    echo "❌ Final build failed with new version."
    mv Cargo.toml.backup Cargo.toml
    exit 1
fi

# Verify version in binary
BINARY_VERSION=$(./target/release/rascal-light --version 2>/dev/null | grep -oE 'v?[0-9]+\.[0-9]+\.[0-9]+' || echo "unknown")
echo "  Binary reports version: $BINARY_VERSION"

# 6. Quality metrics report
echo ""
echo "📊 Step 6: Quality metrics report..."

echo "  Lines of Code:"
LOC=$(find src -name "*.rs" -exec wc -l {} + | tail -n 1 | awk '{print $1}')
echo "    Source: $LOC lines"

echo "  Binary Size:"
BINARY_SIZE=$(ls -lh target/release/rascal-light | awk '{print $5}')
echo "    Release binary: $BINARY_SIZE"

echo "  Test Count:"
TEST_COUNT=$(cargo +stable test --list 2>/dev/null | grep -c "test " || echo "unknown")
echo "    Total tests: $TEST_COUNT"

# 7. Generate release summary
echo ""
echo "📋 Step 7: Release summary..."

cat > RELEASE_NOTES.md << EOF
# Release $VERSION Preparation Summary

## Quality Assurance ✅
- [x] Code formatting (rustfmt)
- [x] Linting (clippy)
- [x] Security audit (cargo-audit)
- [x] Release build
- [x] Test suite (all tests passing)
- [x] Integration tests
- [x] CLI functionality

## Performance Benchmarks 📊
- Transpilation performance: $(grep "transpilation.*time:" benchmark_results.txt 2>/dev/null | head -1 || echo "See benchmark_results.txt")
- Binary size: $BINARY_SIZE
- Lines of code: $LOC

## Version Information 📝
- Release version: $VERSION
- Cargo version: $CARGO_VERSION
- Binary version: $BINARY_VERSION

## Next Steps 🚀
1. Review changes: \`git diff\`
2. Commit changes: \`git add -A && git commit -m "chore: prepare release $VERSION"\`
3. Create tag: \`git tag -a $VERSION -m "Release $VERSION"\`
4. Push to GitHub: \`git push origin main --tags\`

GitHub Actions will automatically:
- Build multi-platform binaries
- Create GitHub release
- Generate installation script
- Run additional validation

## Toyota Way Principles Applied 🏭
- 自働化 (Jidoka): Built-in quality with comprehensive validation
- 現地現物 (Genchi Genbutsu): Direct performance measurement and verification
- 改善 (Kaizen): Continuous improvement through automation

EOF

echo "✅ Release $VERSION prepared successfully!"
echo ""
echo "📋 Summary:"
echo "  Version: $VERSION"
echo "  Binary size: $BINARY_SIZE"
echo "  Lines of code: $LOC"
echo "  Tests: $TEST_COUNT"
echo ""
echo "📄 Detailed report saved to: RELEASE_NOTES.md"
echo ""
echo "🚀 Next steps:"
echo "  1. Review: git diff"
echo "  2. Commit: git add -A && git commit -m 'chore: prepare release $VERSION'"
echo "  3. Tag: git tag -a $VERSION -m 'Release $VERSION'"
echo "  4. Push: git push origin main --tags"
echo ""
echo "Following 改善 (Kaizen): Each release improves upon the last! 🎯"

# Clean up
rm -f Cargo.toml.backup benchmark_results.txt

exit 0