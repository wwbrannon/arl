# Rye Execution Coverage

This document describes the execution coverage system implemented for Rye code.

## Overview

The Rye coverage system tracks **execution coverage** — which lines of `.rye` source files actually execute during runtime. The compiler injects `.rye_coverage_track()` calls into compiled code; these fire at runtime and record which source lines were reached.

## Architecture

### Core Components

1. **RyeCoverageTracker** (`R/coverage.R`)
   - R6 class that tracks line-level execution counts
   - Generates reports in multiple formats (console, HTML, JSON)
   - Exported for use by package users and CI tools

2. **Compiler Instrumentation** (`R/compiler.R`)
   - When a `coverage_tracker` is present in the compilation context, the compiler injects `.rye_coverage_track(file, start_line, end_line)` calls into:
     - **Lambda bodies**: Before each statement via `interleave_coverage()`
     - **If-expression branches**: Around each branch via `wrap_branch_coverage()`
   - Source location comes from `rye_src` attributes on the AST
   - Zero overhead when coverage is disabled (no calls injected)

3. **Runtime Hook** (`R/runtime.R`)
   - `install_helpers()` installs `.rye_coverage_track` as a closure over the tracker when coverage is enabled
   - The function is not installed at all when coverage is disabled

4. **Engine Integration** (`R/rye-engine.R`)
   - `enable_coverage()` / `disable_coverage()` / `get_coverage()` / `reset_coverage()`

5. **Test Propagation** (`tests/testthat/helper-engine.R`)
   - `make_engine()` checks `getOption("rye.coverage_tracker")` and passes the tracker through to `RyeEngine$new()`
   - Test files use `make_engine()` instead of `RyeEngine$new()` so that coverage data is collected across all tests, not just stdlib loading

6. **CI Tool** (`tools/rye-coverage.R`)
   - Sets `options(rye.coverage_tracker = tracker)` before running the test suite
   - Test engines created by `make_engine()` pick up the tracker automatically

## What Gets Counted

**Denominator** (total lines): Non-blank, non-comment lines in each `.rye` file. A line is considered code if it matches `^\s*[^[:space:];]` — i.e., it has non-whitespace content and doesn't start with `;`.

**Numerator** (covered lines): Code lines where a `.rye_coverage_track()` call fired with a source range covering that line. Each hit increments a per-line execution count.

The tracker filters on both sides: injected calls only record hits for lines in the code-lines set, and reports only count code lines in the denominator.

## Usage

### From R

```r
# Create tracker first, then engine with tracker
# This ensures coverage is tracked from the start, including stdlib loading
library(rye)
tracker <- RyeCoverageTracker$new()
engine <- RyeEngine$new(use_env_cache = FALSE, coverage_tracker = tracker)

# Run code
engine$load_file("my-code.rye")

# Generate reports
tracker$report_console()
tracker$report_html("coverage.html")
tracker$report_json("coverage.json")
```

### From Command Line

```bash
# Run coverage and generate all reports
Rscript tools/rye-coverage.R

# Or use make target
make coverage-rye
```

## Reports

### Console Report
Plain text summary showing per-file coverage percentages and totals.

### HTML Report
Interactive report with:
- Summary table with per-file statistics
- Line-by-line coverage for each file
- Color-coded coverage status (covered/uncovered/non-code)
- Hit counts for each executed line

### JSON Report
Codecov-compatible JSON format for CI integration:
```json
{
  "coverage": {
    "path/to/file.rye": [
      null,  // line 1: not code
      0,     // line 2: code, not executed
      5,     // line 3: code, executed 5 times
      ...
    ]
  }
}
```

## Integration with CI

1. **R Code Coverage** (`make coverage-r`)
   - Uses `covr::package_coverage()`
   - Tracks R implementation in `R/` directory

2. **Rye Code Coverage** (`make coverage-rye`)
   - Uses `RyeCoverageTracker`
   - Tracks Rye language code in `inst/rye/stdlib/`
   - Runs the full testthat suite with coverage propagated to all test engines

3. **Combined Coverage** (`make coverage-combined`)
   - Merges R and Rye coverage (reads JSON from both sources)

4. **GitHub Actions** (`.github/workflows/coverage.yaml`)
   - Runs all coverage steps, uploads to codecov

## Implementation Details

### How It Works

1. **Tracker Creation**: Coverage tracker is created **before** engine initialization
   ```r
   tracker <- RyeCoverageTracker$new()
   engine <- RyeEngine$new(use_env_cache = FALSE, coverage_tracker = tracker)
   ```
   `use_env_cache = FALSE` is required: cached modules lack source info for instrumentation, and instrumented code must not be written to cache.

2. **Compiler Instrumentation**: When the compiler has a `coverage_tracker` in its context, it injects calls:
   - **Lambda bodies** (`interleave_coverage`): Inserts `.rye_coverage_track(file, line, line)` before each statement in the body. The compiled lambda becomes `{ .rye_env <- environment(), .rye_coverage_track(...), stmt1, .rye_coverage_track(...), stmt2, ... }`.
   - **If branches** (`wrap_branch_coverage`): Wraps each branch in `{ .rye_coverage_track(...), branch_expr }` so that taking a branch records which source line was reached.

3. **Runtime Execution**: `.rye_coverage_track()` is a closure installed by `install_helpers()` that calls `tracker$track()`. The tracker records a hit for each code line in the `start_line:end_line` range.

4. **Reporting**: Tracker compares executed lines against all code lines in discovered `.rye` files.

### Test Propagation

The coverage tool needs to track execution across all test engines, not just the initial engine that loads stdlib. This is accomplished via a global R option:

1. `tools/rye-coverage.R` sets `options(rye.coverage_tracker = tracker)` before running the test suite
2. `tests/testthat/helper-engine.R` defines `make_engine()` which checks this option
3. Test files use `make_engine()` instead of bare `RyeEngine$new()`
4. When the option is set, `make_engine()` passes the tracker and forces `use_env_cache = FALSE`

Some test files must use `RyeEngine$new()` directly because they inspect compiler internals that change under instrumentation:
- `test-coverage.R` — tests coverage itself with explicit tracker arguments
- `test-cache-options.R` — tests caching with explicit `use_env_cache` arguments
- `test-compiler-optimizations.R` — checks compiled AST structure (e.g., constant folding produces literals)
- `test-inspect-compilation.R` — checks compilation output shape
- `test-examples.R` — snapshot tests sensitive to gensym counter values

### Edge Cases

- **Multi-line expressions**: All code lines in the source range are marked as executed
- **Macro-generated code**: Attributed to macro call site (via `src_inherit`)
- **Module cache**: Must be disabled during coverage (`use_env_cache = FALSE`) to avoid caching instrumented code and to ensure source info is available for instrumentation
- **Unreachable code**: Correctly shows 0% coverage
- **Helper leaking**: When `use_env_cache = FALSE`, module loading can copy `.rye_env` into parent environments pointing to the wrong env. `install_helpers()` corrects `.rye_env` on its fast path to handle this.

## Performance

- **Overhead when enabled**: ~10 minutes for full test suite (vs ~3 minutes without)
- **Overhead when disabled**: Zero (no instrumentation calls injected)
- **Normal execution**: Unaffected (coverage is opt-in)
