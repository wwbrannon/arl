# Rye Execution Coverage

This document describes the execution coverage system implemented for Rye code.

## Overview

The Rye coverage system now tracks **execution coverage** - which lines of `.rye` source files actually execute during runtime - rather than just compilation coverage. This provides more accurate insight into code usage and test coverage.

## Architecture

### Core Components

1. **RyeCoverageTracker** (`R/coverage.R`)
   - R6 class that tracks line-level execution counts
   - Generates reports in multiple formats (console, HTML, JSON)
   - Exported for use by package users and CI tools

2. **Runtime Hook** (`R/runtime.R`)
   - `CompiledRuntime$eval_compiled()` tracks execution when coverage is enabled
   - Extracts `rye_src` attributes from compiled expressions
   - Zero overhead when coverage is disabled (NULL check)

3. **Engine Integration** (`R/rye-engine.R`)
   - `enable_coverage()` - Enable tracking and return tracker instance
   - `disable_coverage()` - Disable tracking
   - `get_coverage()` - Retrieve tracker instance
   - `reset_coverage()` - Clear coverage data

4. **CI Tool** (`tools/rye-coverage.R`)
   - Thin wrapper that orchestrates coverage collection
   - Runs native tests with coverage enabled
   - Generates all report formats

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

The execution coverage system integrates seamlessly with existing CI workflows:

1. **R Code Coverage** (`make coverage-r`)
   - Uses `covr::package_coverage()`
   - Tracks R implementation in `R/` directory
   - **UNCHANGED**

2. **Rye Code Coverage** (`make coverage-rye`)
   - Uses `RyeCoverageTracker`
   - Tracks Rye language code in `inst/rye/` and `tests/native/`
   - **NOW USES EXECUTION COVERAGE**

3. **Combined Coverage** (`make coverage-combined`)
   - Merges R and Rye coverage
   - **UNCHANGED** (reads JSON from both sources)

4. **GitHub Actions** (`.github/workflows/coverage.yaml`)
   - Runs all coverage steps
   - Uploads to codecov
   - **UNCHANGED**

## Coverage Metrics

### Before (Compilation Coverage)
- **Total: 94.5%** - measured which lines could be compiled
- High coverage but didn't reflect actual execution

### After (Execution Coverage)
- **Total: 91.7%** - measured which lines actually execute during tests
- Accurate execution-based coverage
- Most stdlib files: 90-98% coverage
- Reveals specific gaps:
  - equality.rye: 80.7% (some comparison operators not tested)
  - logic.rye: 76.9% (some logical operations not exercised)
  - _r.rye: 70.9% (some R interop functions unused)

This provides accurate insight into:
- Which code paths actually execute during tests
- Specific functions and branches that lack test coverage
- Opportunities for targeted test improvements

## Implementation Details

### How It Works

1. **Tracker Creation**: Coverage tracker is created **before** engine initialization
   ```r
   tracker <- RyeCoverageTracker$new()
   engine <- RyeEngine$new(coverage_tracker = tracker)
   ```
   This ensures stdlib loading is tracked from the start.

2. **Compilation**: Rye compiler attaches `rye_src` attributes to compiled expressions (unchanged)

3. **Execution Tracking**: When `eval_compiled()` runs:
   ```r
   # Extract source info from compiled expression
   rye_src <- source_tracker$src_get(compiled_expr)

   # Track execution if coverage enabled
   if (!is.null(coverage_tracker)) {
     coverage_tracker$track(rye_src)  # Marks lines as executed
   }
   ```

4. **Reporting**: Tracker compares executed lines against all code lines to calculate coverage

### Edge Cases

- **Multi-line expressions**: All lines in range are marked as executed
- **Macro-generated code**: Attributed to macro call site (via `src_inherit`)
- **Module cache**: Disabled during coverage (`use_env_cache = FALSE`)
- **Unreachable code**: Correctly shows 0% coverage

## Performance

- **Overhead when enabled**: Minimal (<2s for full test suite)
- **Overhead when disabled**: Zero (NULL check short-circuits)
- **Normal execution**: Unaffected (coverage is opt-in)

## Files Changed

### Created
- `R/coverage.R` - RyeCoverageTracker class (~450 lines)

### Modified
- `R/runtime.R` - Added coverage tracking to `eval_compiled()` (~10 lines)
- `R/rye-engine.R` - Added coverage methods (~50 lines)
- `tools/rye-coverage.R` - Simplified to thin wrapper (~100 lines, down from 481)

### Updated
- `NAMESPACE` - Exported RyeCoverageTracker
- `man/RyeCoverageTracker.Rd` - Documentation (auto-generated)
- `man/RyeEngine.Rd` - Documentation (auto-generated)

### Unchanged
- `tools/coverage-combine.R` - Still reads JSON from both sources
- `.github/workflows/coverage.yaml` - CI workflow unchanged
- `codecov.yml` - Configuration unchanged
