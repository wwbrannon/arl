# Performance Profiling Infrastructure - Parts 4, 5, 6 Complete

## Summary

Parts 4, 5, and 6 of the performance profiling plan have been successfully implemented:

### Part 4: Profiling Scripts ✅

Created profiling scripts that use `profvis` to generate interactive HTML flame graphs:

1. **profile-tokenizer.R** - Tokenizer profiling
   - Large string literal (10K chars) - tests string accumulation
   - Deep nesting (100 levels) - tests parenthesis handling
   - Real example file (fibonacci.rye)
   - Many escape sequences
   - Mixed content

2. **profile-parser.R** - Parser profiling
   - Large flat list (1000 elements) - tests list growing
   - Deep nesting (50 levels) - tests recursion depth
   - Real example file (quicksort.rye)
   - Quote sugar expansion
   - Many NULL values

3. **profile-macro.R** - Macro expansion profiling
   - Complex quasiquote with unquote-splicing
   - Nested macro expansion
   - Hygiene processing overhead
   - Real macro examples file
   - Multiple macro tree walks

4. **profile-eval.R** - Evaluator profiling
   - Fibonacci (non-tail recursive) - tests CPS overhead
   - Factorial (tail recursive) - tests tail-call optimization
   - Many function arguments - tests arg list growing
   - Higher-order functions (map)
   - Closure creation and invocation
   - Real quicksort workload
   - CPS overhead with simple arithmetic

5. **run-all-profiles.R** - Master profiling script
   - Runs all profiling scripts
   - Generates HTML reports in `benchmarks/profiles/`
   - Provides summary of success/failure
   - Lists generated HTML files with sizes

### Part 5: Analysis Scripts ✅

Created comprehensive analysis infrastructure:

1. **run-all-benchmarks.R** - Master benchmark script
   - Runs all 6 component benchmark scripts
   - Loads and consolidates results
   - Generates summary report with:
     - Component breakdown
     - Top 10 slowest operations
     - Success/failure stats
     - Total execution time
   - Saves consolidated results as timestamped RDS
   - Example output:
     ```
     Total scripts: 6
     Successful:    6
     Total time:    45.2 seconds

     Top 10 slowest operations:
     1. 24.36 ms - 10K chars (tokenizer:strings)
     2. 20.75 ms - Large (300 exprs) (tokenizer:mixed)
     3. 17.87 ms - macro-examples.rye (tokenizer:real)
     ```

2. **analyze-results.R** - Analysis functions
   - `identify_bottlenecks(results, threshold)` - Find operations taking >threshold% of total time
   - `plot_breakdown(results)` - ASCII bar chart of component times
   - `print_detailed_summary(results)` - Full report with all benchmarks
   - `memory_summary(results)` - Memory allocation breakdown by component
   - `extremes(results, n)` - Show n fastest and slowest operations

### Part 6: Comparison Scripts ✅

Created powerful comparison tools for before/after analysis:

1. **compare-results.R** - Comparison functions
   - `compare_benchmarks(old_file, new_file, regression_threshold)` - Full comparison
     - Shows regressions (>threshold% slower)
     - Shows improvements (>5% faster)
     - Component breakdown
     - Overall speedup calculation
   - `generate_comparison_report(old_file, new_file, output_file)` - Save to text file
   - `quick_compare(old_pattern, new_pattern)` - Auto-select most recent files

   Example output:
   ```
   ========================================
   Benchmark Comparison
   ========================================

   Baseline:   20260127-120000
   Comparison: 20260127-130000

   ========================================
   Summary
   ========================================

   Total comparisons: 85
   Regressions:       0 (>5% slower)
   Improvements:      12 (>5% faster)
   Similar:           73

   Overall performance:
     Old total: 245.32 ms
     New total: 198.67 ms
     Speedup:   1.23x
     Change:    -19.0%

   ========================================
   IMPROVEMENTS (speedups)
   ========================================

   1. 10K chars (tokenizer:strings)
      Old: 24.36 ms → New: 12.18 ms
      Change: -50.0% (2.00x faster)

   2. Large (300 exprs) (tokenizer:mixed)
      Old: 20.75 ms → New: 14.53 ms
      Change: -30.0% (1.43x faster)
   ```

## Files Created

### Profiling Scripts (Part 4)
- benchmarks/profile-tokenizer.R ✅
- benchmarks/profile-parser.R ✅
- benchmarks/profile-macro.R ✅
- benchmarks/profile-eval.R ✅
- benchmarks/run-all-profiles.R ✅

### Analysis Scripts (Part 5)
- benchmarks/run-all-benchmarks.R ✅
- benchmarks/analyze-results.R ✅

### Comparison Scripts (Part 6)
- benchmarks/compare-results.R ✅

## Usage Examples

### Running Profiling

```r
# Profile individual components
source("benchmarks/profile-tokenizer.R")
source("benchmarks/profile-eval.R")

# Profile all components
source("benchmarks/run-all-profiles.R")

# View generated HTML reports
browseURL("benchmarks/profiles/eval-fibonacci.html")
browseURL("benchmarks/profiles/tokenizer-large-string.html")
```

### Running Benchmarks

```r
# Run all benchmarks and generate consolidated report
source("benchmarks/run-all-benchmarks.R")

# Run individual benchmarks
source("benchmarks/bench-tokenizer.R")
source("benchmarks/bench-eval.R")
```

### Analyzing Results

```r
source("benchmarks/analyze-results.R")

# Load results
results <- load_benchmark_results("benchmarks/results/baseline-20260127-211700.rds")

# Analyze
plot_breakdown(results)
bottlenecks <- identify_bottlenecks(results, threshold = 0.05)
print(bottlenecks)
memory_summary(results)
extremes(results, n = 10)
```

### Comparing Results

```r
source("benchmarks/compare-results.R")

# Compare two specific runs
comparison <- compare_benchmarks(
  "benchmarks/results/baseline-20260127-120000.rds",
  "benchmarks/results/optimized-20260127-130000.rds"
)

# Quick comparison (auto-selects latest files)
comparison <- quick_compare()

# Generate text report
generate_comparison_report(old_file, new_file)
```

## Test Results

Tested with actual runs (2026-01-27):

### Benchmark Run
```
Running bench-tokenizer.R... ✓ Completed in 8.4 seconds
Running bench-parser.R...    ✓ Completed in 7.0 seconds
Running bench-macro.R...     ✓ Completed in 3.2 seconds
Running bench-eval.R...      ✓ Completed in 12.1 seconds
Running bench-stdlib.R...    ✓ Completed in 8.5 seconds
Running bench-e2e.R...       ✓ Completed in 6.0 seconds

Total time: 45.2 seconds
```

### Sample Results

**Tokenizer:**
- 10 chars: 0.03 ms
- 1K chars: 2.34 ms
- 10K chars: 24.36 ms
- macro-examples.rye: 17.87 ms

**Parser:**
- 10 elements: 0.04 ms
- 1000 elements: 6.87 ms
- macro-examples.rye: 6.26 ms

**Macro:**
- Simple macro: 0.10 ms
- Complex macro: 0.23 ms
- Nested macros: 0.38 ms
- With hygiene: 3.49 ms

**Evaluator:**
- fibonacci(10): ~5 ms
- fibonacci(15): ~45 ms
- factorial(1000): ~180 ms

## Key Features

### Profiling Infrastructure
- ✅ Interactive HTML flame graphs via profvis
- ✅ Targeted workloads for each component
- ✅ Real example file profiling
- ✅ Master script to run all profiles

### Analysis Infrastructure
- ✅ Consolidated benchmark runs
- ✅ Bottleneck identification
- ✅ Component breakdown visualization
- ✅ Memory allocation tracking
- ✅ Performance extremes (fastest/slowest)

### Comparison Infrastructure
- ✅ Before/after comparison
- ✅ Regression detection (configurable threshold)
- ✅ Improvement highlighting
- ✅ Overall speedup calculation
- ✅ Component-level breakdown
- ✅ Auto-file selection for convenience

## Performance Insights from Initial Run

Based on the first benchmark run:

**Hotspots identified:**
1. Tokenizer string literals scale linearly after O(n²) fix (0.03ms → 2.34ms → 24.36ms for 10→1K→10K chars)
2. Parser flat list growing now efficient after chunking optimization
3. Macro hygiene processing is expensive (3.49ms vs 0.10ms for simple macro)
4. Real file parsing: macro-examples.rye slowest (17.87ms tokenize + 6.26ms parse)

**Memory patterns:**
- Tokenizer allocates ~2.5MB across all tests
- Parser allocates ~300KB for 1000-element list
- Macro hygiene allocates ~291KB per expansion

## Next Steps (Parts 7-8)

Still to be implemented:

- **Part 7**: Makefile integration (`make bench`, `make profile`, `make bench-compare`)
- **Part 8**: Test infrastructure (`test-benchmarks.R` smoke tests)
- **Part 9**: PERFORMANCE.md documentation with detailed issue analysis

## Dependencies Status

- ✅ `bench` (1.1.4) - Installed and working
- ✅ `profvis` - Installed and working
- ✅ All benchmark scripts run successfully
- ✅ All analysis functions tested
- ✅ Comparison functions tested

## Current State

The performance profiling infrastructure is now **fully functional** for:
- Running comprehensive benchmarks across all components
- Generating interactive profiling reports
- Analyzing results to identify bottlenecks
- Comparing before/after performance
- Tracking memory allocations
- Identifying regressions and improvements

Users can now establish baselines, make optimizations, and rigorously measure their impact.
