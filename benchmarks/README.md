# Arl Performance Benchmarks

This directory contains the performance profiling and benchmarking infrastructure for Arl.

## Quick Start

```bash
# Via Makefile (recommended)
make bench      # Run all benchmarks
make profile    # Note: profvis requires interactive R

# Or via R console
R -e "source('benchmarks/run-all-benchmarks.R')"
```

```r
# In interactive R session
install.packages(c("bench", "profvis"))

# Benchmarking (works in all contexts)
source("benchmarks/run-all-benchmarks.R")
source("benchmarks/bench-tokenizer.R")  # Individual components

# Profiling (requires interactive session)
# Note: profvis doesn't work from sourced scripts - run interactively
library(arl)
source("benchmarks/profile-tokenizer.R")
source("benchmarks/profile-parser.R")
source("benchmarks/profile-macro.R")
source("benchmarks/profile-compile.R")
source("benchmarks/profile-r-eval.R")
```

## Files

### Infrastructure
- `benchmark-helpers.R` - Helper functions for benchmarking and profiling
- `workloads.R` - Pre-defined test workloads (micro, small, medium, large, XL, real examples)
- `README.md` - This file

### Component Benchmarks
- `bench-tokenizer.R` - Lexical analysis benchmarks
- `bench-parser.R` - S-expression parsing benchmarks
- `bench-macro.R` - Macro expansion benchmarks
- `bench-compile.R` - Compiler benchmarks
- `bench-r-eval.R` - R eval() benchmarks
- `bench-stdlib.R` - Standard library function benchmarks
- `bench-interop.R` - R interoperability benchmarks (function calls, object construction)
- `bench-modules.R` - Module/import and load/run overhead benchmarks
- `bench-e2e.R` - End-to-end pipeline benchmarks with component breakdown

### Master Scripts
- `run-all-benchmarks.R` - Run all benchmarks and save results
- `run-all-profiles.R` - Generate profiling reports for all components
- `analyze-results.R` - Analysis functions for benchmark results
- `compare-results.R` - Compare two benchmark runs

### Profiling Scripts
- `profile-tokenizer.R` - Profile tokenizer with large strings
- `profile-parser.R` - Profile parser with deep nesting
- `profile-macro.R` - Profile macro expansion
- `profile-compile.R` - Profile compiler with various workloads
- `profile-r-eval.R` - Profile R eval() with recursive workloads

## Output Directories

- `results/` - Benchmark results saved as timestamped RDS files (gitignored)
- `profiles/` - HTML profiling reports from profvis (gitignored)

## Benchmark Components

Each benchmark script measures different aspects of Arl performance:

### Tokenizer (`bench-tokenizer.R`)
- String literals (10, 100, 1K, 10K characters)
- Nested parentheses (10, 50, 100 levels)
- Mixed content (strings, numbers, symbols)
- Escape sequences
- Real example files

### Parser (`bench-parser.R`)
- Flat lists (10, 100, 1000 elements)
- Nested lists (depth 5, 10, 20)
- Quote sugar and keywords
- NULL value handling
- Real example files (pre-tokenized to isolate parser)

### Macro (`bench-macro.R`)
- Simple macros (single unquote)
- Complex quasiquote (multiple unquotes, splicing)
- Nested macro expansion
- Hygiene overhead
- Macro-heavy code

### Compiler (`bench-compile.R`)
- Simple arithmetic compilation
- Function calls (1, 5, 10 arguments)
- Special forms (if, define, lambda, begin)
- Recursive functions (fibonacci, factorial)
- Closures and environments

### R eval() (`bench-r-eval.R`)
- Simple arithmetic evaluation
- Function calls (1, 5, 10 arguments)
- Special forms (if, define, lambda, begin)
- Recursive functions (fibonacci, factorial)
- Closures and environments

### Standard Library (`bench-stdlib.R`)
- List operations (car, cdr, length, reverse)
- Higher-order functions (map, filter, reduce) on 10, 100, 1000 elements
- Function composition
- String operations
- Predicates
- List construction
- Nested operations

### End-to-End (`bench-e2e.R`)
- Full pipeline (tokenize → parse → eval)
- Component breakdown showing % time in each phase
- Synthetic workloads (micro, small, medium, large)
- Real example files
- String-heavy workloads
- Many-argument workloads
- REPL-style interaction

### R Interop (`bench-interop.R`)
- R function calls (positional vs named arguments)
- R object construction (vectors, lists, data.frame, formulas)

### Modules/Load/Run (`bench-modules.R`)
- `(import ...)` module load overhead
- `(load ...)` vs `(run ...)` overhead for small scripts

## Usage Examples

```r
# Run tokenizer benchmarks
source("benchmarks/bench-tokenizer.R")

# Run end-to-end with component breakdown
source("benchmarks/bench-e2e.R")

# Load and analyze saved results
source("benchmarks/benchmark-helpers.R")
results <- load_benchmark_results("benchmarks/results/baseline-20260127-123456.rds")
summarize_benchmark(results)

# Profile a specific component
source("benchmarks/benchmark-helpers.R")
profile_component({
  engine <- Engine$new()
  tokens <- engine_field(engine, "tokenizer")$tokenize(paste(rep("x", 10000), collapse = ""))
}, "large-string")
```

## Workloads

Pre-defined workloads are available in `workloads.R`:

- **WORKLOAD_MICRO**: `(+ 1 2)` - baseline overhead
- **WORKLOAD_SMALL**: 10-line recursive fibonacci
- **WORKLOAD_MEDIUM**: 100-line quicksort with data
- **WORKLOAD_LARGE**: 500-line macro expansion examples
- **WORKLOAD_XL**: 2000-element synthetic nested lists
- **Real workloads**: fibonacci.arl, quicksort.arl, macro-examples.arl, etc.

## Performance Issues Identified

See `PERFORMANCE.md` for detailed analysis and optimization history.

## Dependencies

- `bench` (>= 1.1.2) - Precise benchmarking with statistical rigor
- `profvis` (>= 0.3.7) - Interactive flame graph profiling

Both are listed in `DESCRIPTION` Suggests field.
