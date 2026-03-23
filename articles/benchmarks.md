# Benchmarks

This page summarizes Arl’s performance characteristics. For full
profiling details and optimization history, see
[`benchmarks/PERFORMANCE.md`](https://github.com/wwbrannon/arl/blob/main/benchmarks/PERFORMANCE.md)
in the repository.

## Pipeline Overview

Every Arl expression passes through five stages:

    Source → Tokenizer → Parser → Macro Expander → Compiler → R eval() → Result

The table below shows where time is spent on each of the example
programs shipped with the package:

*Benchmark data not available — rebuild with access to the `gh-pages`
branch.*

The compiler, R eval(), and macro expander together dominate runtime.
The tokenizer and parser are fast by comparison.

## End-to-End Timings

The same workloads measured end-to-end:

*Benchmark data not available — rebuild with access to the `gh-pages`
branch.*

These are end-to-end times (i.e., tokenize + parse + expand + compile +
evaluate), and also include engine startup and module loading.

## Optimizations Applied

Three O(n²) bottlenecks were identified by code inspection and fixed:

1.  **Tokenizer string accumulation** — character-by-character
    [`c()`](https://rdrr.io/r/base/c.html) replaced with a regex-based
    approach that processes entire tokens in one pass, eliminating
    per-character overhead entirely.

2.  **Parser list growing** — repeated `c(elements, list(elem))`
    replaced with chunked collection. ~9× improvement for large flat
    lists (1000 elements: ~60 ms → 6.9 ms).

3.  **CPS overhead removed** — the evaluator was converted from
    continuation-passing style with trampolines to a compiler that emits
    R code evaluated by R’s native
    [`eval()`](https://rdrr.io/r/base/eval.html), removing
    per-expression closure allocation.

*Benchmark data not available.*

All fixes preserve correctness (full test suite passes) and
significantly improve performance on most code, as well as preventing
worst-case blowups.

## Benchmark Data

Historical benchmark results are stored on the `gh-pages` branch in
`dev/bench/data.js`. You can inspect them with:

``` bash
git show gh-pages:dev/bench/data.js
```

Each entry records a benchmark run with commit metadata and
per-benchmark timings. The tables on this page are generated from the
latest run.

## Running Benchmarks

From the repository root:

``` bash
make bench
```

This runs the full benchmark suite (component-level and end-to-end) and
saves results to `benchmarks/results/`. Individual component benchmarks
are also available:

``` bash
# COMPONENT can be tokenizer, parser, macro, compile, r-eval, stdlib, e2e
make bench-component COMPONENT=tokenizer
```

Profiling reports (HTML flame graphs) can be generated with:

``` bash
make profile

# View: open benchmarks/profiles/eval-fibonacci.html
```
