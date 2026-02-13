# Code Generation Quality Benchmarks
# Measures the runtime performance and code size of generated expressions

library(arl)

source("benchmarks/benchmark-helpers.R")

cat("=== Code Generation Quality Benchmarks ===\n\n")

# Helper to access private engine fields in benchmarks
engine_field <- function(engine, name) {
  engine$.__enclos_env__$private[[paste0(".", name)]]
}

# Helper to measure code size
measure_code_size <- function(expr_str, engine) {
  expr <- engine$read(expr_str)[[1]]
  compiled <- engine_field(engine, "compiler")$compile(expr, engine$get_env(), strict = TRUE)
  deparsed <- deparse(compiled)
  list(
    lines = length(deparsed),
    chars = sum(nchar(deparsed)),
    depth = max_expression_depth(compiled)
  )
}

# Helper to measure expression depth
max_expression_depth <- function(expr, depth = 0) {
  if (!is.call(expr)) return(depth)
  max_child_depth <- depth
  for (i in seq_along(expr)) {
    child_depth <- max_expression_depth(expr[[i]], depth + 1)
    max_child_depth <- max(max_child_depth, child_depth)
  }
  max_child_depth
}

# Benchmark 1: Constant Folding - Runtime Performance
cat("Benchmark 1: Constant Folding - Runtime Performance\n")
engine1 <- Engine$new()

# Compile the expressions once
arithmetic_expr <- engine1$read("(+ 1 2)")[[1]]
compiled_arithmetic <- engine_field(engine1, "compiler")$compile(arithmetic_expr, engine1$get_env(), strict = TRUE)

nested_expr <- engine1$read("(+ (* 2 3) (* 4 5))")[[1]]
compiled_nested <- engine_field(engine1, "compiler")$compile(nested_expr, engine1$get_env(), strict = TRUE)

comparison_expr <- engine1$read("(< 1 2)")[[1]]
compiled_comparison <- engine_field(engine1, "compiler")$compile(comparison_expr, engine1$get_env(), strict = TRUE)

bench_constant_folding <- benchmark_component(
  "Arithmetic (+ 1 2)" = engine_field(engine1, "compiled_runtime")$eval_compiled(compiled_arithmetic, engine1$get_env()),
  "Nested arithmetic" = engine_field(engine1, "compiled_runtime")$eval_compiled(compiled_nested, engine1$get_env()),
  "Comparison (< 1 2)" = engine_field(engine1, "compiled_runtime")$eval_compiled(compiled_comparison, engine1$get_env()),
  iterations = 5000,
  check = FALSE
)
print(bench_constant_folding[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Constant Folding - Code Size
cat("Benchmark 2: Constant Folding - Code Size\n")
engine2 <- Engine$new()

size_arithmetic <- measure_code_size("(+ 1 2)", engine2)
size_nested <- measure_code_size("(+ (* 2 3) (* 4 5))", engine2)
size_comparison <- measure_code_size("(< 1 2)", engine2)

cat(sprintf("  Arithmetic (+ 1 2):       %2d lines, %3d chars, depth %d\n",
            size_arithmetic$lines, size_arithmetic$chars, size_arithmetic$depth))
cat(sprintf("  Nested arithmetic:        %2d lines, %3d chars, depth %d\n",
            size_nested$lines, size_nested$chars, size_nested$depth))
cat(sprintf("  Comparison (< 1 2):       %2d lines, %3d chars, depth %d\n",
            size_comparison$lines, size_comparison$chars, size_comparison$depth))
cat("\n")

# Benchmark 3: Math Functions - Runtime Performance
cat("Benchmark 3: Math Functions - Runtime Performance\n")
engine3 <- Engine$new()

abs_expr <- engine3$read("(abs -5)")[[1]]
compiled_abs <- engine_field(engine3, "compiler")$compile(abs_expr, engine3$get_env(), strict = TRUE)

sqrt_expr <- engine3$read("(sqrt 16)")[[1]]
compiled_sqrt <- engine_field(engine3, "compiler")$compile(sqrt_expr, engine3$get_env(), strict = TRUE)

bench_math <- benchmark_component(
  "abs(-5)" = engine_field(engine3, "compiled_runtime")$eval_compiled(compiled_abs, engine3$get_env()),
  "sqrt(16)" = engine_field(engine3, "compiled_runtime")$eval_compiled(compiled_sqrt, engine3$get_env()),
  iterations = 5000,
  check = FALSE
)
print(bench_math[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Baseline - Non-constant expressions (should not change)
cat("Benchmark 4: Baseline - Variable expressions (should not change)\n")
engine4 <- Engine$new()
env <- new.env(parent = baseenv())
env$x <- 10
env$y <- 5

var_expr <- engine4$read("(+ x y)")[[1]]
compiled_var <- engine_field(engine4, "compiler")$compile(var_expr, env, strict = TRUE)

bench_baseline <- benchmark_component(
  "Variable addition (+ x y)" = engine_field(engine4, "compiled_runtime")$eval_compiled(compiled_var, env),
  iterations = 5000,
  check = FALSE
)
print(bench_baseline[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
codegen_results <- list(
  constant_folding_runtime = bench_constant_folding,
  constant_folding_size = list(
    arithmetic = size_arithmetic,
    nested = size_nested,
    comparison = size_comparison
  ),
  math_functions = bench_math,
  baseline = bench_baseline
)

# Save results
save_benchmark_results(codegen_results, "codegen-quality")

cat("=== Code Generation Quality Benchmarks Complete ===\n")
cat("\nNOTE: These benchmarks measure the RUNTIME performance of compiled code.\n")
cat("After implementing constant folding, expect:\n")
cat("  - Runtime: 10-20% faster for constant expressions\n")
cat("  - Code size: Simpler generated code (fewer lines/chars)\n")
cat("  - Baseline: Should remain unchanged (no regression)\n")
