# Strength Reduction Benchmark
#
# Measures the impact of replacing expensive operations with cheaper ones:
# - Priority 9: Pattern-based arithmetic rewrites

# Load development version
devtools::load_all()
library(bench)

# Create engine
engine <- RyeEngine$new()

# Set up test data
engine$eval(engine$read("(define x 5)")[[1]])
engine$eval(engine$read("(define y 10)")[[1]])

# ============================================================================
# Benchmark 1: Multiplication by 2
# ============================================================================

cat("\n=== Benchmark 1: Multiplication by 2 ===\n")

# Compare (* x 2) vs (+ x x)
mult_by_2 <- engine$inspect_compilation("(* x 2)")
add_twice <- engine$inspect_compilation("(+ x x)")

cat("\nCompiled code:\n")
cat("  (* x 2):", deparse(mult_by_2$compiled), "\n")
cat("  (+ x x):", deparse(add_twice$compiled), "\n")

cat("\nRuntime Performance:\n")
bench_result1 <- bench::mark(
  mult = engine$eval(engine$read("(* x 2)")[[1]]),
  add = engine$eval(engine$read("(+ x x)")[[1]]),
  iterations = 10000,
  check = FALSE
)
print(bench_result1[, c("expression", "min", "median", "mem_alloc")])

# ============================================================================
# Benchmark 2: Power of 2
# ============================================================================

cat("\n\n=== Benchmark 2: Power of 2 ===\n")

# Compare (^ x 2) vs (* x x)
power_2 <- engine$inspect_compilation("(^ x 2)")
mult_self <- engine$inspect_compilation("(* x x)")

cat("\nCompiled code:\n")
cat("  (^ x 2):", deparse(power_2$compiled), "\n")
cat("  (* x x):", deparse(mult_self$compiled), "\n")

cat("\nRuntime Performance:\n")
bench_result2 <- bench::mark(
  power = engine$eval(engine$read("(^ x 2)")[[1]]),
  mult = engine$eval(engine$read("(* x x)")[[1]]),
  iterations = 10000,
  check = FALSE
)
print(bench_result2[, c("expression", "min", "median", "mem_alloc")])

# ============================================================================
# Benchmark 3: Arithmetic-Heavy Code
# ============================================================================

cat("\n\n=== Benchmark 3: Arithmetic-Heavy Computation ===\n")

# Typical numeric computation with multiple strength reduction opportunities
computation <- "(+ (* x 2) (^ y 2))"
comp_compiled <- engine$inspect_compilation(computation)

cat("\nCompiled code complexity:", length(deparse(comp_compiled$compiled)), "lines\n")
cat("Code:", deparse(comp_compiled$compiled)[1], "\n")

cat("\nRuntime Performance:\n")
bench_result3 <- bench::mark(
  computation = engine$eval(engine$read(computation)[[1]]),
  iterations = 10000,
  check = FALSE
)
print(bench_result3[, c("expression", "min", "median", "mem_alloc")])

# ============================================================================
# Performance Impact Summary
# ============================================================================

cat("\n\n=== Performance Impact Analysis ===\n")

# Manual comparison: what's the theoretical speedup?
cat("\nTheoretical impact (R primitives) - mult by 2:\n")
manual_bench1 <- bench::mark(
  mult_by_2 = 5 * 2,
  add_twice = 5 + 5,
  iterations = 100000,
  check = FALSE
)
print(manual_bench1[, c("expression", "min", "median")])

cat("\nTheoretical impact (R primitives) - power of 2:\n")
manual_bench2 <- bench::mark(
  power_2 = 5 ^ 2,
  mult_self = 5 * 5,
  iterations = 100000,
  check = FALSE
)
print(manual_bench2[, c("expression", "min", "median")])

cat("\n=== Baseline Established ===\n")
cat("After optimization, expect:\n")
cat("  - Multiplication by 2: replace with addition\n")
cat("  - Power of 2: replace with multiplication\n")
cat("  - 10-20% speedup for arithmetic-heavy code\n")
