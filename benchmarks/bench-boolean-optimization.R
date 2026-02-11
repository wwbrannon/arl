# Boolean Operator Optimization Benchmark
#
# Measures the impact of optimizing and/or compilation:
# - Priority 6: Eliminating temps for simple values
# - Priority 7: Flattening nested boolean chains

# Load development version
devtools::load_all()
library(bench)

# Create engine
engine <- Engine$new()

# Helper to compile and extract R code
compile_expr <- function(code_str) {
  engine$inspect_compilation(code_str)$compiled
}

# ============================================================================
# Benchmark 1: Simple Value Temps Elimination
# ============================================================================

cat("\n=== Benchmark 1: And/Or with Simple Values ===\n")

# Compile test expressions
and_literals <- compile_expr("(and 1 2 3 4 5)")
and_symbols <- compile_expr("(and a b c d e)")
or_literals <- compile_expr("(or 1 2 3 4 5)")
or_symbols <- compile_expr("(or a b c d e)")

# Measure code complexity
cat("\nCode Complexity (deparse length):\n")
cat("  (and 1 2 3 4 5):", length(deparse(and_literals)), "lines\n")
cat("  (and a b c d e):", length(deparse(and_symbols)), "lines\n")
cat("  (or 1 2 3 4 5):", length(deparse(or_literals)), "lines\n")
cat("  (or a b c d e):", length(deparse(or_symbols)), "lines\n")

# Runtime benchmark with symbols
engine$eval(engine$read("(define a #t)"))
engine$eval(engine$read("(define b #t)"))
engine$eval(engine$read("(define c #t)"))
engine$eval(engine$read("(define d #f)"))
engine$eval(engine$read("(define e #t)"))

cat("\nRuntime Performance:\n")
bench_result <- bench::mark(
  and_chain = engine$eval(engine$read("(and a b c d e)")),
  or_chain = engine$eval(engine$read("(or a b c d e)")),
  iterations = 10000,
  check = FALSE
)
print(bench_result[, c("expression", "min", "median", "mem_alloc")])

# ============================================================================
# Benchmark 2: Nested Boolean Flattening
# ============================================================================

cat("\n\n=== Benchmark 2: Nested Boolean Chains ===\n")

# Compare nested vs flat
nested_and <- compile_expr("(and (and (and a b) c) d)")
flat_and <- compile_expr("(and a b c d)")
nested_or <- compile_expr("(or (or (or a b) c) d)")
flat_or <- compile_expr("(or a b c d)")

cat("\nCode Complexity:\n")
cat("  Nested AND:", length(deparse(nested_and)), "lines\n")
cat("  Flat AND:", length(deparse(flat_and)), "lines\n")
cat("  Nested OR:", length(deparse(nested_or)), "lines\n")
cat("  Flat OR:", length(deparse(flat_or)), "lines\n")

cat("\nRuntime Performance:\n")
bench_result2 <- bench::mark(
  nested_and = engine$eval(engine$read("(and (and (and a b) c) d)")),
  flat_and = engine$eval(engine$read("(and a b c d)")),
  nested_or = engine$eval(engine$read("(or (or (or a b) c) d)")),
  flat_or = engine$eval(engine$read("(or a b c d)")),
  iterations = 10000,
  check = FALSE
)
print(bench_result2[, c("expression", "min", "median", "mem_alloc")])

# ============================================================================
# Benchmark 3: Real-World Boolean Logic
# ============================================================================

cat("\n\n=== Benchmark 3: Real-World Boolean-Heavy Code ===\n")

# Simulate a chain of boolean checks (common in validation/filtering)
chain_code <- "(and
  (> x 10)
  (< x 100)
  (> y 5)
  (< y 50)
  (not (= z 0))
)"

chain_compiled <- compile_expr(chain_code)
cat("\nBoolean chain complexity:", length(deparse(chain_compiled)), "lines\n")

# Setup test data
engine$eval(engine$read("(define x 50)"))
engine$eval(engine$read("(define y 25)"))
engine$eval(engine$read("(define z 1)"))

cat("\nRuntime Performance:\n")
bench_result3 <- bench::mark(
  validation = engine$eval(engine$read(chain_code)),
  iterations = 5000,
  check = FALSE
)
print(bench_result3[, c("expression", "min", "median", "mem_alloc")])

cat("\n=== Baseline Established ===\n")
cat("After optimization, expect:\n")
cat("  - 20-40% reduction in code size for simple values\n")
cat("  - 30-50% faster execution for boolean-heavy code\n")
cat("  - Reduced memory allocation\n")
