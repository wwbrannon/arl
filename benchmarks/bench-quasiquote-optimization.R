# Quasiquote Optimization Benchmark
#
# Measures the impact of simplifying quasiquote compilation:
# - Priority 8: Detect all-quoted templates and simplify construction

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
# Benchmark 1: All-Quoted Templates
# ============================================================================

cat("\n=== Benchmark 1: All-Quoted Templates (No Unquotes) ===\n")

# Compile various quasiquote expressions
simple_list <- compile_expr("`(a b c)")
longer_list <- compile_expr("`(a b c d e f g h)")
nested_list <- compile_expr("`(a (b c) (d (e f)))")

cat("\nCode Complexity (deparse length):\n")
cat("  `(a b c):", length(deparse(simple_list)), "lines\n")
cat("  `(a b c d e f g h):", length(deparse(longer_list)), "lines\n")
cat("  `(a (b c) (d (e f))):", length(deparse(nested_list)), "lines\n")

cat("\nCode sample - `(a b c):\n")
cat(deparse(simple_list)[1], "\n")

# Runtime benchmark
cat("\nRuntime Performance:\n")
bench_result <- bench::mark(
  simple = eval(simple_list),
  longer = eval(longer_list),
  nested = eval(nested_list),
  iterations = 10000,
  check = FALSE
)
print(bench_result[, c("expression", "min", "median", "mem_alloc")])

# ============================================================================
# Benchmark 2: Mixed (Quoted + Unquoted)
# ============================================================================

cat("\n\n=== Benchmark 2: With Unquotes ===\n")

# Set up environment
engine$eval(engine$read("(define x 42)")[[1]])
engine$eval(engine$read("(define y 100)")[[1]])

with_unquote <- compile_expr("`(a ,x c)")
multiple_unquotes <- compile_expr("`(a ,x b ,y c)")

cat("\nCode Complexity:\n")
cat("  `(a ,x c):", length(deparse(with_unquote)), "lines\n")
cat("  `(a ,x b ,y c):", length(deparse(multiple_unquotes)), "lines\n")

# ============================================================================
# Benchmark 3: Macro Use Case
# ============================================================================

cat("\n\n=== Benchmark 3: Macro Template Expansion ===\n")

# Macros heavily use quasiquote for code generation
# Measure quasiquote evaluation in typical macro pattern
macro_template <- compile_expr("`(lambda (x) (+ x 1))")

cat("\nMacro template complexity:", length(deparse(macro_template)), "lines\n")

cat("\nRuntime Performance:\n")
bench_result3 <- bench::mark(
  macro_template = eval(macro_template),
  iterations = 5000,
  check = FALSE
)
print(bench_result3[, c("expression", "min", "median", "mem_alloc")])

# ============================================================================
# Benchmark 4: Code Size Analysis
# ============================================================================

cat("\n\n=== Code Size Analysis ===\n")

count_nodes <- function(expr) {
  if (is.call(expr)) {
    1 + sum(sapply(as.list(expr), count_nodes))
  } else if (is.list(expr)) {
    sum(sapply(expr, count_nodes))
  } else {
    1
  }
}

cat("\nAST Node Count:\n")
cat("  `(a b c):", count_nodes(simple_list), "nodes\n")
cat("  `(a ,x c):", count_nodes(with_unquote), "nodes\n")
cat("  `(lambda (x) (+ x 1)):", count_nodes(macro_template), "nodes\n")

cat("\n=== Baseline Established ===\n")
cat("After optimization, expect:\n")
cat("  - 30-50% reduction in compiled code size for all-quoted templates\n")
cat("  - 20-30% faster quasiquote evaluation\n")
cat("  - Reduced AST complexity\n")
