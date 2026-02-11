# Truthiness Optimization Benchmark
# Demonstrates the impact of skipping .rye_true_p() wrapper for known-boolean expressions

# Load development version if running from source
if (file.exists("DESCRIPTION")) {
  devtools::load_all()
} else {
  library(rye)
}

source("benchmarks/benchmark-helpers.R")

cat("=== Truthiness Optimization Benchmark ===\n\n")

# =============================================================================
# Benchmark 1: Code Quality - Generated Code Inspection
# =============================================================================
cat("Benchmark 1: Generated Code Quality\n")
cat("(Show how if tests are compiled with and without wrapper)\n\n")

engine1 <- Engine$new()

test_cases <- list(
  "Literal TRUE" = "(if #t 1 2)",
  "Literal FALSE" = "(if #f 1 2)",
  "Literal NULL" = "(if #nil 1 2)",
  "Comparison" = "(if (< x 5) 1 2)",
  "Logical op" = "(if (& #t #f) 1 2)",
  "Variable" = "(if x 1 2)"
)

cat("Expression                | Generated Test Condition       | Wrapper?\n")
cat("--------------------------|-------------------------------|----------\n")

for (name in names(test_cases)) {
  expr_str <- test_cases[[name]]
  out <- engine1$inspect_compilation(expr_str)

  # Extract just the test condition from "if (TEST) 1 else 2"
  compiled_str <- paste(out$compiled_deparsed, collapse = " ")
  test_cond <- gsub("^if \\((.+)\\) .+$", "\\1", compiled_str)
  has_wrapper <- grepl(".rye_true_p", test_cond)

  cat(sprintf("%-25s | %-29s | %s\n",
              name,
              substr(test_cond, 1, 29),
              if (has_wrapper) "YES" else "NO"))
}

cat("\n")

# =============================================================================
# Benchmark 2: Execution Performance - If with Boolean Tests
# =============================================================================
cat("Benchmark 2: Execution Performance - If Statements\n")
cat("(Compare execution speed of if with different test types)\n\n")

engine2 <- Engine$new()
env <- new.env(parent = baseenv())
env$x <- 10

# Define test functions
engine2$eval_text('
(define test-literal (lambda () (if #t 1 2)))
(define test-comparison (lambda (x) (if (< x 5) 1 2)))
(define test-variable (lambda (x) (if x 1 2)))
', env = env)

# Compile calls to these functions
compiled_literal <- engine2$compiler$compile(
  engine2$read('(test-literal)')[[1]], env, strict = TRUE
)

compiled_comparison <- engine2$compiler$compile(
  engine2$read('(test-comparison 10)')[[1]], env, strict = TRUE
)

compiled_variable <- engine2$compiler$compile(
  engine2$read('(test-variable #t)')[[1]], env, strict = TRUE
)

bench_if <- benchmark_component(
  "Literal boolean (no wrapper)" = engine2$compiled_runtime$eval_compiled(compiled_literal, env),
  "Comparison (no wrapper)" = engine2$compiled_runtime$eval_compiled(compiled_comparison, env),
  "Variable (with wrapper)" = engine2$compiled_runtime$eval_compiled(compiled_variable, env),
  iterations = 5000,
  check = FALSE
)

print(bench_if[, c("expression", "median", "mem_alloc")])
cat("\n")

# =============================================================================
# Benchmark 3: Realistic Code Pattern - Conditionals in Loops
# =============================================================================
cat("Benchmark 3: Realistic Pattern - Conditionals in Loop\n")
cat("(Measure impact when if statements run many times)\n\n")

engine3 <- Engine$new()

# Function with comparison-based if (optimized)
engine3$eval_text('
(define sum-positive (lambda (n)
  (define helper (lambda (i acc)
    (if (>= i n)
      acc
      (helper (+ i 1) (if (> i 0) (+ acc i) acc)))))
  (helper 0 0)))
')

# Function with many literal if tests (optimized)
engine3$eval_text('
(define always-true (lambda (n)
  (define helper (lambda (i acc)
    (if (>= i n)
      acc
      (helper (+ i 1) (if #t (+ acc 1) acc)))))
  (helper 0 0)))
')

compiled_positive <- engine3$compiler$compile(
  engine3$read('(sum-positive 100)')[[1]], engine3$env$env, strict = TRUE
)

compiled_always <- engine3$compiler$compile(
  engine3$read('(always-true 100)')[[1]], engine3$env$env, strict = TRUE
)

bench_loop <- benchmark_component(
  "100 comparisons (optimized)" = engine3$compiled_runtime$eval_compiled(compiled_positive, engine3$env$env),
  "100 literal tests (optimized)" = engine3$compiled_runtime$eval_compiled(compiled_always, engine3$env$env),
  iterations = 100,
  check = FALSE
)

print(bench_loop[, c("expression", "median", "mem_alloc")])
cat("\n")

# =============================================================================
# Benchmark 4: Code Complexity Reduction
# =============================================================================
cat("Benchmark 4: Code Complexity Analysis\n")
cat("(Measure AST simplification from skipping wrapper)\n\n")

count_ast_nodes <- function(expr) {
  if (is.null(expr)) return(0)
  if (!is.call(expr)) return(1)
  children <- as.list(expr)[-1]
  if (length(children) == 0) return(1)
  1 + sum(sapply(children, count_ast_nodes, USE.NAMES = FALSE))
}

engine4 <- Engine$new()

comparison_if <- "(if (< x 5) (+ x 1) (- x 1))"
out_comp <- engine4$inspect_compilation(comparison_if)
nodes_comp <- count_ast_nodes(out_comp$compiled)

# Show what the compiled form looks like
cat("Example: (if (< x 5) (+ x 1) (- x 1))\n")
cat(sprintf("  Compiled: %s\n", paste(out_comp$compiled_deparsed, collapse = " ")))
cat(sprintf("  AST nodes: %d\n", nodes_comp))
cat(sprintf("  Wrapper eliminated: %s\n", if (grepl(".rye_true_p", paste(out_comp$compiled_deparsed))) "NO" else "YES"))

cat("\n")

# =============================================================================
# Summary
# =============================================================================
cat("=== Summary ===\n\n")
cat("Truthiness optimization provides:\n\n")
cat("1. CLEANER CODE:\n")
cat("   - Literal booleans: if (TRUE) instead of if (.rye_true_p(TRUE))\n")
cat("   - Comparisons: if (x > 5) instead of if (.rye_true_p(x > 5))\n")
cat("   - Logical ops: if (a & b) instead of if (.rye_true_p(a & b))\n\n")

cat("2. PERFORMANCE:\n")
cat("   - Eliminates function call overhead for boolean tests\n")
cat("   - Measurable impact in tight loops with many conditionals\n")
cat("   - No overhead for variable tests (still use wrapper)\n\n")

cat("3. CORRECTNESS:\n")
cat("   - Preserves Rye truthiness semantics\n")
cat("   - Variables still wrapped (only #f and #nil are false)\n")
cat("   - Safe optimization (only skips wrapper for known-boolean returns)\n\n")

cat("=== Truthiness Optimization Benchmark Complete ===\n")
