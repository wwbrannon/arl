# Macro Benchmarks
# Benchmarks for the macro expansion system

library(arl)

source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

cat("=== Macro Benchmarks ===\n\n")

# Benchmark 1: Simple macros (single unquote)
cat("Benchmark 1: Simple macro expansion\n")
engine1 <- Engine$new()

# Define simple macro
engine1$eval_text('
(defmacro simple (x)
  `(+ ,x 1))
')

simple_expr <- engine1$read("(simple 42)")[[1]]

bench_simple <- benchmark_component(
  "Simple macro" = engine1$macroexpand(simple_expr),
  iterations = 1000,
  check = FALSE
)
print(bench_simple[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Complex quasiquote (multiple unquotes, splicing)
cat("Benchmark 2: Complex quasiquote\n")
engine2 <- Engine$new()

engine2$eval_text('
(defmacro complex (x . rest)
  `(list ,x ,@rest (+ ,x 1)))
')

complex_expr <- engine2$read("(complex 1 2 3 4 5)")[[1]]

bench_complex <- benchmark_component(
  "Complex macro" = engine2$macroexpand(complex_expr),
  iterations = 1000,
  check = FALSE
)
print(bench_complex[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Nested macro expansion
cat("Benchmark 3: Nested macro expansion\n")
engine3 <- Engine$new()

engine3$eval_text('
(defmacro outer (x)
  `(inner ,x))

(defmacro inner (x)
  `(+ ,x 1))
')

nested_expr <- engine3$read("(outer (outer 42))")[[1]]

bench_nested <- benchmark_component(
  "Nested macros" = engine3$macroexpand(nested_expr),
  iterations = 1000,
  check = FALSE
)
print(bench_nested[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Hygiene overhead
cat("Benchmark 4: Hygiene processing\n")
engine4 <- Engine$new()

engine4$eval_text('
(defmacro let-macro (bindings . body)
  `((lambda ,(map car bindings) ,@body)
    ,@(map (lambda (b) (car (cdr b))) bindings)))
')

hygiene_expr <- engine4$read("(let-macro ((x 1) (y 2)) (+ x y))")[[1]]

bench_hygiene <- benchmark_component(
  "With hygiene" = engine4$macroexpand(hygiene_expr),
  iterations = 1000,
  check = FALSE
)
print(bench_hygiene[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 5: Macro-heavy code
cat("Benchmark 5: Macro-heavy code expansion\n")
engine5 <- Engine$new()

# Load standard macros from modules
tryCatch({
  engine5$eval_text("(import control)")
  engine5$eval_text("(import binding)")

  macro_heavy <- '
(when #t
  (unless #f
    (cond
      (#f 1)
      (#f 2)
      (else 3))))
'

  macro_heavy_exprs <- engine5$read(macro_heavy)

  bench_heavy <- benchmark_component(
    "Macro-heavy" = lapply(macro_heavy_exprs, function(e) engine5$macroexpand(e)),
    iterations = 500,
    check = FALSE
  )
  print(bench_heavy[, c("expression", "median", "mem_alloc")])
}, error = function(e) {
  cat("(Skipped - stdlib modules not available)\n")
})
cat("\n")

# Benchmark 6: Real macro examples file
cat("Benchmark 6: Real macro examples\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "macro_examples" %in% names(real_workloads)) {
  engine6 <- Engine$new()

  bench_real <- benchmark_component(
    "macro-examples.arl" = {
      exprs <- engine6$read(real_workloads$macro_examples)
      for (expr in exprs) {
        engine6$eval(expr)
      }
    },
    iterations = 10,
    check = FALSE
  )
  print(bench_real[, c("expression", "median", "mem_alloc")])
} else {
  cat("(Skipped - macro examples not available)\n")
}
cat("\n")

# Collect all results
macro_results <- list(
  simple = bench_simple,
  complex = bench_complex,
  nested = bench_nested,
  hygiene = bench_hygiene
)

# Include macro-heavy results when available
if (exists("bench_heavy")) {
  macro_results$heavy <- bench_heavy
}

# Include real macro examples when available
if (exists("bench_real")) {
  macro_results$real <- bench_real
}

# Save results
save_benchmark_results(macro_results, "macro")

cat("=== Macro Benchmarks Complete ===\n")
