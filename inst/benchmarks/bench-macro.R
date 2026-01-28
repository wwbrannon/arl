# Macro Benchmarks
# Benchmarks for the macro expansion system

library(rye)
source("inst/benchmarks/benchmark-helpers.R")
source("inst/benchmarks/workloads.R")

cat("=== Macro Benchmarks ===\n\n")

# Set up environment with macros
setup_macro_env <- function() {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)
  env
}

# Benchmark 1: Simple macros (single unquote)
cat("Benchmark 1: Simple macro expansion\n")
env1 <- setup_macro_env()

# Define simple macro
rye:::rye_eval_text('
(defmacro simple (x)
  `(+ ,x 1))
', env1)

simple_expr <- rye_read("(simple 42)")[[1]]

bench_simple <- benchmark_component(
  "Simple macro" = rye_macroexpand(simple_expr, env1),
  iterations = 1000,
  check = FALSE
)
print(bench_simple[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Complex quasiquote (multiple unquotes, splicing)
cat("Benchmark 2: Complex quasiquote\n")
env2 <- setup_macro_env()

rye:::rye_eval_text('
(defmacro complex (x . rest)
  `(list ,x ,@rest (+ ,x 1)))
', env2)

complex_expr <- rye_read("(complex 1 2 3 4 5)")[[1]]

bench_complex <- benchmark_component(
  "Complex macro" = rye_macroexpand(complex_expr, env2),
  iterations = 1000,
  check = FALSE
)
print(bench_complex[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Nested macro expansion
cat("Benchmark 3: Nested macro expansion\n")
env3 <- setup_macro_env()

rye:::rye_eval_text('
(defmacro outer (x)
  `(inner ,x))

(defmacro inner (x)
  `(+ ,x 1))
', env3)

nested_expr <- rye_read("(outer (outer 42))")[[1]]

bench_nested <- benchmark_component(
  "Nested macros" = rye_macroexpand(nested_expr, env3),
  iterations = 1000,
  check = FALSE
)
print(bench_nested[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Hygiene overhead
cat("Benchmark 4: Hygiene processing\n")
env4 <- setup_macro_env()

rye:::rye_eval_text('
(defmacro let-macro (bindings . body)
  `((lambda ,(map car bindings) ,@body)
    ,@(map (lambda (b) (car (cdr b))) bindings)))
', env4)

hygiene_expr <- rye_read("(let-macro ((x 1) (y 2)) (+ x y))")[[1]]

bench_hygiene <- benchmark_component(
  "With hygiene" = rye_macroexpand(hygiene_expr, env4),
  iterations = 1000,
  check = FALSE
)
print(bench_hygiene[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 5: Macro-heavy code
cat("Benchmark 5: Macro-heavy code expansion\n")
env5 <- setup_macro_env()

# Load standard macros from modules
tryCatch({
  rye:::import_stdlib_modules(env5, c("control", "binding"))

  macro_heavy <- '
(when #t
  (unless #f
    (cond
      (#f 1)
      (#f 2)
      (else 3))))
'

  macro_heavy_exprs <- rye_read(macro_heavy)

  bench_heavy <- benchmark_component(
    "Macro-heavy" = lapply(macro_heavy_exprs, function(e) rye_macroexpand(e, env5)),
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
  env6 <- setup_macro_env()

  bench_real <- benchmark_component(
    "macro-examples.rye" = {
      exprs <- rye_read(real_workloads$macro_examples)
      for (expr in exprs) {
        rye_eval(expr, env6)
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

# Save results
save_benchmark_results(macro_results, "macro")

cat("=== Macro Benchmarks Complete ===\n")
