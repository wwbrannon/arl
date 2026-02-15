# Compiler Benchmarks
# Benchmarks for the compiler component (Compiler$compile only)

library(arl)

source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

cat("=== Compiler Benchmarks ===\n\n")

# Benchmark 1: Simple arithmetic
cat("Benchmark 1: Simple arithmetic\n")
engine1 <- Engine$new()
compiler1 <- engine_field(engine1, "compiler")
env1 <- engine1$get_env()

# Pre-macroexpand expressions
expr_single <- engine1$macroexpand(engine1$read("(+ 1 2)")[[1]], env = env1, preserve_src = TRUE)
expr_nested <- engine1$macroexpand(engine1$read("(+ (+ 1 2) (+ 3 4))")[[1]], env = env1, preserve_src = TRUE)
expr_many <- engine1$macroexpand(engine1$read("(+ (+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9) 10)")[[1]], env = env1, preserve_src = TRUE)

bench_arithmetic <- benchmark_component(
  "Single add" = compiler1$compile(expr_single, env1, strict = TRUE),
  "Nested adds" = compiler1$compile(expr_nested, env1, strict = TRUE),
  "Many adds" = compiler1$compile(expr_many, env1, strict = TRUE),
  iterations = 1000,
  check = FALSE
)
print(bench_arithmetic[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Function calls with varying argument counts
cat("Benchmark 2: Function call overhead\n")
engine2 <- Engine$new()
compiler2 <- engine_field(engine2, "compiler")
env2 <- engine2$get_env()

engine2$eval_text('(define f1 (lambda (x) x))')
engine2$eval_text('(define f5 (lambda (a b c d e) (+ (+ (+ (+ a b) c) d) e)))')
engine2$eval_text('(define f10 (lambda (a b c d e f g h i j) (+ (+ (+ (+ (+ (+ (+ (+ (+ a b) c) d) e) f) g) h) i) j)))')

expr_1arg <- engine2$macroexpand(engine2$read("(f1 42)")[[1]], env = env2, preserve_src = TRUE)
expr_5arg <- engine2$macroexpand(engine2$read("(f5 1 2 3 4 5)")[[1]], env = env2, preserve_src = TRUE)
expr_10arg <- engine2$macroexpand(engine2$read("(f10 1 2 3 4 5 6 7 8 9 10)")[[1]], env = env2, preserve_src = TRUE)

bench_calls <- benchmark_component(
  "1 arg" = compiler2$compile(expr_1arg, env2, strict = TRUE),
  "5 args" = compiler2$compile(expr_5arg, env2, strict = TRUE),
  "10 args" = compiler2$compile(expr_10arg, env2, strict = TRUE),
  iterations = 1000,
  check = FALSE
)
print(bench_calls[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Special forms
cat("Benchmark 3: Special forms\n")
engine3 <- Engine$new()
compiler3 <- engine_field(engine3, "compiler")
env3 <- engine3$get_env()

expr_if <- engine3$macroexpand(engine3$read("(if #t 1 2)")[[1]], env = env3, preserve_src = TRUE)
expr_define <- engine3$macroexpand(engine3$read("(define temp 42)")[[1]], env = env3, preserve_src = TRUE)
expr_lambda <- engine3$macroexpand(engine3$read("(lambda (x) (+ x 1))")[[1]], env = env3, preserve_src = TRUE)
expr_begin <- engine3$macroexpand(engine3$read("(begin 1 2 3)")[[1]], env = env3, preserve_src = TRUE)

bench_special <- benchmark_component(
  "if" = compiler3$compile(expr_if, env3, strict = TRUE),
  "define" = compiler3$compile(expr_define, env3, strict = TRUE),
  "lambda" = compiler3$compile(expr_lambda, env3, strict = TRUE),
  "begin" = compiler3$compile(expr_begin, env3, strict = TRUE),
  iterations = 1000,
  check = FALSE
)
print(bench_special[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Recursive functions
cat("Benchmark 4: Recursive functions\n")
engine4 <- Engine$new()
compiler4 <- engine_field(engine4, "compiler")
env4 <- engine4$get_env()

engine4$eval_text('
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))
')

engine4$eval_text('
(define fact-helper (lambda (n acc)
  (if (= n 0)
    acc
    (fact-helper (- n 1) (* n acc)))))

(define fact (lambda (n)
  (fact-helper n 1)))
')

expr_fib10 <- engine4$macroexpand(engine4$read("(fib 10)")[[1]], env = env4, preserve_src = TRUE)
expr_fib12 <- engine4$macroexpand(engine4$read("(fib 12)")[[1]], env = env4, preserve_src = TRUE)
expr_fact100 <- engine4$macroexpand(engine4$read("(fact 100)")[[1]], env = env4, preserve_src = TRUE)
expr_fact500 <- engine4$macroexpand(engine4$read("(fact 500)")[[1]], env = env4, preserve_src = TRUE)

bench_recursive <- benchmark_component(
  "fibonacci(10)" = compiler4$compile(expr_fib10, env4, strict = TRUE),
  "fibonacci(12)" = compiler4$compile(expr_fib12, env4, strict = TRUE),
  "factorial(100)" = compiler4$compile(expr_fact100, env4, strict = TRUE),
  "factorial(500)" = compiler4$compile(expr_fact500, env4, strict = TRUE),
  iterations = 1000,
  check = FALSE
)
print(bench_recursive[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 5: Real workloads
cat("Benchmark 5: Real workloads\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0) {
  real_rows <- list()
  for (wl_name in names(real_workloads)) {
    timing <- split_pipeline_timing(real_workloads[[wl_name]], n = 10)
    label <- switch(wl_name,
      fibonacci = "fibonacci.arl",
      quicksort = "quicksort.arl",
      graph_paths = "graph-paths.arl",
      macro_examples = "macro-examples.arl",
      wl_name
    )
    real_rows[[length(real_rows) + 1L]] <- data.frame(
      benchmark = "real",
      expression = label,
      median_ms = timing$compile_ms,
      mem_alloc_bytes = NA_real_,
      n_itr = 10L,
      stringsAsFactors = FALSE
    )
  }
  bench_real_df <- do.call(rbind, real_rows)
  print(bench_real_df)
} else {
  cat("(Skipped - example files not available)\n")
}
cat("\n")

# Benchmark 6: Closures and environments
cat("Benchmark 6: Closures and environments\n")
engine6 <- Engine$new()
engine6$eval_text('(import binding)')
compiler6 <- engine_field(engine6, "compiler")
env6 <- engine6$get_env()

engine6$eval_text('
(define make-counter (lambda ()
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count))))
')

expr_closure <- engine6$macroexpand(engine6$read("(make-counter)")[[1]], env = env6, preserve_src = TRUE)

bench_closures <- benchmark_component(
  "Create closure" = compiler6$compile(expr_closure, env6, strict = TRUE),
  iterations = 1000,
  check = FALSE
)
print(bench_closures[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
compile_results <- list(
  arithmetic = bench_arithmetic,
  calls = bench_calls,
  special = bench_special,
  recursive = bench_recursive,
  closures = bench_closures
)

if (length(real_workloads) > 0) {
  # Save combined: micro-benchmarks flattened + real workload df
  all_df <- rbind(flatten_bench_results(compile_results), bench_real_df)
  save_benchmark_results(all_df, "compile")
} else {
  save_benchmark_results(compile_results, "compile")
}

cat("=== Compiler Benchmarks Complete ===\n")
