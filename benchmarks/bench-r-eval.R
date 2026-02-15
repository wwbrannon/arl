# R Eval Benchmarks
# Benchmarks for the R eval() phase (CompiledRuntime$eval_compiled only)

library(arl)

source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

cat("=== R Eval Benchmarks ===\n\n")

# Benchmark 1: Simple arithmetic
cat("Benchmark 1: Simple arithmetic\n")
engine1 <- Engine$new()
compiler1 <- engine_field(engine1, "compiler")
rt1 <- engine_field(engine1, "compiled_runtime")
env1 <- engine1$get_env()

# Pre-macroexpand and pre-compile
compiled_single <- compiler1$compile(engine1$macroexpand(engine1$read("(+ 1 2)")[[1]], env = env1, preserve_src = TRUE), env1, strict = TRUE)
compiled_nested <- compiler1$compile(engine1$macroexpand(engine1$read("(+ (+ 1 2) (+ 3 4))")[[1]], env = env1, preserve_src = TRUE), env1, strict = TRUE)
compiled_many <- compiler1$compile(engine1$macroexpand(engine1$read("(+ (+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9) 10)")[[1]], env = env1, preserve_src = TRUE), env1, strict = TRUE)

bench_arithmetic <- benchmark_component(
  "Single add" = rt1$eval_compiled(compiled_single, env1),
  "Nested adds" = rt1$eval_compiled(compiled_nested, env1),
  "Many adds" = rt1$eval_compiled(compiled_many, env1),
  iterations = 1000,
  check = FALSE
)
print(bench_arithmetic[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Function calls with varying argument counts
cat("Benchmark 2: Function call overhead\n")
engine2 <- Engine$new()
compiler2 <- engine_field(engine2, "compiler")
rt2 <- engine_field(engine2, "compiled_runtime")
env2 <- engine2$get_env()

engine2$eval_text('(define f1 (lambda (x) x))')
engine2$eval_text('(define f5 (lambda (a b c d e) (+ (+ (+ (+ a b) c) d) e)))')
engine2$eval_text('(define f10 (lambda (a b c d e f g h i j) (+ (+ (+ (+ (+ (+ (+ (+ (+ a b) c) d) e) f) g) h) i) j)))')

compiled_1arg <- compiler2$compile(engine2$macroexpand(engine2$read("(f1 42)")[[1]], env = env2, preserve_src = TRUE), env2, strict = TRUE)
compiled_5arg <- compiler2$compile(engine2$macroexpand(engine2$read("(f5 1 2 3 4 5)")[[1]], env = env2, preserve_src = TRUE), env2, strict = TRUE)
compiled_10arg <- compiler2$compile(engine2$macroexpand(engine2$read("(f10 1 2 3 4 5 6 7 8 9 10)")[[1]], env = env2, preserve_src = TRUE), env2, strict = TRUE)

bench_calls <- benchmark_component(
  "1 arg" = rt2$eval_compiled(compiled_1arg, env2),
  "5 args" = rt2$eval_compiled(compiled_5arg, env2),
  "10 args" = rt2$eval_compiled(compiled_10arg, env2),
  iterations = 1000,
  check = FALSE
)
print(bench_calls[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Special forms
cat("Benchmark 3: Special forms\n")
engine3 <- Engine$new()
compiler3 <- engine_field(engine3, "compiler")
rt3 <- engine_field(engine3, "compiled_runtime")
env3 <- engine3$get_env()

compiled_if <- compiler3$compile(engine3$macroexpand(engine3$read("(if #t 1 2)")[[1]], env = env3, preserve_src = TRUE), env3, strict = TRUE)
compiled_define <- compiler3$compile(engine3$macroexpand(engine3$read("(define temp 42)")[[1]], env = env3, preserve_src = TRUE), env3, strict = TRUE)
compiled_lambda <- compiler3$compile(engine3$macroexpand(engine3$read("(lambda (x) (+ x 1))")[[1]], env = env3, preserve_src = TRUE), env3, strict = TRUE)
compiled_begin <- compiler3$compile(engine3$macroexpand(engine3$read("(begin 1 2 3)")[[1]], env = env3, preserve_src = TRUE), env3, strict = TRUE)

bench_special <- benchmark_component(
  "if" = rt3$eval_compiled(compiled_if, env3),
  "define" = rt3$eval_compiled(compiled_define, env3),
  "lambda" = rt3$eval_compiled(compiled_lambda, env3),
  "begin" = rt3$eval_compiled(compiled_begin, env3),
  iterations = 1000,
  check = FALSE
)
print(bench_special[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Recursive functions
cat("Benchmark 4: Recursive functions\n")
engine4 <- Engine$new()
compiler4 <- engine_field(engine4, "compiler")
rt4 <- engine_field(engine4, "compiled_runtime")
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

compiled_fib10 <- compiler4$compile(engine4$macroexpand(engine4$read("(fib 10)")[[1]], env = env4, preserve_src = TRUE), env4, strict = TRUE)
compiled_fib12 <- compiler4$compile(engine4$macroexpand(engine4$read("(fib 12)")[[1]], env = env4, preserve_src = TRUE), env4, strict = TRUE)
compiled_fact100 <- compiler4$compile(engine4$macroexpand(engine4$read("(fact 100)")[[1]], env = env4, preserve_src = TRUE), env4, strict = TRUE)
compiled_fact500 <- compiler4$compile(engine4$macroexpand(engine4$read("(fact 500)")[[1]], env = env4, preserve_src = TRUE), env4, strict = TRUE)

bench_recursive <- benchmark_component(
  "fibonacci(10)" = rt4$eval_compiled(compiled_fib10, env4),
  "fibonacci(12)" = rt4$eval_compiled(compiled_fib12, env4),
  "factorial(100)" = rt4$eval_compiled(compiled_fact100, env4),
  "factorial(500)" = rt4$eval_compiled(compiled_fact500, env4),
  iterations = 50,
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
      median_ms = timing$eval_ms,
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
rt6 <- engine_field(engine6, "compiled_runtime")
env6 <- engine6$get_env()

engine6$eval_text('
(define make-counter (lambda ()
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count))))
')

compiled_closure <- compiler6$compile(engine6$macroexpand(engine6$read("(make-counter)")[[1]], env = env6, preserve_src = TRUE), env6, strict = TRUE)

bench_closures <- benchmark_component(
  "Create closure" = rt6$eval_compiled(compiled_closure, env6),
  "Call closure" = {
    counter <- rt6$eval_compiled(compiled_closure, env6)
    counter()
  },
  iterations = 1000,
  check = FALSE
)
print(bench_closures[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
reval_results <- list(
  arithmetic = bench_arithmetic,
  calls = bench_calls,
  special = bench_special,
  recursive = bench_recursive,
  closures = bench_closures
)

if (length(real_workloads) > 0) {
  all_df <- rbind(flatten_bench_results(reval_results), bench_real_df)
  save_benchmark_results(all_df, "r-eval")
} else {
  save_benchmark_results(reval_results, "r-eval")
}

cat("=== R Eval Benchmarks Complete ===\n")
