# Evaluator Benchmarks
# Benchmarks for the evaluator component

library(arl)

source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

cat("=== Evaluator Benchmarks ===\n\n")

# Benchmark 1: Simple arithmetic
cat("Benchmark 1: Simple arithmetic\n")
engine1 <- Engine$new()

bench_arithmetic <- benchmark_component(
  "Single add" = engine1$eval(engine1$read("(+ 1 2)")[[1]]),
  "Nested adds" = engine1$eval(engine1$read("(+ (+ 1 2) (+ 3 4))")[[1]]),
  "Many adds" = engine1$eval(engine1$read("(+ (+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9) 10)")[[1]]),
  iterations = 1000,
  check = FALSE
)
print(bench_arithmetic[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Function calls with varying argument counts
cat("Benchmark 2: Function call overhead\n")
engine2 <- Engine$new()

engine2$eval_text('(define f1 (lambda (x) x))')
engine2$eval_text('(define f5 (lambda (a b c d e) (+ (+ (+ (+ a b) c) d) e)))')
engine2$eval_text('(define f10 (lambda (a b c d e f g h i j) (+ (+ (+ (+ (+ (+ (+ (+ (+ a b) c) d) e) f) g) h) i) j)))')

bench_calls <- benchmark_component(
  "1 arg" = engine2$eval(engine2$read("(f1 42)")[[1]]),
  "5 args" = engine2$eval(engine2$read("(f5 1 2 3 4 5)")[[1]]),
  "10 args" = engine2$eval(engine2$read("(f10 1 2 3 4 5 6 7 8 9 10)")[[1]]),
  iterations = 1000,
  check = FALSE
)
print(bench_calls[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Special forms
cat("Benchmark 3: Special forms\n")
engine3 <- Engine$new()

bench_special <- benchmark_component(
  "if" = engine3$eval(engine3$read("(if #t 1 2)")[[1]]),
  "define" = engine3$eval(engine3$read("(define temp 42)")[[1]]),
  "lambda" = engine3$eval(engine3$read("(lambda (x) (+ x 1))")[[1]]),
  "begin" = engine3$eval(engine3$read("(begin 1 2 3)")[[1]]),
  iterations = 1000,
  check = FALSE
)
print(bench_special[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Recursive functions
cat("Benchmark 4: Recursive functions\n")
engine4 <- Engine$new()

# Fibonacci
engine4$eval_text('
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))
')

# Factorial
engine4$eval_text('
(define fact-helper (lambda (n acc)
  (if (= n 0)
    acc
    (fact-helper (- n 1) (* n acc)))))

(define fact (lambda (n)
  (fact-helper n 1)))
')

bench_recursive <- benchmark_component(
  "fibonacci(10)" = engine4$eval(engine4$read("(fib 10)")[[1]]),
  "fibonacci(12)" = engine4$eval(engine4$read("(fib 12)")[[1]]),
  "factorial(100)" = engine4$eval(engine4$read("(fact 100)")[[1]]),
  "factorial(500)" = engine4$eval(engine4$read("(fact 500)")[[1]]),
  iterations = 50,
  check = FALSE
)
print(bench_recursive[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 5: Real workloads
cat("Benchmark 5: Real workloads\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0) {
  engine5 <- Engine$new()

  bench_real <- benchmark_component(
    "fibonacci.arl" = engine5$eval_text(real_workloads$fibonacci),
    "quicksort.arl" = engine5$eval_text(real_workloads$quicksort),
    iterations = 10,
    check = FALSE
  )
  print(bench_real[, c("expression", "median", "mem_alloc")])
} else {
  cat("(Skipped - example files not available)\n")
}
cat("\n")

# Benchmark 6: Closures and environments
cat("Benchmark 6: Closures and environments\n")
engine6 <- Engine$new()
engine6$eval_text('(import binding)')

engine6$eval_text('
(define make-counter (lambda ()
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count))))
')

bench_closures <- benchmark_component(
  "Create closure" = engine6$eval(engine6$read("(make-counter)")[[1]]),
  "Call closure" = {
    counter <- engine6$eval(engine6$read("(make-counter)")[[1]])
    counter()
  },
  iterations = 1000,
  check = FALSE
)
print(bench_closures[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
eval_results <- list(
  arithmetic = bench_arithmetic,
  calls = bench_calls,
  special = bench_special,
  recursive = bench_recursive,
  closures = bench_closures
)

if (length(real_workloads) > 0) {
  eval_results$real <- bench_real
}

# Save results
save_benchmark_results(eval_results, "eval")

cat("=== Evaluator Benchmarks Complete ===\n")
