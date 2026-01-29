# Evaluator Benchmarks
# Benchmarks for the CPS evaluator component

library(rye)
source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

engine <- RyeEngine$new()

cat("=== Evaluator Benchmarks ===\n\n")

# Set up clean environment
setup_env <- function() {
  env <- new.env(parent = baseenv())
  engine$load_stdlib(env)
  env
}

# Helper to load binding module (for let, let*, letrec)
load_binding <- function(env) {
  engine$eval_text('(import binding)', env)
  invisible(env)
}

# Benchmark 1: Simple arithmetic (CPS overhead)
cat("Benchmark 1: Simple arithmetic (CPS overhead)\n")
env1 <- setup_env()

bench_arithmetic <- benchmark_component(
  "Single add" = engine$eval(engine$read("(+ 1 2)")[[1]], env1),
  "Nested adds" = engine$eval(engine$read("(+ (+ 1 2) (+ 3 4))")[[1]], env1),
  "Many adds" = engine$eval(engine$read("(+ (+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9) 10)")[[1]], env1),
  iterations = 1000,
  check = FALSE
)
print(bench_arithmetic[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Function calls with varying argument counts
cat("Benchmark 2: Function call overhead\n")
env2 <- setup_env()

engine$eval_text('(define f1 (lambda (x) x))', env2)
engine$eval_text('(define f5 (lambda (a b c d e) (+ (+ (+ (+ a b) c) d) e)))', env2)
engine$eval_text('(define f10 (lambda (a b c d e f g h i j) (+ (+ (+ (+ (+ (+ (+ (+ (+ a b) c) d) e) f) g) h) i) j)))', env2)

bench_calls <- benchmark_component(
  "1 arg" = engine$eval(engine$read("(f1 42)")[[1]], env2),
  "5 args" = engine$eval(engine$read("(f5 1 2 3 4 5)")[[1]], env2),
  "10 args" = engine$eval(engine$read("(f10 1 2 3 4 5 6 7 8 9 10)")[[1]], env2),
  iterations = 1000,
  check = FALSE
)
print(bench_calls[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Special forms
cat("Benchmark 3: Special forms\n")
env3 <- setup_env()

bench_special <- benchmark_component(
  "if" = engine$eval(engine$read("(if #t 1 2)")[[1]], env3),
  "define" = engine$eval(engine$read("(define temp 42)")[[1]], env3),
  "lambda" = engine$eval(engine$read("(lambda (x) (+ x 1))")[[1]], env3),
  "begin" = engine$eval(engine$read("(begin 1 2 3)")[[1]], env3),
  iterations = 1000,
  check = FALSE
)
print(bench_special[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Recursive functions
cat("Benchmark 4: Recursive functions\n")
env4 <- setup_env()

# Fibonacci
engine$eval_text('
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))
', env4)

# Factorial
engine$eval_text('
(define fact-helper (lambda (n acc)
  (if (= n 0)
    acc
    (fact-helper (- n 1) (* n acc)))))

(define fact (lambda (n)
  (fact-helper n 1)))
', env4)

bench_recursive <- benchmark_component(
  "fibonacci(10)" = engine$eval(engine$read("(fib 10)")[[1]], env4),
  "fibonacci(12)" = engine$eval(engine$read("(fib 12)")[[1]], env4),
  "factorial(100)" = engine$eval(engine$read("(fact 100)")[[1]], env4),
  "factorial(500)" = engine$eval(engine$read("(fact 500)")[[1]], env4),
  iterations = 50,
  check = FALSE
)
print(bench_recursive[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 5: Real workloads
cat("Benchmark 5: Real workloads\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0) {
  env6 <- setup_env()

  bench_real <- benchmark_component(
    "fibonacci.rye" = engine$eval_text(real_workloads$fibonacci, env6),
    "quicksort.rye" = engine$eval_text(real_workloads$quicksort, env6),
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
env7 <- setup_env()
load_binding(env7)

engine$eval_text('
(define make-counter (lambda ()
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count))))
', env7)

bench_closures <- benchmark_component(
  "Create closure" = engine$eval(engine$read("(make-counter)")[[1]], env7),
  "Call closure" = {
    counter <- engine$eval(engine$read("(make-counter)")[[1]], env7)
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
  tail = bench_tail,
  closures = bench_closures
)

if (length(real_workloads) > 0) {
  eval_results$real <- bench_real
}

# Save results
save_benchmark_results(eval_results, "eval")

cat("=== Evaluator Benchmarks Complete ===\n")
