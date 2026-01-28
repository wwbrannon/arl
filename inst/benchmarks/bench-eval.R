# Evaluator Benchmarks
# Benchmarks for the CPS evaluator component

library(rye)
source("inst/benchmarks/benchmark-helpers.R")
source("inst/benchmarks/workloads.R")

cat("=== Evaluator Benchmarks ===\n\n")

# Set up clean environment
setup_env <- function() {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)
  env
}

# Helper to load binding module (for let, let*, letrec)
load_binding <- function(env) {
  rye:::rye_eval_text('(import binding)', env)
  invisible(env)
}

# Benchmark 1: Simple arithmetic (CPS overhead)
cat("Benchmark 1: Simple arithmetic (CPS overhead)\n")
env1 <- setup_env()

bench_arithmetic <- benchmark_component(
  "Single add" = rye_eval(rye_read("(+ 1 2)")[[1]], env1),
  "Nested adds" = rye_eval(rye_read("(+ (+ 1 2) (+ 3 4))")[[1]], env1),
  "Many adds" = rye_eval(rye_read("(+ (+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9) 10)")[[1]], env1),
  iterations = 1000,
  check = FALSE
)
print(bench_arithmetic[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Function calls with varying argument counts
cat("Benchmark 2: Function call overhead\n")
env2 <- setup_env()

rye:::rye_eval_text('(define f1 (lambda (x) x))', env2)
rye:::rye_eval_text('(define f5 (lambda (a b c d e) (+ (+ (+ (+ a b) c) d) e)))', env2)
rye:::rye_eval_text('(define f10 (lambda (a b c d e f g h i j) (+ (+ (+ (+ (+ (+ (+ (+ (+ a b) c) d) e) f) g) h) i) j)))', env2)

bench_calls <- benchmark_component(
  "1 arg" = rye_eval(rye_read("(f1 42)")[[1]], env2),
  "5 args" = rye_eval(rye_read("(f5 1 2 3 4 5)")[[1]], env2),
  "10 args" = rye_eval(rye_read("(f10 1 2 3 4 5 6 7 8 9 10)")[[1]], env2),
  iterations = 1000,
  check = FALSE
)
print(bench_calls[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Special forms
cat("Benchmark 3: Special forms\n")
env3 <- setup_env()

bench_special <- benchmark_component(
  "if" = rye_eval(rye_read("(if #t 1 2)")[[1]], env3),
  "define" = rye_eval(rye_read("(define temp 42)")[[1]], env3),
  "lambda" = rye_eval(rye_read("(lambda (x) (+ x 1))")[[1]], env3),
  "begin" = rye_eval(rye_read("(begin 1 2 3)")[[1]], env3),
  iterations = 1000,
  check = FALSE
)
print(bench_special[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Recursive functions
cat("Benchmark 4: Recursive functions\n")
env4 <- setup_env()

# Fibonacci (non-tail recursive)
rye:::rye_eval_text('
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))
', env4)

# Factorial (tail recursive)
rye:::rye_eval_text('
(define fact-helper (lambda (n acc)
  (if (= n 0)
    acc
    (fact-helper (- n 1) (* n acc)))))

(define fact (lambda (n)
  (fact-helper n 1)))
', env4)

bench_recursive <- benchmark_component(
  "fibonacci(10)" = rye_eval(rye_read("(fib 10)")[[1]], env4),
  "fibonacci(12)" = rye_eval(rye_read("(fib 12)")[[1]], env4),
  "factorial(100)" = rye_eval(rye_read("(fact 100)")[[1]], env4),
  "factorial(500)" = rye_eval(rye_read("(fact 500)")[[1]], env4),
  iterations = 50,
  check = FALSE
)
print(bench_recursive[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 5: Tail-call vs non-tail patterns
cat("Benchmark 5: Tail-call optimization\n")
env5 <- setup_env()

rye:::rye_eval_text('
(define non-tail (lambda (n)
  (if (= n 0)
    0
    (+ 1 (non-tail (- n 1))))))

(define tail-helper (lambda (n acc)
  (if (= n 0)
    acc
    (tail-helper (- n 1) (+ acc 1)))))

(define tail (lambda (n)
  (tail-helper n 0)))
', env5)

bench_tail <- benchmark_component(
  "Non-tail (50)" = rye_eval(rye_read("(non-tail 50)")[[1]], env5),
  "Tail (100)" = rye_eval(rye_read("(tail 100)")[[1]], env5),
  "Tail (500)" = rye_eval(rye_read("(tail 500)")[[1]], env5),
  iterations = 50,
  check = FALSE
)
print(bench_tail[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 6: Real workloads
cat("Benchmark 6: Real workloads\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0) {
  env6 <- setup_env()

  bench_real <- benchmark_component(
    "fibonacci.rye" = rye:::rye_eval_text(real_workloads$fibonacci, env6),
    "quicksort.rye" = rye:::rye_eval_text(real_workloads$quicksort, env6),
    iterations = 10,
    check = FALSE
  )
  print(bench_real[, c("expression", "median", "mem_alloc")])
} else {
  cat("(Skipped - example files not available)\n")
}
cat("\n")

# Benchmark 7: Closures and environments
cat("Benchmark 7: Closures and environments\n")
env7 <- setup_env()
load_binding(env7)

rye:::rye_eval_text('
(define make-counter (lambda ()
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count))))
', env7)

bench_closures <- benchmark_component(
  "Create closure" = rye_eval(rye_read("(make-counter)")[[1]], env7),
  "Call closure" = {
    counter <- rye_eval(rye_read("(make-counter)")[[1]], env7)
    rye:::rye_apply_cps(counter, list(), function(x) x)
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
