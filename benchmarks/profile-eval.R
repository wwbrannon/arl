# Evaluator Profiling
# Generate profvis flame graphs for evaluator/CPS performance

library(rye)

# Source helpers (works from different working directories)
if (file.exists("benchmarks/benchmark-helpers.R")) {
  source("benchmarks/benchmark-helpers.R")
} else if (file.exists("benchmark-helpers.R")) {
  source("benchmark-helpers.R")
} else {
  helpers_path <- system.file("benchmarks/benchmark-helpers.R", package = "rye")
  if (helpers_path != "") source(helpers_path)
}

if (file.exists("benchmarks/workloads.R")) {
  source("benchmarks/workloads.R")
} else if (file.exists("workloads.R")) {
  source("workloads.R")
} else {
  workloads_path <- system.file("benchmarks/workloads.R", package = "rye")
  if (workloads_path != "") source(workloads_path)
}

cat("=== Profiling Evaluator ===\n\n")

# Set up environment
setup_env <- function() {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)
  env
}

# Profile 1: Fibonacci (non-tail recursive - tests CPS overhead)
cat("Profile 1: Fibonacci (non-tail recursive)\n")
env1 <- setup_env()

eval_text('
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))
', env1)

profile_component({
  for (i in 1:20) {
    rye_eval(rye_read("(fib 15)")[[1]], env1)
  }
}, "eval-fibonacci")


# Profile 2: Tail-recursive factorial (tests tail-call optimization)
cat("Profile 2: Factorial (tail recursive)\n")
env2 <- setup_env()

eval_text('
(define fact-helper (lambda (n acc)
  (if (= n 0)
    acc
    (fact-helper (- n 1) (* n acc)))))

(define fact (lambda (n)
  (fact-helper n 1)))
', env2)

profile_component({
  for (i in 1:50) {
    rye_eval(rye_read("(fact 1000)")[[1]], env2)
  }
}, "eval-factorial")


# Profile 3: Many function arguments (tests arg list growing)
cat("Profile 3: Many function arguments\n")
env3 <- setup_env()

eval_text('
(define sum-many (lambda (a b c d e f g h i j k l m n o p q r s t)
  (+ a b c d e f g h i j k l m n o p q r s t)))
', env3)

profile_component({
  for (i in 1:1000) {
    rye_eval(rye_read("(sum-many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)")[[1]], env3)
  }
}, "eval-many-args")


# Profile 4: Higher-order functions (map over large list)
cat("Profile 4: Higher-order functions (map)\n")
env4 <- setup_env()

eval_text('
(define inc (lambda (x) (+ x 1)))
(define list1000 (range 1 1000))
', env4)

profile_component({
  for (i in 1:20) {
    rye_eval(rye_read("(map inc list1000)")[[1]], env4)
  }
}, "eval-map")


# Profile 5: Closure creation and invocation
cat("Profile 5: Closures\n")
env5 <- setup_env()

eval_text('
(define make-adder (lambda (n)
  (lambda (x) (+ x n))))
', env5)

profile_component({
  for (i in 1:1000) {
    # Call closure by evaluating the full expression
    rye_eval(rye_read("((make-adder 10) 5)")[[1]], env5)
  }
}, "eval-closures")


# Profile 6: Real quicksort workload
cat("Profile 6: Real quicksort workload\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "quicksort" %in% names(real_workloads)) {
  env6 <- setup_env()

  profile_component({
    for (i in 1:20) {
      eval_text(real_workloads$quicksort, env6)
    }
  }, "eval-quicksort")

} else {
  cat("(Skipped - quicksort.rye not available)\n\n")
}

# Profile 7: CPS overhead with simple arithmetic
cat("Profile 7: CPS overhead (simple arithmetic)\n")
env7 <- setup_env()

profile_component({
  for (i in 1:5000) {
    rye_eval(rye_read("(+ (+ 1 2) (+ 3 4))")[[1]], env7)
  }
}, "eval-cps-overhead")


cat("=== Evaluator Profiling Complete ===\n")
cat("View HTML reports in benchmarks/profiles/\n")
