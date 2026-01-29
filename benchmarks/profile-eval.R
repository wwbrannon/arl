# Evaluator Profiling
# Generate profvis flame graphs for evaluator/CPS performance

library(rye)

engine <- RyeEngine$new()

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
  engine$load_stdlib(env)
  env
}

# Profile 1: Fibonacci
cat("Profile 1: Fibonacci\n")
env1 <- setup_env()

eval_text('
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))
', engine, env1)

profile_component({
  for (i in 1:20) {
    engine$eval(engine$read("(fib 15)")[[1]], env1)
  }
}, "eval-fibonacci")


# Profile 2: Many function arguments (tests arg list growing)
cat("Profile 2: Many function arguments\n")
env2 <- setup_env()

eval_text('
(define sum-many (lambda (a b c d e f g h i j k l m n o p q r s t)
  (+ a b c d e f g h i j k l m n o p q r s t)))
', engine, env2)

profile_component({
  for (i in 1:1000) {
    engine$eval(engine$read("(sum-many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)")[[1]], env2)
  }
}, "eval-many-args")


# Profile 3: Higher-order functions (map over large list)
cat("Profile 3: Higher-order functions (map)\n")
env3 <- setup_env()

eval_text('
(define inc (lambda (x) (+ x 1)))
(define list1000 (range 1 1000))
', engine, env3)

profile_component({
  for (i in 1:20) {
    engine$eval(engine$read("(map inc list1000)")[[1]], env3)
  }
}, "eval-map")


# Profile 4: Closure creation and invocation
cat("Profile 4: Closures\n")
env4 <- setup_env()

eval_text('
(define make-adder (lambda (n)
  (lambda (x) (+ x n))))
', engine, env4)

profile_component({
  for (i in 1:1000) {
    # Call closure by evaluating the full expression
    engine$eval(engine$read("((make-adder 10) 5)")[[1]], env4)
  }
}, "eval-closures")


# Profile 6: Real quicksort workload
cat("Profile 6: Real quicksort workload\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "quicksort" %in% names(real_workloads)) {
  env6 <- setup_env()

  profile_component({
    for (i in 1:20) {
      eval_text(real_workloads$quicksort, engine, env6)
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
    engine$eval(engine$read("(+ (+ 1 2) (+ 3 4))")[[1]], env7)
  }
}, "eval-cps-overhead")


cat("=== Evaluator Profiling Complete ===\n")
cat("View HTML reports in benchmarks/profiles/\n")
