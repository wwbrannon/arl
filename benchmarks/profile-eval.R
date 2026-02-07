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

# Profile 1: Fibonacci
cat("Profile 1: Fibonacci\n")
engine1 <- RyeEngine$new()

engine1$eval_text('
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))
')

profile_component({
  for (i in 1:20) {
    engine1$eval(engine1$read("(fib 15)")[[1]])
  }
}, "eval-fibonacci")


# Profile 2: Many function arguments (tests arg list growing)
cat("Profile 2: Many function arguments\n")
engine2 <- RyeEngine$new()

engine2$eval_text('
(define sum-many (lambda (a b c d e f g h i j k l m n o p q r s t)
  (+ a b c d e f g h i j k l m n o p q r s t)))
')

profile_component({
  for (i in 1:1000) {
    engine2$eval(engine2$read("(sum-many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)")[[1]])
  }
}, "eval-many-args")


# Profile 3: Higher-order functions (map over large list)
cat("Profile 3: Higher-order functions (map)\n")
engine3 <- RyeEngine$new()

engine3$eval_text('
(define inc (lambda (x) (+ x 1)))
(define list1000 (range 1 1000))
')

profile_component({
  for (i in 1:20) {
    engine3$eval(engine3$read("(map inc list1000)")[[1]])
  }
}, "eval-map")


# Profile 4: Closure creation and invocation
cat("Profile 4: Closures\n")
engine4 <- RyeEngine$new()

engine4$eval_text('
(define make-adder (lambda (n)
  (lambda (x) (+ x n))))
')

profile_component({
  for (i in 1:1000) {
    # Call closure by evaluating the full expression
    engine4$eval(engine4$read("((make-adder 10) 5)")[[1]])
  }
}, "eval-closures")


# Profile 6: Real quicksort workload
cat("Profile 6: Real quicksort workload\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "quicksort" %in% names(real_workloads)) {
  engine6 <- RyeEngine$new()

  old_quiet <- Sys.getenv("RYE_QUIET", unset = NA)
  on.exit({
    if (is.na(old_quiet)) {
      Sys.unsetenv("RYE_QUIET")
    } else {
      Sys.setenv(RYE_QUIET = old_quiet)
    }
  }, add = TRUE)
  Sys.setenv(RYE_QUIET = "1")

  profile_component({
    for (i in 1:20) {
      engine6$eval_text(real_workloads$quicksort)
    }
  }, "eval-quicksort")

} else {
  cat("(Skipped - quicksort.rye not available)\n\n")
}

# Profile 7: CPS overhead with simple arithmetic
cat("Profile 7: CPS overhead (simple arithmetic)\n")
engine7 <- RyeEngine$new()

profile_component({
  for (i in 1:5000) {
    engine7$eval(engine7$read("(+ (+ 1 2) (+ 3 4))")[[1]])
  }
}, "eval-cps-overhead")


cat("=== Evaluator Profiling Complete ===\n")
cat("Profiling data saved to benchmarks/profiles/*.rds\n")
cat("Load with: prof <- readRDS('benchmarks/profiles/eval-*.rds'); print(prof)\n")
