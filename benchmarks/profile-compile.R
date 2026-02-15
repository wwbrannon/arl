# Compiler Profiling
# Generate profvis flame graphs for compiler (Compiler$compile) performance

library(arl)

# Source helpers (works from different working directories)
if (file.exists("benchmarks/benchmark-helpers.R")) {
  source("benchmarks/benchmark-helpers.R")
} else if (file.exists("benchmark-helpers.R")) {
  source("benchmark-helpers.R")
} else {
  helpers_path <- system.file("benchmarks/benchmark-helpers.R", package = "arl")
  if (helpers_path != "") source(helpers_path)
}

if (file.exists("benchmarks/workloads.R")) {
  source("benchmarks/workloads.R")
} else if (file.exists("workloads.R")) {
  source("workloads.R")
} else {
  workloads_path <- system.file("benchmarks/workloads.R", package = "arl")
  if (workloads_path != "") source(workloads_path)
}

cat("=== Profiling Compiler ===\n\n")

# Profile 1: Fibonacci compilation
cat("Profile 1: Fibonacci compilation\n")
engine1 <- Engine$new()
compiler1 <- engine_field(engine1, "compiler")
env1 <- engine1$get_env()

engine1$eval_text('
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))
')

fib_expanded <- engine1$macroexpand(engine1$read("(fib 15)")[[1]], env = env1, preserve_src = TRUE)

profile_component({
  for (i in 1:500) {
    compiler1$compile(fib_expanded, env1, strict = TRUE)
  }
}, "compile-fibonacci")


# Profile 2: Many function arguments
cat("Profile 2: Many function arguments compilation\n")
engine2 <- Engine$new()
compiler2 <- engine_field(engine2, "compiler")
env2 <- engine2$get_env()

engine2$eval_text('
(define sum-many (lambda (a b c d e f g h i j k l m n o p q r s t)
  (+ a b c d e f g h i j k l m n o p q r s t)))
')

many_args_expanded <- engine2$macroexpand(
  engine2$read("(sum-many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)")[[1]],
  env = env2, preserve_src = TRUE
)

profile_component({
  for (i in 1:2000) {
    compiler2$compile(many_args_expanded, env2, strict = TRUE)
  }
}, "compile-many-args")


# Profile 3: Lambda/closure compilation
cat("Profile 3: Lambda/closure compilation\n")
engine3 <- Engine$new()
compiler3 <- engine_field(engine3, "compiler")
env3 <- engine3$get_env()

closure_expanded <- engine3$macroexpand(
  engine3$read("(lambda (x) (lambda (y) (+ x y)))")[[1]],
  env = env3, preserve_src = TRUE
)

profile_component({
  for (i in 1:2000) {
    compiler3$compile(closure_expanded, env3, strict = TRUE)
  }
}, "compile-closures")


# Profile 4: Real quicksort workload compilation
cat("Profile 4: Real quicksort workload compilation\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "quicksort" %in% names(real_workloads)) {
  engine4 <- Engine$new()
  compiler4 <- engine_field(engine4, "compiler")
  env4 <- engine4$get_env()

  qs_exprs <- engine4$read(real_workloads$quicksort)
  qs_expanded <- lapply(qs_exprs, function(e) {
    engine4$macroexpand(e, env = env4, preserve_src = TRUE)
  })

  profile_component({
    for (i in 1:50) {
      for (expanded in qs_expanded) {
        compiler4$compile(expanded, env4, strict = TRUE)
      }
    }
  }, "compile-quicksort")

} else {
  cat("(Skipped - quicksort.arl not available)\n\n")
}

# Profile 5: Simple arithmetic compilation overhead
cat("Profile 5: Simple arithmetic compilation overhead\n")
engine5 <- Engine$new()
compiler5 <- engine_field(engine5, "compiler")
env5 <- engine5$get_env()

arith_expanded <- engine5$macroexpand(
  engine5$read("(+ (+ 1 2) (+ 3 4))")[[1]],
  env = env5, preserve_src = TRUE
)

profile_component({
  for (i in 1:5000) {
    compiler5$compile(arith_expanded, env5, strict = TRUE)
  }
}, "compile-arithmetic")


cat("=== Compiler Profiling Complete ===\n")
cat("Profiling data saved to benchmarks/profiles/*.rds\n")
cat("Load with: prof <- readRDS('benchmarks/profiles/compile-*.rds'); print(prof)\n")
