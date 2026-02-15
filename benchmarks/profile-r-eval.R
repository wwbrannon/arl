# R Eval Profiling
# Generate profvis flame graphs for R eval() (CompiledRuntime$eval_compiled) performance

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

cat("=== Profiling R Eval ===\n\n")

# Profile 1: Fibonacci evaluation
cat("Profile 1: Fibonacci evaluation\n")
engine1 <- Engine$new()
compiler1 <- engine_field(engine1, "compiler")
rt1 <- engine_field(engine1, "compiled_runtime")
env1 <- engine1$get_env()

engine1$eval_text('
(define fib (lambda (n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))
')

fib_compiled <- compiler1$compile(
  engine1$macroexpand(engine1$read("(fib 15)")[[1]], env = env1, preserve_src = TRUE),
  env1, strict = TRUE
)

profile_component({
  for (i in 1:20) {
    rt1$eval_compiled(fib_compiled, env1)
  }
}, "r-eval-fibonacci")


# Profile 2: Many function arguments
cat("Profile 2: Many function arguments evaluation\n")
engine2 <- Engine$new()
compiler2 <- engine_field(engine2, "compiler")
rt2 <- engine_field(engine2, "compiled_runtime")
env2 <- engine2$get_env()

engine2$eval_text('
(define sum-many (lambda (a b c d e f g h i j k l m n o p q r s t)
  (+ a b c d e f g h i j k l m n o p q r s t)))
')

many_args_compiled <- compiler2$compile(
  engine2$macroexpand(
    engine2$read("(sum-many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)")[[1]],
    env = env2, preserve_src = TRUE
  ),
  env2, strict = TRUE
)

profile_component({
  for (i in 1:1000) {
    rt2$eval_compiled(many_args_compiled, env2)
  }
}, "r-eval-many-args")


# Profile 3: Higher-order functions (map over large list)
cat("Profile 3: Higher-order functions (map)\n")
engine3 <- Engine$new()
compiler3 <- engine_field(engine3, "compiler")
rt3 <- engine_field(engine3, "compiled_runtime")
env3 <- engine3$get_env()

engine3$eval_text('
(define inc (lambda (x) (+ x 1)))
(define list1000 (range 1 1000))
')

map_compiled <- compiler3$compile(
  engine3$macroexpand(engine3$read("(map inc list1000)")[[1]], env = env3, preserve_src = TRUE),
  env3, strict = TRUE
)

profile_component({
  for (i in 1:20) {
    rt3$eval_compiled(map_compiled, env3)
  }
}, "r-eval-map")


# Profile 4: Closure creation and invocation
cat("Profile 4: Closures\n")
engine4 <- Engine$new()
compiler4 <- engine_field(engine4, "compiler")
rt4 <- engine_field(engine4, "compiled_runtime")
env4 <- engine4$get_env()

engine4$eval_text('
(define make-adder (lambda (n)
  (lambda (x) (+ x n))))
')

adder_compiled <- compiler4$compile(
  engine4$macroexpand(engine4$read("((make-adder 10) 5)")[[1]], env = env4, preserve_src = TRUE),
  env4, strict = TRUE
)

profile_component({
  for (i in 1:1000) {
    rt4$eval_compiled(adder_compiled, env4)
  }
}, "r-eval-closures")


# Profile 5: Real quicksort workload
cat("Profile 5: Real quicksort workload\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "quicksort" %in% names(real_workloads)) {
  engine5 <- Engine$new()
  compiler5 <- engine_field(engine5, "compiler")
  rt5 <- engine_field(engine5, "compiled_runtime")
  env5 <- engine5$get_env()

  old_quiet <- Sys.getenv("ARL_QUIET", unset = NA)
  on.exit({
    if (is.na(old_quiet)) {
      Sys.unsetenv("ARL_QUIET")
    } else {
      Sys.setenv(ARL_QUIET = old_quiet)
    }
  }, add = TRUE)
  Sys.setenv(ARL_QUIET = "1")

  # Pre-compile all expressions
  qs_exprs <- engine5$read(real_workloads$quicksort)
  qs_compiled <- lapply(qs_exprs, function(e) {
    expanded <- engine5$macroexpand(e, env = env5, preserve_src = TRUE)
    compiler5$compile(expanded, env5, strict = TRUE)
  })

  profile_component({
    for (i in 1:20) {
      for (compiled in qs_compiled) {
        rt5$eval_compiled(compiled, env5)
      }
    }
  }, "r-eval-quicksort")

} else {
  cat("(Skipped - quicksort.arl not available)\n\n")
}

# Profile 6: Simple arithmetic evaluation overhead
cat("Profile 6: Simple arithmetic evaluation overhead\n")
engine6 <- Engine$new()
compiler6 <- engine_field(engine6, "compiler")
rt6 <- engine_field(engine6, "compiled_runtime")
env6 <- engine6$get_env()

arith_compiled <- compiler6$compile(
  engine6$macroexpand(engine6$read("(+ (+ 1 2) (+ 3 4))")[[1]], env = env6, preserve_src = TRUE),
  env6, strict = TRUE
)

profile_component({
  for (i in 1:5000) {
    rt6$eval_compiled(arith_compiled, env6)
  }
}, "r-eval-arithmetic")


cat("=== R Eval Profiling Complete ===\n")
cat("Profiling data saved to benchmarks/profiles/*.rds\n")
cat("Load with: prof <- readRDS('benchmarks/profiles/r-eval-*.rds'); print(prof)\n")
