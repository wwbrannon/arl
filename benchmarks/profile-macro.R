# Macro Profiling
# Generate profvis flame graphs for macro expansion performance

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

cat("=== Profiling Macro Expansion ===\n\n")

# Profile 1: Complex quasiquote with unquote-splicing
cat("Profile 1: Complex quasiquote\n")
engine1 <- Engine$new()

engine1$eval_text('
(defmacro complex (x . rest)
  `(list ,x ,@rest (+ ,x 1) ,@rest))
')

complex_expr <- engine1$read("(complex 1 2 3 4 5)")[[1]]

profile_component({
  for (i in 1:1000) {
    engine1$macroexpand(complex_expr)
  }
}, "macro-complex-quasiquote")


# Profile 2: Nested macro expansion
cat("Profile 2: Nested macro expansion\n")
engine2 <- Engine$new()

engine2$eval_text('
(defmacro outer (x)
  `(middle ,x))

(defmacro middle (x)
  `(inner ,x))

(defmacro inner (x)
  `(+ ,x 1))
')

nested_expr <- engine2$read("(outer (outer (outer 42)))")[[1]]

profile_component({
  for (i in 1:1000) {
    engine2$macroexpand(nested_expr)
  }
}, "macro-nested")


# Profile 3: Hygiene processing overhead
cat("Profile 3: Hygiene processing\n")
engine3 <- Engine$new()

engine3$eval_text('
(defmacro let-like (bindings . body)
  `((lambda ,(map car bindings) ,@body)
    ,@(map (lambda (b) (car (cdr b))) bindings)))
')

hygiene_expr <- engine3$read("(let-like ((x 1) (y 2) (z 3) (a 4) (b 5)) (+ x y z a b))")[[1]]

profile_component({
  for (i in 1:500) {
    engine3$macroexpand(hygiene_expr)
  }
}, "macro-hygiene")


# Profile 4: Real macro examples file
cat("Profile 4: Real macro examples file\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "macro_examples" %in% names(real_workloads)) {
  engine4 <- Engine$new()

  old_quiet <- Sys.getenv("ARL_QUIET", unset = NA)
  on.exit({
    if (is.na(old_quiet)) {
      Sys.unsetenv("ARL_QUIET")
    } else {
      Sys.setenv(ARL_QUIET = old_quiet)
    }
  }, add = TRUE)
  Sys.setenv(ARL_QUIET = "1")

  profile_component({
    for (i in 1:20) {
      exprs <- engine4$read(real_workloads$macro_examples)
      for (expr in exprs) {
        tryCatch({
          engine4$eval(expr)
        }, error = function(e) {
          # Ignore errors in profiling
        })
      }
    }
  }, "macro-examples")

} else {
  cat("(Skipped - macro-examples.arl not available)\n\n")
}

# Profile 5: Multiple macro tree walks
cat("Profile 5: Macro with large body (tests tree walking)\n")
engine5 <- Engine$new()

engine5$eval_text('
(defmacro when (test . body)
  `(if ,test (begin ,@body) #nil))
')

# Large body with many expressions
large_body_exprs <- paste(sprintf("(+ %d %d)", seq_len(50), seq_len(50) + 1), collapse = " ")
large_body <- paste0("(when #t ", large_body_exprs, ")")
large_body_expr <- engine5$read(large_body)[[1]]

profile_component({
  for (i in 1:500) {
    engine5$macroexpand(large_body_expr)
  }
}, "macro-tree-walk")

cat("=== Macro Profiling Complete ===\n")
cat("Profiling data saved to benchmarks/profiles/*.rds\n")
cat("Load with: prof <- readRDS('benchmarks/profiles/macro-*.rds'); print(prof)\n")
