# Macro Profiling
# Generate profvis flame graphs for macro expansion performance

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

cat("=== Profiling Macro Expansion ===\n\n")

# Set up environment with macros
setup_macro_env <- function() {
  env <- new.env(parent = baseenv())
  engine$load_stdlib(env)
  env
}

# Profile 1: Complex quasiquote with unquote-splicing
cat("Profile 1: Complex quasiquote\n")
env1 <- setup_macro_env()

eval_text('
(defmacro complex (x . rest)
  `(list ,x ,@rest (+ ,x 1) ,@rest))
', engine, env1)

complex_expr <- engine$read("(complex 1 2 3 4 5)")[[1]]

profile_component({
  for (i in 1:1000) {
    engine$macroexpand(complex_expr, env1)
  }
}, "macro-complex-quasiquote")


# Profile 2: Nested macro expansion
cat("Profile 2: Nested macro expansion\n")
env2 <- setup_macro_env()

eval_text('
(defmacro outer (x)
  `(middle ,x))

(defmacro middle (x)
  `(inner ,x))

(defmacro inner (x)
  `(+ ,x 1))
', engine, env2)

nested_expr <- engine$read("(outer (outer (outer 42)))")[[1]]

profile_component({
  for (i in 1:1000) {
    engine$macroexpand(nested_expr, env2)
  }
}, "macro-nested")


# Profile 3: Hygiene processing overhead
cat("Profile 3: Hygiene processing\n")
env3 <- setup_macro_env()

eval_text('
(defmacro let-like (bindings . body)
  `((lambda ,(map car bindings) ,@body)
    ,@(map (lambda (b) (car (cdr b))) bindings)))
', engine, env3)

hygiene_expr <- engine$read("(let-like ((x 1) (y 2) (z 3) (a 4) (b 5)) (+ x y z a b))")[[1]]

profile_component({
  for (i in 1:500) {
    engine$macroexpand(hygiene_expr, env3)
  }
}, "macro-hygiene")


# Profile 4: Real macro examples file
cat("Profile 4: Real macro examples file\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "macro_examples" %in% names(real_workloads)) {
  env4 <- setup_macro_env()
  tryCatch({
    # Load stdlib (exported function)
    engine$load_stdlib(env4)
  }, error = function(e) {
    message("Could not load stdlib modules: ", e$message)
  })

  profile_component({
    for (i in 1:20) {
      exprs <- engine$read(real_workloads$macro_examples)
      for (expr in exprs) {
        tryCatch({
          engine$eval(expr, env4)
        }, error = function(e) {
          # Ignore errors in profiling
        })
      }
    }
  }, "macro-examples")

} else {
  cat("(Skipped - macro-examples.rye not available)\n\n")
}

# Profile 5: Multiple macro tree walks
cat("Profile 5: Macro with large body (tests tree walking)\n")
env5 <- setup_macro_env()

eval_text('
(defmacro when (test . body)
  `(if ,test (begin ,@body) #nil))
', env5)

# Large body with many expressions
large_body_exprs <- paste(sprintf("(+ %d %d)", seq_len(50), seq_len(50) + 1), collapse = " ")
large_body <- paste0("(when #t ", large_body_exprs, ")")
large_body_expr <- engine$read(large_body)[[1]]

profile_component({
  for (i in 1:500) {
    engine$macroexpand(large_body_expr, env5)
  }
}, "macro-tree-walk")


cat("=== Macro Profiling Complete ===\n")
cat("View HTML reports in benchmarks/profiles/\n")
