# End-to-End Benchmarks
# Full pipeline benchmarks (tokenize → parse → eval) with component breakdown

library(rye)
source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

engine <- RyeEngine$new()

cat("=== End-to-End Benchmarks ===\n\n")

# Helper to time components separately
time_components <- function(code, env) {
  # Tokenize
  t_start <- proc.time()
  tokens <- engine$tokenize(code)
  t_tokenize <- (proc.time() - t_start)[["elapsed"]]

  # Parse
  t_start <- proc.time()
  exprs <- engine$parse(tokens)
  t_parse <- (proc.time() - t_start)[["elapsed"]]

  # Eval
  t_start <- proc.time()
  for (expr in exprs) {
    engine$eval(expr, env)
  }
  t_eval <- (proc.time() - t_start)[["elapsed"]]

  list(
    tokenize = t_tokenize,
    parse = t_parse,
    eval = t_eval,
    total = t_tokenize + t_parse + t_eval
  )
}

# Set up environment
setup_env <- function() {
  env <- new.env(parent = baseenv())
  engine$load_stdlib(env)
  env
}

# Helper to load modules
load_modules <- function(env, modules) {
  for (mod in modules) {
    engine$eval_text(paste0('(import ', mod, ')'), env)
  }
  invisible(env)
}

# Benchmark 1: Synthetic workloads
cat("Benchmark 1: Synthetic workloads (full pipeline)\n")

workloads <- get_all_workloads()

bench_synthetic <- benchmark_component(
  "Micro" = {
    env <- setup_env()
    engine$eval_text(workloads$micro, env)
  },
  "Small" = {
    env <- setup_env()
    engine$eval_text(workloads$small, env)
  },
  "Medium" = {
    env <- setup_env()
    load_modules(env, "binding")  # Needed for let
    engine$eval_text(workloads$medium, env)
  },
  "Deep recursion" = {
    env <- setup_env()
    engine$eval_text(workloads$deep_recursion, env)
  },
  iterations = 50,
  check = FALSE
)
print(bench_synthetic[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Real example files
cat("Benchmark 2: Real example files (full pipeline)\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0) {
  bench_real <- benchmark_component(
    "fibonacci.rye" = {
      env <- setup_env()
      engine$eval_text(real_workloads$fibonacci, env)
    },
    "quicksort.rye" = {
      env <- setup_env()
      engine$eval_text(real_workloads$quicksort, env)
    },
    "macro-examples.rye" = {
      env <- setup_env()
      load_modules(env, c("control", "binding"))
      engine$eval_text(real_workloads$macro_examples, env)
    },
    iterations = 10,
    check = FALSE
  )
  print(bench_real[, c("expression", "median", "mem_alloc")])
  cat("\n")

  # Component breakdown for real workloads
  cat("Component breakdown for real workloads:\n\n")

  for (name in names(real_workloads)) {
    cat(sprintf("=== %s ===\n", name))

    # Run multiple times and average
    timings <- replicate(10, {
      env <- setup_env()
      if (name == "macro_examples") {
        load_modules(env, c("control", "binding"))
      }
      time_components(real_workloads[[name]], env)
    }, simplify = FALSE)

    # Average the timings
    avg_tokenize <- mean(sapply(timings, function(x) x$tokenize))
    avg_parse <- mean(sapply(timings, function(x) x$parse))
    avg_eval <- mean(sapply(timings, function(x) x$eval))
    total <- avg_tokenize + avg_parse + avg_eval

    # Calculate percentages
    pct_tokenize <- (avg_tokenize / total) * 100
    pct_parse <- (avg_parse / total) * 100
    pct_eval <- (avg_eval / total) * 100

    cat(sprintf("  Tokenize: %.2f ms (%2.0f%%)\n", avg_tokenize * 1000, pct_tokenize))
    cat(sprintf("  Parse:    %.2f ms (%2.0f%%)\n", avg_parse * 1000, pct_parse))
    cat(sprintf("  Eval:     %.2f ms (%2.0f%%)\n", avg_eval * 1000, pct_eval))
    cat(sprintf("  Total:    %.2f ms\n\n", total * 1000))
  }
} else {
  cat("(Skipped - example files not available)\n\n")
}

# Benchmark 3: String-heavy workloads
cat("Benchmark 3: String-heavy workloads\n")

bench_strings <- benchmark_component(
  "1K string" = {
    env <- setup_env()
    engine$eval_text(workloads$string_1k, env)
  },
  "10K string" = {
    env <- setup_env()
    engine$eval_text(workloads$string_10k, env)
  },
  iterations = 50,
  check = FALSE
)
print(bench_strings[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Many arguments
cat("Benchmark 4: Many arguments\n")

bench_args <- benchmark_component(
  "50 args" = {
    env <- setup_env()
    engine$eval_text(workloads$many_args_50, env)
  },
  "100 args" = {
    env <- setup_env()
    engine$eval_text(workloads$many_args_100, env)
  },
  iterations = 100,
  check = FALSE
)
print(bench_args[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 5: REPL-style interaction (multiple small expressions)
cat("Benchmark 5: REPL-style interaction\n")

repl_code <- '
(define x 10)
(define y 20)
(+ x y)
(define square (lambda (n) (* n n)))
(square 5)
(map square (list 1 2 3 4 5))
'

bench_repl <- benchmark_component(
  "REPL session" = {
    env <- setup_env()
    engine$eval_text(repl_code, env)
  },
  iterations = 100,
  check = FALSE
)
print(bench_repl[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
e2e_results <- list(
  synthetic = bench_synthetic,
  strings = bench_strings,
  args = bench_args,
  repl = bench_repl
)

if (length(real_workloads) > 0) {
  e2e_results$real <- bench_real
}

# Save results
save_benchmark_results(e2e_results, "e2e")

cat("=== End-to-End Benchmarks Complete ===\n")
