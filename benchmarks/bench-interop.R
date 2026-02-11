# R Interop Benchmarks
# Benchmarks for calling R functions and building R objects from Rye

library(rye)

source("benchmarks/benchmark-helpers.R")

cat("=== R Interop Benchmarks ===\n\n")

engine <- Engine$new()

# Benchmark 1: Calling R functions (positional vs keywords)
cat("Benchmark 1: R function calls\n")

bench_calls <- benchmark_component(
  "mean positional" = engine$eval_text("(mean (c 1 2 3 4 5 6 7 8 9 10))"),
  "mean named" = engine$eval_text("(mean :x (c 1 2 3 4 5 6 7 8 9 10))"),
  "seq positional" = engine$eval_text("(seq 1 100 2)"),
  "seq named" = engine$eval_text("(seq :from 1 :to 100 :by 2)"),
  iterations = 1000,
  check = FALSE
)
print(bench_calls[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: R object construction
cat("Benchmark 2: R object construction\n")

bench_objects <- benchmark_component(
  "vector" = engine$eval_text("(c 1 2 3 4 5 6 7 8 9 10)"),
  "list" = engine$eval_text("(list 1 2 3 4 5 6 7 8 9 10)"),
  "data.frame" = engine$eval_text("(data.frame :x (c 1 2 3) :y (c 4 5 6))"),
  "formula" = engine$eval_text("(~ y x)"),
  iterations = 1000,
  check = FALSE
)
print(bench_objects[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
interop_results <- list(
  calls = bench_calls,
  objects = bench_objects
)

# Save results
save_benchmark_results(interop_results, "interop")

cat("=== R Interop Benchmarks Complete ===\n")
