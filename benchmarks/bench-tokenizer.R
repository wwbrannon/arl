# Tokenizer Benchmarks
# Benchmarks for the lexical analysis component

library(rye)

source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

cat("=== Tokenizer Benchmarks ===\n\n")

# Benchmark 1: String literals of varying sizes
cat("Benchmark 1: String literals\n")

engine1 <- RyeEngine$new()
bench_strings <- benchmark_component(
  "10 chars" = engine1$tokenize('"0123456789"'),
  "100 chars" = engine1$tokenize(paste0('"', paste(rep("x", 100), collapse = ""), '"')),
  "1K chars" = engine1$tokenize(paste0('"', paste(rep("x", 1000), collapse = ""), '"')),
  "10K chars" = engine1$tokenize(paste0('"', paste(rep("x", 10000), collapse = ""), '"')),
  check = FALSE
)
print(bench_strings[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Nested parentheses
cat("Benchmark 2: Nested parentheses\n")

engine2 <- RyeEngine$new()
nested_10 <- paste(rep("(", 10), collapse = "")
nested_10 <- paste0(nested_10, "x", paste(rep(")", 10), collapse = ""))

nested_50 <- paste(rep("(", 50), collapse = "")
nested_50 <- paste0(nested_50, "x", paste(rep(")", 50), collapse = ""))

nested_100 <- paste(rep("(", 100), collapse = "")
nested_100 <- paste0(nested_100, "x", paste(rep(")", 100), collapse = ""))

bench_nested <- benchmark_component(
  "10 levels" = engine2$tokenize(nested_10),
  "50 levels" = engine2$tokenize(nested_50),
  "100 levels" = engine2$tokenize(nested_100),
  check = FALSE
)
print(bench_nested[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Mixed content (strings, numbers, symbols)
cat("Benchmark 3: Mixed content\n")

engine3 <- RyeEngine$new()
mixed_small <- '(define x 42) (+ 1 2 3) (str "hello" "world")'
mixed_medium <- paste(rep(mixed_small, 10), collapse = " ")
mixed_large <- paste(rep(mixed_small, 100), collapse = " ")

bench_mixed <- benchmark_component(
  "Small (3 exprs)" = engine3$tokenize(mixed_small),
  "Medium (30 exprs)" = engine3$tokenize(mixed_medium),
  "Large (300 exprs)" = engine3$tokenize(mixed_large),
  check = FALSE
)
print(bench_mixed[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Real example files
cat("Benchmark 4: Real example files\n")

engine4 <- RyeEngine$new()
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0) {
  bench_real <- benchmark_component(
    "fibonacci.rye" = engine4$tokenize(real_workloads$fibonacci),
    "quicksort.rye" = engine4$tokenize(real_workloads$quicksort),
    "macro-examples.rye" = engine4$tokenize(real_workloads$macro_examples),
    check = FALSE
  )
  print(bench_real[, c("expression", "median", "mem_alloc")])
} else {
  cat("(Skipped - example files not available)\n")
}
cat("\n")

# Benchmark 5: Escape sequences
cat("Benchmark 5: Escape sequences in strings\n")

engine5 <- RyeEngine$new()
bench_escapes <- benchmark_component(
  "No escapes" = engine5$tokenize('"simple string"'),
  "Few escapes" = engine5$tokenize('"hello\\nworld\\t!"'),
  "Many escapes" = engine5$tokenize(paste0('"', paste(rep('\\n\\t\\r\\"', 100), collapse = ""), '"')),
  check = FALSE
)
print(bench_escapes[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
tokenizer_results <- list(
  strings = bench_strings,
  nested = bench_nested,
  mixed = bench_mixed,
  escapes = bench_escapes
)

if (length(real_workloads) > 0) {
  tokenizer_results$real <- bench_real
}

# Save results
save_benchmark_results(tokenizer_results, "tokenizer")

cat("=== Tokenizer Benchmarks Complete ===\n")
