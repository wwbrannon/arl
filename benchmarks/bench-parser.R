# Parser Benchmarks
# Benchmarks for the S-expression parsing component

library(rye)
source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

cat("=== Parser Benchmarks ===\n\n")

# Helper: pre-tokenize to isolate parser performance
parse_from_string <- function(code) {
  tokens <- rye:::rye_tokenize(code)
  rye:::rye_parse(tokens)
}

# Benchmark 1: Flat lists of varying sizes
cat("Benchmark 1: Flat lists\n")
flat_10 <- paste0("(list ", paste(seq_len(10), collapse = " "), ")")
flat_100 <- paste0("(list ", paste(seq_len(100), collapse = " "), ")")
flat_1000 <- paste0("(list ", paste(seq_len(1000), collapse = " "), ")")

# Pre-tokenize
tokens_flat_10 <- rye:::rye_tokenize(flat_10)
tokens_flat_100 <- rye:::rye_tokenize(flat_100)
tokens_flat_1000 <- rye:::rye_tokenize(flat_1000)

bench_flat <- benchmark_component(
  "10 elements" = rye:::rye_parse(tokens_flat_10),
  "100 elements" = rye:::rye_parse(tokens_flat_100),
  "1000 elements" = rye:::rye_parse(tokens_flat_1000),
  check = FALSE
)
print(bench_flat[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Nested lists (depth)
cat("Benchmark 2: Nested lists (depth)\n")
nested_depth_5 <- "(list (list (list (list (list 1)))))"
nested_depth_10 <- paste(rep("(list ", 10), collapse = "")
nested_depth_10 <- paste0(nested_depth_10, "1", paste(rep(")", 10), collapse = ""))
nested_depth_20 <- paste(rep("(list ", 20), collapse = "")
nested_depth_20 <- paste0(nested_depth_20, "1", paste(rep(")", 20), collapse = ""))

tokens_nest_5 <- rye:::rye_tokenize(nested_depth_5)
tokens_nest_10 <- rye:::rye_tokenize(nested_depth_10)
tokens_nest_20 <- rye:::rye_tokenize(nested_depth_20)

bench_nested <- benchmark_component(
  "Depth 5" = rye:::rye_parse(tokens_nest_5),
  "Depth 10" = rye:::rye_parse(tokens_nest_10),
  "Depth 20" = rye:::rye_parse(tokens_nest_20),
  check = FALSE
)
print(bench_nested[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Mixed structures with quote sugar
cat("Benchmark 3: Quote sugar and keywords\n")
code_quote <- "(list 'x `(1 ,a ,@b) :key value)"
code_complex <- paste(rep(code_quote, 10), collapse = " ")
code_complex <- paste0("(begin ", code_complex, ")")

tokens_quote <- rye:::rye_tokenize(code_quote)
tokens_complex <- rye:::rye_tokenize(code_complex)

bench_sugar <- benchmark_component(
  "Single quote expr" = rye:::rye_parse(tokens_quote),
  "10 quote exprs" = rye:::rye_parse(tokens_complex),
  check = FALSE
)
print(bench_sugar[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Real example files (pre-tokenized)
cat("Benchmark 4: Real example files (parsing only)\n")
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0) {
  tokens_fib <- rye:::rye_tokenize(real_workloads$fibonacci)
  tokens_qs <- rye:::rye_tokenize(real_workloads$quicksort)
  tokens_macro <- rye:::rye_tokenize(real_workloads$macro_examples)

  bench_real <- benchmark_component(
    "fibonacci.rye" = rye:::rye_parse(tokens_fib),
    "quicksort.rye" = rye:::rye_parse(tokens_qs),
    "macro-examples.rye" = rye:::rye_parse(tokens_macro),
    check = FALSE
  )
  print(bench_real[, c("expression", "median", "mem_alloc")])
} else {
  cat("(Skipped - example files not available)\n")
}
cat("\n")

# Benchmark 5: Lists with NULL values
cat("Benchmark 5: Lists with NULL values (#nil)\n")
code_nulls <- "(list 1 #nil 2 #nil 3 #nil)"
code_many_nulls <- paste0("(list ", paste(rep("#nil", 100), collapse = " "), ")")

tokens_nulls <- rye:::rye_tokenize(code_nulls)
tokens_many_nulls <- rye:::rye_tokenize(code_many_nulls)

bench_nulls <- benchmark_component(
  "Few NULLs" = rye:::rye_parse(tokens_nulls),
  "100 NULLs" = rye:::rye_parse(tokens_many_nulls),
  check = FALSE
)
print(bench_nulls[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
parser_results <- list(
  flat = bench_flat,
  nested = bench_nested,
  sugar = bench_sugar,
  nulls = bench_nulls
)

if (length(real_workloads) > 0) {
  parser_results$real <- bench_real
}

# Save results
save_benchmark_results(parser_results, "parser")

cat("=== Parser Benchmarks Complete ===\n")
