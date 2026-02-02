# Parser Benchmarks
# Benchmarks for the S-expression parsing component

library(rye)
source("benchmarks/benchmark-helpers.R")
source("benchmarks/workloads.R")

cat("=== Parser Benchmarks ===\n\n")

# Benchmark 1: Flat lists of varying sizes
cat("Benchmark 1: Flat lists\n")

engine1 <- RyeEngine$new()
flat_10 <- paste0("(list ", paste(seq_len(10), collapse = " "), ")")
flat_100 <- paste0("(list ", paste(seq_len(100), collapse = " "), ")")
flat_1000 <- paste0("(list ", paste(seq_len(1000), collapse = " "), ")")

# Pre-tokenize
tokens_flat_10 <- engine1$tokenize(flat_10)
tokens_flat_100 <- engine1$tokenize(flat_100)
tokens_flat_1000 <- engine1$tokenize(flat_1000)

bench_flat <- benchmark_component(
  "10 elements" = engine1$parse(tokens_flat_10),
  "100 elements" = engine1$parse(tokens_flat_100),
  "1000 elements" = engine1$parse(tokens_flat_1000),
  check = FALSE
)
print(bench_flat[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: Nested lists (depth)
cat("Benchmark 2: Nested lists (depth)\n")

engine2 <- RyeEngine$new()
nested_depth_5 <- "(list (list (list (list (list 1)))))"
nested_depth_10 <- paste(rep("(list ", 10), collapse = "")
nested_depth_10 <- paste0(nested_depth_10, "1", paste(rep(")", 10), collapse = ""))
nested_depth_20 <- paste(rep("(list ", 20), collapse = "")
nested_depth_20 <- paste0(nested_depth_20, "1", paste(rep(")", 20), collapse = ""))

tokens_nest_5 <- engine2$tokenize(nested_depth_5)
tokens_nest_10 <- engine2$tokenize(nested_depth_10)
tokens_nest_20 <- engine2$tokenize(nested_depth_20)

bench_nested <- benchmark_component(
  "Depth 5" = engine2$parse(tokens_nest_5),
  "Depth 10" = engine2$parse(tokens_nest_10),
  "Depth 20" = engine2$parse(tokens_nest_20),
  check = FALSE
)
print(bench_nested[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 3: Mixed structures with quote sugar
cat("Benchmark 3: Quote sugar and keywords\n")

engine3 <- RyeEngine$new()
code_quote <- "(list 'x `(1 ,a ,@b) :key value)"
code_complex <- paste(rep(code_quote, 10), collapse = " ")
code_complex <- paste0("(begin ", code_complex, ")")

tokens_quote <- engine3$tokenize(code_quote)
tokens_complex <- engine3$tokenize(code_complex)

bench_sugar <- benchmark_component(
  "Single quote expr" = engine3$parse(tokens_quote),
  "10 quote exprs" = engine3$parse(tokens_complex),
  check = FALSE
)
print(bench_sugar[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 4: Real example files (pre-tokenized)
cat("Benchmark 4: Real example files (parsing only)\n")

engine4 <- RyeEngine$new()
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0) {
  tokens_fib <- engine4$tokenize(real_workloads$fibonacci)
  tokens_qs <- engine4$tokenize(real_workloads$quicksort)
  tokens_macro <- engine4$tokenize(real_workloads$macro_examples)

  bench_real <- benchmark_component(
    "fibonacci.rye" = engine4$parse(tokens_fib),
    "quicksort.rye" = engine4$parse(tokens_qs),
    "macro-examples.rye" = engine4$parse(tokens_macro),
    check = FALSE
  )
  print(bench_real[, c("expression", "median", "mem_alloc")])
} else {
  cat("(Skipped - example files not available)\n")
}
cat("\n")

# Benchmark 5: Lists with NULL values
cat("Benchmark 5: Lists with NULL values (#nil)\n")

engine5 <- RyeEngine$new()
code_nulls <- "(list 1 #nil 2 #nil 3 #nil)"
code_many_nulls <- paste0("(list ", paste(rep("#nil", 100), collapse = " "), ")")

tokens_nulls <- engine5$tokenize(code_nulls)
tokens_many_nulls <- engine5$tokenize(code_many_nulls)

bench_nulls <- benchmark_component(
  "Few NULLs" = engine5$parse(tokens_nulls),
  "100 NULLs" = engine5$parse(tokens_many_nulls),
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
