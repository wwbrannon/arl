# Module and Load/Run Benchmarks
# Benchmarks for import/load/run overhead

library(rye)

source("benchmarks/benchmark-helpers.R")

cat("=== Module/Load/Run Benchmarks ===\n\n")

engine <- Engine$new()

# Prepare a small Rye file in a temp dir
script_lines <- c(
  "(define add2 (lambda (x) (+ x 2)))",
  "(add2 40)"
)
script_path <- file.path(tempdir(), "rye-bench-module.rye")
writeLines(script_lines, script_path)

# Benchmark 1: Module import overhead
cat("Benchmark 1: Module import\n")

bench_import <- benchmark_component(
  "import binding" = {
    eng <- Engine$new()
    eng$eval_text("(import binding)")
  },
  "import control" = {
    eng <- Engine$new()
    eng$eval_text("(import control)")
  },
  iterations = 200,
  check = FALSE
)
print(bench_import[, c("expression", "median", "mem_alloc")])
cat("\n")

# Benchmark 2: load/run overhead
cat("Benchmark 2: load/run overhead\n")

bench_load_run <- benchmark_component(
  "load" = {
    eng <- Engine$new()
    eng$eval_text(paste0("(load \"", script_path, "\")"))
  },
  "run" = {
    eng <- Engine$new()
    eng$eval_text(paste0("(run \"", script_path, "\")"))
  },
  iterations = 200,
  check = FALSE
)
print(bench_load_run[, c("expression", "median", "mem_alloc")])
cat("\n")

# Collect all results
module_results <- list(
  import = bench_import,
  load_run = bench_load_run
)

# Save results
save_benchmark_results(module_results, "modules")

cat("=== Module/Load/Run Benchmarks Complete ===\n")
