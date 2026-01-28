# Extracted from test-benchmarks.R:163

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rye", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
find_bench_dir <- function() {
  # Try installed package first
  bench_dir <- system.file("benchmarks", package = "rye")
  if (bench_dir != "") return(bench_dir)

  # Try source directory paths
  candidates <- c(
    "inst/benchmarks",
    "../../inst/benchmarks",
    "../../../inst/benchmarks"
  )

  for (path in candidates) {
    if (file.exists(path)) return(normalizePath(path))
  }

  return(NULL)
}

# test -------------------------------------------------------------------------
skip_if_not_installed("profvis")
bench_dir <- find_bench_dir()
if (is.null(bench_dir)) skip("Benchmark infrastructure not found")
source(file.path(bench_dir, "benchmark-helpers.R"), local = TRUE)
temp_dir <- tempfile()
dir.create(temp_dir)
output_path <- tryCatch({
    profile_component({
      for (i in 1:10) {
        rye_read("(+ 1 2)")
      }
    }, "test", temp_dir)
  }, error = function(e) {
    skip(paste("Profiling failed:", e$message))
  })
expect_true(file.exists(output_path))
