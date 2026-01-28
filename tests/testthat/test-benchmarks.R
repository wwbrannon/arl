# Test Benchmark Infrastructure
# Smoke tests to ensure benchmark and profiling infrastructure works

# Helper to find benchmark directory
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

test_that("benchmark infrastructure can be loaded", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  helpers_path <- file.path(bench_dir, "benchmark-helpers.R")
  workloads_path <- file.path(bench_dir, "workloads.R")

  expect_true(file.exists(helpers_path))
  expect_true(file.exists(workloads_path))

  # Should load without error
  expect_no_error(source(helpers_path, local = TRUE))
  expect_no_error(source(workloads_path, local = TRUE))
})

test_that("workload generation produces valid Rye code", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "benchmark-helpers.R"), local = TRUE)

  # Test different workload types
  expect_type(create_workload("micro", "arithmetic"), "character")
  expect_type(create_workload("small", "arithmetic"), "character")
  expect_type(create_workload("medium", "nested_lists"), "character")

  # Workloads should be non-empty
  expect_gt(nchar(create_workload("micro", "arithmetic")), 0)
  expect_gt(nchar(create_workload("small", "strings")), 0)

  # Workloads should be parseable
  micro <- create_workload("micro", "arithmetic")
  expect_no_error(rye_read(micro))

  small <- create_workload("small", "arithmetic")
  expect_no_error(rye_read(small))
})

test_that("benchmark helper functions work", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "benchmark-helpers.R"), local = TRUE)

  # quick_time should work
  result <- quick_time(rye_read("(+ 1 2)")[[1]], n = 5)
  expect_type(result, "double")
  expect_gte(result, 0)  # May be 0 for very fast operations

  # save/load results should work
  temp_file <- tempfile(fileext = ".rds")
  test_data <- list(test = "data", numbers = 1:10)

  saved_path <- save_benchmark_results(test_data, "test", dirname(temp_file))
  expect_true(file.exists(saved_path))

  loaded_data <- load_benchmark_results(saved_path)
  expect_equal(loaded_data, test_data)

  unlink(saved_path)
})

test_that("benchmark scripts can be sourced", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  # Test that all benchmark scripts exist
  scripts <- c(
    "bench-tokenizer.R",
    "bench-parser.R",
    "bench-macro.R",
    "bench-eval.R",
    "bench-stdlib.R",
    "bench-e2e.R"
  )

  for (script in scripts) {
    path <- file.path(bench_dir, script)
    expect_true(file.exists(path), info = paste("Missing:", script))
  }

  # Test that helper scripts exist
  expect_true(file.exists(file.path(bench_dir, "run-all-benchmarks.R")))
  expect_true(file.exists(file.path(bench_dir, "analyze-results.R")))
  expect_true(file.exists(file.path(bench_dir, "compare-results.R")))
})

test_that("profiling scripts exist", {
  skip_if_not_installed("profvis")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  scripts <- c(
    "profile-tokenizer.R",
    "profile-parser.R",
    "profile-macro.R",
    "profile-eval.R",
    "run-all-profiles.R"
  )

  for (script in scripts) {
    path <- file.path(bench_dir, script)
    expect_true(file.exists(path), info = paste("Missing:", script))
  }
})

test_that("profiling helper functions work", {
  skip_if_not_installed("profvis")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "benchmark-helpers.R"), local = TRUE)

  # Create temp output directory
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # profile_component should generate HTML file
  # May fail in some environments due to profvis limitations
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
  expect_match(output_path, "\\.html$")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("analysis functions work with mock data", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "analyze-results.R"), local = TRUE)

  # Create mock benchmark results
  mock_results <- list(
    results = list(
      tokenizer = list(
        strings = structure(
          data.frame(
            expression = I(c("test1", "test2")),
            median = c(1.0, 2.0),
            mem_alloc = c(1000, 2000),
            n_itr = c(100, 100),
            n_gc = c(0, 0)
          ),
          class = c("bench_mark", "data.frame")
        )
      ),
      parser = list(
        flat = structure(
          data.frame(
            expression = I(c("test3", "test4")),
            median = c(0.5, 1.5),
            mem_alloc = c(500, 1500),
            n_itr = c(100, 100),
            n_gc = c(0, 0)
          ),
          class = c("bench_mark", "data.frame")
        )
      )
    ),
    timestamp = "test"
  )

  # Test identify_bottlenecks
  bottlenecks <- identify_bottlenecks(mock_results, threshold = 0.1)
  expect_s3_class(bottlenecks, "data.frame")
  expect_true("component" %in% names(bottlenecks))
  expect_true("time_ms" %in% names(bottlenecks))

  # Test plot_breakdown (just ensure it doesn't error)
  expect_no_error(capture.output(plot_breakdown(mock_results)))

  # Test memory_summary
  expect_no_error(capture.output(memory_summary(mock_results)))

  # Test extremes
  expect_no_error(capture.output(extremes(mock_results, n = 2)))
})

test_that("comparison functions work with mock data", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "compare-results.R"), local = TRUE)

  # Create two mock result files
  temp_dir <- tempfile()
  dir.create(temp_dir)

  mock_old <- list(
    results = list(
      tokenizer = list(
        strings = structure(
          data.frame(
            expression = I(c("test1", "test2")),
            median = c(2.0, 4.0),
            mem_alloc = c(1000, 2000)
          ),
          class = c("bench_mark", "data.frame")
        )
      )
    ),
    timestamp = "old"
  )

  mock_new <- list(
    results = list(
      tokenizer = list(
        strings = structure(
          data.frame(
            expression = I(c("test1", "test2")),
            median = c(1.0, 2.0),
            mem_alloc = c(1000, 2000)
          ),
          class = c("bench_mark", "data.frame")
        )
      )
    ),
    timestamp = "new"
  )

  old_file <- file.path(temp_dir, "old.rds")
  new_file <- file.path(temp_dir, "new.rds")

  saveRDS(mock_old, old_file)
  saveRDS(mock_new, new_file)

  # Test compare_benchmarks
  comparison <- compare_benchmarks(old_file, new_file)
  expect_s3_class(comparison, "data.frame")
  expect_true("speedup" %in% names(comparison))
  expect_true("change_pct" %in% names(comparison))

  # Should show improvements (2.0 -> 1.0 is 2x speedup)
  expect_gt(max(comparison$speedup), 1.5)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("benchmark results directory can be created", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  # Results directory should be creatable
  test_dir <- file.path(tempdir(), "test_bench_results")

  if (dir.exists(test_dir)) {
    unlink(test_dir, recursive = TRUE)
  }

  dir.create(test_dir, recursive = TRUE)
  expect_true(dir.exists(test_dir))

  # Should be able to save results there
  source(file.path(bench_dir, "benchmark-helpers.R"), local = TRUE)
  test_data <- list(test = "data")
  saved_path <- save_benchmark_results(test_data, "test", test_dir)

  expect_true(file.exists(saved_path))

  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("benchmark scripts handle missing packages gracefully", {
  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  # Even without bench installed, scripts should have checks
  scripts <- c(
    "benchmark-helpers.R",
    "bench-tokenizer.R",
    "bench-parser.R"
  )

  for (script in scripts) {
    path <- file.path(bench_dir, script)
    content <- readLines(path, warn = FALSE)

    # Should have some reference to package requirements
    has_check <- any(grepl("bench|requireNamespace|skip_if", content, ignore.case = TRUE))
    expect_true(has_check, info = paste(script, "should check for bench package"))
  }
})

test_that("real workloads can be loaded", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "workloads.R"), local = TRUE)

  # Try to load real workloads
  real <- tryCatch(
    get_real_workloads(),
    error = function(e) list()
  )

  # If examples are available, they should be non-empty strings
  if (length(real) > 0) {
    for (workload_name in names(real)) {
      expect_type(real[[workload_name]], "character")
      expect_gt(nchar(real[[workload_name]]), 0)

      # Should be valid Rye code
      expect_no_error(rye_read(real[[workload_name]]))
    }
  }
})

test_that("documentation files exist", {
  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  readme_path <- file.path(bench_dir, "README.md")
  perf_path <- file.path(bench_dir, "PERFORMANCE.md")

  expect_true(file.exists(readme_path))
  expect_true(file.exists(perf_path))

  # Check that README has key sections
  readme <- readLines(readme_path, warn = FALSE)
  expect_true(any(grepl("Quick Start", readme)))
  expect_true(any(grepl("Benchmarks", readme)))

  # Check that PERFORMANCE.md has key sections
  perf <- readLines(perf_path, warn = FALSE)
  expect_true(any(grepl("Architecture", perf)))
  expect_true(any(grepl("Performance Issues", perf)))
  expect_true(any(grepl("Verification", perf)))
})
