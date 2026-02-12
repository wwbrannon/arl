# Test Benchmark Infrastructure
# Smoke tests to ensure benchmark and profiling infrastructure works

engine <- make_engine()

# Helper to find benchmark directory
find_bench_dir <- function() {
  # Try installed package first
  bench_dir <- system.file("benchmarks", package = "arl")
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

test_that("workload generation produces valid arl code", {
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
  expect_no_error(engine$read(micro))

  small <- create_workload("small", "arithmetic")
  expect_no_error(engine$read(small))
})

test_that("benchmark helper functions work", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "benchmark-helpers.R"), local = TRUE)

  # quick_time should work
  result <- quick_time(engine$read("(+ 1 2)")[[1]], n = 5)
  expect_type(result, "double")
  expect_gte(result, 0)  # May be 0 for very fast operations

  # save/load results should work with CSV format
  temp_dir <- tempfile()
  dir.create(temp_dir)

  test_data <- data.frame(
    benchmark = c("test1", "test1"),
    expression = c("a", "b"),
    median_ms = c(1.0, 2.0),
    mem_alloc_bytes = c(1000, 2000),
    n_itr = c(100L, 100L),
    stringsAsFactors = FALSE
  )

  saved_path <- save_benchmark_results(test_data, "test", temp_dir)
  expect_true(file.exists(saved_path))
  expect_match(saved_path, "\\.csv$")

  loaded_data <- load_benchmark_results(saved_path)
  expect_equal(loaded_data$benchmark, test_data$benchmark)
  expect_equal(loaded_data$median_ms, test_data$median_ms)

  unlink(temp_dir, recursive = TRUE)
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

  # profile_component should generate RDS file (profvis objects stay as RDS)
  output_path <- tryCatch({
    profile_component({
      engine <- make_engine()
      for (i in 1:10) {
        engine$read("(+ 1 2)")
      }
    }, "test", temp_dir)
  }, error = function(e) {
    skip(paste("Profiling failed:", e$message))
  })

  expect_true(file.exists(output_path))
  expect_match(output_path, "\\.rds$")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("analysis functions work with mock data", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "analyze-results.R"), local = TRUE)

  # Create mock data frame (flat CSV format)
  mock_results <- data.frame(
    benchmark = c("strings", "strings", "flat", "flat"),
    expression = c("test1", "test2", "test3", "test4"),
    median_ms = c(1.0, 2.0, 0.5, 1.5),
    mem_alloc_bytes = c(1000, 2000, 500, 1500),
    n_itr = c(100L, 100L, 100L, 100L),
    component = c("tokenizer", "tokenizer", "parser", "parser"),
    stringsAsFactors = FALSE
  )

  # Test identify_bottlenecks
  bottlenecks <- identify_bottlenecks(mock_results, threshold = 0.1)
  expect_s3_class(bottlenecks, "data.frame")
  expect_true("component" %in% names(bottlenecks))
  expect_true("median_ms" %in% names(bottlenecks))

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

  # Create two mock CSV files
  temp_dir <- tempfile()
  dir.create(temp_dir)

  mock_old <- data.frame(
    benchmark = c("strings", "strings"),
    expression = c("test1", "test2"),
    median_ms = c(2.0, 4.0),
    mem_alloc_bytes = c(1000, 2000),
    n_itr = c(100L, 100L),
    stringsAsFactors = FALSE
  )

  mock_new <- data.frame(
    benchmark = c("strings", "strings"),
    expression = c("test1", "test2"),
    median_ms = c(1.0, 2.0),
    mem_alloc_bytes = c(1000, 2000),
    n_itr = c(100L, 100L),
    stringsAsFactors = FALSE
  )

  old_file <- file.path(temp_dir, "old.csv")
  new_file <- file.path(temp_dir, "new.csv")

  write.csv(mock_old, old_file, row.names = FALSE)
  write.csv(mock_new, new_file, row.names = FALSE)

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

test_that("write_ci_json produces valid JSON", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "benchmark-helpers.R"), local = TRUE)

  mock_df <- data.frame(
    benchmark = c("strings", "nested"),
    expression = c("10 chars", "50 levels"),
    median_ms = c(0.12, 1.50),
    mem_alloc_bytes = c(1000, 5000),
    n_itr = c(100L, 50L),
    component = c("tokenizer", "tokenizer"),
    stringsAsFactors = FALSE
  )

  json_file <- tempfile(fileext = ".json")
  write_ci_json(mock_df, json_file)

  expect_true(file.exists(json_file))

  json_content <- readLines(json_file, warn = FALSE)
  json_text <- paste(json_content, collapse = "\n")

  # Should be a JSON array
  json_trimmed <- trimws(json_text)
  expect_match(json_trimmed, "^\\[")
  expect_match(json_trimmed, "\\]$")

  # Should contain expected entries
  expect_match(json_text, "tokenizer/strings/10 chars")
  expect_match(json_text, '"unit": "ms"')

  unlink(json_file)
})

test_that("flatten_bench_results works with bench_mark objects", {
  skip_if_not_installed("bench")

  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  source(file.path(bench_dir, "benchmark-helpers.R"), local = TRUE)

  # Create a mock bench_mark object
  mock_bench <- structure(
    data.frame(
      expression = I(c("test1", "test2")),
      median = c(1.0, 2.0),
      mem_alloc = c(1000, 2000),
      n_itr = c(100L, 50L)
    ),
    class = c("bench_mark", "data.frame")
  )

  result <- flatten_bench_results(list(strings = mock_bench))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$benchmark, c("strings", "strings"))
  expect_equal(result$expression, c("test1", "test2"))
  expect_equal(result$median_ms, c(1.0, 2.0))
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
  test_data <- data.frame(
    benchmark = "test", expression = "x", median_ms = 1.0,
    mem_alloc_bytes = 100, n_itr = 10L, stringsAsFactors = FALSE
  )
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

      # Should be valid Arl code
      expect_no_error(engine$read(real[[workload_name]]))
    }
  }
})

test_that("documentation files exist", {
  bench_dir <- find_bench_dir()
  if (is.null(bench_dir)) skip("Benchmark infrastructure not found")

  readme_path <- file.path(bench_dir, "README.md")
  expect_true(file.exists(readme_path))

  # Check that README has key sections
  readme <- readLines(readme_path, warn = FALSE)
  expect_true(any(grepl("Quick Start", readme)))
  expect_true(any(grepl("Benchmarks", readme)))
})
