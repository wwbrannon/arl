if (!nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
  withr::local_envvar(NOT_CRAN = "true")
}

run_example <- function(example_name) {
  example_path <- system.file("examples", example_name, package = "arl")
  if (!nzchar(example_path)) {
    example_path <- testthat::test_path("../../inst/examples", example_name)
  }
  testthat::skip_if_not(file.exists(example_path), "Example file not found")

  engine <- Engine$new(env = new.env())
  stdlib_env(engine) # nolint: object_usage_linter.
  env <- engine$env$env

  out_dir <- tempfile("arl-example-")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  old_env <- Sys.getenv("ARL_OUTPUT_DIR", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("ARL_OUTPUT_DIR")
    } else {
      Sys.setenv(ARL_OUTPUT_DIR = old_env)
    }
  }, add = TRUE)
  Sys.setenv(ARL_OUTPUT_DIR = out_dir)

  output <- capture.output(engine$load_file_in_env(example_path, env))
  list(env = env, output = output, out_dir = out_dir, path = example_path)
}

test_that("fibonacci example runs end-to-end", {
  result <- run_example("fibonacci.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$fib10, 55)
  expect_equal(result$env$`example-result`$sum10, 88)
  expect_true(file.exists(file.path(result$out_dir, "fibonacci-report.txt")))
})

test_that("quicksort example runs end-to-end", {
  result <- run_example("quicksort.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$sorted_small, list(1, 3, 4))
  expect_equal(result$env$`example-result`$merge_sort, list(1, 1, 2, 3, 4, 5, 6, 9))
  expect_true(file.exists(file.path(result$out_dir, "quicksort-report.txt")))
})

test_that("fizzbuzz example runs end-to-end", {
  result <- run_example("fizzbuzz.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$count_fizzbuzz, 6)
  expect_equal(result$env$`example-result`$count_numbers, 53)
  expect_true(file.exists(file.path(result$out_dir, "fizzbuzz-report.txt")))
})

test_that("macro examples run end-to-end", {
  result <- run_example("macro-examples.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$threading_result, 15, ignore_attr = "arl_src")
  expect_equal(result$env$`example-result`$aif_result, 5)
  expect_true(file.exists(file.path(result$out_dir, "macro-report.txt")))
})

test_that("data analysis example runs end-to-end", {
  result <- run_example("data-analysis.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$passing_avg, 92.1)
  expect_equal(result$env$`example-result`$food_total, 175)
  expect_true(file.exists(file.path(result$out_dir, "data-analysis-report.txt")))
})

test_that("graph paths example runs end-to-end", {
  result <- run_example("graph-paths.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$shortest_cost, 7)
  expect_equal(result$env$`example-result`$bfs_order, list("A", "B", "C", "D", "E"))
  expect_true(file.exists(file.path(result$out_dir, "graph-report.txt")))
})

test_that("sales report example runs end-to-end", {
  result <- run_example("sales-report.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$total_sales, 690)
  expect_equal(result$env$`example-result`$top_product, "beta")
  expect_true(file.exists(file.path(result$out_dir, "sales-report.csv")))
})

test_that("log parser example runs end-to-end", {
  result <- run_example("log-parser.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$status_500, 2)
  expect_equal(result$env$`example-result`$avg_latency, 120)
  expect_true(file.exists(file.path(result$out_dir, "log-summary.txt")))
})

test_that("pipeline macros example runs end-to-end", {
  result <- run_example("pipeline-macros.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$pipeline_total, 45)
  expect_equal(result$env$`example-result`$expanded_steps, 3)
  expect_true(file.exists(file.path(result$out_dir, "pipeline-report.txt")))
})

test_that("task runner example runs end-to-end", {
  result <- run_example("task-runner.arl")
  expect_snapshot(result$output)
  expect_equal(result$env$`example-result`$run_order,
               list("clean", "compile", "test", "package", "deploy"))
  expect_true(file.exists(file.path(result$out_dir, "task-runner.log")))
})
