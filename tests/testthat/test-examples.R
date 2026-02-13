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
  toplevel_env(engine) # nolint: object_usage_linter.
  capture.output(engine$load_file_in_env(example_path, engine$get_env()))
  invisible(TRUE)
}

test_that("fibonacci example runs end-to-end", {
  expect_no_error(run_example("fibonacci.arl"))
})

test_that("quicksort example runs end-to-end", {
  expect_no_error(run_example("quicksort.arl"))
})

test_that("fizzbuzz example runs end-to-end", {
  expect_no_error(run_example("fizzbuzz.arl"))
})

test_that("macro examples run end-to-end", {
  expect_no_error(run_example("macro-examples.arl"))
})

test_that("data analysis example runs end-to-end", {
  expect_no_error(run_example("data-analysis.arl"))
})

test_that("graph paths example runs end-to-end", {
  expect_no_error(run_example("graph-paths.arl"))
})

test_that("sales report example runs end-to-end", {
  expect_no_error(run_example("sales-report.arl"))
})

test_that("log parser example runs end-to-end", {
  expect_no_error(run_example("log-parser.arl"))
})

test_that("pipeline macros example runs end-to-end", {
  expect_no_error(run_example("pipeline-macros.arl"))
})

test_that("task runner example runs end-to-end", {
  expect_no_error(run_example("task-runner.arl"))
})
