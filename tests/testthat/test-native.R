# Entry point for running native .rye test files

test_that("native tests run successfully", {
  # Find the native tests directory relative to this test file
  test_dir <- testthat::test_path("..", "native")
  run_native_tests(test_dir)
})
