# Entry point for running native .arl test files

thin <- make_cran_thinner()

test_that("native tests run successfully", {
  thin()
  # Find the native tests directory relative to this test file
  test_dir <- testthat::test_path("..", "native")
  run_native_tests(test_dir)
})
