# Extracted from test-stdlib-edge-cases.R:91

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rye", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
setup_env <- function() {
  env <- new.env()
  rye_load_stdlib(env)
  env
}

# test -------------------------------------------------------------------------
env <- setup_env()
expect_equal(env$reverse(list()), list())
expect_equal(env$reverse(list(42)), list(42))
result <- env$reverse(list(list(1, 2), list(3, 4)))
expect_equal(result[[1]], list(3, 4))
