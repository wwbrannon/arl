# Extracted from test-stdlib-edge-cases.R:332

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
expect_equal(env$flatten(list(1, 2, 3)), list(1, 2, 3))
expect_equal(env$flatten(list()), list())
expect_equal(env$flatten(list(list(1, 2, 3))), list(1, 2, 3))
deep <- list(1, list(2, list(3, list(4))))
result <- env$flatten(deep)
expect_equal(result, list(1, 2, 3, 4))
