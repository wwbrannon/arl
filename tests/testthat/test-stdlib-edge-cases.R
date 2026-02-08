# Remaining edge case tests for Rye standard library
# List operation edge cases moved to test-stdlib-edge-list.R
# Higher-order function edge cases moved to test-stdlib-edge-functions.R
# Sequence operation edge cases moved to test-stdlib-edge-sequences.R
# Predicates edge cases moved to test-stdlib-edge-predicates.R
# String edge cases moved to test-stdlib-strings.R
# Math boundary conditions moved to test-stdlib-math.R

engine <- RyeEngine$new()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  stdlib_env(engine, env)
  env
}

# ============================================================================
# Performance Tests (Large Lists)
# ============================================================================

test_that("stdlib handles large lists efficiently", {
  env <- setup_env()

  # Large list (1000 elements)
  large_list <- as.list(1:1000)

  # map should handle large lists
  result <- env$map(function(x) x * 2, large_list)
  expect_equal(length(result), 1000)
  expect_equal(result[[1]], 2)
  expect_equal(result[[1000]], 2000)

  # filter should handle large lists
  result <- env$filter(function(x) x %% 2 == 0, large_list)
  expect_equal(length(result), 500)

  # reduce should handle large lists
  result <- env$reduce(`+`, large_list)
  expect_equal(result, sum(1:1000))

  # reverse should handle large lists
  result <- env$reverse(large_list)
  expect_equal(result[[1]], 1000)
  expect_equal(result[[1000]], 1)
})

# ============================================================================
# Mixed Type Tests
# ============================================================================

test_that("stdlib handles mixed types correctly", {
  env <- setup_env()

  # List with mixed types
  mixed <- list(1, "two", 3.0, TRUE, NULL, list(5))

  # map should work with mixed types
  result <- env$map(function(x) is.null(x), mixed)
  expect_equal(result[[5]], TRUE)

  # filter should work with mixed types
  result <- env$filter(function(x) is.numeric(x), mixed)
  expect_equal(length(result), 2)  # 1 and 3.0

  # str should convert all types
  result <- env$str(1, "two", TRUE, NULL)
  expect_true(is.character(result))
})
