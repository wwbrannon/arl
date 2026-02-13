# Edge case tests for list operations

engine <- make_engine()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  toplevel_env(engine, env) # nolint: object_usage_linter.
  env = env
}

test_that("car handles edge cases", {
  env <- setup_env()

  # Empty list should return NULL
  expect_null(env$car(list()))

  # Single element
  expect_equal(env$car(list(42)), 42)

  # Nested lists
  expect_equal(env$car(list(list(1, 2), 3)), list(1, 2))

  # NULL as element
  expect_null(env$car(list(NULL, 2)))
})

test_that("cdr handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$cdr(list()), list())

  # Single element (should return empty list)
  expect_equal(env$cdr(list(42)), list())

  # Two elements
  expect_equal(env$cdr(list(1, 2)), list(2))

  # Nested structures
  result <- env$cdr(list(1, list(2, 3), 4))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(2, 3))
})

test_that("cons handles edge cases", {
  env <- setup_env()

  # Cons to empty list
  expect_equal(env$cons(1, list()), list(1))

  # Cons NULL
  expect_equal(env$cons(NULL, list(1, 2)), list(NULL, 1, 2))

  # Cons nested list
  result <- env$cons(list(1, 2), list(3, 4))
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], 3)
})

test_that("append handles edge cases", {
  env <- setup_env()

  # Both empty
  expect_equal(env$append(list(), list()), list())

  # First empty
  expect_equal(env$append(list(), list(1, 2)), list(1, 2))

  # Second empty
  expect_equal(env$append(list(1, 2), list()), list(1, 2))

  # Single elements
  expect_equal(env$append(list(1), list(2)), list(1, 2))
})

test_that("reverse handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$reverse(list()), list())

  # Single element
  expect_equal(env$reverse(list(42)), list(42))

  # Nested lists (should reverse top level only)
  result <- env$reverse(list(list(1, 2), list(3, 4)))
  expect_equal(result[[1]], list(3, 4))
  expect_equal(result[[2]], list(1, 2))
})

test_that("list* handles edge cases", {
  env <- setup_env()

  # Single element with empty list
  expect_equal(env$`list*`(1, list()), list(1))

  # Multiple elements
  expect_equal(env$`list*`(1, 2, list(3)), list(1, 2, 3))

  # With NULL
  expect_equal(env$`list*`(NULL, list(1)), list(NULL, 1))
})

# ============================================================================
# Nested Structure Tests
# ============================================================================

test_that("stdlib handles deeply nested structures", {
  env <- setup_env()

  # Create deeply nested list (10 levels)
  deep <- list(1)
  for (i in 2:10) {
    deep <- list(i, deep)
  }

  # flatten should handle deep nesting
  result <- env$flatten(deep)
  expect_equal(length(result), 10)
  expect_equal(result[[1]], 10)
  expect_equal(result[[10]], 1)

  # car/cdr should navigate nested structures
  expect_equal(env$car(deep), 10)
  expect_equal(env$car(env$cdr(deep)), list(9, list(8, list(7, list(6, list(5, list(4, list(3, list(2, list(1))))))))))
})
