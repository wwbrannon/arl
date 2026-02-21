# Edge case tests for list operations

engine <- make_engine()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  toplevel_env(engine, env) # nolint: object_usage_linter.
  env
}

test_that("car handles edge cases", {
  env <- setup_env()

  # Empty list should return NULL
  expect_null(get("car", envir = env)(list()))

  # Single element
  expect_equal(get("car", envir = env)(list(42)), 42)

  # Nested lists
  expect_equal(get("car", envir = env)(list(list(1, 2), 3)), list(1, 2))

  # NULL as element
  expect_null(get("car", envir = env)(list(NULL, 2)))
})

test_that("cdr handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(get("cdr", envir = env)(list()), list())

  # Single element (should return empty list)
  expect_equal(get("cdr", envir = env)(list(42)), list())

  # Two elements
  expect_equal(get("cdr", envir = env)(list(1, 2)), list(2))

  # Nested structures
  result <- get("cdr", envir = env)(list(1, list(2, 3), 4))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(2, 3))
})

test_that("cons handles edge cases", {
  env <- setup_env()

  # Cons to empty list
  expect_equal(get("cons", envir = env)(1, list()), list(1))

  # Cons NULL
  expect_equal(get("cons", envir = env)(NULL, list(1, 2)), list(NULL, 1, 2))

  # Cons nested list
  result <- get("cons", envir = env)(list(1, 2), list(3, 4))
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], 3)
})

test_that("append handles edge cases", {
  env <- setup_env()

  # Both empty
  expect_equal(get("append", envir = env)(list(), list()), list())

  # First empty
  expect_equal(get("append", envir = env)(list(), list(1, 2)), list(1, 2))

  # Second empty
  expect_equal(get("append", envir = env)(list(1, 2), list()), list(1, 2))

  # Single elements
  expect_equal(get("append", envir = env)(list(1), list(2)), list(1, 2))
})

test_that("reverse handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(get("reverse", envir = env)(list()), list())

  # Single element
  expect_equal(get("reverse", envir = env)(list(42)), list(42))

  # Nested lists (should reverse top level only)
  result <- get("reverse", envir = env)(list(list(1, 2), list(3, 4)))
  expect_equal(result[[1]], list(3, 4))
  expect_equal(result[[2]], list(1, 2))
})

test_that("list* handles edge cases", {
  env <- setup_env()

  # Single element with empty list
  expect_equal(get("list*", envir = env)(1, list()), list(1))

  # Multiple elements
  expect_equal(get("list*", envir = env)(1, 2, list(3)), list(1, 2, 3))

  # With NULL
  expect_equal(get("list*", envir = env)(NULL, list(1)), list(NULL, 1))
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
  result <- get("flatten", envir = env)(deep)
  expect_equal(length(result), 10)
  expect_equal(result[[1]], 10)
  expect_equal(result[[10]], 1)

  # car/cdr should navigate nested structures
  expect_equal(get("car", envir = env)(deep), 10)
  expect_equal(get("car", envir = env)(get("cdr", envir = env)(deep)), list(9, list(8, list(7, list(6, list(5, list(4, list(3, list(2, list(1))))))))))
})
