# Edge case tests for sequence operations

engine <- make_engine()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  toplevel_env(engine, env) # nolint: object_usage_linter.
  env = env
}

test_that("take handles edge cases", {
  env <- setup_env()

  # n = 0
  expect_equal(get("take", envir = env)(0, list(1, 2, 3)), list())

  # n = 1
  expect_equal(get("take", envir = env)(1, list(1, 2, 3)), list(1))

  # n equals list length
  expect_equal(get("take", envir = env)(3, list(1, 2, 3)), list(1, 2, 3))

  # n > list length (should return whole list)
  expect_equal(get("take", envir = env)(10, list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(get("take", envir = env)(5, list()), list())
})

test_that("drop handles edge cases", {
  env <- setup_env()

  # n = 0
  expect_equal(get("drop", envir = env)(0, list(1, 2, 3)), list(1, 2, 3))

  # n = 1
  expect_equal(get("drop", envir = env)(1, list(1, 2, 3)), list(2, 3))

  # n equals list length
  expect_equal(get("drop", envir = env)(3, list(1, 2, 3)), list())

  # n > list length (should return empty list)
  expect_equal(get("drop", envir = env)(10, list(1, 2, 3)), list())

  # Empty list
  expect_equal(get("drop", envir = env)(5, list()), list())
})

test_that("take-while handles edge cases", {
  env <- setup_env()

  # Always true predicate
  expect_equal(get("take-while", envir = env)(function(x) TRUE, list(1, 2, 3)), list(1, 2, 3))

  # Always false predicate
  expect_equal(get("take-while", envir = env)(function(x) FALSE, list(1, 2, 3)), list())

  # Empty list
  expect_equal(get("take-while", envir = env)(function(x) TRUE, list()), list())

  # First element fails
  expect_equal(get("take-while", envir = env)(function(x) x > 5, list(1, 2, 3)), list())

  # All but last pass
  expect_equal(get("take-while", envir = env)(function(x) x < 3, list(1, 2, 3)), list(1, 2))
})

test_that("drop-while handles edge cases", {
  env <- setup_env()

  # Always true predicate
  expect_equal(get("drop-while", envir = env)(function(x) TRUE, list(1, 2, 3)), list())

  # Always false predicate
  expect_equal(get("drop-while", envir = env)(function(x) FALSE, list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(get("drop-while", envir = env)(function(x) TRUE, list()), list())

  # First element fails
  expect_equal(get("drop-while", envir = env)(function(x) x > 5, list(1, 2, 3)), list(1, 2, 3))
})

test_that("partition handles edge cases", {
  env <- setup_env()

  # n = 1
  expect_equal(get("partition", envir = env)(1, list(1, 2, 3)), list(list(1), list(2), list(3)))

  # n equals list length
  expect_equal(get("partition", envir = env)(3, list(1, 2, 3)), list(list(1, 2, 3)))

  # n > list length (returns empty list - incomplete partition is dropped)
  expect_equal(get("partition", envir = env)(10, list(1, 2, 3)), list())

  # List length not divisible by n (incomplete partition is dropped)
  result <- get("partition", envir = env)(2, list(1, 2, 3, 4, 5))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], list(3, 4))

  # Empty list
  expect_equal(get("partition", envir = env)(2, list()), list())

  # Single element (incomplete partition is dropped)
  expect_equal(get("partition", envir = env)(2, list(42)), list())
})

test_that("partition with step parameter handles edge cases", {
  env <- setup_env()

  # step < n (overlapping partitions)
  result <- get("partition", envir = env)(3, list(1, 2, 3, 4, 5), 1)
  expect_equal(length(result), 3)
  expect_equal(result[[1]], list(1, 2, 3))
  expect_equal(result[[2]], list(2, 3, 4))
  expect_equal(result[[3]], list(3, 4, 5))

  # step > n (gaps between partitions)
  result <- get("partition", envir = env)(2, list(1, 2, 3, 4, 5, 6), 3)
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], list(4, 5))

  # step = n (non-overlapping, same as default)
  result <- get("partition", envir = env)(2, list(1, 2, 3, 4), 2)
  expect_equal(result, list(list(1, 2), list(3, 4)))
})

test_that("flatten handles edge cases", {
  env <- setup_env()

  # Already flat
  expect_equal(get("flatten", envir = env)(list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(get("flatten", envir = env)(list()), list())

  # Single nested list
  expect_equal(get("flatten", envir = env)(list(list(1, 2, 3))), list(1, 2, 3))

  # Deeply nested (3 levels)
  deep <- list(1, list(2, list(3, list(4))))
  result <- get("flatten", envir = env)(deep)
  expect_equal(result, list(1, 2, 3, 4))

  # Mixed nesting
  mixed <- list(1, list(2, 3), 4, list(list(5, 6), 7))
  result <- get("flatten", envir = env)(mixed)
  expect_equal(result, list(1, 2, 3, 4, 5, 6, 7))

  # Empty nested lists
  expect_equal(get("flatten", envir = env)(list(list(), list(), list())), list())
})
