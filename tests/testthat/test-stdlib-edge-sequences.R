# Edge case tests for sequence operations

engine <- RyeEngine$new()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  stdlib_env(engine, env)
  env
}

test_that("take handles edge cases", {
  env <- setup_env()

  # n = 0
  expect_equal(env$take(0, list(1, 2, 3)), list())

  # n = 1
  expect_equal(env$take(1, list(1, 2, 3)), list(1))

  # n equals list length
  expect_equal(env$take(3, list(1, 2, 3)), list(1, 2, 3))

  # n > list length (should return whole list)
  expect_equal(env$take(10, list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(env$take(5, list()), list())
})

test_that("drop handles edge cases", {
  env <- setup_env()

  # n = 0
  expect_equal(env$drop(0, list(1, 2, 3)), list(1, 2, 3))

  # n = 1
  expect_equal(env$drop(1, list(1, 2, 3)), list(2, 3))

  # n equals list length
  expect_equal(env$drop(3, list(1, 2, 3)), list())

  # n > list length (should return empty list)
  expect_equal(env$drop(10, list(1, 2, 3)), list())

  # Empty list
  expect_equal(env$drop(5, list()), list())
})

test_that("take-while handles edge cases", {
  env <- setup_env()

  # Always true predicate
  expect_equal(env$`take-while`(function(x) TRUE, list(1, 2, 3)), list(1, 2, 3))

  # Always false predicate
  expect_equal(env$`take-while`(function(x) FALSE, list(1, 2, 3)), list())

  # Empty list
  expect_equal(env$`take-while`(function(x) TRUE, list()), list())

  # First element fails
  expect_equal(env$`take-while`(function(x) x > 5, list(1, 2, 3)), list())

  # All but last pass
  expect_equal(env$`take-while`(function(x) x < 3, list(1, 2, 3)), list(1, 2))
})

test_that("drop-while handles edge cases", {
  env <- setup_env()

  # Always true predicate
  expect_equal(env$`drop-while`(function(x) TRUE, list(1, 2, 3)), list())

  # Always false predicate
  expect_equal(env$`drop-while`(function(x) FALSE, list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(env$`drop-while`(function(x) TRUE, list()), list())

  # First element fails
  expect_equal(env$`drop-while`(function(x) x > 5, list(1, 2, 3)), list(1, 2, 3))
})

test_that("partition handles edge cases", {
  env <- setup_env()

  # n = 1
  expect_equal(env$partition(1, list(1, 2, 3)), list(list(1), list(2), list(3)))

  # n equals list length
  expect_equal(env$partition(3, list(1, 2, 3)), list(list(1, 2, 3)))

  # n > list length (returns empty list - incomplete partition is dropped)
  expect_equal(env$partition(10, list(1, 2, 3)), list())

  # List length not divisible by n (incomplete partition is dropped)
  result <- env$partition(2, list(1, 2, 3, 4, 5))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], list(3, 4))

  # Empty list
  expect_equal(env$partition(2, list()), list())

  # Single element (incomplete partition is dropped)
  expect_equal(env$partition(2, list(42)), list())
})

test_that("partition with step parameter handles edge cases", {
  env <- setup_env()

  # step < n (overlapping partitions)
  result <- env$partition(3, list(1, 2, 3, 4, 5), 1)
  expect_equal(length(result), 3)
  expect_equal(result[[1]], list(1, 2, 3))
  expect_equal(result[[2]], list(2, 3, 4))
  expect_equal(result[[3]], list(3, 4, 5))

  # step > n (gaps between partitions)
  result <- env$partition(2, list(1, 2, 3, 4, 5, 6), 3)
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], list(4, 5))

  # step = n (non-overlapping, same as default)
  result <- env$partition(2, list(1, 2, 3, 4), 2)
  expect_equal(result, list(list(1, 2), list(3, 4)))
})

test_that("flatten handles edge cases", {
  env <- setup_env()

  # Already flat
  expect_equal(env$flatten(list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(env$flatten(list()), list())

  # Single nested list
  expect_equal(env$flatten(list(list(1, 2, 3))), list(1, 2, 3))

  # Deeply nested (3 levels)
  deep <- list(1, list(2, list(3, list(4))))
  result <- env$flatten(deep)
  expect_equal(result, list(1, 2, 3, 4))

  # Mixed nesting
  mixed <- list(1, list(2, 3), 4, list(list(5, 6), 7))
  result <- env$flatten(mixed)
  expect_equal(result, list(1, 2, 3, 4, 5, 6, 7))

  # Empty nested lists
  expect_equal(env$flatten(list(list(), list(), list())), list())
})
