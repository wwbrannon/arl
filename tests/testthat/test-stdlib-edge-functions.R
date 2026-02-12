# Edge case tests for higher-order functions

engine <- make_engine()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  toplevel_env(engine, env) # nolint: object_usage_linter.
  env
}

test_that("map handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$map(function(x) x * 2, list()), list())

  # Single element
  expect_equal(env$map(function(x) x * 2, list(5)), list(10))

  # Function returning NULL
  result <- env$map(function(x) NULL, list(1, 2, 3))
  expect_equal(length(result), 3)
  expect_null(result[[1]])

  # Identity function
  expect_equal(env$map(function(x) x, list(1, 2, 3)), list(1, 2, 3))
})

test_that("filter handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$filter(function(x) TRUE, list()), list())

  # All match
  expect_equal(env$filter(function(x) TRUE, list(1, 2, 3)), list(1, 2, 3))

  # None match
  expect_equal(env$filter(function(x) FALSE, list(1, 2, 3)), list())

  # Single element matches
  expect_equal(env$filter(function(x) x == 2, list(1, 2, 3)), list(2))
})

test_that("reduce handles edge cases", {
  env <- setup_env()

  # Single element
  expect_equal(env$reduce(`+`, list(42)), 42)

  # Two elements
  expect_equal(env$reduce(`+`, list(1, 2)), 3)

  # String concatenation
  concat <- function(a, b) paste0(a, b)
  expect_equal(env$reduce(concat, list("a", "b", "c")), "abc")
})

test_that("mapcat handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$mapcat(function(x) list(x), list()), list())

  # Function returning empty lists
  expect_equal(env$mapcat(function(x) list(), list(1, 2, 3)), list())

  # Function returning single element
  expect_equal(env$mapcat(function(x) list(x), list(1, 2)), list(1, 2))

  # Mixed result sizes
  result <- env$mapcat(function(x) if (x == 1) list(x) else list(x, x), list(1, 2, 3))
  expect_equal(result, list(1, 2, 2, 3, 3))
})

test_that("every? and any? handle edge cases", {
  env <- setup_env()

  # Empty list - every? should be TRUE (vacuous truth)
  expect_true(env$`every?`(function(x) FALSE, list()))

  # Empty list - any? should be FALSE
  expect_false(env$`any?`(function(x) TRUE, list()))

  # Single element
  expect_true(env$`every?`(function(x) x > 0, list(5)))
  expect_true(env$`any?`(function(x) x > 0, list(5)))

  # All same value
  expect_true(env$`every?`(function(x) x == 1, list(1, 1, 1)))
})

test_that("apply handles edge cases", {
  env <- setup_env()

  # Single argument
  expect_equal(env$funcall(identity, list(42)), 42)

  # Empty list (should work if function accepts no args)
  zero_arg_fn <- function() 42
  expect_equal(env$funcall(zero_arg_fn, list()), 42)

  # Many arguments
  many_sum <- function(...) sum(...)
  expect_equal(env$funcall(many_sum, as.list(1:100)), 5050)
})
