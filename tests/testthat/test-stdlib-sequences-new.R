# Tests for new sequences.arl additions:
# find, distinct, split-at, split-with, interpose, partition-by

engine <- make_engine()

# ============================================================================
# find
# ============================================================================

test_that("find returns first matching element", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(find even? '(1 3 4 5 6))")[[1]], env = env)
  expect_equal(result, 4)

  # Not found returns #f
  result <- engine$eval(
    engine$read("(find even? '(1 3 5))")[[1]], env = env)
  expect_false(result)

  # Empty list returns #f
  result <- engine$eval(
    engine$read("(find even? '())")[[1]], env = env)
  expect_false(result)

  # First element matches
  result <- engine$eval(
    engine$read("(find even? '(2 3 4))")[[1]], env = env)
  expect_equal(result, 2)
})

# ============================================================================
# distinct
# ============================================================================

test_that("distinct removes duplicates preserving order", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(distinct '(1 2 1 3 2))")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))

  # Empty list
  result <- engine$eval(
    engine$read("(distinct '())")[[1]], env = env)
  expect_equal(result, list())

  # No duplicates
  result <- engine$eval(
    engine$read("(distinct '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))

  # All same
  result <- engine$eval(
    engine$read("(distinct '(1 1 1))")[[1]], env = env)
  expect_equal(result, list(1))
})

test_that("distinct works with symbols", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(distinct '(a b a c b))")[[1]], env = env)
  expect_equal(result, list(quote(a), quote(b), quote(c)))
})

# ============================================================================
# split-at
# ============================================================================

test_that("split-at splits list at index", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(split-at 2 '(a b c d))")[[1]], env = env)
  expect_equal(result, list(list(quote(a), quote(b)), list(quote(c), quote(d))))

  # Split at 0
  result <- engine$eval(
    engine$read("(split-at 0 '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(list(), list(1, 2, 3)))

  # Split beyond length
  result <- engine$eval(
    engine$read("(split-at 10 '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(list(1, 2, 3), list()))

  # Empty list
  result <- engine$eval(
    engine$read("(split-at 2 '())")[[1]], env = env)
  expect_equal(result, list(list(), list()))
})

# ============================================================================
# split-with
# ============================================================================

test_that("split-with splits at first non-matching element", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(split-with even? '(2 4 1 3))")[[1]], env = env)
  expect_equal(result, list(list(2, 4), list(1, 3)))

  # No match at start
  result <- engine$eval(
    engine$read("(split-with even? '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(list(), list(1, 2, 3)))

  # All match
  result <- engine$eval(
    engine$read("(split-with even? '(2 4 6))")[[1]], env = env)
  expect_equal(result, list(list(2, 4, 6), list()))

  # Empty list
  result <- engine$eval(
    engine$read("(split-with even? '())")[[1]], env = env)
  expect_equal(result, list(list(), list()))
})

# ============================================================================
# interpose
# ============================================================================

test_that("interpose inserts separator between elements", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(interpose 0 '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(1, 0, 2, 0, 3))

  # Single element
  result <- engine$eval(
    engine$read("(interpose 0 '(1))")[[1]], env = env)
  expect_equal(result, list(1))

  # Empty list
  result <- engine$eval(
    engine$read("(interpose 0 '())")[[1]], env = env)
  expect_equal(result, list())

  # String separator
  result <- engine$eval(
    engine$read("(interpose \",\" '(\"a\" \"b\" \"c\"))")[[1]], env = env)
  expect_equal(result, list("a", ",", "b", ",", "c"))
})

# ============================================================================
# partition-by
# ============================================================================

test_that("partition-by splits into runs by key", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(partition-by even? '(2 4 1 3 6))")[[1]], env = env)
  expect_equal(result, list(list(2, 4), list(1, 3), list(6)))

  # All same key
  result <- engine$eval(
    engine$read("(partition-by even? '(2 4 6))")[[1]], env = env)
  expect_equal(result, list(list(2, 4, 6)))

  # Empty list
  result <- engine$eval(
    engine$read("(partition-by even? '())")[[1]], env = env)
  expect_equal(result, list())

  # Single element
  result <- engine$eval(
    engine$read("(partition-by even? '(1))")[[1]], env = env)
  expect_equal(result, list(list(1)))

  # Alternating
  result <- engine$eval(
    engine$read("(partition-by even? '(1 2 3 4 5))")[[1]], env = env)
  expect_equal(result, list(list(1), list(2), list(3), list(4), list(5)))
})
