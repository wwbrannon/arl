# Sequence helper tests: take, drop, partition, flatten, member, contains?

engine <- make_engine()

test_that("sequence helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$take(2, list(1, 2, 3)), list(1, 2))
  expect_equal(env$drop(2, list(1, 2, 3)), list(3))
  expect_equal(env$`take-while`(function(x) x < 3, list(1, 2, 3, 1)), list(1, 2))
  expect_equal(env$`drop-while`(function(x) x < 3, list(1, 2, 3, 1)), list(3, 1))
  expect_equal(env$partition(2, list(1, 2, 3, 4)), list(list(1, 2), list(3, 4)))
  expect_equal(env$flatten(list(1, list(2, list(3)), 4)), list(1, 2, 3, 4))
})

test_that("member and contains? sequence helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$member(2, list(1, 2, 3)), list(2, 3))
  expect_false(env$member(5, list(1, 2, 3)))

  # member uses equal? by default (structural equality)
  expect_equal(env$member(list(1, 2), list(list(3, 4), list(1, 2), list(5, 6))),
               list(list(1, 2), list(5, 6)))

  # use-identical keyword for identity comparison (2 is double, identical match)
  expect_equal(env$member(2, list(1, 2, 3), `use-identical` = TRUE), list(2, 3))
  # 2L (integer) is not identical to 2 (double) in R
  expect_false(env$member(2L, list(1, 2, 3), `use-identical` = TRUE))

  expect_true(env$`contains?`(2, list(1, 2, 3)))
  expect_false(env$`contains?`(5, list(1, 2, 3)))
})

test_that("length= checks exact length", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(length= '(1 2 3) 3)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(length= '(1 2) 3)")[[1]], env))
})

test_that("length> checks greater length", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(length> '(1 2 3) 2)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(length> '(1 2) 2)")[[1]], env))
})

test_that("length< checks less length", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(length< '(1 2) 3)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(length< '(1 2 3) 3)")[[1]], env))
})

test_that("length predicates work with empty sequences", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(length= '() 0)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(length< '() 1)")[[1]], env))
})

test_that("length predicates work with vectors", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  # Note: length("hello") is 1 in R (vector length), not 5 (character count)
  # Use nchar() for string character count
  expect_true(engine$eval_in_env(engine$read("(length= \"hello\" 1)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(length> \"hello\" 1)")[[1]], env))
})

test_that("length predicates handle boundaries", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(length= '(1 2 3) 3)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(length> '(1 2 3) 3)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(length> '(1 2 3) 2)")[[1]], env))
})

# ============================================================================
# Coverage: partition zero-n error, flatten recursive
# ============================================================================

test_that("partition errors when n is zero", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_error(
    engine$eval_in_env(engine$read("(partition 0 (list 1 2 3))")[[1]], env),
    "requires positive n and step")
})

test_that("flatten handles deeply nested lists", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  result <- engine$eval_in_env(
    engine$read("(flatten (list 1 (list 2 (list 3))))")[[1]], env)
  expect_equal(result, list(1, 2, 3))
})

test_that("zip with no arguments returns empty list", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  result <- engine$eval_in_env(engine$read("(zip)")[[1]], env)
  expect_equal(result, list())
})
