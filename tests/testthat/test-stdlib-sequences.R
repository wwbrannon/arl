# Sequence helper tests: take, drop, partition, flatten, member, contains?

engine <- make_engine()

test_that("sequence helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("take", envir = env)(2, list(1, 2, 3)), list(1, 2))
  expect_equal(get("drop", envir = env)(2, list(1, 2, 3)), list(3))
  expect_equal(get("take-while", envir = env)(function(x) x < 3, list(1, 2, 3, 1)), list(1, 2))
  expect_equal(get("drop-while", envir = env)(function(x) x < 3, list(1, 2, 3, 1)), list(3, 1))
  expect_equal(get("partition", envir = env)(2, list(1, 2, 3, 4)), list(list(1, 2), list(3, 4)))
  expect_equal(get("flatten", envir = env)(list(1, list(2, list(3)), 4)), list(1, 2, 3, 4))
})

test_that("member and contains? sequence helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("member", envir = env)(2, list(1, 2, 3)), list(2, 3))
  expect_false(get("member", envir = env)(5, list(1, 2, 3)))

  # member uses equal? by default (structural equality)
  expect_equal(get("member", envir = env)(list(1, 2), list(list(3, 4), list(1, 2), list(5, 6))),
               list(list(1, 2), list(5, 6)))

  # use-identical keyword for identity comparison (2 is double, identical match)
  expect_equal(get("member", envir = env)(2, list(1, 2, 3), `use-identical` = TRUE), list(2, 3))
  # 2L (integer) is not identical to 2 (double) in R
  expect_false(get("member", envir = env)(2L, list(1, 2, 3), `use-identical` = TRUE))

  expect_true(get("contains?", envir = env)(2, list(1, 2, 3)))
  expect_false(get("contains?", envir = env)(5, list(1, 2, 3)))
})

test_that("length= checks exact length", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length= '(1 2 3) 3)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length= '(1 2) 3)")[[1]], env = env))
})

test_that("length> checks greater length", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length> '(1 2 3) 2)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length> '(1 2) 2)")[[1]], env = env))
})

test_that("length< checks less length", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length< '(1 2) 3)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length< '(1 2 3) 3)")[[1]], env = env))
})

test_that("length predicates work with empty sequences", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length= '() 0)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(length< '() 1)")[[1]], env = env))
})

test_that("length predicates work with vectors", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # Note: length("hello") is 1 in R (vector length), not 5 (character count)
  # Use nchar() for string character count
  expect_true(engine$eval(engine$read("(length= \"hello\" 1)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length> \"hello\" 1)")[[1]], env = env))
})

test_that("length predicates handle boundaries", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length= '(1 2 3) 3)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length> '(1 2 3) 3)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(length> '(1 2 3) 2)")[[1]], env = env))
})

# ============================================================================
# Coverage: partition zero-n error, flatten recursive
# ============================================================================

test_that("partition errors when n is zero", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_error(
    engine$eval(engine$read("(partition 0 (list 1 2 3))")[[1]], env = env),
    "requires positive n and step")
})

test_that("flatten handles deeply nested lists", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(flatten (list 1 (list 2 (list 3))))")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))
})

test_that("zip with no arguments returns empty list", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  result <- engine$eval(engine$read("(zip)")[[1]], env = env)
  expect_equal(result, list())
})
