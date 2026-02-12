# Comprehensive type conversion tests

engine <- make_engine()

# Type conversion tests
test_that("->symbol converts to symbols", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  result <- engine$eval_in_env(engine$read("(->symbol \"foo\")")[[1]], env)
  expect_true(is.symbol(result))
})

test_that("->number converts to numbers", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(->number \"42\")")[[1]], env), 42)
})

test_that("->integer converts to integers", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(->integer \"42\")")[[1]], env), 42L)
  expect_equal(engine$eval_in_env(engine$read("(->integer 3.14)")[[1]], env), 3L)
})

test_that("->double converts to doubles", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(->double 42)")[[1]], env), 42.0)
})

test_that("->complex converts to complex", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  z <- engine$eval_in_env(engine$read("(->complex 42)")[[1]], env)
  expect_equal(Re(z), 42.0)
})

test_that("symbol->string and string->symbol work", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(symbol->string 'foo)")[[1]], env), "foo")
  result <- engine$eval_in_env(engine$read("(string->symbol \"bar\")")[[1]], env)
  expect_true(is.symbol(result))
})

test_that("->list converts vectors to lists", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  result <- engine$eval_in_env(engine$read("(->list 1)")[[1]], env)
  expect_equal(length(result), 1)
})

test_that("->vector converts lists to vectors", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  result <- engine$eval_in_env(engine$read("(->vector '(1 2 3))")[[1]], env)
  expect_equal(length(result), 3)
})

test_that("exact->inexact converts integers to doubles", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(exact->inexact 5)")[[1]], env), 5.0)
})

test_that("inexact->exact converts doubles to integers", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(inexact->exact 5.0)")[[1]], env), 5L)
  expect_equal(engine$eval_in_env(engine$read("(inexact->exact 5.7)")[[1]], env), 6L)
})

test_that("conversion roundtrips work", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  result <- engine$eval_in_env(engine$read("(inexact->exact (exact->inexact 42))")[[1]], env)
  expect_equal(result, 42L)
})

test_that("->integer truncates towards zero", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(->integer 3.7)")[[1]], env), 3L)
  expect_equal(engine$eval_in_env(engine$read("(->integer -3.7)")[[1]], env), -3L)
})

# ============================================================================
# Coverage: Type validation error paths
# ============================================================================

test_that("symbol->string errors on non-symbol", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_error(
    engine$eval_in_env(engine$read("(symbol->string 42)")[[1]], env),
    "must be a symbol")
})

test_that("string->symbol errors on non-string", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_error(
    engine$eval_in_env(engine$read("(string->symbol 42)")[[1]], env),
    "must be a string")
})

test_that("exact->inexact errors on non-number", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_error(
    engine$eval_in_env(engine$read('(exact->inexact "foo")')[[1]], env),
    "must be a number")
})

test_that("inexact->exact errors on non-number", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  expect_error(
    engine$eval_in_env(engine$read('(inexact->exact "foo")')[[1]], env),
    "must be a number")
})

# ============================================================================
# Coverage: String-to-number conversion paths and errors
# ============================================================================

test_that("->integer string path and error paths", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  # String -> integer success
  expect_equal(engine$eval_in_env(engine$read('(->integer "42")')[[1]], env), 42L)

  # String -> integer failure
  expect_error(
    engine$eval_in_env(engine$read('(->integer "not-a-number")')[[1]], env),
    "Cannot convert")

  # Non-string, non-number -> error
  expect_error(
    engine$eval_in_env(engine$read("(->integer #t)")[[1]], env),
    "cannot convert to integer")
})

test_that("->double string path and error paths", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  # String -> double success
  expect_equal(engine$eval_in_env(engine$read('(->double "3.14")')[[1]], env), 3.14)

  # String -> double failure
  expect_error(
    engine$eval_in_env(engine$read('(->double "nope")')[[1]], env),
    "Cannot convert")

  # Non-string, non-number -> error
  expect_error(
    engine$eval_in_env(engine$read("(->double #t)")[[1]], env),
    "cannot convert to double")
})

test_that("->complex string path and error paths", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env)

  # String -> complex success
  result <- engine$eval_in_env(engine$read('(->complex "1+2i")')[[1]], env)
  expect_equal(Re(result), 1)
  expect_equal(Im(result), 2)

  # String -> complex failure
  expect_error(
    engine$eval_in_env(engine$read('(->complex "nope")')[[1]], env),
    "Cannot convert")

  # Non-string, non-number -> error
  expect_error(
    engine$eval_in_env(engine$read("(->complex #t)")[[1]], env),
    "cannot convert to complex")
})
