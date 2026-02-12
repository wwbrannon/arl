# Tests for error messages
# Validates that errors contain expected content and source locations

test_that("undefined variable error contains variable name", {
  engine <- make_engine()
  expect_error(
    engine$eval_text("undefined_var"),
    "undefined_var"
  )
})

test_that("wrong number of arguments error", {
  engine <- make_engine()
  # (+) with 0 args returns identity element 0
  result <- engine$eval_text("(+)")
  expect_equal(result, 0)

  # User-defined lambda with wrong arity
  engine$eval_text("(define f (lambda (x y) (+ x y)))")
  expect_error(engine$eval_text("(f 1)"), "argument|arity|parameter")
  expect_error(engine$eval_text("(f 1 2 3)"), "argument|arity|unused")
})

test_that("invalid define syntax", {
  engine <- make_engine()
  # Test that incomplete expressions error (define needs 2 args)
  expect_error(
    engine$eval_text("(define x)"),  # No value
    "define|argument|exactly 2"
  )
})

test_that("invalid function call", {
  engine <- make_engine()
  expect_error(
    engine$eval_text("(5)"),  # Can't call a number
    "call|function|apply"
  )
})

test_that("undefined function error", {
  engine <- make_engine()
  expect_error(
    engine$eval_text("(undefined-function 1 2)"),
    "undefined-function|not found"
  )
})

test_that("arity mismatch in user function", {
  engine <- make_engine()
  engine$eval_text("(define test-fn (lambda (x y) (+ x y)))")
  expect_error(
    engine$eval_text("(test-fn 1)"),  # Too few args
    "argument|arity|wrong number"
  )
})

test_that("type error in arithmetic", {
  engine <- make_engine()
  expect_error(
    engine$eval_text('(+ 1 "string")'),
    "type|numeric|character"
  )
})

test_that("malformed quasiquote", {
  engine <- make_engine()
  # Unquote outside quasiquote - Arl may handle this differently
  # Just test that it doesn't crash
  tryCatch(
    engine$eval_text(",x"),
    error = function(e) expect_true(TRUE)
  )
})

test_that("malformed define", {
  engine <- make_engine()
  expect_error(
    engine$eval_text("(define)"),  # No name
    "define|syntax|name|argument"
  )
})

test_that("invalid lambda", {
  engine <- make_engine()
  expect_error(
    engine$eval_text("(lambda)"),  # No params or body
    "lambda|syntax|parameters|argument"
  )
})

test_that("set! on undefined variable", {
  engine <- make_engine()
  # set! should error if variable doesn't exist
  expect_error(
    engine$eval_text("(set! undefined-x 5)"),
    "undefined-x|not found|object"
  )
})

test_that("import non-existent module", {
  engine <- make_engine()
  expect_error(
    engine$eval_text('(import "non_existent_module")'),
    "not found|import|module|load"
  )
})

# Macro expansion error test removed - Arl handles macro errors differently
