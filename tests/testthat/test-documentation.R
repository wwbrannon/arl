test_that("doc! macro attaches docstrings to functions", {
  engine <- make_engine()
  env <- engine$env$env

  # Import core module for doc! and doc
  engine$eval_in_env(engine$read('(import core)')[[1]], env)

  # Create a test function
  engine$eval_in_env(engine$read('(define test-fn (lambda (x) (* x 2)))')[[1]], env)

  # Attach docstring
  engine$eval_in_env(engine$read('(doc! test-fn "Doubles the input value.")')[[1]], env)

  # Retrieve and verify
  result <- engine$eval_in_env(engine$read('(doc test-fn)')[[1]], env)
  expect_equal(result, "Doubles the input value.")
})

test_that("doc returns NULL for undocumented functions", {
  engine <- make_engine()
  env <- engine$env$env

  engine$eval_in_env(engine$read('(import core)')[[1]], env)
  engine$eval_in_env(engine$read('(define no-doc-fn (lambda (x) x))')[[1]], env)

  result <- engine$eval_in_env(engine$read('(doc no-doc-fn)')[[1]], env)
  expect_null(result)
})

test_that("doc! works with direct assignment functions", {
  engine <- make_engine()
  env <- engine$env$env

  engine$eval_in_env(engine$read('(import core)')[[1]], env)

  # Direct assignment from R function
  engine$eval_in_env(engine$read('(define my-abs abs)')[[1]], env)
  engine$eval_in_env(engine$read('(doc! my-abs "My absolute value function")')[[1]], env)

  result <- engine$eval_in_env(engine$read('(doc my-abs)')[[1]], env)
  expect_equal(result, "My absolute value function")
})

test_that("doc! updates existing docstrings", {
  engine <- make_engine()
  env <- engine$env$env

  engine$eval_in_env(engine$read('(import core)')[[1]], env)
  engine$eval_in_env(engine$read('(define fn-with-doc (lambda (x) (+ x 1)))')[[1]], env)

  # First docstring
  engine$eval_in_env(engine$read('(doc! fn-with-doc "Original doc")')[[1]], env)
  result1 <- engine$eval_in_env(engine$read('(doc fn-with-doc)')[[1]], env)
  expect_equal(result1, "Original doc")

  # Update docstring
  engine$eval_in_env(engine$read('(doc! fn-with-doc "Updated doc")')[[1]], env)
  result2 <- engine$eval_in_env(engine$read('(doc fn-with-doc)')[[1]], env)
  expect_equal(result2, "Updated doc")
})

test_that("lambda docstrings are preserved (existing behavior)", {
  engine <- make_engine()
  env <- engine$env$env

  # Lambda with docstring as first expression
  engine$eval_in_env(engine$read('(define documented-lambda (lambda (x) "This is a docstring" (* x 2)))')[[1]], env)

  fn <- engine$eval_in_env(engine$read('documented-lambda')[[1]], env)
  doc_attr <- attr(fn, "rye_doc", exact = TRUE)

  expect_false(is.null(doc_attr))
  expect_equal(doc_attr$description, "This is a docstring")
})

test_that("optimized math functions have docstrings", {
  engine <- make_engine()
  env <- engine$env$env

  engine$eval_in_env(engine$read('(import math)')[[1]], env)
  engine$eval_in_env(engine$read('(import core)')[[1]], env)

  # Test a few optimized functions
  abs_doc <- engine$eval_in_env(engine$read('(doc abs)')[[1]], env)
  expect_match(abs_doc, "absolute value", ignore.case = TRUE)

  sqrt_doc <- engine$eval_in_env(engine$read('(doc sqrt)')[[1]], env)
  expect_match(sqrt_doc, "square root", ignore.case = TRUE)

  sin_doc <- engine$eval_in_env(engine$read('(doc sin)')[[1]], env)
  expect_match(sin_doc, "sine", ignore.case = TRUE)
})

test_that("optimized predicate functions have docstrings", {
  engine <- make_engine()
  env <- engine$env$env

  engine$eval_in_env(engine$read('(import types)')[[1]], env)
  engine$eval_in_env(engine$read('(import core)')[[1]], env)

  symbol_doc <- engine$eval_in_env(engine$read('(doc symbol?)')[[1]], env)
  expect_match(symbol_doc, "symbol", ignore.case = TRUE)

  number_doc <- engine$eval_in_env(engine$read('(doc number?)')[[1]], env)
  expect_match(number_doc, "number", ignore.case = TRUE)
})

test_that("optimized string functions have docstrings", {
  engine <- make_engine()
  env <- engine$env$env

  engine$eval_in_env(engine$read('(import strings)')[[1]], env)
  engine$eval_in_env(engine$read('(import core)')[[1]], env)

  trim_doc <- engine$eval_in_env(engine$read('(doc trim)')[[1]], env)
  expect_match(trim_doc, "trim", ignore.case = TRUE)

  to_string_doc <- engine$eval_in_env(engine$read('(doc ->string)')[[1]], env)
  expect_match(to_string_doc, "string", ignore.case = TRUE)
})
