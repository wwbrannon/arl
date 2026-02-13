test_that("doc! macro attaches docstrings to functions", {
  engine <- make_engine()
  env <- engine$get_env()

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
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(define no-doc-fn (lambda (x) x))')[[1]], env)

  result <- engine$eval_in_env(engine$read('(doc no-doc-fn)')[[1]], env)
  expect_null(result)
})

test_that("doc! works with direct assignment functions", {
  engine <- make_engine()
  env <- engine$get_env()

  # Direct assignment from R function
  engine$eval_in_env(engine$read('(define my-abs abs)')[[1]], env)
  engine$eval_in_env(engine$read('(doc! my-abs "My absolute value function")')[[1]], env)

  result <- engine$eval_in_env(engine$read('(doc my-abs)')[[1]], env)
  expect_equal(result, "My absolute value function")
})

test_that("doc! updates existing docstrings", {
  engine <- make_engine()
  env <- engine$get_env()

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

test_that("inline strings in lambda body are not stripped as docstrings", {
  engine <- make_engine()
  env <- engine$get_env()

  # Lambda with a string as first body expression — not treated as docstring
  engine$eval_in_env(engine$read('(define test-fn (lambda (x) "This is just a string" (* x 2)))')[[1]], env)

  fn <- engine$eval_in_env(engine$read('test-fn')[[1]], env)
  doc_attr <- attr(fn, "arl_doc", exact = TRUE)

  # No arl_doc attribute — strings are not treated as docstrings
  expect_null(doc_attr)
  # The function still works (string is evaluated and discarded)
  result <- engine$eval_in_env(engine$read('(test-fn 3)')[[1]], env)
  expect_equal(result, 6)
})

test_that("optimized math functions have docstrings", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(import math)')[[1]], env)

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
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(import types)')[[1]], env)

  symbol_doc <- engine$eval_in_env(engine$read('(doc symbol?)')[[1]], env)
  expect_match(symbol_doc, "symbol", ignore.case = TRUE)

  number_doc <- engine$eval_in_env(engine$read('(doc number?)')[[1]], env)
  expect_match(number_doc, "number", ignore.case = TRUE)
})

test_that("optimized string functions have docstrings", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(import strings)')[[1]], env)

  trim_doc <- engine$eval_in_env(engine$read('(doc trim)')[[1]], env)
  expect_match(trim_doc, "trim", ignore.case = TRUE)

  to_string_doc <- engine$eval_in_env(engine$read('(doc ->string)')[[1]], env)
  expect_match(to_string_doc, "string", ignore.case = TRUE)
})

# --- New tests for expanded doc!/doc API ---

test_that("doc! sets specific fields with keyword args", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(define my-fn (lambda (x) (* x 2)))')[[1]], env)
  engine$eval_in_env(engine$read('(doc! my-fn :examples "(my-fn 3) ; => 6")')[[1]], env)

  result <- engine$eval_in_env(engine$read('(doc my-fn "examples")')[[1]], env)
  expect_equal(result, "(my-fn 3) ; => 6")

  # description should be NULL since we only set examples
  desc <- engine$eval_in_env(engine$read('(doc my-fn)')[[1]], env)
  expect_null(desc)
})

test_that("doc! sets multiple fields with keyword args", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(define my-fn (lambda (x) (* x 2)))')[[1]], env)
  engine$eval_in_env(engine$read('(doc! my-fn :description "Doubles x." :examples "(my-fn 3) ; => 6" :note "Fast.")')[[1]], env)

  expect_equal(engine$eval_in_env(engine$read('(doc my-fn)')[[1]], env), "Doubles x.")
  expect_equal(engine$eval_in_env(engine$read('(doc my-fn "examples")')[[1]], env), "(my-fn 3) ; => 6")
  expect_equal(engine$eval_in_env(engine$read('(doc my-fn "note")')[[1]], env), "Fast.")
})

test_that("doc! merges fields — setting description then examples preserves both", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(define my-fn (lambda (x) (* x 2)))')[[1]], env)

  # Set description first
  engine$eval_in_env(engine$read('(doc! my-fn "Doubles x.")')[[1]], env)
  # Then add examples via keyword
  engine$eval_in_env(engine$read('(doc! my-fn :examples "(my-fn 3) ; => 6")')[[1]], env)

  # Both should be present
  expect_equal(engine$eval_in_env(engine$read('(doc my-fn)')[[1]], env), "Doubles x.")
  expect_equal(engine$eval_in_env(engine$read('(doc my-fn "examples")')[[1]], env), "(my-fn 3) ; => 6")
})

test_that("doc with 'all' returns full named list", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(define my-fn (lambda (x) (* x 2)))')[[1]], env)
  engine$eval_in_env(engine$read('(doc! my-fn :description "Doubles x." :examples "(my-fn 3)")')[[1]], env)

  result <- engine$eval_in_env(engine$read('(doc my-fn "all")')[[1]], env)
  expect_type(result, "list")
  expect_equal(result$description, "Doubles x.")
  expect_equal(result$examples, "(my-fn 3)")
})

test_that("doc returns NULL for non-existent field", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(define my-fn (lambda (x) x))')[[1]], env)
  engine$eval_in_env(engine$read('(doc! my-fn "Some desc.")')[[1]], env)

  result <- engine$eval_in_env(engine$read('(doc my-fn "examples")')[[1]], env)
  expect_null(result)
})

test_that("doc! works on primitives", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(define my-sum sum)')[[1]], env)
  engine$eval_in_env(engine$read('(doc! my-sum :description "Sum values." :note "Wraps primitive.")')[[1]], env)

  expect_equal(engine$eval_in_env(engine$read('(doc my-sum)')[[1]], env), "Sum values.")
  expect_equal(engine$eval_in_env(engine$read('(doc my-sum "note")')[[1]], env), "Wraps primitive.")
  # Function should still work after wrapping
  result <- engine$eval_in_env(engine$read('(my-sum (c 1 2 3))')[[1]], env)
  expect_equal(result, 6)
})

test_that("@internal flag is present in arl_doc at runtime", {
  engine <- make_engine()
  env <- engine$get_env()

  code <- '(module int-test-mod
  (export __int-helper int-pub-fn)

  ;;\' @internal
  ;;\' @description Internal helper.
  (define __int-helper (lambda (x) x))

  ;;\' @description Public function.
  (define int-pub-fn (lambda (x) x))
)'

  child <- new.env(parent = env)
  engine$eval_text(code, env = child)
  engine$eval_text("(import int-test-mod)", env = child)

  helper <- get("__int-helper", envir = child)
  doc_h <- attr(helper, "arl_doc", exact = TRUE)
  expect_true(isTRUE(doc_h$internal))

  pub <- get("int-pub-fn", envir = child)
  doc_p <- attr(pub, "arl_doc", exact = TRUE)
  expect_null(doc_p$internal)
})

test_that("@noeval flag is present in arl_doc at runtime", {
  engine <- make_engine()
  env <- engine$get_env()

  code <- '(module noeval-test-mod
  (export io-fn pure-fn)

  ;;\' @noeval
  ;;\' @description Reads a file.
  ;;\' @examples
  ;;\' (io-fn "test.txt")
  (define io-fn (lambda (path) path))

  ;;\' @description Pure function.
  ;;\' @examples
  ;;\' (pure-fn 42)
  (define pure-fn (lambda (x) x))
)'

  engine$eval_text(code, env = env)
  engine$eval_text("(import noeval-test-mod)", env = env)

  io <- get("io-fn", envir = env)
  doc_io <- attr(io, "arl_doc", exact = TRUE)
  expect_true(isTRUE(doc_io$noeval))

  pure <- get("pure-fn", envir = env)
  doc_pure <- attr(pure, "arl_doc", exact = TRUE)
  expect_null(doc_pure$noeval)
})

test_that("@noeval flag round-trips through doc/doc!", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(define my-fn (lambda (x) x))')[[1]], env)
  engine$eval_in_env(engine$read('(doc! my-fn :noeval #t)')[[1]], env)

  result <- engine$eval_in_env(engine$read('(doc my-fn "noeval")')[[1]], env)
  expect_true(result)
})

test_that("stdlib @noeval functions carry the flag at runtime", {
  engine <- make_engine()
  env <- engine$get_env()

  rf <- get("read-file", envir = env)
  doc <- attr(rf, "arl_doc", exact = TRUE)
  expect_true(isTRUE(doc$noeval))
})

test_that("doc! keyword args evaluate variable values", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_in_env(engine$read('(define my-fn (lambda (x) x))')[[1]], env)
  engine$eval_in_env(engine$read('(define d "computed description")')[[1]], env)
  engine$eval_in_env(engine$read('(doc! my-fn :description d)')[[1]], env)

  result <- engine$eval_in_env(engine$read('(doc my-fn)')[[1]], env)
  expect_equal(result, "computed description")
})
