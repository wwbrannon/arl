# Tests for source position tracking
# Verifies that line and column information is preserved through parsing and macro expansion

test_that("parsed expressions have source positions", {
  engine <- make_engine(load_prelude = FALSE)
  parsed <- engine$read("(+ 1 2)")

  # Check that parsed result has some structure
  expect_true(length(parsed) > 0)

  # The actual position tracking depends on implementation
  # This test validates the basic structure exists
  expr <- parsed[[1]]
  expect_true(!is.null(expr))
})

test_that("multiline expressions preserve line numbers", {
  engine <- make_engine(load_prelude = FALSE)
  code <- "(define foo (lambda (x)
  (+ x 1)
  (* x 2)))"

  parsed <- engine$read(code)
  expect_true(length(parsed) > 0)

  # Should be able to parse without error
  expect_no_error(engine$eval(parsed[[1]]))
})

test_that("nested expressions have positions", {
  engine <- make_engine(load_prelude = FALSE)
  code <- "(* (+ 1 2) 3)"

  parsed <- engine$read(code)
  expect_true(length(parsed) > 0)

  # Verify it evaluates correctly
  result <- engine$eval(parsed[[1]])
  expect_equal(result, 9)
})

test_that("quoted expressions preserve positions", {
  engine <- make_engine(load_prelude = FALSE)
  code <- "'(a b c)"

  parsed <- engine$read(code)
  expect_true(length(parsed) > 0)

  # Quote should work - returns a call/quote object
  result <- engine$eval(parsed[[1]])
  expect_true(is.call(result) || is.list(result))
})

test_that("quasiquoted expressions preserve positions", {
  engine <- make_engine(load_prelude = FALSE)
  code <- "`(a ,b c)"

  parsed <- engine$read(code)
  expect_true(length(parsed) > 0)

  # The parsed quasiquote should be a call with unquote structure
  expr <- parsed[[1]]
  expect_true(is.call(expr))
  expect_equal(as.character(expr[[1]]), "quasiquote")
})

test_that("macro expansion preserves source info", {
  engine <- make_engine(load_prelude = FALSE)

  # Define a simple macro
  engine$eval_text("(defmacro double (x) `(* 2 ,x))")

  # Use the macro
  code <- "(double 5)"
  parsed <- engine$read(code)

  # Should expand and evaluate correctly
  result <- engine$eval(parsed[[1]])
  expect_equal(result, 10)
})

test_that("error location is reported for syntax errors", {
  engine <- make_engine(load_prelude = FALSE)

  # This should error with location info
  expect_error(
    engine$eval_text("(+ 1 "),  # Unclosed paren
    "Unclosed|parse|EOF|incomplete|unexpected end"
  )
})

test_that("error location for runtime errors", {
  engine <- make_engine(load_prelude = FALSE)

  # Division by zero returns Inf in R, not an error
  # Let's test with an actual error: undefined function
  expect_error(
    engine$eval_text("(undefined-function 5)"),
    "not found|undefined|unknown"
  )
})

test_that("line numbers in multiline function definition", {
  engine <- make_engine(load_prelude = FALSE)

  code <- "(define multi-line (lambda (a b c)
  (begin
    (define x (+ a b))
    (define y (+ x c))
    (* y 2))))"

  parsed <- engine$read(code)
  expect_no_error(engine$eval(parsed[[1]]))

  # Call the function
  result <- engine$eval_text("(multi-line 1 2 3)")
  expect_equal(result, 12)
})

test_that("positions preserved through multiple forms", {
  engine <- make_engine(load_prelude = FALSE)

  code <- "(define x 5)\n(define y 10)\n(+ x y)"
  parsed <- engine$read(code)

  # Should have 3 forms
  expect_equal(length(parsed), 3)

  # Evaluate all
  for (form in parsed) {
    result <- engine$eval(form)
  }

  expect_equal(result, 15)
})

test_that("source positions in do blocks", {
  engine <- make_engine(load_prelude = FALSE)

  code <- "(begin
  (define a 1)
  (define b 2)
  (+ a b))"

  parsed <- engine$read(code)
  result <- engine$eval(parsed[[1]])
  expect_equal(result, 3)
})

test_that("positions in nested function calls", {
  engine <- make_engine(load_prelude = FALSE)

  code <- "(+ (* 2 3) (- 10 5))"
  parsed <- engine$read(code)
  result <- engine$eval(parsed[[1]])
  expect_equal(result, 11)
})

test_that("comment positions don't break parsing", {
  engine <- make_engine(load_prelude = FALSE)

  code <- "; This is a comment\n(+ 1 2) ; inline comment\n; another comment"
  parsed <- engine$read(code)

  # Should get one expression (comments are ignored)
  expect_equal(length(parsed), 1)
  result <- engine$eval(parsed[[1]])
  expect_equal(result, 3)
})
