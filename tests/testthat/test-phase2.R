test_that("quote returns unevaluated expression", {
  result <- rye_eval(rye_read("(quote x)")[[1]])
  expect_true(is.symbol(result))
  expect_equal(as.character(result), "x")

  result <- rye_eval(rye_read("(quote (+ 1 2))")[[1]])
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")
})

test_that("quote sugar works", {
  result <- rye_eval(rye_read("'x")[[1]])
  expect_true(is.symbol(result))
  expect_equal(as.character(result), "x")

  result <- rye_eval(rye_read("'(+ 1 2)")[[1]])
  expect_true(is.call(result))
})

test_that("if evaluates conditionally", {
  result <- rye_eval(rye_read("(if #t 1 2)")[[1]])
  expect_equal(result, 1)

  result <- rye_eval(rye_read("(if #f 1 2)")[[1]])
  expect_equal(result, 2)

  result <- rye_eval(rye_read("(if #nil 1 2)")[[1]])
  expect_equal(result, 2)
})

test_that("if with no else branch returns NULL", {
  result <- rye_eval(rye_read("(if #f 1)")[[1]])
  expect_null(result)
})

test_that("if evaluates test expression", {
  result <- rye_eval(rye_read("(if (> 5 3) 'yes 'no)")[[1]])
  expect_equal(as.character(result), "yes")

  result <- rye_eval(rye_read("(if (< 5 3) 'yes 'no)")[[1]])
  expect_equal(as.character(result), "no")
})

test_that("define creates variables", {
  env <- new.env()
  rye_eval(rye_read("(define x 42)")[[1]], env)
  expect_equal(env$x, 42)

  rye_eval(rye_read("(define y (+ 1 2))")[[1]], env)
  expect_equal(env$y, 3)
})

test_that("lambda creates functions", {
  env <- new.env()
  rye_eval(rye_read("(define add (lambda (a b) (+ a b)))")[[1]], env)
  expect_true(is.function(env$add))

  result <- rye_eval(rye_read("(add 3 4)")[[1]], env)
  expect_equal(result, 7)
})

test_that("lambda with no arguments", {
  env <- new.env()
  rye_eval(rye_read("(define get-five (lambda () 5))")[[1]], env)
  result <- rye_eval(rye_read("(get-five)")[[1]], env)
  expect_equal(result, 5)
})

test_that("lambda with multiple body expressions", {
  env <- new.env()
  rye_eval(rye_read("(define f (lambda (x) (define y 10) (+ x y)))")[[1]], env)
  result <- rye_eval(rye_read("(f 5)")[[1]], env)
  expect_equal(result, 15)
})

test_that("lambda captures environment", {
  env <- new.env()
  rye_eval(rye_read("(define x 10)")[[1]], env)
  rye_eval(rye_read("(define add-x (lambda (y) (+ x y)))")[[1]], env)
  result <- rye_eval(rye_read("(add-x 5)")[[1]], env)
  expect_equal(result, 15)
})

test_that("begin evaluates expressions in sequence", {
  env <- new.env()
  result <- rye_eval(rye_read("(begin (define x 1) (define y 2) (+ x y))")[[1]], env)
  expect_equal(result, 3)
  expect_equal(env$x, 1)
  expect_equal(env$y, 2)
})

test_that("begin returns last expression", {
  result <- rye_eval(rye_read("(begin 1 2 3)")[[1]])
  expect_equal(result, 3)
})

test_that("factorial function works", {
  env <- new.env()

  # Define factorial using recursion
  factorial_def <- "
    (define factorial
      (lambda (n)
        (if (< n 2)
          1
          (* n (factorial (- n 1))))))
  "

  rye_eval(rye_read(factorial_def)[[1]], env)

  # Test factorial
  result <- rye_eval(rye_read("(factorial 5)")[[1]], env)
  expect_equal(result, 120)

  result <- rye_eval(rye_read("(factorial 0)")[[1]], env)
  expect_equal(result, 1)

  result <- rye_eval(rye_read("(factorial 1)")[[1]], env)
  expect_equal(result, 1)

  result <- rye_eval(rye_read("(factorial 10)")[[1]], env)
  expect_equal(result, 3628800)
})
