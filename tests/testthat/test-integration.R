engine <- make_engine()

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

  engine$eval(engine$read(factorial_def)[[1]], env = env)

  # Test factorial
  result <- engine$eval(engine$read("(factorial 5)")[[1]], env = env)
  expect_equal(result, 120)

  result <- engine$eval(engine$read("(factorial 0)")[[1]], env = env)
  expect_equal(result, 1)

  result <- engine$eval(engine$read("(factorial 1)")[[1]], env = env)
  expect_equal(result, 1)

  result <- engine$eval(engine$read("(factorial 10)")[[1]], env = env)
  expect_equal(result, 3628800)
})
