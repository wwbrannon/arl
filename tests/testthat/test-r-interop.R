engine <- make_engine()

test_that("named arguments work with keywords", {
  env <- new.env()

  # Create a simple test function
  env$test_fn <- function(a, b = 10, c = 20) {
    list(a = a, b = b, c = c)
  }

  # Call with keyword argument
  result <- engine$eval(engine$read("(test_fn 1 :c 30)")[[1]], env = env)

  expect_equal(result$a, 1)
  expect_equal(result$b, 10)  # default value
  expect_equal(result$c, 30)  # keyword argument
})

test_that("multiple keyword arguments", {
  env <- new.env()

  env$test_fn <- function(a = 1, b = 2, c = 3, d = 4) {
    list(a = a, b = b, c = c, d = d)
  }

  result <- engine$eval(engine$read("(test_fn :b 20 :d 40)")[[1]], env = env)

  expect_equal(result$a, 1)   # default
  expect_equal(result$b, 20)  # keyword
  expect_equal(result$c, 3)   # default
  expect_equal(result$d, 40)  # keyword
})

test_that("mixing positional and keyword arguments", {
  env <- new.env()

  env$test_fn <- function(a, b, c = 3) {
    list(a = a, b = b, c = c)
  }

  result <- engine$eval(engine$read("(test_fn 10 20 :c 30)")[[1]], env = env)

  expect_equal(result$a, 10)
  expect_equal(result$b, 20)
  expect_equal(result$c, 30)
})

test_that("dollar operator for list access", {
  env <- new.env()
  env$mylist <- list(x = 10, y = 20)

  result <- engine$eval(engine$read("($ mylist 'x)")[[1]], env = env)
  expect_equal(result, 10)
})

test_that("bracket operator for vector access", {
  env <- new.env()
  env$vec <- c(10, 20, 30, 40)

  result <- engine$eval(engine$read("([ vec 2)")[[1]], env = env)
  expect_equal(result, 20)
})

test_that("double bracket operator for list extraction", {
  env <- new.env()
  env$mylist <- list(a = 1, b = 2, c = 3)

  result <- engine$eval(engine$read('([[ mylist "b")')[[1]], env = env)
  expect_equal(result, 2)
})

test_that("tilde operator for formulas", {
  env <- new.env()
  # Variables need to exist or be quoted for formula
  env$y <- 1:10
  env$x <- 1:10

  result <- engine$eval(engine$read("(~ y x)")[[1]], env = env)

  expect_true(inherits(result, "formula"))
  expect_equal(as.character(result), c("~", "y", "x"))
})

test_that("lm with formula and data argument", {
  env <- new.env()

  # Create test data
  env$df <- data.frame(x = 1:10, y = 2 * (1:10) + rnorm(10))

  # Fit linear model
  result <- engine$eval(engine$read("(lm (~ y x) :data df)")[[1]], env = env)

  expect_true(inherits(result, "lm"))
  expect_equal(length(coef(result)), 2)
})

test_that("R functions with named arguments", {
  # Test seq with named arguments
  result <- engine$eval(engine$read("(seq :from 1 :to 10 :by 2)")[[1]])

  expect_equal(result, seq(from = 1, to = 10, by = 2))
})

test_that("plot with named arguments would work", {
  # Just test that the expression parses and evaluates without error
  # Don't actually create a plot
  env <- new.env()
  env$x <- 1:10
  env$y <- 1:10

  # This should parse without error
  expr <- engine$read("(plot x y :main \"Test Plot\" :col \"red\")")[[1]]
  expect_true(is.call(expr))

  # We won't actually run plot to avoid creating graphics in tests
  # But we can check that the expression structure is correct
  expect_equal(as.character(expr[[1]]), "plot")
})
