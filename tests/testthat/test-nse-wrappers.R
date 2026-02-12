engine <- make_engine()

test_that("suppressWarnings suppresses warnings", {
  env <- toplevel_env(engine)

  # Without suppressWarnings, this would generate a warning
  # We capture output to check that no warning appears
  output <- capture.output(
    result <- engine$eval_text("(suppressWarnings (log -1))", env = env),
    type = "message"
  )

  expect_true(is.nan(result))
  expect_length(output, 0)  # No warning messages
})

test_that("suppressWarnings returns the value", {
  env <- toplevel_env(engine)

  result <- engine$eval_text("(suppressWarnings (+ 1 2))", env = env)
  expect_equal(result, 3)
})

test_that("suppressMessages suppresses messages", {
  env <- toplevel_env(engine)

  # Create an R function in global env that generates a message
  test_msg <- function() { message("test message"); 42 }
  assign("test_msg", test_msg, envir = .GlobalEnv)

  # Without suppressMessages, this would print a message
  output <- capture.output(
    result <- engine$eval_text("(suppressMessages (r/call \"test_msg\"))", env = env),
    type = "message"
  )

  expect_equal(result, 42)
  expect_length(output, 0)  # No messages
})

test_that("with evaluates expression in data context", {
  env <- toplevel_env(engine)

  # Create a simple test with a named list (easier than data.frame)
  test_list <- list(x = c(1, 2, 3), y = c(4, 5, 6))
  assign("test_list", test_list, envir = .GlobalEnv)

  # Note: The expression inside 'with' uses R syntax, not Arl syntax
  # R will evaluate '+ x y' which in R means we need proper R syntax
  # But this is being quoted by our macro, so let's test carefully
  result <- engine$eval_text("(with (get \"test_list\") (+ x y))", env = env)

  expect_equal(as.numeric(result), c(5, 7, 9))
})

test_that("within modifies and returns data", {
  env <- toplevel_env(engine)

  # Create a data frame in global env
  test_df <- data.frame(x = 1:3, y = 4:6)
  assign("test_df", test_df, envir = .GlobalEnv)

  # within uses R syntax for assignment, which is <- or =
  # Note: Arl's 'define' won't work in R context, need R's assignment
  result <- engine$eval_text("(within (get \"test_df\") (<- z (+ x y)))", env = env)

  expect_true("z" %in% names(result))
  expect_equal(as.numeric(result$z), c(5, 7, 9))
})

test_that("subset filters rows with NSE condition", {
  env <- toplevel_env(engine)

  # Create a data frame in global env
  test_df <- data.frame(x = 1:5, y = c(10, 20, 30, 40, 50))
  assign("test_df", test_df, envir = .GlobalEnv)

  # Subset where x > 2
  result <- engine$eval_text("(subset (get \"test_df\") (> x 2))", env = env)

  expect_equal(nrow(result), 3)
  expect_equal(as.numeric(result$x), c(3, 4, 5))
})

test_that("subset works with complex conditions", {
  env <- toplevel_env(engine)

  test_df <- data.frame(a = 1:4, b = 5:8)
  assign("test_df2", test_df, envir = .GlobalEnv)

  # Multiple conditions using R's & operator
  result <- engine$eval_text("(subset (get \"test_df2\") (& (> a 1) (< b 8)))", env = env)

  expect_equal(nrow(result), 2)
  expect_equal(as.numeric(result$a), c(2, 3))
})

test_that("transform raises helpful error", {
  env <- toplevel_env(engine)

  expect_error(
    engine$eval_text("(transform)", env = env),
    "not yet supported"
  )
})

test_that("NSE wrappers work with R's base functions correctly", {
  env <- toplevel_env(engine)

  test_df <- data.frame(a = 1:3)
  assign("test_df6", test_df, envir = .GlobalEnv)

  # with should work exactly like R's with
  r_result <- with(test_df, a * 2)
  arl_result <- engine$eval_text("(with (get \"test_df6\") (* a 2))", env = env)

  expect_equal(as.numeric(arl_result), r_result)
})

test_that("NSE wrappers handle nested expressions", {
  env <- toplevel_env(engine)

  test_df <- data.frame(x = 1:3, y = 4:6)
  assign("test_df7", test_df, envir = .GlobalEnv)

  # Nested expression in with
  result <- engine$eval_text(
    "(with (get \"test_df7\") (sqrt (+ (* x x) (* y y))))",
    env = env
  )

  expected <- sqrt((1:3)^2 + (4:6)^2)
  expect_equal(as.numeric(result), expected)
})

test_that("promise-expr extracts expression from delay", {
  env <- toplevel_env(engine)

  # Create a delayed expression
  result <- engine$eval_text("
    (define p (delay (+ 1 2)))
    (promise-expr p)
  ", env = env)

  # Should get the expression, not the value (R format)
  expect_true(is.language(result))
  expect_equal(deparse(result), "1 + 2")
})

test_that("promise-expr errors on non-promise", {
  env <- toplevel_env(engine)

  expect_error(
    engine$eval_text("(promise-expr 42)", env = env),
    "promise-expr requires a promise"
  )
})

test_that("substitute with 1 arg gives helpful error", {
  env <- toplevel_env(engine)

  expect_error(
    engine$eval_text("(substitute 'x)", env = env),
    "doesn't work in Arl functions"
  )
})

test_that("substitute with 2 args performs substitution", {
  env <- toplevel_env(engine)

  result <- engine$eval_text("
    (define expr '(+ x y))
    (define env-vals (dict :x 10))
    (substitute expr env-vals)
  ", env = env)

  # Should have substituted x with 10
  expect_equal(deparse(result), "10 + y")
})

test_that("substitute with wrong number of args errors", {
  env <- toplevel_env(engine)

  expect_error(
    engine$eval_text("(substitute 'a 'b 'c)", env = env),
    "substitute requires 1 or 2 arguments"
  )
})
