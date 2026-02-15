# Comprehensive format-value system tests

engine <- make_engine()

test_that("format-value handles environments correctly", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Plain environment should format as <environment>
  plain_env <- new.env(hash = TRUE)
  expect_equal(env$`format-value`(plain_env), "<environment>")

  # Environment with class should show class name
  classed_env <- new.env()
  class(classed_env) <- c("MyClass", "environment")
  expect_equal(env$`format-value`(classed_env), "<MyClass, environment>")

  # Dict should still format as values (regression test)
  dict <- env$dict(a = 1, b = 2)
  formatted_dict <- env$`format-value`(dict)
  expect_true(grepl("1", formatted_dict))
  expect_true(grepl("2", formatted_dict))

  # Set should still format as values (regression test)
  set_obj <- env$set(1, 2, 3)
  formatted_set <- env$`format-value`(set_obj)
  expect_true(grepl("[123]", formatted_set))

  # Promise should still format as <promise> (regression test)
  promise_obj <- engine$eval_text("(delay 42)")
  expect_equal(env$`format-value`(promise_obj), "<promise>")

  # R6 class if available
  if (requireNamespace("R6", quietly = TRUE)) {
    r6_class <- R6::R6Class("TestClass")
    expect_true(grepl("R6ClassGenerator", env$`format-value`(r6_class)))
  }
})

test_that("format-value wraps lists and calls in parentheses", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Simple list
  expect_equal(env$`format-value`(list(1, 2, 3)), "(1 2 3)")

  # Nested list
  expect_equal(env$`format-value`(list(1, 2, list(3, 4))), "(1 2 (3 4))")

  # Empty list
  expect_equal(env$`format-value`(list()), "()")

  # List with empty inner list (the flexible function case)
  expect_equal(env$`format-value`(list(1, 10, list())), "(1 10 ())")

  # Call/quote
  expect_equal(env$`format-value`(quote(f(a, b))), "(f a b)")
})

test_that("format-value for dotted pair (arl_cons) shows dotted form", {
  env <- new.env()
  toplevel_env(engine, env = env)
  pair <- engine$read("'(a . b)")[[1]][[2]]
  expect_true(r6_isinstance(pair, "Cons"))
  formatted <- env$`format-value`(pair)
  expect_true(grepl(" \\. ", formatted))
  expect_true(grepl("a", formatted))
  expect_true(grepl("b", formatted))
})

test_that("format-value for improper list shows dotted tail", {
  env <- new.env()
  toplevel_env(engine, env = env)
  improper <- engine$read("'(a b . c)")[[1]][[2]]
  expect_true(r6_isinstance(improper, "Cons"))
  formatted <- env$`format-value`(improper)
  expect_true(grepl(" \\. ", formatted))
  expect_true(grepl("a", formatted))
  expect_true(grepl("b", formatted))
  expect_true(grepl("c", formatted))
})

test_that("format-value displays named lists with names", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Named list with keyword-style keys
  named <- list(a = 1, b = 2)
  expect_equal(env$`format-value`(named), "(:a 1 :b 2)")

  # Named list with string values
  named2 <- list(description = "Double x.", note = "Pure function.")
  expect_equal(env$`format-value`(named2), "(:description Double x. :note Pure function.)")

  # Partially named list - names present so format with names
  partial <- list(a = 1, 2, b = 3)
  formatted <- env$`format-value`(partial)
  expect_true(grepl(":a", formatted))
  expect_true(grepl(":b", formatted))
})

test_that("format-value displays S3 objects compactly", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # lm object (S3 class "lm")
  m <- lm(y ~ x, data = data.frame(x = 1:3, y = c(2, 4, 6)))
  formatted <- env$`format-value`(m)
  # Should be compact R print output (8 lines) or <lm> if too long
  expect_true(grepl("lm|Coefficients", formatted))

  # glm object
  g <- suppressWarnings(glm(y ~ x, data = data.frame(x = 1:3, y = c(0, 1, 1)), family = binomial))
  formatted_g <- env$`format-value`(g)
  expect_true(grepl("glm|Coefficients", formatted_g))
})

test_that("format-value displays call objects from quasiquote as s-expressions", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Call object: quote(+(10, 20)) -> (+ 10 20)
  expect_equal(env$`format-value`(quote(`+`(10, 20))), "(+ 10 20)")

  # quasiquote result via engine
  engine$eval_text("(define x 10)")
  result <- engine$eval_text("(format-value `(+ ,x 20))")
  expect_equal(result, "(+ 10 20)")
})

test_that("format-value displays lists containing symbols with parens", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # List with a symbol
  sym_list <- list(as.symbol("+"), 10, 20)
  expect_equal(env$`format-value`(sym_list), "(+ 10 20)")

  # List with nested structure
  nested <- list(as.symbol("if"), TRUE, 1, 2)
  expect_equal(env$`format-value`(nested), "(if TRUE 1 2)")
})

test_that("format_value fallback warns on format-value error", {
  # Create an env with a broken format-value to test the fallback
  test_env <- new.env(parent = baseenv())
  test_env$`format-value` <- function(x) stop("intentional test error")
  arl_env <- Env$new(env = test_env)

  expect_warning(
    result <- arl_env$format_value(42),
    "format-value failed, using fallback: intentional test error"
  )
  # Fallback should use capture.output(print(x))
  expect_equal(result, "[1] 42")
})
