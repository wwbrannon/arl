# Comprehensive format-value system tests

engine <- make_engine()

test_that("format-value handles environments correctly", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Plain environment should format as <environment>
  plain_env <- new.env(hash = TRUE)
  expect_equal(get("format-value", envir = env)(plain_env), "<environment>")

  # Environment with class should show class name
  classed_env <- new.env()
  class(classed_env) <- c("MyClass", "environment")
  expect_equal(get("format-value", envir = env)(classed_env), "<MyClass, environment>")

  # Dict should still format as values (regression test)
  dict <- get("dict", envir = env)(a = 1, b = 2)
  formatted_dict <- get("format-value", envir = env)(dict)
  expect_true(grepl("1", formatted_dict))
  expect_true(grepl("2", formatted_dict))

  # Set should still format as values (regression test)
  set_obj <- get("set", envir = env)(1, 2, 3)
  formatted_set <- get("format-value", envir = env)(set_obj)
  expect_true(grepl("[123]", formatted_set))

  # Promise should still format as <promise> (regression test)
  promise_obj <- engine$eval_text("(delay 42)")
  expect_equal(get("format-value", envir = env)(promise_obj), "<promise>")

  # R6 class if available
  if (requireNamespace("R6", quietly = TRUE)) {
    r6_class <- R6::R6Class("TestClass")
    expect_true(grepl("R6ClassGenerator", get("format-value", envir = env)(r6_class)))
  }
})

test_that("format-value wraps lists and calls in parentheses", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Simple list
  expect_equal(get("format-value", envir = env)(list(1, 2, 3)), "(1 2 3)")

  # Nested list
  expect_equal(get("format-value", envir = env)(list(1, 2, list(3, 4))), "(1 2 (3 4))")

  # Empty list
  expect_equal(get("format-value", envir = env)(list()), "()")

  # List with empty inner list (the flexible function case)
  expect_equal(get("format-value", envir = env)(list(1, 10, list())), "(1 10 ())")

  # Call/quote
  expect_equal(get("format-value", envir = env)(quote(f(a, b))), "(f a b)")
})

test_that("format-value for dotted pair (arl_cons) shows dotted form", {
  env <- new.env()
  toplevel_env(engine, env = env)
  pair <- engine$read("'(a . b)")[[1]][[2]]
  expect_true(r6_isinstance(pair, "Cons"))
  formatted <- get("format-value", envir = env)(pair)
  expect_true(grepl(" \\. ", formatted))
  expect_true(grepl("a", formatted))
  expect_true(grepl("b", formatted))
})

test_that("format-value for improper list shows dotted tail", {
  env <- new.env()
  toplevel_env(engine, env = env)
  improper <- engine$read("'(a b . c)")[[1]][[2]]
  expect_true(r6_isinstance(improper, "Cons"))
  formatted <- get("format-value", envir = env)(improper)
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
  expect_equal(get("format-value", envir = env)(named), "(:a 1 :b 2)")

  # Named list with string values
  named2 <- list(description = "Double x.", note = "Pure function.")
  expect_equal(get("format-value", envir = env)(named2), '(:description "Double x." :note "Pure function.")')

  # Partially named list - names present so format with names
  partial <- list(a = 1, 2, b = 3)
  formatted <- get("format-value", envir = env)(partial)
  expect_true(grepl(":a", formatted))
  expect_true(grepl(":b", formatted))
})

test_that("format-value displays S3 objects using R print output", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # lm object (S3 class "lm")
  m <- lm(y ~ x, data = data.frame(x = 1:3, y = c(2, 4, 6)))
  formatted <- get("format-value", envir = env)(m)
  # Should show R's print output including Call: and Coefficients
  expect_true(grepl("Call:", formatted))
  expect_true(grepl("Coefficients", formatted))

  # glm object
  g <- suppressWarnings(glm(y ~ x, data = data.frame(x = 1:3, y = c(0, 1, 1)), family = binomial))
  formatted_g <- get("format-value", envir = env)(g)
  expect_true(grepl("Call:", formatted_g))
  expect_true(grepl("Coefficients", formatted_g))
})

test_that("format-value truncates long S3 output with configurable limit", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Create an S3 object with many lines of output
  obj <- structure(as.list(1:50), class = "verbose_obj")
  local({
    print.verbose_obj <<- function(x, ...) {
      for (i in seq_along(x)) cat(sprintf("line %d: %d\n", i, x[[i]]))
    }
  })
  on.exit(rm("print.verbose_obj", envir = globalenv()), add = TRUE)

  # Default limit (20) should truncate and show message
  formatted <- get("format-value", envir = env)(obj)
  expect_true(grepl("output truncated at 20 lines", formatted))
  expect_true(grepl("arl.display.max.lines", formatted))

  # Custom limit
  old <- options(arl.display.max.lines = 5)
  on.exit(options(old), add = TRUE)
  formatted2 <- get("format-value", envir = env)(obj)
  expect_true(grepl("output truncated at 5 lines", formatted2))

  # Inf disables truncation
  options(arl.display.max.lines = Inf)
  formatted3 <- get("format-value", envir = env)(obj)
  expect_false(grepl("truncated", formatted3))
})

test_that("ARL_DISPLAY_MAX_LINES env var controls S3 truncation", {
  env <- new.env()
  toplevel_env(engine, env = env)

  obj <- structure(as.list(1:50), class = "verbose_obj2")
  local({
    print.verbose_obj2 <<- function(x, ...) {
      for (i in seq_along(x)) cat(sprintf("line %d: %d\n", i, x[[i]]))
    }
  })
  on.exit(rm("print.verbose_obj2", envir = globalenv()), add = TRUE)

  # Env var sets limit when R option is not set
  withr::local_options(arl.display.max.lines = NULL)
  withr::local_envvar(ARL_DISPLAY_MAX_LINES = "10")
  formatted <- get("format-value", envir = env)(obj)
  expect_true(grepl("output truncated at 10 lines", formatted))

  # R option takes precedence over env var
  withr::local_options(arl.display.max.lines = 5)
  formatted2 <- get("format-value", envir = env)(obj)
  expect_true(grepl("output truncated at 5 lines", formatted2))

  # Env var Inf disables truncation
  withr::local_options(arl.display.max.lines = NULL)
  withr::local_envvar(ARL_DISPLAY_MAX_LINES = "Inf")
  formatted3 <- get("format-value", envir = env)(obj)
  expect_false(grepl("truncated", formatted3))
})

test_that("format-value displays call objects from quasiquote as s-expressions", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Call object: quote(+(10, 20)) -> (+ 10 20)
  expect_equal(get("format-value", envir = env)(quote(`+`(10, 20))), "(+ 10 20)")

  # quasiquote result via engine (use env with display loaded)
  engine$eval_text("(define x 10)", env = env)
  result <- engine$eval_text("(format-value `(+ ,x 20))", env = env)
  expect_equal(result, "(+ 10 20)")
})

test_that("format-value displays lists containing symbols with parens", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # List with a symbol
  sym_list <- list(as.symbol("+"), 10, 20)
  expect_equal(get("format-value", envir = env)(sym_list), "(+ 10 20)")

  # List with nested structure
  nested <- list(as.symbol("if"), TRUE, 1, 2)
  expect_equal(get("format-value", envir = env)(nested), "(if TRUE 1 2)")
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

test_that("format_value does not evaluate call objects passed as values", {
  # Call with too many args for +: would error if evaluated
  val1 <- quote(`+`(1, 2, 3, 4))
  expect_no_warning(result1 <- engine$format_value(val1))
  expect_equal(result1, "(+ 1 2 3 4)")

  # Call to nonexistent function: would error if evaluated
  val2 <- quote(nonexistent_fn(1, 2))
  expect_no_warning(result2 <- engine$format_value(val2))
  expect_equal(result2, "(nonexistent_fn 1 2)")

  # Nested quasiquote-style call structure
  val3 <- quote(a(quasiquote(b(10))))
  expect_no_warning(result3 <- engine$format_value(val3))
  expect_equal(result3, "(a (quasiquote (b 10)))")
})
