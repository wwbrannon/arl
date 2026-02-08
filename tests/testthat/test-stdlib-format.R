# Comprehensive format-value system tests

engine <- RyeEngine$new()

test_that("format-value handles environments correctly", {
  env <- new.env()
  stdlib_env(engine, env)

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

test_that("format-value for dotted pair (rye_cons) shows dotted form", {
  env <- new.env()
  stdlib_env(engine, env)
  pair <- engine$read("'(a . b)")[[1]][[2]]
  expect_true(r6_isinstance(pair, "RyeCons"))
  formatted <- env$`format-value`(pair)
  expect_true(grepl(" \\. ", formatted))
  expect_true(grepl("a", formatted))
  expect_true(grepl("b", formatted))
})

test_that("format-value for improper list shows dotted tail", {
  env <- new.env()
  stdlib_env(engine, env)
  improper <- engine$read("'(a b . c)")[[1]][[2]]
  expect_true(r6_isinstance(improper, "RyeCons"))
  formatted <- env$`format-value`(improper)
  expect_true(grepl(" \\. ", formatted))
  expect_true(grepl("a", formatted))
  expect_true(grepl("b", formatted))
  expect_true(grepl("c", formatted))
})
