# Tests for Engine public API methods.

test_that("define() binds a value visible to Arl code", {
  engine <- make_engine()
  engine$define("my_val", 42)
  expect_equal(engine$eval_text("my_val"), 42)
})

test_that("define() works with complex R objects", {
  engine <- make_engine()
  engine$define("df", mtcars)
  expect_equal(engine$eval_text("(nrow df)"), nrow(mtcars))
  expect_equal(engine$eval_text("(median ($ df \"mpg\"))"), median(mtcars$mpg))
})

test_that("define() returns engine invisibly for chaining", {
  engine <- make_engine()
  result <- engine$define("x", 10)$define("y", 20)$eval_text("(+ x y)")
  expect_equal(result, 30)
})

test_that("define() validates name argument", {
  engine <- make_engine()
  expect_error(engine$define(42, "val"), "non-empty character string")
  expect_error(engine$define("", "val"), "non-empty character string")
  expect_error(engine$define(c("a", "b"), "val"), "non-empty character string")
})

test_that("define() overwrites existing bindings", {
  engine <- make_engine()
  engine$define("x", 1)
  expect_equal(engine$eval_text("x"), 1)
  engine$define("x", 2)
  expect_equal(engine$eval_text("x"), 2)
})

test_that("get_env() provides access to engine bindings", {
  engine <- make_engine()
  engine$eval_text("(define test-binding 99)")
  env <- engine$get_env()
  expect_true(exists("test-binding", envir = env, inherits = FALSE))
  expect_equal(get("test-binding", envir = env), 99)
})
