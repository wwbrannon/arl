# Tests for first-class module features

test_that("module? predicate works on module envs", {
  engine <- make_engine()
  engine$eval_text("(import math :refer :all)")
  result <- engine$eval_text("(module? math)")
  expect_true(result)
})

test_that("module? returns false for non-modules", {
  engine <- make_engine()
  expect_false(engine$eval_text("(module? 42)"))
  expect_false(engine$eval_text("(module? +)"))
})

test_that("module-exports returns export list", {
  engine <- make_engine()
  engine$eval_text("(import math :refer :all)")
  exports <- engine$eval_text("(module-exports math)")
  expect_true(is.list(exports))
  expect_true("inc" %in% unlist(exports))
})

test_that("module-name returns canonical name", {
  engine <- make_engine()
  engine$eval_text("(import math :refer :all)")
  name <- engine$eval_text("(module-name math)")
  expect_equal(name, "math")
})

test_that("module bindings are locked (immutable from outside)", {
  engine <- make_engine()
  engine$eval_text("(import math :refer :all)")
  mod_env <- engine$eval_text("math")
  expect_true(is.environment(mod_env))
  expect_true(bindingIsLocked("inc", mod_env))
})

test_that("bare import binds module env but does not dump exports", {
  engine <- make_engine()
  engine$eval_text("(import math)")
  # Module env is bound
  expect_true(engine$eval_text("(module? math)"))
  # Qualified access works
  result <- engine$eval_text("(math/inc 5)")
  expect_equal(result, 6)
  # Unqualified access does NOT work
  expect_error(engine$eval_text("(inc 5)"))
})

test_that(":refer :all dumps exports into scope", {
  engine <- make_engine()
  engine$eval_text("(import math :refer :all)")
  # Module env is bound
  expect_true(engine$eval_text("(module? math)"))
  # Unqualified access works
  result <- engine$eval_text("(inc 5)")
  expect_equal(result, 6)
})
