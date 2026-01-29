engine <- new_engine()

test_that("parser handles simple expressions", {
  exprs <- engine$read("(+ 1 2)")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  expect_equal(as.character(exprs[[1]][[1]]), "+")
})

test_that("parser handles nested expressions", {
  exprs <- engine$read("(+ 1 (* 2 3))")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  expect_true(is.call(exprs[[1]][[3]]))
})

test_that("parser handles quote sugar", {
  exprs <- engine$read("'x")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  expect_equal(as.character(exprs[[1]][[1]]), "quote")
  expect_equal(as.character(exprs[[1]][[2]]), "x")
})

test_that("parser converts :: sugar to function call", {
  exprs <- engine$read("base::mean")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  expect_equal(as.character(exprs[[1]][[1]]), "::")
  expect_equal(as.character(exprs[[1]][[2]]), "base")
  expect_equal(as.character(exprs[[1]][[3]]), "mean")
})

test_that("parser converts ::: sugar to function call", {
  exprs <- engine$read("stats:::fitted.default")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  expect_equal(as.character(exprs[[1]][[1]]), ":::")
  expect_equal(as.character(exprs[[1]][[2]]), "stats")
  expect_equal(as.character(exprs[[1]][[3]]), "fitted.default")
})

test_that(":: can still be used in explicit form", {
  exprs <- engine$read("(:: base mean)")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  expect_equal(as.character(exprs[[1]][[1]]), "::")
})

test_that(":: sugar works in expressions", {
  exprs <- engine$read("(base::mean (c 1 2 3))")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  # First element should be the base::mean call
  expect_true(is.call(exprs[[1]][[1]]))
  expect_equal(as.character(exprs[[1]][[1]][[1]]), "::")
})
