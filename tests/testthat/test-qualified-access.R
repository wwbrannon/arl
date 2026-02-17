# Tests for qualified access via / syntax and module-ref

test_that("parser: math/inc produces (module-ref math inc)", {
  engine <- make_engine()
  result <- engine$read("math/inc")
  expr <- result[[1]]
  expect_true(is.call(expr))
  expect_equal(as.character(expr[[1]]), "module-ref")
  expect_equal(as.character(expr[[2]]), "math")
  expect_equal(as.character(expr[[3]]), "inc")
})

test_that("parser: a/b/c produces nested module-ref", {
  engine <- make_engine()
  result <- engine$read("a/b/c")
  expr <- result[[1]]
  # (module-ref (module-ref a b) c)
  expect_true(is.call(expr))
  expect_equal(as.character(expr[[1]]), "module-ref")
  expect_equal(as.character(expr[[3]]), "c")
  inner <- expr[[2]]
  expect_true(is.call(inner))
  expect_equal(as.character(inner[[1]]), "module-ref")
  expect_equal(as.character(inner[[2]]), "a")
  expect_equal(as.character(inner[[3]]), "b")
})

test_that("parser: bare / stays as division symbol", {
  engine <- make_engine()
  result <- engine$read("(/ 10 2)")
  expr <- result[[1]]
  expect_true(is.call(expr))
  expect_equal(as.character(expr[[1]]), "/")
})

test_that("parser: round-trip math/inc -> write -> math/inc", {
  engine <- make_engine()
  result <- engine$read("math/inc")
  written <- engine$write(result[[1]])
  expect_equal(written, "math/inc")
})

test_that("parser: round-trip a/b/c -> write -> a/b/c", {
  engine <- make_engine()
  result <- engine$read("a/b/c")
  written <- engine$write(result[[1]])
  expect_equal(written, "a/b/c")
})

test_that("parser: R infix operators %/% are not split", {
  engine <- make_engine()
  result <- engine$read("(%/% 10 3)")
  expr <- result[[1]]
  expect_true(is.call(expr))
  expect_equal(as.character(expr[[1]]), "%/%")
})

test_that("qualified access: import :refer then mod/sym resolves", {
  engine <- make_engine()
  engine$eval_text("(import math :refer (inc))")
  result <- engine$eval_text("math/inc")
  expect_true(is.function(result))
})

test_that("qualified access: import :as then alias/sym resolves", {
  engine <- make_engine()
  engine$eval_text("(import math :as m)")
  result <- engine$eval_text("m/inc")
  expect_true(is.function(result))
})

test_that("bare / is still division", {
  engine <- make_engine()
  result <- engine$eval_text("(/ 10 2)")
  expect_equal(result, 5)
})
