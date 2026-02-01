engine <- RyeEngine$new()

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

test_that("parser handles integer literals", {
  exprs <- engine$read("4L")
  expect_equal(length(exprs), 1)
  expect_equal(as.vector(exprs[[1]]), 4L)
  expect_true(is.integer(exprs[[1]]))
})

test_that("parser handles pure imaginary numbers", {
  exprs <- engine$read("4i")
  expect_equal(length(exprs), 1)
  expect_equal(as.vector(exprs[[1]]), 0+4i)
  expect_true(is.complex(exprs[[1]]))
})

test_that("parser handles full complex number syntax", {
  exprs <- engine$read("2+4i")
  expect_equal(length(exprs), 1)
  expect_equal(as.vector(exprs[[1]]), 2+4i)
  expect_true(is.complex(exprs[[1]]))

  exprs <- engine$read("3.14-2.5i")
  expect_equal(length(exprs), 1)
  expect_equal(as.vector(exprs[[1]]), 3.14-2.5i)
  expect_true(is.complex(exprs[[1]]))

  exprs <- engine$read("-1+2i")
  expect_equal(length(exprs), 1)
  expect_equal(as.vector(exprs[[1]]), -1+2i)
  expect_true(is.complex(exprs[[1]]))
})

test_that("parser handles NA values", {
  exprs <- engine$read("NA")
  expect_equal(length(exprs), 1)
  expect_true(is.na(exprs[[1]]))

  exprs <- engine$read("NA_integer_")
  expect_equal(length(exprs), 1)
  expect_true(is.na(exprs[[1]]))
  expect_equal(typeof(exprs[[1]]), "integer")

  exprs <- engine$read("NA_real_")
  expect_equal(length(exprs), 1)
  expect_true(is.na(exprs[[1]]))
  expect_equal(typeof(exprs[[1]]), "double")
})
