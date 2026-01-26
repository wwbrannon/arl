test_that("rye_translate handles simple definitions", {
  rye_code <- "(define x 42)"
  expected <- "x <- 42"
  expect_equal(rye_translate(rye_code, is_file = FALSE), expected)
})

test_that("rye_translate handles lambda expressions", {
  rye_code <- "(define add (lambda (a b) (+ a b)))"
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_true(grepl("function\\(a, b\\)", result))
  expect_true(grepl("add <-", result))
})

test_that("rye_translate handles if statements", {
  rye_code <- "(if (> x 0) 1 -1)"
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_true(grepl("if", result))
  expect_true(grepl("else", result))
})

test_that("rye_translate handles begin blocks", {
  rye_code <- "(begin (define x 10) (+ x 1))"
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_true(grepl("\\{", result))
  expect_true(grepl("x <- 10", result))
})

test_that("rye_translate handles keywords for named arguments", {
  rye_code <- "(plot x y :main \"Test\" :col \"red\")"
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_true(grepl('main = "Test"', result, fixed = TRUE))
  expect_true(grepl('col = "red"', result, fixed = TRUE))
})

test_that("rye_translate handles package accessors", {
  rye_code <- "(dplyr::filter df)"
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_true(grepl("dplyr::filter", result))
})

test_that("rye_translate marks macros as comments", {
  rye_code <- "(defmacro when (test body) `(if ,test ,body #nil))"
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_true(grepl("# Macro:", result))
  expect_true(grepl("when", result))
})

test_that("rye_translate handles quote", {
  rye_code <- "(quote (a b c))"
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_true(grepl("quote\\(", result))
})

test_that("rye_translate handles multiple expressions", {
  rye_code <- "(define x 10)\n(define y 20)\n(+ x y)"
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_true(grepl("x <- 10", result))
  expect_true(grepl("y <- 20", result))
  expect_true(grepl("\\+", result))
})

test_that("rye_translate handles NULL values", {
  rye_code <- "#nil"
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_equal(result, "NULL")
})

test_that("rye_translate handles strings with escaped quotes", {
  rye_code <- '(define msg "He said \\"hello\\"")'
  result <- rye_translate(rye_code, is_file = FALSE)
  expect_true(grepl('\\\\"', result))
})
