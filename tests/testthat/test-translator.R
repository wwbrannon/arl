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

# Edge Cases for Literals ----

test_that("rye_translate handles negative numbers", {
  result <- rye_translate("(+ -5 10)", is_file = FALSE)
  expect_match(result, "-5")
  expect_match(result, "10")
})

test_that("rye_translate handles decimal numbers", {
  result <- rye_translate("(* 3.14 2)", is_file = FALSE)
  expect_match(result, "3\\.14")
})

test_that("rye_translate handles special numeric values", {
  result <- rye_translate("(list #inf #-inf #nan)", is_file = FALSE)
  expect_match(result, "Inf")
  expect_match(result, "-Inf")
  expect_match(result, "NaN")
})

test_that("rye_translate handles TRUE and FALSE", {
  result <- rye_translate("(if #t #t #f)", is_file = FALSE)
  expect_match(result, "TRUE")
  expect_match(result, "FALSE")
})

test_that("rye_translate handles empty strings", {
  result <- rye_translate('""', is_file = FALSE)
  expect_match(result, '""')
})

# Quasiquote and Unquote ----

test_that("rye_translate handles quasiquote", {
  result <- rye_translate("`(a b c)", is_file = FALSE)
  expect_match(result, "bquote")
})

test_that("rye_translate handles unquote in quasiquote", {
  result <- rye_translate("`(a ,b c)", is_file = FALSE)
  expect_match(result, "\\.")
})

test_that("rye_translate handles unquote-splicing", {
  result <- rye_translate("`(a ,@b c)", is_file = FALSE)
  expect_match(result, "\\.\\(")
})

# Special Forms and Operators ----

test_that("rye_translate handles formula operator", {
  result <- rye_translate("(~ x y)", is_file = FALSE)
  expect_match(result, "~")
})

test_that("rye_translate handles binary operators", {
  result <- rye_translate("(+ 1 2 3)", is_file = FALSE)
  expect_match(result, "1 \\+ 2 \\+ 3")

  result <- rye_translate("(* 2 3 4)", is_file = FALSE)
  expect_match(result, "2 \\* 3 \\* 4")

  result <- rye_translate("(== a b)", is_file = FALSE)
  expect_match(result, "a == b")
})

test_that("rye_translate handles comparison operators", {
  result <- rye_translate("(< x 10)", is_file = FALSE)
  expect_match(result, "x < 10")

  result <- rye_translate("(>= y 0)", is_file = FALSE)
  expect_match(result, "y >= 0")
})

test_that("rye_translate handles logical operators", {
  result <- rye_translate("(&& a b)", is_file = FALSE)
  expect_match(result, "a && b")

  result <- rye_translate("(|| x y)", is_file = FALSE)
  expect_match(result, "x \\|\\| y")
})

test_that("rye_translate handles :: and ::: operators", {
  result <- rye_translate("(pkg:::internal-fn)", is_file = FALSE)
  expect_match(result, ":::")
})

# Lambda Edge Cases ----

test_that("rye_translate handles lambda with no arguments", {
  result <- rye_translate("(lambda () 42)", is_file = FALSE)
  expect_match(result, "function\\(\\)")
})

test_that("rye_translate handles lambda with empty body", {
  result <- rye_translate("(lambda (x))", is_file = FALSE)
  expect_match(result, "function\\(x\\)")
})

test_that("rye_translate handles lambda with multiple body expressions", {
  result <- rye_translate("(lambda (x) (print x) (+ x 1))", is_file = FALSE)
  expect_match(result, "function\\(x\\)")
  expect_match(result, "print")
  expect_match(result, "x \\+ 1")
})

test_that("rye_translate handles lambda defaults", {
  result <- rye_translate("(lambda ((x 10) (y (+ 1 2))) (+ x y))", is_file = FALSE)
  expect_match(result, "function\\(x = 10, y = 1 \\+ 2\\)")
  expect_match(result, "x \\+ y")
})

test_that("rye_translate handles lambda dotted rest args", {
  result <- rye_translate("(lambda (x . rest) (list x rest))", is_file = FALSE)
  expect_match(result, "function\\(x, \\.\\.\\.\\)")
  expect_match(result, "rest <- list\\(\\.\\.\\.\\)")
  expect_match(result, "list\\(x, rest\\)")
})

# Complex Nested Expressions ----

test_that("rye_translate handles deeply nested expressions", {
  code <- "(if (> x 0) (+ x (* 2 y)) (- x y))"
  result <- rye_translate(code, is_file = FALSE)
  expect_match(result, "if")
  expect_match(result, ">")
  expect_match(result, "\\*")
  expect_match(result, "else")
})

test_that("rye_translate preserves infix grouping", {
  result <- rye_translate("(* (+ 1 2) 3)", is_file = FALSE)
  expect_match(result, "\\(1 \\+ 2\\) \\* 3")

  result <- rye_translate("(- (+ 1 2))", is_file = FALSE)
  expect_match(result, "-\\(1 \\+ 2\\)")
})

test_that("rye_translate handles nested begin blocks", {
  code <- "(begin (begin (define x 1) (define y 2)) (+ x y))"
  result <- rye_translate(code, is_file = FALSE)
  expect_match(result, "\\{")
  expect_match(result, "x <- 1")
  expect_match(result, "y <- 2")
})

# File Reading ----

test_that("rye_translate reads from .rye file", {
  temp_file <- tempfile(fileext = ".rye")
  writeLines("(+ 1 2)", temp_file)

  result <- rye_translate(temp_file)
  expect_match(result, "\\+")
  expect_match(result, "1")
  expect_match(result, "2")

  unlink(temp_file)
})

test_that("rye_translate handles file with multiple expressions", {
  temp_file <- tempfile(fileext = ".rye")
  writeLines(c("(define x 10)", "(+ x 5)"), temp_file)

  result <- rye_translate(temp_file)
  expect_match(result, "x <- 10")
  expect_match(result, "x \\+ 5")

  unlink(temp_file)
})

test_that("rye_translate errors on non-existent file", {
  expect_error(
    rye_translate("nonexistent.rye", is_file = TRUE),
    "cannot open"
  )
})

test_that("rye_translate auto-detects .rye extension", {
  temp_file <- tempfile(fileext = ".rye")
  writeLines("(* 2 3)", temp_file)

  result <- rye_translate(temp_file, is_file = NULL)
  expect_match(result, "\\*")

  unlink(temp_file)
})
