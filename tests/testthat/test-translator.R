engine <- RyeEngine$new()
translator_env <- import_stdlib_modules(engine, "translator", engine$env$env)
translate_source <- translator_env$`translate-source`

translate <- function(source, is_file = NULL) {
  if (is.null(is_file)) {
    translate_source(source, NULL)
  } else {
    translate_source(source, is_file)
  }
}

test_that("translate-source handles simple definitions", {
  rye_code <- "(define x 42)"
  expected <- "x <- 42"
  expect_equal(translate(rye_code, is_file = FALSE), expected)
})

test_that("translate-source handles lambda expressions", {
  rye_code <- "(define add (lambda (a b) (+ a b)))"
  result <- translate(rye_code, is_file = FALSE)
  expect_true(grepl("function\\(a, b\\)", result))
  expect_true(grepl("add <-", result))
})

test_that("translate-source handles if statements", {
  rye_code <- "(if (> x 0) 1 -1)"
  result <- translate(rye_code, is_file = FALSE)
  expect_true(grepl("if", result))
  expect_true(grepl("else", result))
})

test_that("translate-source handles begin blocks", {
  rye_code <- "(begin (define x 10) (+ x 1))"
  result <- translate(rye_code, is_file = FALSE)
  expect_true(grepl("\\{", result))
  expect_true(grepl("x <- 10", result))
})

test_that("translate-source handles keywords for named arguments", {
  rye_code <- "(plot x y :main \"Test\" :col \"red\")"
  result <- translate(rye_code, is_file = FALSE)
  expect_true(grepl('main = "Test"', result, fixed = TRUE))
  expect_true(grepl('col = "red"', result, fixed = TRUE))
})

test_that("translate-source backticks non-syntactic identifiers", {
  result <- translate("(define foo-bar 1)", is_file = FALSE)
  expect_true(grepl("`foo-bar` <- 1", result, fixed = TRUE))

  result <- translate("(plot x y :foo-bar 1)", is_file = FALSE)
  expect_true(grepl("`foo-bar` = 1", result, fixed = TRUE))

  result <- translate("(lambda (foo-bar) foo-bar)", is_file = FALSE)
  expect_true(grepl("function\\(`foo-bar`\\)", result))
  expect_true(grepl("`foo-bar`", result, fixed = TRUE))
})

test_that("translate-source handles package accessors", {
  rye_code <- "(dplyr::filter df)"
  result <- translate(rye_code, is_file = FALSE)
  expect_true(grepl("dplyr::filter", result))
})

test_that("translate-source marks macros as comments", {
  rye_code <- "(defmacro when (test body) `(if ,test ,body #nil))"
  result <- translate(rye_code, is_file = FALSE)
  expect_true(grepl("# Macro:", result))
  expect_true(grepl("when", result))
})

test_that("translate-source handles quote", {
  rye_code <- "(quote (a b c))"
  result <- translate(rye_code, is_file = FALSE)
  expect_true(grepl("quote\\(", result))
})

test_that("translate-source handles multiple expressions", {
  rye_code <- "(define x 10)\n(define y 20)\n(+ x y)"
  result <- translate(rye_code, is_file = FALSE)
  expect_true(grepl("x <- 10", result))
  expect_true(grepl("y <- 20", result))
  expect_true(grepl("\\+", result))
})

test_that("translate-source handles NULL values", {
  rye_code <- "#nil"
  result <- translate(rye_code, is_file = FALSE)
  expect_equal(result, "NULL")
})

test_that("translate-source handles strings with escaped quotes", {
  rye_code <- '(define msg "He said \\"hello\\"")'
  result <- translate(rye_code, is_file = FALSE)
  expect_true(grepl('\\\\"', result))
})

# Edge Cases for Literals ----

test_that("translate-source handles negative numbers", {
  result <- translate("(+ -5 10)", is_file = FALSE)
  expect_match(result, "-5")
  expect_match(result, "10")
})

test_that("translate-source handles decimal numbers", {
  result <- translate("(* 3.14 2)", is_file = FALSE)
  expect_match(result, "3\\.14")
})

test_that("translate-source handles special numeric values", {
  result <- translate("(list #inf #-inf #nan)", is_file = FALSE)
  expect_match(result, "Inf")
  expect_match(result, "-Inf")
  expect_match(result, "NaN")
})

test_that("translate-source handles TRUE and FALSE", {
  result <- translate("(if #t #t #f)", is_file = FALSE)
  expect_match(result, "TRUE")
  expect_match(result, "FALSE")
})

test_that("translate-source handles empty strings", {
  result <- translate('""', is_file = FALSE)
  expect_match(result, '""')
})

# Quasiquote and Unquote ----

test_that("translate-source handles quasiquote", {
  result <- translate("`(a b c)", is_file = FALSE)
  expect_match(result, "bquote")
})

test_that("translate-source handles unquote in quasiquote", {
  result <- translate("`(a ,b c)", is_file = FALSE)
  expect_match(result, "\\.")
})

test_that("translate-source handles unquote-splicing", {
  result <- translate("`(a ,@b c)", is_file = FALSE)
  expect_match(result, "\\.\\(")
})

# Special Forms and Operators ----

test_that("translate-source handles formula operator", {
  result <- translate("(~ x y)", is_file = FALSE)
  expect_match(result, "~")
})

test_that("translate-source handles binary operators", {
  result <- translate("(+ 1 2 3)", is_file = FALSE)
  expect_match(result, "1 \\+ 2 \\+ 3")

  result <- translate("(* 2 3 4)", is_file = FALSE)
  expect_match(result, "2 \\* 3 \\* 4")

  result <- translate("(== a b)", is_file = FALSE)
  expect_match(result, "a == b")
})

test_that("translate-source handles comparison operators", {
  result <- translate("(< x 10)", is_file = FALSE)
  expect_match(result, "x < 10")

  result <- translate("(>= y 0)", is_file = FALSE)
  expect_match(result, "y >= 0")
})

test_that("translate-source handles logical operators", {
  result <- translate("(&& a b)", is_file = FALSE)
  expect_match(result, "a && b")

  result <- translate("(|| x y)", is_file = FALSE)
  expect_match(result, "x \\|\\| y")
})

test_that("translate-source handles :: and ::: operators", {
  result <- translate("(pkg:::internal-fn)", is_file = FALSE)
  expect_match(result, ":::")
})

# Lambda Edge Cases ----

test_that("translate-source handles lambda with no arguments", {
  result <- translate("(lambda () 42)", is_file = FALSE)
  expect_match(result, "function\\(\\)")
})

test_that("translate-source handles lambda with empty body", {
  result <- translate("(lambda (x))", is_file = FALSE)
  expect_match(result, "function\\(x\\)")
})

test_that("translate-source handles lambda with multiple body expressions", {
  result <- translate("(lambda (x) (print x) (+ x 1))", is_file = FALSE)
  expect_match(result, "function\\(x\\)")
  expect_match(result, "print")
  expect_match(result, "x \\+ 1")
})

test_that("translate-source handles lambda defaults", {
  result <- translate("(lambda ((x 10) (y (+ 1 2))) (+ x y))", is_file = FALSE)
  expect_match(result, "function\\(x = 10, y = 1 \\+ 2\\)")
  expect_match(result, "x \\+ y")
})

test_that("translate-source handles lambda dotted rest args", {
  result <- translate("(lambda (x . rest) (list x rest))", is_file = FALSE)
  expect_match(result, "function\\(x, \\.\\.\\.\\)")
  expect_match(result, "rest <- list\\(\\.\\.\\.\\)")
  expect_match(result, "list\\(x, rest\\)")
})

# Complex Nested Expressions ----

test_that("translate-source handles deeply nested expressions", {
  code <- "(if (> x 0) (+ x (* 2 y)) (- x y))"
  result <- translate(code, is_file = FALSE)
  expect_match(result, "if")
  expect_match(result, ">")
  expect_match(result, "\\*")
  expect_match(result, "else")
})

test_that("translate-source preserves infix grouping", {
  result <- translate("(* (+ 1 2) 3)", is_file = FALSE)
  expect_match(result, "\\(1 \\+ 2\\) \\* 3")

  result <- translate("(- (+ 1 2))", is_file = FALSE)
  expect_match(result, "-\\(1 \\+ 2\\)")
})

test_that("translate-source handles nested begin blocks", {
  code <- "(begin (begin (define x 1) (define y 2)) (+ x y))"
  result <- translate(code, is_file = FALSE)
  expect_match(result, "\\{")
  expect_match(result, "x <- 1")
  expect_match(result, "y <- 2")
})

# File Reading ----

test_that("translate-source reads from .rye file", {
  temp_file <- tempfile(fileext = ".rye")
  writeLines("(+ 1 2)", temp_file)

  result <- translate(temp_file)
  expect_match(result, "\\+")
  expect_match(result, "1")
  expect_match(result, "2")

  unlink(temp_file)
})

test_that("translate-source handles file with multiple expressions", {
  temp_file <- tempfile(fileext = ".rye")
  writeLines(c("(define x 10)", "(+ x 5)"), temp_file)

  result <- translate(temp_file)
  expect_match(result, "x <- 10")
  expect_match(result, "x \\+ 5")

  unlink(temp_file)
})

test_that("translate-source errors on non-existent file", {
  expect_error(
    translate("nonexistent.rye", is_file = TRUE),
    "cannot open"
  )
})

test_that("translate-source auto-detects .rye extension", {
  temp_file <- tempfile(fileext = ".rye")
  writeLines("(* 2 3)", temp_file)

  result <- translate(temp_file, is_file = NULL)
  expect_match(result, "\\*")

  unlink(temp_file)
})
