test_that("evaluator handles simple arithmetic", {
  expect_equal(rye_eval(rye_read("(+ 1 2)")[[1]]), 3)
  expect_equal(rye_eval(rye_read("(- 5 3)")[[1]]), 2)
  expect_equal(rye_eval(rye_read("(* 4 5)")[[1]]), 20)
  expect_equal(rye_eval(rye_read("(/ 10 2)")[[1]]), 5)
})

test_that("evaluator handles R functions", {
  result <- rye_eval(rye_read("(mean (c 1 2 3 4 5))")[[1]])
  expect_equal(result, 3)
})

test_that("evaluator handles nested calls", {
  result <- rye_eval(rye_read("(+ (* 2 3) (* 4 5))")[[1]])
  expect_equal(result, 26)
})

test_that("evaluator evaluates arguments left-to-right", {
  env <- new.env(parent = baseenv())
  rye_eval(rye_read("(define x 0)")[[1]], env)
  rye_eval(rye_read("(define collect (lambda (a b) (list a b)))")[[1]], env)

  result <- rye_eval(
    rye_read("(collect (begin (set! x (+ x 1)) x) (begin (set! x (+ x 1)) x))")[[1]],
    env
  )

  expect_equal(result, list(1, 2))
  expect_equal(env$x, 2)
})

test_that("evaluator handles :: sugar", {
  result <- rye_eval(rye_read("(base::mean (c 1 2 3))")[[1]])
  expect_equal(result, 2)
})

test_that("calculator with nested expressions", {
  result <- rye_eval(rye_read("(+ 1 (* 2 3))")[[1]])
  expect_equal(result, 7)
})

test_that("evaluator validates special form arity and types", {
  expect_error(rye_eval(rye_read("(quote 1 2)")[[1]]), "quote requires exactly 1 argument")
  expect_error(rye_eval(rye_read("(quasiquote)")[[1]]), "quasiquote requires exactly 1 argument")
  expect_error(rye_eval(rye_read("(if 1)")[[1]]), "if requires 2 or 3 arguments")
  expect_error(rye_eval(rye_read("(define 1 2)")[[1]]), "define requires a symbol")
  expect_error(rye_eval(rye_read("(set! 1 2)")[[1]]), "set! requires a symbol")
})

test_that("evaluator handles set! scoping and missing bindings", {
  env <- new.env(parent = emptyenv())
  assign(".rye_env", TRUE, envir = env)
  env$x <- 1
  child <- new.env(parent = env)
  assign(".rye_env", TRUE, envir = child)

  rye_eval(rye_read("(set! x 2)")[[1]], child)
  expect_equal(env$x, 2)
  expect_error(rye_eval(rye_read("(set! y 1)")[[1]], child), "variable 'y' is not defined")
})

test_that("evaluator validates load arguments and missing files", {
  expect_error(rye_eval(rye_read("(load 1)")[[1]]), "load requires a single file path string")
  expect_error(rye_eval(rye_read('(load "missing-file.rye")')[[1]]), "File not found")
})

test_that("evaluator builds formulas without evaluating arguments", {
  env <- new.env(parent = baseenv())
  env$x <- 10
  result <- rye_eval(rye_read("(~ x y)")[[1]], env)
  expect_s3_class(result, "formula")
  expect_equal(as.character(result)[2], "x")
  expect_equal(as.character(result)[3], "y")
})

test_that("evaluator validates package accessor arguments", {
  expect_error(rye_eval(rye_read("(:: base mean extra)")[[1]]), "requires exactly 2 arguments")
  expect_error(rye_eval(rye_read("(:: 1 mean)")[[1]]), "Package name must be a symbol or string")
  expect_error(rye_eval(rye_read("(:: base 1)")[[1]]), "Function/object name must be a symbol or string")
})

test_that("evaluator validates keyword usage", {
  expect_error(rye_eval(rye_read("(mean :trim)")[[1]]), "requires a value")
})

test_that("evaluator validates lambda argument lists", {
  expect_error(rye_eval(rye_read("(lambda 1 2)")[[1]]), "lambda arguments must be a list")
  expect_error(
    rye_eval(rye_read("(lambda (1) 2)")[[1]]),
    "lambda arguments must be symbols or \\(name default\\) pairs"
  )
  expect_error(
    rye_eval(rye_read("(lambda (a .) a)")[[1]]),
    "Dotted parameter list must have exactly one parameter after '\\.'"
  )
})

test_that("eval text errors include source and stack context", {
  env <- new.env(parent = baseenv())
  err <- tryCatch(
    rye_eval_text("(+ 1 nope)", env, source_name = "test.rye"),
    error = function(e) e
  )
  expect_s3_class(err, "rye_error")

  formatted <- rye_format_error(err)
  expect_match(formatted, "test\\.rye:1:1-1:10")
  expect_match(formatted, "R stack:")
  expect_match(formatted, "rye_eval")
})
