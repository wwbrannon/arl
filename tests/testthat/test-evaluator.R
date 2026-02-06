engine <- RyeEngine$new()

test_that("evaluator handles simple arithmetic", {
  expect_equal(engine$eval(engine$read("(+ 1 2)")[[1]]), 3)
  expect_equal(engine$eval(engine$read("(- 5 3)")[[1]]), 2)
  expect_equal(engine$eval(engine$read("(* 4 5)")[[1]]), 20)
  expect_equal(engine$eval(engine$read("(/ 10 2)")[[1]]), 5)
})

test_that("evaluator handles R functions", {
  result <- engine$eval(engine$read("(mean (c 1 2 3 4 5))")[[1]])
  expect_equal(result, 3)
})

test_that("evaluator handles nested calls", {
  result <- engine$eval(engine$read("(+ (* 2 3) (* 4 5))")[[1]])
  expect_equal(result, 26)
})

test_that("evaluator evaluates arguments left-to-right", {
  env <- new.env(parent = baseenv())
  engine$eval_in_env(engine$read("(define x 0)")[[1]], env)
  engine$eval_in_env(engine$read("(define collect (lambda (a b) (list a b)))")[[1]], env)

  result <- engine$eval_in_env(
    engine$read("(collect (begin (set! x (+ x 1)) x) (begin (set! x (+ x 1)) x))")[[1]],
    env
  )

  expect_equal(result, list(1, 2))
  expect_equal(env$x, 2)
})

test_that("evaluator handles :: sugar", {
  result <- engine$eval(engine$read("(base::mean (c 1 2 3))")[[1]])
  expect_equal(result, 2)
})

test_that("calculator with nested expressions", {
  result <- engine$eval(engine$read("(+ 1 (* 2 3))")[[1]])
  expect_equal(result, 7)
})

test_that("evaluator validates special form arity and types", {
  expect_error(engine$eval(engine$read("(quote 1 2)")[[1]]), "quote requires exactly 1 argument")
  expect_error(engine$eval(engine$read("(quasiquote)")[[1]]), "quasiquote requires exactly 1 argument")
  expect_error(engine$eval(engine$read("(if 1)")[[1]]), "if requires 2 or 3 arguments")
  expect_error(engine$eval(engine$read("(define 1 2)")[[1]]), "define requires a symbol")
  expect_error(engine$eval(engine$read("(set! 1 2)")[[1]]), "set! requires a symbol")
})

test_that("evaluator handles set! scoping and missing bindings", {
  env <- new.env(parent = emptyenv())
  env$x <- 1
  child <- new.env(parent = env)

  engine$eval_in_env(engine$read("(set! x 2)")[[1]], child)
  expect_equal(env$x, 2)
  expect_error(engine$eval_in_env(engine$read("(set! y 1)")[[1]], child), "variable 'y' is not defined")
})

test_that("evaluator validates load arguments and missing files", {
  expect_error(engine$eval(engine$read("(load 1)")[[1]]), "load requires a single file path string")
  expect_error(engine$eval(engine$read('(load "missing-file.rye")')[[1]]), "File not found")
})

test_that("evaluator builds formulas without evaluating arguments", {
  env <- new.env(parent = baseenv())
  env$x <- 10
  result <- engine$eval_in_env(engine$read("(~ x y)")[[1]], env)
  expect_s3_class(result, "formula")
  expect_equal(as.character(result)[2], "x")
  expect_equal(as.character(result)[3], "y")
})

test_that("evaluator validates package accessor arguments", {
  expect_error(engine$eval(engine$read("(:: base mean extra)")[[1]]), "requires exactly 2 arguments")
  expect_error(engine$eval(engine$read("(:: 1 mean)")[[1]]), "Package name must be a symbol or string")
  expect_error(engine$eval(engine$read("(:: base 1)")[[1]]), "Function/object name must be a symbol or string")
})

test_that("evaluator validates keyword usage", {
  expect_error(engine$eval(engine$read("(mean :trim)")[[1]]), "requires a value")
})

test_that("evaluator validates lambda argument lists", {
  expect_error(engine$eval(engine$read("(lambda 1 2)")[[1]]), "lambda arguments must be a list")
  expect_error(
    engine$eval(engine$read("(lambda (1) 2)")[[1]]),
    "lambda arguments must be symbols, \\(name default\\) pairs, or \\(pattern <pat> \\[default\\]\\)"
  )
  expect_error(
    engine$eval(engine$read("(lambda (a .) a)")[[1]]),
    "Dotted parameter list must have exactly one parameter after '\\.'"
  )
})

test_that("eval text errors include source and stack context", {
  env <- new.env(parent = baseenv())
  err <- tryCatch(
    engine$eval_text_in_env("(+ 1 nope)", env, source_name = "test.rye"),
    error = function(e) e
  )
  expect_s3_class(err, "rye_error")

  formatted <- engine$source_tracker$format_error(err)
  expect_match(formatted, "test\\.rye:1:1-1:10")
  expect_match(formatted, "R stack:")
  expect_match(formatted, "eval_text")
})

test_that("quote_arg quotes symbols and calls by default", {
  result <- engine$compiled_runtime$quote_arg(as.symbol("x"))
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "quote")
  expect_equal(result[[2]], as.symbol("x"))

  call_result <- engine$compiled_runtime$quote_arg(quote(f(1)))
  expect_true(is.call(call_result))

  literal_result <- engine$compiled_runtime$quote_arg(42)
  expect_equal(literal_result, 42)
})

test_that("quote_arg can skip symbol quoting", {
  result <- engine$compiled_runtime$quote_arg(as.symbol("x"), quote_symbols = FALSE)
  expect_true(is.symbol(result))
  expect_equal(as.character(result), "x")
})

# =============================================================================
# current-env and r/eval (per-engine env stack, no global state)
# =============================================================================

test_that("current-env returns the active evaluation environment", {
  engine$eval(engine$read("(define _ce_test 123)")[[1]])
  curr <- engine$eval(engine$read("(current-env)")[[1]])
  expect_true(is.environment(curr))
  expect_equal(get("_ce_test", envir = curr, inherits = FALSE), 123)
})

test_that("r/eval with no env uses current environment", {
  # + is in the env (from stdlib); r/eval (quote +) should return it
  result <- engine$eval(engine$read("(r/eval (quote +))")[[1]])
  expect_true(is.function(result))
})

test_that("r/eval with no env sees bindings from same evaluation context", {
  # current-env returns the active env (with bindings from previous evals in same engine)
  eng <- RyeEngine$new()
  eng$eval(eng$read("(define _reval_secret 99)")[[1]])
  curr <- eng$eval(eng$read("(current-env)")[[1]])
  expect_equal(get("_reval_secret", envir = curr, inherits = FALSE), 99)
  # r/eval (quote x) looks up x in current env when called in same eval
  result <- eng$eval(eng$read("(r/eval (quote +))")[[1]])
  expect_true(is.function(result))
})

test_that("multiple engines have independent current-env", {
  engine_a <- RyeEngine$new()
  engine_b <- RyeEngine$new()
  engine_a$eval(engine_a$read("(define _eng_x 1)")[[1]])
  engine_b$eval(engine_b$read("(define _eng_x 2)")[[1]])
  curr_a <- engine_a$eval(engine_a$read("(current-env)")[[1]])
  curr_b <- engine_b$eval(engine_b$read("(current-env)")[[1]])
  expect_equal(get("_eng_x", envir = curr_a, inherits = FALSE), 1)
  expect_equal(get("_eng_x", envir = curr_b, inherits = FALSE), 2)
  # r/eval (quote +) works in each engine (each has its own current-env closure)
  result_a <- engine_a$eval(engine_a$read("(r/eval (quote +))")[[1]])
  result_b <- engine_b$eval(engine_b$read("(r/eval (quote +))")[[1]])
  expect_true(is.function(result_a))
  expect_true(is.function(result_b))
})
