# Core stdlib functions: identity, values, call-with-values, funcall, r/call,
# delay/force, call/cc

engine <- make_engine()

test_that("stdlib loads successfully", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_true(exists("car", envir = env))
  expect_true(exists("cdr", envir = env))
  expect_true(exists("map", envir = env))
  expect_true(exists("filter", envir = env))
  expect_true(exists("reduce", envir = env))
})

test_that("force evaluates promises", {
  env <- stdlib_env(engine, new.env())
  forced <- engine$eval_in_env(engine$read("(force (delay (+ 1 2)))")[[1]], env)
  expect_equal(forced, 3)
})

test_that("force memoizes delayed expressions", {
  env <- stdlib_env(engine, new.env())
  engine$eval_in_env(
    engine$read("(begin (define counter 0)\n  (define p (delay (begin (set! counter (+ counter 1)) counter)))\n  (force p)\n  (force p)\n  counter)")[[1]],
    env
  )
  expect_equal(env$counter, 1)
})

test_that("force returns non-promises unchanged", {
  env <- stdlib_env(engine)
  result <- engine$eval_in_env(engine$read("(force 42)")[[1]], env)
  expect_equal(result, 42)
})

test_that("call/cc exits to current continuation", {
  env <- stdlib_env(engine)
  result <- engine$eval_in_env(
    engine$read("(call/cc (lambda (k) (+ 1 (k 42) 3)))")[[1]],
    env
  )
  expect_equal(result, 42)
})

test_that("call/cc is downward-only (R's callCC behavior)", {
  env <- stdlib_env(engine)
  # R's callCC is one-shot and downward-only
  result <- engine$eval_in_env(
    engine$read("(call/cc (lambda (k) (k 5)))")[[1]],
    env
  )
  expect_equal(result, 5)

  # Test that it works as a regular function
  result2 <- engine$eval_in_env(
    engine$read("(call/cc (lambda (exit) (if (> 2 1) (exit 10) 20)))")[[1]],
    env
  )
  expect_equal(result2, 10)
})

test_that("call/cc is first-class and has an alias", {
  env <- stdlib_env(engine)
  engine$eval_in_env(engine$read("(define cc call/cc)")[[1]], env)
  result <- engine$eval_in_env(engine$read("(cc (lambda (k) (k 7)))")[[1]], env)
  expect_equal(result, 7)
  alias_result <- engine$eval_in_env(
    engine$read("(call-with-current-continuation (lambda (k) (k 9)))")[[1]],
    env
  )
  expect_equal(alias_result, 9)
})

test_that("funcall applies function to list of arguments", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$funcall(sum, list(1, 2, 3)), 6)
})

test_that("values and call-with-values work", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- engine$eval_in_env(engine$read("(call-with-values (lambda () (values)) (lambda () 42))")[[1]], env)
  expect_equal(result, 42)

  result <- engine$eval_in_env(engine$read("(call-with-values (lambda () (values 1)) (lambda (x) (+ x 1)))")[[1]], env)
  expect_equal(result, 2)

  result <- engine$eval_in_env(engine$read("(call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b)))")[[1]], env)
  expect_equal(result, 3)

  result <- engine$eval_in_env(engine$read("(call-with-values (lambda () 5) (lambda (x) (* x 2)))")[[1]], env)
  expect_equal(result, 10)
})

test_that("identity returns its argument", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$identity(42), 42)
  expect_equal(env$identity("hello"), "hello")
  expect_equal(env$identity(list(1, 2, 3)), list(1, 2, 3))
  expect_null(env$identity(NULL))
})

test_that("r/call invokes R functions with arguments", {
  env <- new.env()
  stdlib_env(engine, env)

  # Call R function by name (string)
  result <- env$`r/call`("sum", list(1, 2, 3, 4))
  expect_equal(result, 10)

  # Call R function by symbol
  result <- env$`r/call`(quote(prod), list(2, 3, 4))
  expect_equal(result, 24)

  # Call with single argument
  result <- env$`r/call`("sqrt", list(16))
  expect_equal(result, 4)

  # Call with no arguments
  result <- env$`r/call`("ls", list())
  expect_true(is.character(result))
})

test_that("call function converts lists to calls", {
  env <- new.env()
  stdlib_env(engine, env)

  # Convert list to call
  lst <- list(quote(`+`), 1, 2)
  result <- env$call(lst)
  expect_true(is.call(result))
  expect_equal(result[[1]], quote(`+`))
  expect_equal(result[[2]], 1)
  expect_equal(result[[3]], 2)

  # Already a call should be returned as-is
  call_obj <- engine$read("(+ 1 2)")[[1]]
  expect_equal(
    engine$source_tracker$strip_src(env$call(call_obj)),
    engine$source_tracker$strip_src(call_obj)
  )
})

test_that("eval function evaluates Arl expressions", {
  env <- new.env()
  stdlib_env(engine, env)

  # Eval simple arithmetic
  result <- env$eval(engine$read("(+ 1 2)")[[1]], env)
  expect_equal(result, 3)

  # Eval with variables
  env$x <- 10
  result <- env$eval(engine$read("(* x 5)")[[1]], env)
  expect_equal(result, 50)

  # Eval function definition and call
  env$eval(engine$read("(define double (lambda (n) (* n 2)))")[[1]], env)
  result <- env$eval(engine$read("(double 21)")[[1]], env)
  expect_equal(result, 42)
})

test_that("gensym generates unique symbols", {
  env <- new.env()
  stdlib_env(engine, env)

  # Generate unique symbols
  sym1 <- env$gensym()
  sym2 <- env$gensym()

  expect_true(is.symbol(sym1))
  expect_true(is.symbol(sym2))
  expect_false(identical(sym1, sym2))

  # Custom prefix
  sym_custom <- env$gensym("foo")
  expect_true(is.symbol(sym_custom))
  expect_true(grepl("^foo", as.character(sym_custom)))
})

test_that("read is available as engine builtin without importing io", {
  env <- new.env()
  stdlib_env(engine, env)

  # Parse expression
  result <- engine$eval_in_env(engine$read('(read "(+ 1 2)")')[[1]], env)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")

  # Parse symbol
  result <- engine$eval_in_env(engine$read('(read "foo")')[[1]], env)
  expect_true(is.symbol(result))

  # Parse number
  result <- engine$eval_in_env(engine$read('(read "42")')[[1]], env)
  expect_equal(result, 42)

  # Empty string returns NULL
  result <- engine$eval_in_env(engine$read('(read "")')[[1]], env)
  expect_null(result)
})

# ============================================================================
# Coverage: call-with-values error paths
# ============================================================================

test_that("call-with-values errors when producer is not a function", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_error(
    engine$eval_in_env(engine$read("(call-with-values 42 +)")[[1]], env),
    "expects a function as the producer")
})

test_that("call-with-values errors when consumer is not a function", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_error(
    engine$eval_in_env(engine$read("(call-with-values (lambda () 1) 42)")[[1]], env),
    "expects a function as the consumer")
})

# ============================================================================
# Coverage: license function
# ============================================================================

test_that("license function executes without error", {
  env <- new.env()
  stdlib_env(engine, env)

  # license prints output; just verify it doesn't error
  expect_no_error(
    capture.output(engine$eval_in_env(engine$read("(license)")[[1]], env))
  )
})
