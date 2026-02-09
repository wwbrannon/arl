# Comprehensive control flow tests: when, unless, cond, case, and, or, not, xor, try*

engine <- make_engine()

# ============================================================================
# NEW: Comprehensive control flow macro tests
# ============================================================================

test_that("when evaluates body when test is truthy", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("control"), env)

  # Truthy test
  result <- engine$eval_in_env(engine$read("(when #t 42)")[[1]], env)
  expect_equal(result, 42)

  # Falsy test returns #nil
  result <- engine$eval_in_env(engine$read("(when #f 42)")[[1]], env)
  expect_null(result)

  # Truthy value (non-boolean)
  result <- engine$eval_in_env(engine$read("(when 1 'success)")[[1]], env)
  expect_equal(as.character(result), "success")

  # With side effects
  engine$eval_in_env(engine$read("(define x 0)")[[1]], env)
  engine$eval_in_env(engine$read("(when #t (set! x 10))")[[1]], env)
  expect_equal(env$x, 10)

  # False condition - no side effects
  engine$eval_in_env(engine$read("(set! x 0)")[[1]], env)
  engine$eval_in_env(engine$read("(when #f (set! x 20))")[[1]], env)
  expect_equal(env$x, 0)

  # Multiple body forms
  result <- engine$eval_in_env(
    engine$read("(when #t (define x 5) (+ x 10))")[[1]], env)
  expect_equal(result, 15)
})

test_that("unless evaluates body when test is falsy", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("control"), env)

  # Falsy test
  result <- engine$eval_in_env(engine$read("(unless #f 42)")[[1]], env)
  expect_equal(result, 42)

  # Truthy test returns #nil
  result <- engine$eval_in_env(engine$read("(unless #t 42)")[[1]], env)
  expect_null(result)

  # Falsy value (non-boolean)
  result <- engine$eval_in_env(engine$read("(unless #f 'success)")[[1]], env)
  expect_equal(as.character(result), "success")

  # With side effects
  engine$eval_in_env(engine$read("(define x 0)")[[1]], env)
  engine$eval_in_env(engine$read("(unless #f (set! x 10))")[[1]], env)
  expect_equal(env$x, 10)

  # True condition - no side effects
  engine$eval_in_env(engine$read("(set! x 0)")[[1]], env)
  engine$eval_in_env(engine$read("(unless #t (set! x 20))")[[1]], env)
  expect_equal(env$x, 0)

  # Multiple body forms
  result <- engine$eval_in_env(
    engine$read("(unless #f (define x 5) (+ x 10))")[[1]], env)
  expect_equal(result, 15)
})

test_that("cond selects first matching clause", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("control"), env)

  # First clause matches
  result <- engine$eval_in_env(
    engine$read("(cond (#t 'first) (#t 'second))")[[1]], env)
  expect_equal(as.character(result), "first")

  # Second clause matches
  result <- engine$eval_in_env(
    engine$read("(cond (#f 'first) (#t 'second))")[[1]], env)
  expect_equal(as.character(result), "second")

  # Else clause
  result <- engine$eval_in_env(
    engine$read("(cond (#f 'first) (#f 'second) (else 'third))")[[1]], env)
  expect_equal(as.character(result), "third")

  # With expressions
  result <- engine$eval_in_env(
    engine$read("(cond ((= 1 2) 'first) ((= 2 2) 'second) (else 'third))")[[1]], env)
  expect_equal(as.character(result), "second")

  # No matching clause without else returns #nil
  result <- engine$eval_in_env(engine$read("(cond (#f 'first) (#f 'second))")[[1]], env)
  expect_null(result)

  # Multiple expressions in body
  result <- engine$eval_in_env(
    engine$read("(cond (#t (define x 5) (+ x 10)))")[[1]], env)
  expect_equal(result, 15)
})

test_that("case branches on key equality", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("control"), env)

  # Match first case
  result <- engine$eval_in_env(
    engine$read("(case 1 (1 'one) (2 'two) (3 'three))")[[1]], env)
  expect_equal(as.character(result), "one")

  # Match middle case
  result <- engine$eval_in_env(
    engine$read("(case 2 (1 'one) (2 'two) (3 'three))")[[1]], env)
  expect_equal(as.character(result), "two")

  # Match last case
  result <- engine$eval_in_env(
    engine$read("(case 3 (1 'one) (2 'two) (3 'three))")[[1]], env)
  expect_equal(as.character(result), "three")

  # Else clause
  result <- engine$eval_in_env(
    engine$read("(case 4 (1 'one) (2 'two) (else 'other))")[[1]], env)
  expect_equal(as.character(result), "other")

  # No matching case without else returns #nil
  result <- engine$eval_in_env(engine$read("(case 5 (1 'one) (2 'two))")[[1]], env)
  expect_null(result)

  # Works with symbols
  result <- engine$eval_in_env(
    engine$read("(case 'b ('a 'first) ('b 'second) ('c 'third))")[[1]], env)
  expect_equal(as.character(result), "second")

  # Multiple expressions in body
  result <- engine$eval_in_env(
    engine$read("(case 1 (1 (define x 10) (* x 2)) (2 'two))")[[1]], env)
  expect_equal(result, 20)
})

# ============================================================================
# Existing tests below
# ============================================================================


test_that("and macro works", {
  env <- new.env()

  # Define and macro
  engine$eval_in_env(engine$read("(defmacro and2 (first second) `(if ,first ,second #f))")[[1]], env)

  result <- engine$eval_in_env(engine$read("(and2 #t #t)")[[1]], env)
  expect_true(result)

  result <- engine$eval_in_env(engine$read("(and2 #t #f)")[[1]], env)
  expect_false(result)

  result <- engine$eval_in_env(engine$read("(and2 #f #t)")[[1]], env)
  expect_false(result)
})

test_that("or macro works", {
  env <- new.env()

  # Define or macro
  engine$eval_in_env(engine$read("(defmacro or2 (first second) `(if ,first #t ,second))")[[1]], env)

  result <- engine$eval_in_env(engine$read("(or2 #t #f)")[[1]], env)
  expect_true(result)

  result <- engine$eval_in_env(engine$read("(or2 #f #t)")[[1]], env)
  expect_true(result)

  result <- engine$eval_in_env(engine$read("(or2 #f #f)")[[1]], env)
  expect_false(result)
})

test_that("variadic and/or short-circuit correctly", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("control"), env)

  result <- engine$eval_in_env(engine$read("(and #t 1 2 3)")[[1]], env)
  expect_equal(result, 3)

  result <- engine$eval_in_env(engine$read("(or #f 1 2)")[[1]], env)
  expect_equal(result, 1)

  engine$eval_in_env(engine$read("(define x 0)")[[1]], env)
  result <- engine$eval_in_env(engine$read("(and #f (begin (set! x 1) x))")[[1]], env)
  expect_false(result)
  expect_equal(env$x, 0)

  result <- engine$eval_in_env(engine$read("(or #t (begin (set! x 2) x))")[[1]], env)
  expect_true(result)
  expect_equal(env$x, 0)
})

test_that("not function works", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_false(engine$eval_in_env(engine$read("(not #t)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(not #f)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(not 42)")[[1]], env))
})

test_that("try* with only error handler works", {
  env <- new.env()
  stdlib_env(engine, env)

  # Success case
  result <- env$`try*`(
    function() 42,
    function(e) "error"
  )
  expect_equal(result, 42)

  # Error case
  result <- env$`try*`(
    function() stop("boom"),
    function(e) "caught"
  )
  expect_equal(result, "caught")
})

test_that("try* with only finally handler works", {
  env <- new.env()
  stdlib_env(engine, env)

  # Track whether finally ran
  finally_ran <- FALSE

  # Success case
  result <- env$`try*`(
    function() 42,
    NULL,
    function() finally_ran <<- TRUE
  )
  expect_equal(result, 42)
  expect_true(finally_ran)

  # Error case (finally should run but error should propagate)
  finally_ran <- FALSE
  expect_error({
    env$`try*`(
      function() stop("boom"),
      NULL,
      function() finally_ran <<- TRUE
    )
  })
  expect_true(finally_ran)
})

test_that("try* with both handlers works", {
  env <- new.env()
  stdlib_env(engine, env)

  # Track execution
  finally_ran <- FALSE

  # Error caught and finally runs
  result <- env$`try*`(
    function() stop("boom"),
    function(e) "caught",
    function() finally_ran <<- TRUE
  )
  expect_equal(result, "caught")
  expect_true(finally_ran)

  # Success and finally runs
  finally_ran <- FALSE
  result <- env$`try*`(
    function() 99,
    function(e) "error",
    function() finally_ran <<- TRUE
  )
  expect_equal(result, 99)
  expect_true(finally_ran)
})

# ============================================================================
# Coverage: try* via R-level calls with explicit #f / NULL handlers
# ============================================================================

test_that("try* with no handlers (thunk only)", {
  env <- new.env()
  stdlib_env(engine, env)

  # Just thunk, no error or finally handler
  result <- env$`try*`(function() 99)
  expect_equal(result, 99)
})

test_that("try* errors when thunk is not a function", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_error(env$`try*`(42), "expects a function as first argument")
})

test_that("try* errors when error handler is not a function", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_error(env$`try*`(function() 1, 42), "error handler must be a function")
})

test_that("try* errors when finally handler is not a function", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_error(env$`try*`(function() 1, NULL, 42), "finally handler must be a function")
})
