# Core stdlib functions: identity, values, call-with-values, funcall, r-call,
# delay/force, call-cc

engine <- make_engine()

test_that("stdlib loads successfully", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(exists("car", envir = env))
  expect_true(exists("cdr", envir = env))
  expect_true(exists("map", envir = env))
  expect_true(exists("filter", envir = env))
  expect_true(exists("reduce", envir = env))
})

test_that("force evaluates promises", {
  env <- toplevel_env(engine, new.env())
  forced <- engine$eval(engine$read("(force (delay (+ 1 2)))")[[1]], env = env)
  expect_equal(forced, 3)
})

test_that("force memoizes delayed expressions", {
  env <- toplevel_env(engine, new.env())
  engine$eval(
    engine$read("(begin (define counter 0)\n  (define p (delay (begin (set! counter (+ counter 1)) counter)))\n  (force p)\n  (force p)\n  counter)")[[1]],
    env = env
  )
  expect_equal(get("counter", envir = env), 1)
})

test_that("force returns non-promises unchanged", {
  env <- toplevel_env(engine)
  result <- engine$eval(engine$read("(force 42)")[[1]], env = env)
  expect_equal(result, 42)
})

test_that("call-cc exits to current continuation", {
  env <- toplevel_env(engine)
  result <- engine$eval(
    engine$read("(call-cc (lambda (k) (+ 1 (k 42) 3)))")[[1]],
    env = env
  )
  expect_equal(result, 42)
})

test_that("call-cc is downward-only (R's callCC behavior)", {
  env <- toplevel_env(engine)
  # R's callCC is one-shot and downward-only
  result <- engine$eval(
    engine$read("(call-cc (lambda (k) (k 5)))")[[1]],
    env = env
  )
  expect_equal(result, 5)

  # Test that it works as a regular function
  result2 <- engine$eval(
    engine$read("(call-cc (lambda (exit) (if (> 2 1) (exit 10) 20)))")[[1]],
    env = env
  )
  expect_equal(result2, 10)
})

test_that("call-cc is first-class and has an alias", {
  env <- toplevel_env(engine)
  engine$eval(engine$read("(define cc call-cc)")[[1]], env = env)
  result <- engine$eval(engine$read("(cc (lambda (k) (k 7)))")[[1]], env = env)
  expect_equal(result, 7)
  alias_result <- engine$eval(
    engine$read("(call-with-current-continuation (lambda (k) (k 9)))")[[1]],
    env = env
  )
  expect_equal(alias_result, 9)
})

test_that("call-cc returns receiver's value when continuation is not invoked", {
  env <- toplevel_env(engine)
  result <- engine$eval(
    engine$read("(call-cc (lambda (k) 99))")[[1]],
    env = env
  )
  expect_equal(result, 99)
})

test_that("call-cc can return complex values", {
  env <- toplevel_env(engine)
  result <- engine$eval(
    engine$read("(call-cc (lambda (k) (k (list 1 2 3))))")[[1]],
    env = env
  )
  expect_equal(result, list(1L, 2L, 3L))
})

test_that("call-cc preserves side effects before escape", {
  env <- toplevel_env(engine)
  engine$eval(engine$read("(define x 0)")[[1]], env = env)
  engine$eval(
    engine$read("(call-cc (lambda (k) (set! x 42) (k #nil)))")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("x")[[1]], env = env)
  expect_equal(result, 42)
})

test_that("nested call-cc works correctly", {
  env <- toplevel_env(engine)
  result <- engine$eval(
    engine$read("(call-cc (lambda (outer) (+ 1 (call-cc (lambda (inner) (inner 10))))))")[[1]],
    env = env
  )
  expect_equal(result, 11)

  # Inner continuation escapes outer
  result2 <- engine$eval(
    engine$read("(call-cc (lambda (outer) (+ 1 (call-cc (lambda (inner) (outer 42))))))")[[1]],
    env = env
  )
  expect_equal(result2, 42)
})

test_that("call-cc can simulate early return from nested computation", {
  env <- toplevel_env(engine)
  # Use call-cc to bail out of a deep computation early
  result <- engine$eval(engine$read("
    (call-cc (lambda (return)
      (define a 1)
      (define b 2)
      (when (= (+ a b) 3) (return 'found))
      'not-found))
  ")[[1]], env = env)
  expect_equal(result, as.symbol("found"))

  # Without early return: continuation not invoked, falls through
  result2 <- engine$eval(engine$read("
    (call-cc (lambda (return)
      (define a 1)
      (define b 2)
      (when (= (+ a b) 99) (return 'found))
      'not-found))
  ")[[1]], env = env)
  expect_equal(result2, as.symbol("not-found"))
})

test_that("funcall applies function to list of arguments", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("funcall", envir = env)(sum, list(1, 2, 3)), 6)
})

test_that("values and call-with-values work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(engine$read("(call-with-values (lambda () (values)) (lambda () 42))")[[1]], env = env)
  expect_equal(result, 42)

  result <- engine$eval(engine$read("(call-with-values (lambda () (values 1)) (lambda (x) (+ x 1)))")[[1]], env = env)
  expect_equal(result, 2)

  result <- engine$eval(engine$read("(call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b)))")[[1]], env = env)
  expect_equal(result, 3)

  result <- engine$eval(engine$read("(call-with-values (lambda () 5) (lambda (x) (* x 2)))")[[1]], env = env)
  expect_equal(result, 10)
})

test_that("identity returns its argument", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("identity", envir = env)(42), 42)
  expect_equal(get("identity", envir = env)("hello"), "hello")
  expect_equal(get("identity", envir = env)(list(1, 2, 3)), list(1, 2, 3))
  expect_null(get("identity", envir = env)(NULL))
})

test_that("r-call invokes R functions with arguments", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Call R function by name (string)
  result <- get("r-call", envir = env)("sum", list(1, 2, 3, 4))
  expect_equal(result, 10)

  # Call R function by symbol
  result <- get("r-call", envir = env)(quote(prod), list(2, 3, 4))
  expect_equal(result, 24)

  # Call with single argument
  result <- get("r-call", envir = env)("sqrt", list(16))
  expect_equal(result, 4)

  # Call with no arguments
  result <- get("r-call", envir = env)("ls", list())
  expect_true(is.character(result))
})

test_that("call function converts lists to calls", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Convert list to call
  lst <- list(quote(`+`), 1, 2)
  result <- get("call", envir = env)(lst)
  expect_true(is.call(result))
  expect_equal(result[[1]], quote(`+`))
  expect_equal(result[[2]], 1)
  expect_equal(result[[3]], 2)

  # Already a call should be returned as-is
  call_obj <- engine$read("(+ 1 2)")[[1]]
  expect_equal(
    engine_field(engine, "source_tracker")$strip_src(get("call", envir = env)(call_obj)),
    engine_field(engine, "source_tracker")$strip_src(call_obj)
  )
})

test_that("eval function evaluates Arl expressions", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Eval simple arithmetic
  result <- get("eval", envir = env)(engine$read("(+ 1 2)")[[1]], env = env)
  expect_equal(result, 3)

  # Eval with variables
  env$x <- 10
  result <- get("eval", envir = env)(engine$read("(* x 5)")[[1]], env = env)
  expect_equal(result, 50)

  # Eval function definition and call
  get("eval", envir = env)(engine$read("(define double (lambda (n) (* n 2)))")[[1]], env = env)
  result <- get("eval", envir = env)(engine$read("(double 21)")[[1]], env = env)
  expect_equal(result, 42)
})

test_that("gensym generates unique symbols", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Generate unique symbols
  sym1 <- get("gensym", envir = env)()
  sym2 <- get("gensym", envir = env)()

  expect_true(is.symbol(sym1))
  expect_true(is.symbol(sym2))
  expect_false(identical(sym1, sym2))

  # Custom prefix
  sym_custom <- get("gensym", envir = env)("foo")
  expect_true(is.symbol(sym_custom))
  expect_true(grepl("^foo", as.character(sym_custom)))
})

test_that("gensym avoids shadowing existing bindings", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Generate a symbol and note its counter value
  sym1 <- get("gensym", envir = env)("G")
  n1 <- as.integer(sub("^G__", "", as.character(sym1)))

  # Define a binding using the next expected gensym name
  next_name <- paste0("G__", n1 + 1)
  get("eval", envir = env)(engine$read(paste0("(define ", next_name, " 42)"))[[1]], env = env)

  # gensym should skip the occupied name
  sym2 <- get("gensym", envir = env)("G")
  n2 <- as.integer(sub("^G__", "", as.character(sym2)))
  expect_equal(n2, n1 + 2)

  # The skipped binding should still hold its value
  result <- get("eval", envir = env)(engine$read(paste0("(+ ", next_name, " 0)"))[[1]], env = env)
  expect_equal(result, 42)
})

test_that("read is available as engine builtin without importing io", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Parse expression
  result <- engine$eval(engine$read('(read "(+ 1 2)")')[[1]], env = env)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")

  # Parse symbol
  result <- engine$eval(engine$read('(read "foo")')[[1]], env = env)
  expect_true(is.symbol(result))

  # Parse number
  result <- engine$eval(engine$read('(read "42")')[[1]], env = env)
  expect_equal(result, 42)

  # Empty string returns NULL
  result <- engine$eval(engine$read('(read "")')[[1]], env = env)
  expect_null(result)
})

# ============================================================================
# Coverage: call-with-values error paths
# ============================================================================

test_that("call-with-values errors when producer is not a function", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_error(
    engine$eval(engine$read("(call-with-values 42 +)")[[1]], env = env),
    "expects a function as the producer")
})

test_that("call-with-values errors when consumer is not a function", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_error(
    engine$eval(engine$read("(call-with-values (lambda () 1) 42)")[[1]], env = env),
    "expects a function as the consumer")
})

# ============================================================================
# Coverage: license function
# ============================================================================

# ============================================================================
# Coverage: unbind-variable
# ============================================================================

test_that("unbind-variable removes a binding from current env", {
  env <- new.env()
  toplevel_env(engine, env = env)

  engine$eval(engine$read("(define ub-test 42)")[[1]], env = env)
  expect_equal(get("ub-test", envir = env), 42)

  engine$eval(engine$read('(unbind-variable "ub-test" (current-env))')[[1]], env = env)
  expect_false(exists("ub-test", envir = env, inherits = FALSE))
})

test_that("unbind-variable accepts symbol name", {
  env <- new.env()
  toplevel_env(engine, env = env)

  engine$eval(engine$read("(define ub-sym 99)")[[1]], env = env)
  engine$eval(engine$read("(unbind-variable 'ub-sym (current-env))")[[1]], env = env)
  expect_false(exists("ub-sym", envir = env, inherits = FALSE))
})

test_that("unbind-variable makes variable inaccessible", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(engine$read('
    (begin
      (define ub-gone 10)
      (unbind-variable "ub-gone" (current-env))
      (try-catch ub-gone (catch e "removed")))
  ')[[1]], env = env)
  expect_equal(result, "removed")
})

test_that("unbind-variable warns on nonexistent binding", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_warning(
    engine$eval(engine$read('(unbind-variable "no-such-var" (current-env))')[[1]], env = env),
    "not found"
  )
})

# ============================================================================
# Coverage: license function
# ============================================================================

test_that("license function executes without error", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # license prints output; just verify it doesn't error
  expect_no_error(
    capture.output(engine$eval(engine$read("(license)")[[1]], env = env))
  )
})
