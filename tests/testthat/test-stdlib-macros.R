# Comprehensive macro expansion and introspection tests

engine <- make_engine()

test_that("macroexpand with depth=1 expands macros one level", {
  env <- new.env()
  toplevel_env(engine, env)

  # Define a simple macro
  engine$eval(engine$read("(defmacro my-when (test body) `(if ,test ,body #nil))")[[1]], env = env)

  # Expand once via Arl-level macroexpand with depth
  expr <- engine$read("(my-when #t 42)")[[1]]
  expanded <- env$macroexpand(expr, 1)

  # Should be an if expression
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "if")
})

test_that("macroexpand fully expands nested macros", {
  env <- new.env()
  toplevel_env(engine, env)

  # Define nested macros
  engine$eval(engine$read("(defmacro inner (x) `(* ,x 2))")[[1]], env = env)
  engine$eval(engine$read("(defmacro outer (y) `(inner (+ ,y 1)))")[[1]], env = env)

  # Fully expand
  expr <- engine$read("(outer 5)")[[1]]
  expanded <- env$macroexpand(expr)

  # Should be fully expanded to arithmetic
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "*")
})

test_that("Engine macroexpand with depth=0 returns expr unchanged", {
  env <- new.env()
  toplevel_env(engine, env)

  engine$eval(engine$read("(defmacro my-when (test body) `(if ,test ,body #nil))")[[1]], env = env)

  expr <- engine$read("(my-when #t 42)")[[1]]
  result <- engine$macroexpand(expr, env = env, depth = 0)

  # depth=0 means no expansion - should return the original form

  expect_equal(as.character(result[[1]]), "my-when")
})

test_that("Engine macroexpand with depth=1 expands one layer", {
  env <- new.env()
  toplevel_env(engine, env)

  engine$eval(engine$read("(defmacro inner (x) `(* ,x 2))")[[1]], env = env)
  engine$eval(engine$read("(defmacro outer (y) `(inner (+ ,y 1)))")[[1]], env = env)

  expr <- engine$read("(outer 5)")[[1]]
  result <- engine$macroexpand(expr, env = env, depth = 1)

  # depth=1: outer expands to (inner (+ 5 1)), but inner is NOT expanded
  expect_equal(as.character(result[[1]]), "inner")
})

test_that("Engine macroexpand with depth=2 expands two layers", {
  env <- new.env()
  toplevel_env(engine, env)

  engine$eval(engine$read("(defmacro inner (x) `(* ,x 2))")[[1]], env = env)
  engine$eval(engine$read("(defmacro middle (y) `(inner (+ ,y 1)))")[[1]], env = env)
  engine$eval(engine$read("(defmacro outer (z) `(middle ,z))")[[1]], env = env)

  expr <- engine$read("(outer 5)")[[1]]
  result <- engine$macroexpand(expr, env = env, depth = 2)

  # depth=2: outer → middle → inner, but inner NOT expanded (only 2 steps)
  expect_equal(as.character(result[[1]]), "inner")
})

test_that("Engine macroexpand with depth=NULL fully expands", {
  env <- new.env()
  toplevel_env(engine, env)

  engine$eval(engine$read("(defmacro inner (x) `(* ,x 2))")[[1]], env = env)
  engine$eval(engine$read("(defmacro outer (y) `(inner (+ ,y 1)))")[[1]], env = env)

  expr <- engine$read("(outer 5)")[[1]]
  result <- engine$macroexpand(expr, env = env)

  # Full expansion: outer → inner → (* (+ 5 1) 2)
  expect_equal(as.character(result[[1]]), "*")
})

test_that("Arl-level macroexpand-1 expands one layer", {
  result <- engine$eval_text('
    (defmacro test-when (test body) `(if ,test ,body #nil))
    (macroexpand-1 (quote (test-when #t 42)))
  ')
  # Should be (if #t 42 #nil) - one layer expanded
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "if")
})

test_that("Arl-level macroexpand-all fully expands nested macros", {
  result <- engine$eval_text('
    (defmacro test-inner (x) `(* ,x 2))
    (defmacro test-outer (y) `(test-inner (+ ,y 1)))
    (macroexpand-all (quote (test-outer 5)))
  ')
  # Should be fully expanded to (* ...)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "*")
})

test_that("Arl-level macroexpand with depth parameter", {
  result <- engine$eval_text('
    (defmacro test-inner2 (x) `(* ,x 2))
    (defmacro test-outer2 (y) `(test-inner2 (+ ,y 1)))
    (macroexpand (quote (test-outer2 5)) 1)
  ')
  # depth=1: outer expands, inner does NOT
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "test-inner2")
})

test_that("macro? predicate identifies macros", {
  env <- new.env()
  toplevel_env(engine, env)

  # Define a macro
  engine$eval(engine$read("(defmacro test-macro (x) x)")[[1]], env = env)

  # Test predicate
  expect_true(env$`macro?`(quote(`test-macro`)))
  expect_false(env$`macro?`(quote(`not-a-macro`)))
  expect_false(env$`macro?`(42))
})
