# Comprehensive macro expansion and introspection tests

engine <- make_engine()

test_that("macroexpand-1 expands macros one level", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define a simple macro
  engine$eval_in_env(engine$read("(defmacro my-when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Expand once
  expr <- engine$read("(my-when #t 42)")[[1]]
  expanded <- env$`macroexpand-1`(expr)

  # Should be an if expression
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "if")
})

test_that("macroexpand fully expands nested macros", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define nested macros
  engine$eval_in_env(engine$read("(defmacro inner (x) `(* ,x 2))")[[1]], env)
  engine$eval_in_env(engine$read("(defmacro outer (y) `(inner (+ ,y 1)))")[[1]], env)

  # Fully expand
  expr <- engine$read("(outer 5)")[[1]]
  expanded <- env$macroexpand(expr)

  # Should be fully expanded to arithmetic
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "*")
})

test_that("macro? predicate identifies macros", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define a macro
  engine$eval_in_env(engine$read("(defmacro test-macro (x) x)")[[1]], env)

  # Test predicate
  expect_true(env$`macro?`(quote(`test-macro`)))
  expect_false(env$`macro?`(quote(`not-a-macro`)))
  expect_false(env$`macro?`(42))
})
