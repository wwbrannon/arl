# Regression tests for module scoping and macro phase ordering.
# Commit 6dd860b fixed two coupled bugs:
#   1. Module envs parent to builtins_env (not engine_env)
#   2. Module bodies compile/eval interleaved (not batch)
# These tests ensure those fixes don't regress.

test_that("variadic arithmetic operators are present as builtins without stdlib", {
  engine <- Engine$new(load_stdlib = FALSE)
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env

  for (op in c("+", "*", "-", "/")) {
    expect_true(exists(op, envir = builtins_env, inherits = FALSE),
      info = sprintf("%s should be in builtins_env", op))
  }

  # Variadic behavior works without stdlib
  expect_equal(engine$eval_text("(+ 1 2 3)"), 6)
  expect_equal(engine$eval_text("(* 2 3 4)"), 24)
  expect_equal(engine$eval_text("(- 10 3 2)"), 5)
  expect_equal(engine$eval_text("(/ 120 6 2)"), 10)
})

test_that("variadic comparison operators are present as builtins without stdlib", {
  engine <- Engine$new(load_stdlib = FALSE)
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env

  for (op in c("<", "<=", ">", ">=")) {
    expect_true(exists(op, envir = builtins_env, inherits = FALSE),
      info = sprintf("%s should be in builtins_env", op))
  }

  # Chained comparison works without stdlib
  expect_true(engine$eval_text("(< 1 2 3)"))
  expect_false(engine$eval_text("(< 1 3 2)"))
  expect_true(engine$eval_text("(<= 1 1 2)"))
  expect_true(engine$eval_text("(>= 3 2 1)"))
})

test_that("modules cannot use stdlib functions without importing them", {
  engine <- make_engine()
  expect_error(
    engine$eval_text("
      (module __scoping_bad
        (export foo)
        (define foo (lambda (xs) (map car xs))))
      (import __scoping_bad)
      (foo (list (list 1 2) (list 3 4)))"),
    "map|not found|object"
  )
})

test_that("macros from imported modules are available in subsequent module body expressions", {
  engine <- make_engine()
  result <- engine$eval_text("
    (module __scoping_macro
      (export safe-double)
      (import control)
      (define safe-double (lambda (x) (when (> x 0) (* x 2)))))
    (import __scoping_macro)
    (safe-double 5)")
  expect_equal(result, 10)
})

test_that("module env chain is module_env -> builtins_env -> baseenv()", {
  engine <- make_engine()
  engine$eval_text("
    (module __scoping_chain
      (export x)
      (define x 1))")

  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env
  engine_env <- engine$get_env()
  registry <- get(".__module_registry", envir = builtins_env)
  entry <- get("__scoping_chain", envir = registry)

  # module_env -> builtins_env (not engine_env)
  expect_identical(parent.env(entry$env), builtins_env)
  expect_false(identical(parent.env(entry$env), engine_env))

  # builtins_env -> baseenv() (not engine_env — otherwise modules
  # would transitively see all stdlib, defeating import enforcement)
  expect_identical(parent.env(builtins_env), baseenv())
})
