# Regression tests for module scoping and macro phase ordering.
# Commit 6dd860b fixed two coupled bugs:
#   1. Module envs parent to builtins_env (not engine_env)
#   2. Module bodies compile/eval interleaved (not batch)
# These tests ensure those fixes don't regress.

test_that("variadic arithmetic operators are present as builtins without stdlib", {
  engine <- Engine$new(load_prelude = FALSE)
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
  engine <- Engine$new(load_prelude = FALSE)
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

test_that("modules cannot use non-prelude stdlib functions without importing them", {
  engine <- make_engine()
  # dict is non-prelude — modules should not have it without explicit import
  expect_error(
    engine$eval_text("
      (module __scoping_bad
        (export foo)
        (define foo (lambda () (dict :a 1))))
      (import __scoping_bad :refer :all)
      (foo)"),
    "dict|not found|object"
  )
})

test_that("macros from imported modules are available in subsequent module body expressions", {
  engine <- make_engine()
  result <- engine$eval_text("
    (module __scoping_macro
      (export safe-double)
      (import control :refer :all)
      (define safe-double (lambda (x) (when (> x 0) (* x 2)))))
    (import __scoping_macro :refer :all)
    (safe-double 5)")
  expect_equal(result, 10)
})

test_that("module env chain is module_env -> prelude_env -> builtins_env -> baseenv()", {
  engine <- make_engine()
  engine$eval_text("
    (module __scoping_chain
      (export x)
      (define x 1))")

  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env
  prelude_env <- engine$.__enclos_env__$private$.compiled_runtime$context$prelude_env
  engine_env <- engine$get_env()
  registry <- get(".__module_registry", envir = builtins_env)
  entry <- get("__scoping_chain", envir = registry)

  # module_env -> prelude_env (not engine_env)
  expect_identical(parent.env(entry$env), prelude_env)
  expect_false(identical(parent.env(entry$env), engine_env))

  # prelude_env -> builtins_env -> default-packages chain -> baseenv()
  expect_identical(parent.env(prelude_env), builtins_env)
  e <- parent.env(builtins_env)
  while (!identical(e, baseenv())) {
    e <- parent.env(e)
  }
  expect_identical(e, baseenv())
})

# =============================================================================
# Environment command helpers
# =============================================================================

test_that("toplevel-env returns engine_env, not builtins_env", {
  engine <- make_engine()
  tl <- engine$eval_text("(toplevel-env)")
  engine_env <- engine$get_env()
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env

  expect_identical(tl, engine_env)
  expect_false(identical(tl, builtins_env))
})

test_that("builtins-env returns builtins_env", {
  engine <- make_engine()
  be <- engine$eval_text("(builtins-env)")
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env
  prelude_env <- engine$.__enclos_env__$private$.compiled_runtime$context$prelude_env
  engine_env <- engine$get_env()

  expect_identical(be, builtins_env)
  expect_false(identical(be, engine_env))
  # engine_env -> prelude_env -> builtins_env
  expect_identical(parent.env(engine_env), prelude_env)
  expect_identical(parent.env(prelude_env), be)
})

test_that("builtins-env is accessible from inside a module", {
  engine <- make_engine()
  result <- engine$eval_text("
    (module __be_test
      (export get-be)
      (define get-be (lambda () (builtins-env))))
    (import __be_test :refer :all)
    (environment? (get-be))")
  expect_true(result)
})

test_that("r-eval works correctly inside a module function", {
  engine <- make_engine()
  # r-eval in a module should see the module's local bindings
  result <- engine$eval_text("
    (module __reval_mod
      (export test-fn)
      (define y 42)
      (define test-fn (lambda () (r-eval (quote y)))))
    (import __reval_mod :refer :all)
    (test-fn)")
  expect_equal(result, 42)
})

# =============================================================================
# Default packages visibility
# =============================================================================

test_that("R default package functions are visible without qualification", {
  engine <- make_engine()
  # One export from each of the 6 default packages:
  # datasets, utils, grDevices, graphics, stats, methods

  # datasets: iris (lazy data)
  expect_true(engine$eval_text("(is.data.frame iris)"))
  # utils: head
  expect_equal(engine$eval_text("(head (c 1 2 3 4 5) 3)"), c(1, 2, 3))
  # grDevices: rgb
  expect_equal(engine$eval_text("(rgb 1 0 0)"), "#FF0000")
  # graphics: xy.coords (a utility that doesn't draw)
  expect_true(engine$eval_text("(is.list (xy.coords (c 1 2 3) (c 4 5 6)))"))
  # stats: median
  expect_equal(engine$eval_text("(median (c 1 2 3 4 5))"), 3)
  # methods: is
  expect_true(engine$eval_text("(is 1 \"numeric\")"))
})

test_that("Arl builtins still shadow R default package functions", {
  engine <- make_engine()
  # Arl's + is variadic (not base R's binary +)
  expect_equal(engine$eval_text("(+ 1 2 3)"), 6)
})

test_that("default-packages chain structure between builtins_env and baseenv()", {
  engine <- make_engine()
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env

  # Walk from builtins_env down to baseenv(), collecting package names
  e <- parent.env(builtins_env)
  seen_names <- character()
  while (!identical(e, baseenv())) {
    nm <- attr(e, "name")
    if (!is.null(nm)) seen_names <- c(seen_names, nm)
    e <- parent.env(e)
  }
  # Should have package: entries for the default packages
  expect_true(length(seen_names) > 0)
  expect_true(any(grepl("^package:", seen_names)))
})

test_that("empty defaultPackages skips the package chain", {
  old <- options(defaultPackages = character(0))
  on.exit(options(old))

  engine <- Engine$new(load_prelude = FALSE, r_packages = getOption("defaultPackages"))
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env

  # builtins_env should parent directly to baseenv() with no packages in between
  expect_identical(parent.env(builtins_env), baseenv())
})

test_that("custom defaultPackages changes which packages are in the chain", {
  old <- options(defaultPackages = c("stats"))
  on.exit(options(old))

  engine <- Engine$new(load_prelude = FALSE, r_packages = getOption("defaultPackages"))
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env

  # Should have exactly one package env (stats) between builtins_env and baseenv()
  e <- parent.env(builtins_env)
  seen_names <- character()
  while (!identical(e, baseenv())) {
    nm <- attr(e, "name")
    if (!is.null(nm)) seen_names <- c(seen_names, nm)
    e <- parent.env(e)
  }
  expect_equal(seen_names, "package:stats")
})

test_that("modules can use default package functions without importing them", {
  engine <- make_engine()
  result <- engine$eval_text("
    (module __dpkg_test
      (export med)
      (define med (lambda (xs) (median xs))))
    (import __dpkg_test :refer :all)
    (med (c 1 2 3 4 5))")
  expect_equal(result, 3)
})

# =============================================================================
# Cross-module macro scoping via value splicing
# =============================================================================

test_that("cross-module macro with private helper function", {
  engine <- make_engine()
  result <- engine$eval_text("
    (module __xmacro_helper
      (export my-double-macro)
      (define private-double (lambda (x) (* x 2)))
      (defmacro my-double-macro (val)
        `(private-double ,val)))
    (import __xmacro_helper :refer :all)
    (my-double-macro 21)")
  expect_equal(result, 42)
})

test_that("cross-module macro with private constant", {
  engine <- make_engine()
  result <- engine$eval_text("
    (module __xmacro_const
      (export scale-macro)
      (define scale-factor 10)
      (defmacro scale-macro (val)
        `(* scale-factor ,val)))
    (import __xmacro_const :refer :all)
    (scale-macro 5)")
  expect_equal(result, 50)
})

test_that("prelude symbols are NOT resolved as refs (they're universally available)", {
  engine <- make_engine()
  # A macro that uses prelude symbols like car, + should NOT create resolved refs
  # because those are available everywhere via the prelude chain
  result <- engine$eval_text("
    (module __xmacro_prelude
      (export inc-macro)
      (defmacro inc-macro (val)
        `(+ ,val 1)))
    (import __xmacro_prelude :refer :all)
    (inc-macro 41)")
  expect_equal(result, 42)
})

test_that("hygiene still works alongside resolved refs", {
  engine <- make_engine()
  # Macro introduces both a local binding (should be gensym'd)
  # and a free reference to a private helper (should be resolved)
  result <- engine$eval_text("
    (module __xmacro_hygiene
      (export safe-compute)
      (define private-transform (lambda (x) (* x 3)))
      (defmacro safe-compute (val)
        (let ((tmp (gensym \"tmp\")))
          `(let ((,tmp ,val))
             (private-transform ,tmp)))))
    (import __xmacro_hygiene :refer :all)
    (define private-transform (lambda (x) (+ x 1)))
    (safe-compute 10)")
  # Should use the module's private-transform (* 3), not the caller's (+ 1)
  expect_equal(result, 30)
})

test_that("nested macros across modules resolve correctly", {
  engine <- make_engine()
  result <- engine$eval_text("
    (module __xmacro_inner
      (export inner-macro)
      (define inner-helper (lambda (x) (+ x 100)))
      (defmacro inner-macro (val)
        `(inner-helper ,val)))
    (module __xmacro_outer
      (export outer-macro)
      (import __xmacro_inner :refer :all)
      (define outer-helper (lambda (x) (* x 2)))
      (defmacro outer-macro (val)
        `(outer-helper (inner-macro ,val))))
    (import __xmacro_outer :refer :all)
    (outer-macro 5)")
  # inner-macro expands: (inner-helper 5) -> 105
  # outer-macro expands: (outer-helper (inner-macro 5)) -> (* 105 2) -> 210
  expect_equal(result, 210)
})

test_that("cross-module macro works with lambda reference", {
  engine <- make_engine()
  # Macro references a private lambda directly (not via define)
  result <- engine$eval_text("
    (module __xmacro_lambda
      (export apply-twice)
      (define do-twice (lambda (f x) (f (f x))))
      (defmacro apply-twice (f val)
        `(do-twice ,f ,val)))
    (import __xmacro_lambda :refer :all)
    (apply-twice (lambda (x) (+ x 1)) 0)")
  expect_equal(result, 2)
})

# =============================================================================
# r_packages parameter
# =============================================================================

test_that("r_packages = NULL exposes only baseenv()", {
  engine <- Engine$new(load_prelude = FALSE, r_packages = NULL)
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env

  # builtins_env should parent directly to baseenv()
  expect_identical(parent.env(builtins_env), baseenv())

  # base functions still work
  expect_equal(engine$eval_text("(+ 1 2)"), 3)
  expect_equal(engine$eval_text("(length (c 1 2 3))"), 3L)
})

test_that("r_packages = c('stats') gives exactly one package env", {
  engine <- Engine$new(load_prelude = FALSE, r_packages = c("stats"))
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env

  # Should have exactly one package env (stats) between builtins_env and baseenv()
  e <- parent.env(builtins_env)
  seen_names <- character()
  while (!identical(e, baseenv())) {
    nm <- attr(e, "name")
    if (!is.null(nm)) seen_names <- c(seen_names, nm)
    e <- parent.env(e)
  }
  expect_equal(seen_names, "package:stats")

  # stats::median is visible
  expect_equal(engine$eval_text("(median (c 1 2 3 4 5))"), 3)
})

test_that("r_packages = 'search_path' picks up current search path", {
  engine <- Engine$new(load_prelude = FALSE, r_packages = "search_path")
  builtins_env <- engine$.__enclos_env__$private$.compiled_runtime$context$builtins_env

  # Should have package envs matching the current search() packages
  e <- parent.env(builtins_env)
  seen_names <- character()
  while (!identical(e, baseenv())) {
    nm <- attr(e, "name")
    if (!is.null(nm)) seen_names <- c(seen_names, nm)
    e <- parent.env(e)
  }
  expect_true(length(seen_names) > 0)
  expect_true(all(grepl("^package:", seen_names)))
})

test_that("search_path mode dynamically tracks library() calls", {
  engine <- Engine$new(load_prelude = FALSE, r_packages = "search_path")

  # tools is not typically in defaultPackages; ensure it's not loaded
  if ("package:tools" %in% search()) {
    skip("tools package already attached")
  }

  # Before loading tools, file_ext should not be found
  expect_error(engine$eval_text("(file_ext \"foo.R\")"), "file_ext|not found|object")

  # Attach tools
  library(tools)
  on.exit(detach("package:tools", unload = FALSE), add = TRUE)

  # Now file_ext should be visible (search path changed, chain rebuilt on eval)
  expect_equal(engine$eval_text("(file_ext \"foo.R\")"), "R")
})
