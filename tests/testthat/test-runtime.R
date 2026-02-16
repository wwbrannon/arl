# Tests for CompiledRuntime (R/runtime.R): compiled code execution and helpers

# Helper installation tests
test_that("install_helpers() creates all required helpers", {
  eng <- make_engine()
  test_env <- new.env()
  engine_field(eng, "compiled_runtime")$install_helpers(test_env)

  # Check all documented helpers exist
  expected_helpers <- c(
    ".__env", ".__quote", ".__true_p", ".__assign_pattern",
    ".__help", ".__subscript_call", "quasiquote",
    ".__delay", ".__defmacro", ".__macro_quasiquote",
    ".__module", ".__import", ".__pkg_access"
  )

  for (helper in expected_helpers) {
    expect_true(exists(helper, envir = test_env, inherits = FALSE),
                info = sprintf("Helper %s should exist", helper))
  }
})

test_that("install_helpers() locks all bindings", {
  eng <- make_engine()
  test_env <- new.env()
  engine_field(eng, "compiled_runtime")$install_helpers(test_env)

  helpers <- c(".__env", ".__quote", ".__true_p", "quasiquote")

  for (helper in helpers) {
    expect_true(bindingIsLocked(helper, test_env),
                info = sprintf("Helper %s should be locked", helper))
  }
})

test_that("install_helpers() skips already locked bindings", {
  eng <- make_engine()
  test_env <- new.env()

  # Pre-lock a binding
  test_env$.__module <- TRUE
  lockBinding(".__module", test_env)

  # Should not error
  expect_silent(engine_field(eng, "compiled_runtime")$install_helpers(test_env))

  # Original value preserved
  expect_true(test_env$.__module)
})

test_that("install_helpers() sets arl_doc attributes", {
  eng <- make_engine()
  test_env <- new.env()
  engine_field(eng, "compiled_runtime")$install_helpers(test_env)

  # Check a non-primitive function has arl_doc
  fn <- test_env$.__true_p
  expect_false(is.null(attr(fn, "arl_doc")))
  expect_true("description" %in% names(attr(fn, "arl_doc")))
  expect_true(grepl("INTERNAL:", attr(fn, "arl_doc")$description))
})

test_that(".__true_p helper handles truthiness correctly", {
  eng <- make_engine()
  test_env <- new.env()
  engine_field(eng, "compiled_runtime")$install_helpers(test_env)

  true_p <- test_env$.__true_p

  # FALSE and NULL are falsy
  expect_false(true_p(FALSE))
  expect_false(true_p(NULL))

  # 0 is also falsy (follows R semantics)
  expect_false(true_p(0))

  # Everything else is truthy
  expect_true(true_p(TRUE))
  expect_true(true_p(""))
  expect_true(true_p(list()))
  expect_true(true_p(NA))
})

test_that(".__env helper points to current environment", {
  eng <- make_engine()
  test_env <- new.env()
  engine_field(eng, "compiled_runtime")$install_helpers(test_env)

  expect_identical(test_env$.__env, test_env)
})

test_that(".__quote helper wraps base::quote", {
  eng <- make_engine()
  test_env <- new.env()
  engine_field(eng, "compiled_runtime")$install_helpers(test_env)

  expect_identical(test_env$.__quote, base::quote)
})

# Module compilation tests
test_that("module_compiled() creates and registers module", {
  eng <- make_engine()
  engine_field(eng, "compiled_runtime")$module_compiled(
    "test-mod",
    c("foo"),
    FALSE,
    list(quote(foo <- 42)),
    NULL,
    eng$get_env()
  )

  expect_true(engine_field(eng, "env")$module_registry$exists("test-mod"))
  entry <- engine_field(eng, "env")$module_registry$get("test-mod")
  expect_equal(entry$exports, c("foo"))
})

test_that("module_compiled() evaluates body expressions", {
  eng <- make_engine()
  engine_field(eng, "compiled_runtime")$module_compiled(
    "test-mod",
    c("foo", "bar"),
    FALSE,
    list(quote(foo <- 42), quote(bar <- "test")),
    NULL,
    eng$get_env()
  )

  entry <- engine_field(eng, "env")$module_registry$get("test-mod")
  expect_equal(entry$env$foo, 42)
  expect_equal(entry$env$bar, "test")
})

test_that("module_compiled() handles export_all flag", {
  eng <- make_engine()
  engine_field(eng, "compiled_runtime")$module_compiled(
    "test-mod",
    character(0),
    TRUE,
    list(quote(foo <- 42), quote(bar <- "test"), quote(baz <- 99)),
    NULL,
    eng$get_env()
  )

  entry <- engine_field(eng, "env")$module_registry$get("test-mod")
  exports <- entry$exports

  expect_true("foo" %in% exports)
  expect_true("bar" %in% exports)
  expect_true("baz" %in% exports)
  expect_false(".__module" %in% exports)  # Should be excluded
})

test_that("export-all excludes symbols imported from other modules", {
  eng <- make_engine()

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Provider module
  writeLines(c(
    "(module provider-mod",
    "  (export provided-fn)",
    "  (define provided-fn (lambda () 42)))"
  ), file.path(tmp_dir, "provider-mod.arl"))

  # Consumer module using export-all that imports provider-mod
  writeLines(c(
    "(module consumer",
    "  (export-all)",
    "  (import provider-mod)",
    "  (define own-fn (lambda () (provided-fn))))"
  ), file.path(tmp_dir, "consumer.arl"))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "consumer.arl"))

  entry <- engine_field(eng, "env")$module_registry$get("consumer")
  exports <- entry$exports

  # own-fn should be exported (defined in the module body)
  expect_true("own-fn" %in% exports)
  # provided-fn should NOT be re-exported (came from import)
  expect_false("provided-fn" %in% exports)
})

test_that("module_compiled() marks module environment", {
  eng <- make_engine()
  engine_field(eng, "compiled_runtime")$module_compiled(
    "test-mod",
    c("foo"),
    FALSE,
    list(quote(foo <- 42)),
    NULL,
    eng$get_env()
  )

  entry <- engine_field(eng, "env")$module_registry$get("test-mod")
  expect_true(entry$env$.__module)
  expect_true(bindingIsLocked(".__module", entry$env))
})

test_that("module_compiled() creates path alias when src_file provided", {
  eng <- make_engine()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo) (define foo 42))", tmp_file)
  on.exit(unlink(tmp_file))

  engine_field(eng, "compiled_runtime")$module_compiled(
    "test-mod",
    c("foo"),
    FALSE,
    list(quote(foo <- 42)),
    tmp_file,
    eng$get_env()
  )

  # Should be accessible by both name and path
  abs_path <- arl:::normalize_path_absolute(tmp_file)
  expect_true(engine_field(eng, "env")$module_registry$exists("test-mod"))
  expect_true(engine_field(eng, "env")$module_registry$exists(abs_path))
})

test_that("module_compiled() installs helpers in module environment", {
  eng <- make_engine()
  engine_field(eng, "compiled_runtime")$module_compiled(
    "test-mod",
    c("foo"),
    FALSE,
    list(quote(foo <- 42)),
    NULL,
    eng$get_env()
  )

  entry <- engine_field(eng, "env")$module_registry$get("test-mod")
  mod_env <- entry$env

  # Check that helpers are installed
  expect_true(exists(".__env", envir = mod_env, inherits = FALSE))
  expect_true(exists(".__quote", envir = mod_env, inherits = FALSE))
})

# Import handling tests
test_that("import_compiled() by module name loads stdlib module", {
  eng <- make_engine()
  test_env <- new.env(parent = eng$get_env())

  # Import a simple stdlib module (types is one of the core modules)
  # Module names are passed as symbols in compiled code
  engine_field(eng, "compiled_runtime")$import_compiled(as.symbol("types"), test_env)

  # Check that some exported functions from math are now available
  expect_true(exists("even?", envir = test_env, inherits = FALSE))
  expect_true(exists("odd?", envir = test_env, inherits = FALSE))
  expect_true(is.function(test_env$`even?`))
})

test_that("import_compiled() by module name as symbol", {
  eng <- make_engine()
  test_env <- new.env(parent = eng$get_env())

  # Import using a symbol (which is how compiled code calls it)
  module_name_sym <- as.symbol("display")
  engine_field(eng, "compiled_runtime")$import_compiled(module_name_sym, test_env)

  # Check that some exported functions from display are now available
  expect_true(exists("str", envir = test_env, inherits = FALSE))
  expect_true(is.function(test_env$str))
})

test_that("import_compiled() errors on missing module", {
  eng <- make_engine()
  test_env <- new.env(parent = eng$get_env())

  expect_error(
    engine_field(eng, "compiled_runtime")$import_compiled("nonexistent-module-xyz", test_env),
    "Module not found"
  )
})

test_that("import_compiled() loads module only once", {
  eng <- make_engine()
  test_env1 <- new.env(parent = eng$get_env())
  test_env2 <- new.env(parent = eng$get_env())

  # Import the same module twice into different environments (using symbols)
  engine_field(eng, "compiled_runtime")$import_compiled(as.symbol("functional"), test_env1)
  engine_field(eng, "compiled_runtime")$import_compiled(as.symbol("functional"), test_env2)

  # Both should get the same module (same function objects)
  expect_true(exists("map", envir = test_env1, inherits = FALSE))
  expect_true(exists("map", envir = test_env2, inherits = FALSE))

  # The functions should be identical (same object from the shared registry)
  expect_identical(test_env1$map, test_env2$map)
})

test_that("import_compiled() by path loads and attaches exports", {
  eng <- make_engine()

  # Create a temporary .arl file with a simple module
  tmp_file <- tempfile(fileext = ".arl")
  writeLines(c(
    "(module test-import",
    "  (export test-value)",
    "  (define test-value 123))"
  ), tmp_file)
  on.exit(unlink(tmp_file))

  # Import using absolute path (strings are treated as paths by import_compiled)
  test_env <- new.env(parent = eng$get_env())
  engine_field(eng, "compiled_runtime")$import_compiled(tmp_file, test_env)

  # Check that the exported value is now in test_env
  expect_true(exists("test-value", envir = test_env, inherits = FALSE))
  expect_equal(test_env$`test-value`, 123)
})

test_that("import_compiled() attaches exports to target environment", {
  eng <- make_engine()
  test_env <- new.env(parent = eng$get_env())

  # Before import, the environment should be empty
  expect_equal(length(ls(test_env, all.names = TRUE)), 0)

  # Import a module (using symbol)
  engine_field(eng, "compiled_runtime")$import_compiled(as.symbol("types"), test_env)

  # After import, exported functions should be in the environment
  exports <- ls(test_env, all.names = TRUE)
  expect_true(length(exports) > 0)

  # Check specific exports from types module
  expect_true(exists("number?", envir = test_env, inherits = FALSE))
  expect_true(exists("string?", envir = test_env, inherits = FALSE))
  expect_true(exists("list?", envir = test_env, inherits = FALSE))

  # Verify these are actually functions
  expect_true(is.function(test_env$`number?`))
  expect_true(is.function(test_env$`string?`))
  expect_true(is.function(test_env$`list?`))
})

# Package access tests
test_that("pkg_access_compiled() with :: gets exported value", {
  eng <- make_engine()

  result <- engine_field(eng, "compiled_runtime")$pkg_access_compiled("::", "base", "identity", eng$get_env())

  expect_identical(result, base::identity)
})

test_that("pkg_access_compiled() with ::: gets internal value", {
  eng <- make_engine()

  # Get an internal function from base (example: .deparseOpts)
  result <- engine_field(eng, "compiled_runtime")$pkg_access_compiled(":::", "base", ".deparseOpts", eng$get_env())

  expect_true(is.function(result))
})

test_that("pkg_access_compiled() errors on invalid operator", {
  eng <- make_engine()

  expect_error(
    engine_field(eng, "compiled_runtime")$pkg_access_compiled(":::", "base", "identity", eng$get_env()),
    NA  # Should not error for ::: (it's valid)
  )

  expect_error(
    engine_field(eng, "compiled_runtime")$pkg_access_compiled("unknown", "base", "identity", eng$get_env()),
    "Unknown package access operator"
  )
})

test_that("pkg_access_compiled() requires string arguments", {
  eng <- make_engine()

  expect_error(
    engine_field(eng, "compiled_runtime")$pkg_access_compiled("::", 123, "identity", eng$get_env()),
    "must be length-1 character"
  )

  expect_error(
    engine_field(eng, "compiled_runtime")$pkg_access_compiled("::", "base", 123, eng$get_env()),
    "must be length-1 character"
  )
})

# Quasiquote tests
test_that("quasiquote_compiled() returns simple values unchanged", {
  eng <- make_engine()

  expect_equal(engine_field(eng, "compiled_runtime")$quasiquote_compiled(42, eng$get_env()), 42)
  expect_equal(engine_field(eng, "compiled_runtime")$quasiquote_compiled("test", eng$get_env()), "test")
  expect_equal(engine_field(eng, "compiled_runtime")$quasiquote_compiled(TRUE, eng$get_env()), TRUE)
})

test_that("quasiquote_compiled() handles unquote", {
  eng <- make_engine()
  env <- eng$get_env()
  env$x <- 42

  expr <- as.call(list(as.symbol("list"), as.call(list(as.symbol("unquote"), as.symbol("x")))))
  result <- engine_field(eng, "compiled_runtime")$quasiquote_compiled(expr, eng$get_env())

  # Result should be (list 42)
  expect_true(is.call(result))
  expect_equal(result[[2]], 42)
})

test_that("quasiquote_compiled() handles unquote-splicing", {
  eng <- make_engine()
  env <- eng$get_env()
  env$lst <- list(1, 2, 3)

  expr <- as.call(list(
    as.symbol("list"),
    as.call(list(as.symbol("unquote-splicing"), as.symbol("lst")))
  ))
  result <- engine_field(eng, "compiled_runtime")$quasiquote_compiled(expr, eng$get_env())

  # Result should be (list 1 2 3)
  expect_true(is.call(result))
  expect_equal(length(result), 4)  # list + 3 elements
  expect_equal(result[[2]], 1)
  expect_equal(result[[3]], 2)
  expect_equal(result[[4]], 3)
})

test_that("quasiquote_compiled() handles nested quasiquote", {
  eng <- make_engine()

  expr <- as.call(list(
    as.symbol("quasiquote"),
    as.call(list(as.symbol("unquote"), quote(x)))
  ))
  result <- engine_field(eng, "compiled_runtime")$quasiquote_compiled(expr, eng$get_env())

  # Nested quasiquote increases depth, so unquote is not evaluated
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "quasiquote")
})

test_that("quasiquote_compiled() errors on misplaced unquote-splicing", {
  eng <- make_engine()

  # unquote-splicing not in list context should error
  expr <- as.call(list(as.symbol("unquote-splicing"), as.symbol("x")))

  expect_error(
    engine_field(eng, "compiled_runtime")$quasiquote_compiled(expr, eng$get_env()),
    "can only appear in list context"
  )
})

test_that("quasiquote_compiled() requires exactly one argument", {
  eng <- make_engine()

  # quasiquote with wrong number of args
  expr <- as.call(list(as.symbol("quasiquote")))
  expect_error(
    engine_field(eng, "compiled_runtime")$quasiquote_compiled(expr, eng$get_env()),
    "requires exactly 1 argument"
  )
})

test_that("quasiquote_compiled() preserves quoted expressions", {
  eng <- make_engine()

  expr <- quote(quote(foo))
  result <- engine_field(eng, "compiled_runtime")$quasiquote_compiled(expr, eng$get_env())

  # Quoted expressions should pass through unchanged
  expect_equal(result, expr)
})

# Macro definition tests
test_that("defmacro_compiled() creates macro in macro registry", {
  eng <- make_engine()
  test_env <- eng$get_env()

  engine_field(eng, "compiled_runtime")$defmacro_compiled(
    "test-macro",
    list(as.symbol("x")),
    quote(x),
    "Test macro",
    test_env
  )

  macro_registry <- engine_field(eng, "env")$macro_registry_env(test_env, create = FALSE)
  expect_true(exists("test-macro", envir = macro_registry, inherits = FALSE))
})

test_that("defmacro_compiled() handles begin body", {
  eng <- make_engine()
  test_env <- eng$get_env()

  body <- as.call(list(as.symbol("begin"), quote(x), quote(y)))

  engine_field(eng, "compiled_runtime")$defmacro_compiled(
    "test-macro",
    list(as.symbol("x"), as.symbol("y")),
    body,
    NULL,
    test_env
  )

  macro_registry <- engine_field(eng, "env")$macro_registry_env(test_env, create = FALSE)
  expect_true(exists("test-macro", envir = macro_registry, inherits = FALSE))
})

test_that("defmacro_compiled() handles non-begin body", {
  eng <- make_engine()
  test_env <- eng$get_env()

  engine_field(eng, "compiled_runtime")$defmacro_compiled(
    "simple-macro",
    list(as.symbol("x")),
    quote(x),
    NULL,
    test_env
  )

  macro_registry <- engine_field(eng, "env")$macro_registry_env(test_env, create = FALSE)
  expect_true(exists("simple-macro", envir = macro_registry, inherits = FALSE))
})

test_that("defmacro_compiled() preserves doc list", {
  eng <- make_engine()
  test_env <- eng$get_env()

  engine_field(eng, "compiled_runtime")$defmacro_compiled(
    "documented-macro",
    list(as.symbol("x")),
    quote(x),
    list(
      description = "This is a documented macro",
      examples = "(documented-macro 1)"
    ),
    test_env
  )

  macro_registry <- engine_field(eng, "env")$macro_registry_env(test_env, create = FALSE)
  macro_fn <- macro_registry$`documented-macro`

  doc <- attr(macro_fn, "arl_doc")
  expect_false(is.null(doc))
  expect_equal(doc$description, "This is a documented macro")
  expect_equal(doc$examples, "(documented-macro 1)")
})

# Promise/delay tests
test_that("promise_new_compiled() creates Promise", {
  eng <- make_engine()

  promise <- engine_field(eng, "compiled_runtime")$promise_new_compiled(quote(1 + 1), eng$get_env())

  expect_true(arl:::r6_isinstance(promise, "Promise"))
})

test_that("promise_new_compiled() delays evaluation", {
  eng <- make_engine()
  env <- eng$get_env()
  env$side_effect <- 0

  promise <- engine_field(eng, "compiled_runtime")$promise_new_compiled(
    quote(side_effect <- side_effect + 1),
    eng$get_env()
  )

  # Side effect should not have happened yet
  expect_equal(engine_field(eng, "env")$env$side_effect, 0)
})

test_that("promise_new_compiled() evaluates when forced", {
  eng <- make_engine()
  env <- eng$get_env()
  env$x <- 42

  promise <- engine_field(eng, "compiled_runtime")$promise_new_compiled(quote(x * 2), env = env)

  result <- promise$value()
  expect_equal(result, 84)
})

test_that("promise_new_compiled() caches result", {
  eng <- make_engine()
  env <- eng$get_env()
  env$counter <- 0

  promise <- engine_field(eng, "compiled_runtime")$promise_new_compiled(
    quote({ counter <- counter + 1; counter }),
    env = env
  )

  result1 <- promise$value()
  result2 <- promise$value()

  # Should only evaluate once
  expect_equal(result1, 1)
  expect_equal(result2, 1)
  expect_equal(env$counter, 1)
})

# eval_compiled tests
test_that("eval_compiled() evaluates compiled expressions", {
  eng <- make_engine()
  test_env <- new.env()

  result <- engine_field(eng, "compiled_runtime")$eval_compiled(quote(1 + 1), test_env)

  expect_equal(result, 2)
})

test_that("eval_compiled() installs helpers", {
  eng <- make_engine()
  test_env <- new.env()

  engine_field(eng, "compiled_runtime")$eval_compiled(quote(NULL), test_env)

  # Helpers should be installed
  expect_true(exists(".__env", envir = test_env, inherits = FALSE))
})

test_that("eval_compiled() handles visibility", {
  eng <- make_engine()
  test_env <- new.env()

  # Visible result
  result1 <- withVisible(engine_field(eng, "compiled_runtime")$eval_compiled(quote(42), test_env))
  expect_true(result1$visible)

  # Invisible result
  result2 <- withVisible(engine_field(eng, "compiled_runtime")$eval_compiled(quote(invisible(42)), test_env))
  expect_false(result2$visible)
})

test_that("eval_compiled() manages environment stack", {
  eng <- make_engine()
  test_env <- new.env()

  # Stack should be empty initially
  initial_stack_len <- length(engine_field(eng, "env")$env_stack)

  result <- engine_field(eng, "compiled_runtime")$eval_compiled(quote(42), test_env)

  # Stack should be back to initial state after evaluation
  final_stack_len <- length(engine_field(eng, "env")$env_stack)
  expect_equal(final_stack_len, initial_stack_len)
})

# subscript_call_compiled tests
test_that("subscript_call_compiled() handles $ operator", {
  eng <- make_engine()
  test_env <- new.env()
  obj <- list(foo = 42)

  result <- engine_field(eng, "compiled_runtime")$subscript_call_compiled("$", list(obj, "foo"), test_env)

  expect_equal(result, 42)
})

test_that("subscript_call_compiled() handles [ operator", {
  eng <- make_engine()
  test_env <- new.env()
  vec <- c(1, 2, 3)

  result <- engine_field(eng, "compiled_runtime")$subscript_call_compiled("[", list(vec, 2), test_env)

  expect_equal(result, 2)
})

test_that("subscript_call_compiled() handles [[ operator", {
  eng <- make_engine()
  test_env <- new.env()
  lst <- list(a = 10, b = 20)

  result <- engine_field(eng, "compiled_runtime")$subscript_call_compiled("[[", list(lst, "b"), test_env)

  expect_equal(result, 20)
})

test_that("subscript_call_compiled() requires valid operator name", {
  eng <- make_engine()
  test_env <- new.env()

  expect_error(
    engine_field(eng, "compiled_runtime")$subscript_call_compiled(123, list(), test_env),
    "must be a single string"
  )
})
