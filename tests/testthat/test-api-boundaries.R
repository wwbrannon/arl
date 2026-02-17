test_that("eval_string is an alias for eval_text", {
  engine <- make_engine()

  result <- engine$eval_string("(+ 2 3)")

  expect_equal(result, 5)
})

test_that("Promise uses private fields", {
  engine <- make_engine()

  # Create a promise
  promise <- engine$eval_text('(delay (+ 1 2))')

  # Internal fields use private R6 mechanism
  # Users should use public methods value() and get_expr()

  # value() method should work
  expect_equal(promise$value(), 3)

  # get_expr() should work
  expr <- promise$get_expr()
  expect_true(is.call(expr))

  # Verify the promise is an R6 object with private fields
  expect_s3_class(promise, "Promise")
  expect_s3_class(promise, "R6")
})

test_that("Promise caching works correctly", {
  engine <- make_engine()

  # Create a promise with side effect
  engine$eval_text('(define counter 0)')
  promise <- engine$eval_text('(delay (begin (set! counter (+ counter 1)) counter))')

  # First force should evaluate
  result1 <- promise$value()
  expect_equal(result1, 1)

  # Second force should return cached value (counter shouldn't increment)
  result2 <- promise$value()
  expect_equal(result2, 1)

  # Counter should only be 1 (evaluated once)
  counter_val <- engine$eval_text('counter')
  expect_equal(counter_val, 1)
})

test_that("module registry bindings are locked", {
  engine <- make_engine()

  # Create a test module
  engine$eval_text('(module testmod (export x) (define x 42))')

  # Get the module registry entry
  reg <- engine_field(engine, "env")$module_registry$get("testmod")

  # Entry should exist and have expected structure
  expect_true(!is.null(reg))
  expect_true(!is.null(reg$env))
  expect_equal(reg$exports, "x")

  # The binding in the registry should be locked
  registry_env <- engine_field(engine, "env")$module_registry_env(create = FALSE)

  # Verify it's locked
  expect_true(bindingIsLocked("testmod", registry_env))

  # Cannot reassign without unlocking
  expect_error({
    assign("testmod", list(), envir = registry_env)
  }, "locked")
})

test_that("r-eval without env parameter works correctly", {
  engine <- make_engine()

  # r-eval should work without explicit env parameter
  result <- engine$eval_text('
    (define x 100)
    (r-eval (quote (+ x 1)))
  ')

  expect_equal(result, 101)

  # Should work in nested contexts
  result2 <- engine$eval_text('
    ((lambda (y) (r-eval (quote (+ y 10)))) 5)
  ')

  expect_equal(result2, 15)
})

test_that(".__env is documented as internal", {
  # This test documents that .__env still exists in lambda bodies
  # but is clearly internal (not part of public API)
  engine <- make_engine()

  # .__env exists in compiled lambda bodies for internal use
  fn <- engine$eval_text('(lambda (x) (r-eval (quote (environment))))')
  env <- fn(42)

  # It should have .__env bound
  expect_true(exists(".__env", envir = env, inherits = FALSE))

  # Users CAN access it if they try hard enough (same R process)
  # But it's documented as internal and may change
  arl_env_val <- get(".__env", envir = env)
  expect_true(is.environment(arl_env_val))
})

test_that("module registry entries are truly immutable", {
  engine <- make_engine()

  # Create a module
  engine$eval_text('(module immutmod (export val) (define val 123))')

  # Get the entry
  entry <- engine_field(engine, "env")$module_registry$get("immutmod")

  # Entry should be a locked environment
  expect_true(is.environment(entry))
  expect_true(environmentIsLocked(entry))

  # Cannot mutate fields
  expect_error({
    entry$exports <- c("hacked")
  }, "locked")

  expect_error({
    entry$env <- new.env()
  }, "locked")

  expect_error({
    entry$path <- "/evil/path"
  }, "locked")

  # Original exports should be unchanged
  expect_equal(entry$exports, "val")
})

test_that("Env fields are read-only via active bindings", {
  engine <- make_engine()

  # Can read fields
  expect_true(is.environment(engine_field(engine, "env")$env))
  expect_s3_class(engine_field(engine, "env")$module_registry, "ModuleRegistry")
  expect_true(is.list(engine_field(engine, "env")$env_stack))

  # Cannot reassign fields
  expect_error({
    engine_field(engine, "env")$env <- new.env()
  }, "Cannot reassign env field")

  expect_error({
    engine_field(engine, "env")$module_registry <- NULL
  }, "Cannot reassign module_registry field")

  expect_error({
    engine_field(engine, "env")$env_stack <- list()
  }, "Cannot reassign env_stack field")

  expect_error({
    engine_field(engine, "env")$macro_registry <- NULL
  }, "Cannot reassign macro_registry field")
})

test_that("Env internal operations still work with private fields", {
  engine <- make_engine()

  # Test env stack operations
  test_env <- new.env()
  engine_field(engine, "env")$push_env(test_env)

  # Should be on stack
  expect_identical(engine_field(engine, "env")$current_env(), test_env)

  # Pop should work
  engine_field(engine, "env")$pop_env()
  expect_identical(engine_field(engine, "env")$current_env(), globalenv())

  # Module operations should work
  engine$eval_text('(module testmod2 (export z) (define z 789))')
  expect_true(engine_field(engine, "env")$module_registry$exists("testmod2"))

  mod_entry <- engine_field(engine, "env")$module_registry$get("testmod2")
  expect_equal(mod_entry$exports, "z")
})
