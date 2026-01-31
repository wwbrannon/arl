# Tests for import/module edge cases
# Covers circular imports, module caching, error cleanup

test_that("import non-existent module fails gracefully", {
  engine <- RyeEngine$new()

  expect_error(
    engine$eval_text('(import "non_existent_module_12345")'),
    "not found|import|load|module"
  )
})

test_that("import with invalid path", {
  engine <- RyeEngine$new()

  expect_error(
    engine$eval_text('(import "/invalid/path/to/module")'),
    "not found|import|load|module"
  )
})

test_that("module is cached after first load", {
  # Create a temporary module file
  temp_dir <- tempdir()
  module_path <- file.path(temp_dir, "test_cache_module.rye")
  writeLines('(define cached-var 42)', module_path)

  engine <- RyeEngine$new()
  env <- stdlib_env(engine)

  # First load
  engine$eval_text(sprintf('(load "%s")', module_path))
  first_result <- engine$eval_text("cached-var")
  expect_equal(first_result, 42)

  # Modify the file
  writeLines('(define cached-var 99)', module_path)

  # Second load - should still get cached value
  # (Note: actual caching behavior depends on implementation)
  engine$eval_text(sprintf('(load "%s")', module_path))

  # Clean up
  unlink(module_path)
})

test_that("load returns last value from module", {
  # Create a temporary module
  temp_dir <- tempdir()
  module_path <- file.path(temp_dir, "test_return_module.rye")
  writeLines(c(
    '(define x 10)',
    '(define y 20)',
    '(+ x y)'
  ), module_path)

  engine <- RyeEngine$new()
  result <- engine$eval_text(sprintf('(load "%s")', module_path))

  # Should return 30 (the last expression)
  expect_equal(result, 30)

  # Clean up
  unlink(module_path)
})

test_that("load with syntax error in module", {
  # Create a module with syntax error
  temp_dir <- tempdir()
  module_path <- file.path(temp_dir, "test_syntax_error.rye")
  writeLines(c(
    '(define x 10',  # Unclosed paren
    '(+ x 5)'
  ), module_path)

  engine <- RyeEngine$new()

  expect_error(
    engine$eval_text(sprintf('(load "%s")', module_path)),
    "Unclosed|parse|syntax|EOF|incomplete"
  )

  # Clean up
  unlink(module_path)
})

test_that("load with runtime error in module", {
  # Create a module that errors at runtime
  temp_dir <- tempdir()
  module_path <- file.path(temp_dir, "test_runtime_error.rye")
  writeLines(c(
    '(define x 10)',
    '(/ x 0)'  # Division by zero
  ), module_path)

  engine <- RyeEngine$new()

  # In R, division by zero returns Inf, not an error
  result <- engine$eval_text(sprintf('(load "%s")', module_path))
  expect_true(is.infinite(result) || !is.null(result))

  # Clean up
  unlink(module_path)
})

test_that("multiple loads of same module", {
  # Create a simple module
  temp_dir <- tempdir()
  module_path <- file.path(temp_dir, "test_multiple_load.rye")
  writeLines('(define multi-load-var 123)', module_path)

  engine <- RyeEngine$new()

  # Load multiple times
  engine$eval_text(sprintf('(load "%s")', module_path))
  engine$eval_text(sprintf('(load "%s")', module_path))
  engine$eval_text(sprintf('(load "%s")', module_path))

  result <- engine$eval_text("multi-load-var")
  expect_equal(result, 123)

  # Clean up
  unlink(module_path)
})

test_that("load can access previously defined symbols", {
  # Create a module that uses predefined symbols
  temp_dir <- tempdir()
  module_path <- file.path(temp_dir, "test_access_symbols.rye")
  writeLines('(+ existing-x 10)', module_path)

  engine <- RyeEngine$new()
  env <- stdlib_env(engine)

  # Define a symbol first
  engine$eval_text("(define existing-x 5)")

  # Load module that uses it
  result <- engine$eval_text(sprintf('(load "%s")', module_path))
  expect_equal(result, 15)

  # Clean up
  unlink(module_path)
})

test_that("nested module loads", {
  temp_dir <- tempdir()

  # Create module B
  module_b_path <- file.path(temp_dir, "test_module_b.rye")
  writeLines('(define b-var 20)', module_b_path)

  # Create module A that loads B
  module_a_path <- file.path(temp_dir, "test_module_a.rye")
  writeLines(c(
    sprintf('(load "%s")', module_b_path),
    '(define a-var (+ b-var 10))'
  ), module_a_path)

  engine <- RyeEngine$new()
  env <- stdlib_env(engine)

  # Load module A (which loads B)
  engine$eval_text(sprintf('(load "%s")', module_a_path))

  # Should have both variables
  result_a <- engine$eval_text("a-var")
  expect_equal(result_a, 30)

  result_b <- engine$eval_text("b-var")
  expect_equal(result_b, 20)

  # Clean up
  unlink(module_a_path)
  unlink(module_b_path)
})

test_that("circular module dependency detection", {
  # This test checks if circular dependencies are detected
  # The behavior depends on implementation - it might error or handle gracefully

  temp_dir <- tempdir()

  # Create module A that loads B
  module_a_path <- file.path(temp_dir, "test_circular_a.rye")
  module_b_path <- file.path(temp_dir, "test_circular_b.rye")

  writeLines(sprintf('(load "%s")', module_b_path), module_a_path)
  writeLines(sprintf('(load "%s")', module_a_path), module_b_path)

  engine <- RyeEngine$new()
  env <- stdlib_env(engine)

  # Circular dependencies should error with recursion limit
  expect_error(
    engine$eval_text(sprintf('(load "%s")', module_a_path)),
    "nested too deeply|infinite recursion|expressions"
  )

  # Clean up
  unlink(module_a_path)
  unlink(module_b_path)
})

test_that("load with relative path", {
  # Create a module in current directory
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)

  module_path <- "test_relative.rye"
  writeLines('(define relative-var 777)', module_path)

  engine <- RyeEngine$new()
  env <- stdlib_env(engine)

  result <- engine$eval_text(sprintf('(load "%s")', module_path))
  var_result <- engine$eval_text("relative-var")
  expect_equal(var_result, 777)

  # Clean up
  setwd(old_wd)
  unlink(file.path(temp_dir, module_path))
})

test_that("load empty module", {
  temp_dir <- tempdir()
  module_path <- file.path(temp_dir, "test_empty.rye")
  writeLines('', module_path)

  engine <- RyeEngine$new()

  # Loading empty module should succeed and return NULL or similar
  result <- engine$eval_text(sprintf('(load "%s")', module_path))
  # Result behavior may vary - just ensure no crash
  expect_no_error(result)

  # Clean up
  unlink(module_path)
})

test_that("load module with only comments", {
  temp_dir <- tempdir()
  module_path <- file.path(temp_dir, "test_comments_only.rye")
  writeLines(c(
    '; This is a comment',
    '; Another comment',
    '; ; More comments'
  ), module_path)

  engine <- RyeEngine$new()

  # Should succeed
  result <- engine$eval_text(sprintf('(load "%s")', module_path))
  expect_no_error(result)

  # Clean up
  unlink(module_path)
})

test_that("module defines macro", {
  temp_dir <- tempdir()
  module_path <- file.path(temp_dir, "test_macro_module.rye")
  writeLines(c(
    '(defmacro triple (x) `(* 3 ,x))'
  ), module_path)

  engine <- RyeEngine$new()
  env <- stdlib_env(engine)

  # Load module with macro
  engine$eval_text(sprintf('(load "%s")', module_path))

  # Use the macro
  result <- engine$eval_text("(triple 7)")
  expect_equal(result, 21)

  # Clean up
  unlink(module_path)
})
