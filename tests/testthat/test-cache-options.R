# Tests for module caching behavior

test_that("code cache is written for loaded modules", {
  # Setup: temporary module file
  temp_dir <- withr::local_tempdir()
  module_file <- file.path(temp_dir, "test-module.arl")
  writeLines(c(
    "(module test-module",
    "  (export x)",
    "  (define x 42))"
  ), module_file)

  engine <- Engine$new()

  # Load module (should write code cache)
  engine$load_file_in_env(module_file)

  # Verify: .code.rds written
  cache_dir <- file.path(temp_dir, ".arl_cache")
  expect_true(dir.exists(cache_dir))

  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = FALSE)
  code_cache_exists <- any(grepl("\\.code\\.rds$", cache_files))

  expect_true(code_cache_exists, "expr cache (.code.rds) should be written")
})

test_that("code cache is safe with file changes", {
  # The expr cache (compiled expressions) is the safe default.
  # It verifies that changes to module files are properly detected through cache invalidation.
  temp_dir <- withr::local_tempdir()

  # Create a simple file (not a module, to avoid import complexity)
  test_file <- file.path(temp_dir, "changing-file.arl")
  writeLines("(define test-value 42)", test_file)

  # Load
  engine1 <- Engine$new()
  engine1$eval_text(sprintf('(load "%s")', test_file))

  # Verify initial value
  expect_equal(engine1$eval_text("test-value"), 42)

  # Change the file
  writeLines("(define test-value 100)", test_file)

  # Reload in new engine (simulating fresh session)
  engine2 <- Engine$new()
  engine2$eval_text(sprintf('(load "%s")', test_file))

  # Verify the change is picked up (cache was invalidated by content hash)
  expect_equal(engine2$eval_text("test-value"), 100)
})

test_that("code cache reused across engine instances", {
  temp_dir <- withr::local_tempdir()
  module_file <- file.path(temp_dir, "test-module.arl")
  writeLines(c(
    "(module test-module",
    "  (export x)",
    "  (define x 42))"
  ), module_file)

  # First engine creates the cache
  engine1 <- Engine$new()
  engine1$load_file_in_env(module_file)

  # Second engine should find and use the cache
  engine2 <- Engine$new()
  engine2$load_file_in_env(module_file)

  # Both should have access to the module's export
  arl_env2 <- arl:::Env$new(engine2$get_env())
  registry2 <- arl_env2$module_registry
  expect_true(registry2$exists("test-module"))
})
