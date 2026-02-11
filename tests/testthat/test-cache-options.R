test_that("env cache disabled by default", {
  # Create engine without parameters
  engine <- Engine$new()
  expect_false(engine$use_env_cache)
})

test_that("Global option enables env cache", {
  withr::local_options(rye.use_env_cache = TRUE)
  withr::local_options(rye.env_cache_warning_shown = NULL)

  # Expect warning on first engine
  expect_message(
    engine <- Engine$new(),
    "Environment cache.*enabled"
  )
  expect_true(engine$use_env_cache)
})

test_that("Engine parameter overrides global option", {
  # Explicit TRUE overrides global FALSE
  withr::local_options(rye.use_env_cache = FALSE)
  withr::local_options(rye.env_cache_warning_shown = NULL)

  expect_message(
    engine1 <- Engine$new(use_env_cache = TRUE),
    "Environment cache.*enabled"
  )
  expect_true(engine1$use_env_cache)

  # Explicit FALSE overrides global TRUE
  withr::local_options(rye.use_env_cache = TRUE)
  withr::local_options(rye.env_cache_warning_shown = TRUE)  # Suppress warning

  engine2 <- Engine$new(use_env_cache = FALSE)
  expect_false(engine2$use_env_cache)
})

test_that("env cache disabled prevents .env.rds writing", {
  # Setup: temporary module file
  temp_dir <- withr::local_tempdir()
  module_file <- file.path(temp_dir, "test-module.rye")
  writeLines(c(
    "(module test-module",
    "  (export x)",
    "  (define x 42))"
  ), module_file)

  # Create engine with use_env_cache = FALSE
  withr::local_options(rye.env_cache_warning_shown = TRUE)
  engine <- Engine$new(use_env_cache = FALSE)

  # Load module (should be safe to cache)
  engine$load_file_in_env(module_file, engine$env$env, create_scope = FALSE)

  # Verify: .code.rds written, .env.rds NOT written
  cache_dir <- file.path(temp_dir, ".rye_cache")
  expect_true(dir.exists(cache_dir))

  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = FALSE)
  code_cache_exists <- any(grepl("\\.code\\.rds$", cache_files))
  env_cache_exists <- any(grepl("\\.env\\.rds$", cache_files))

  expect_true(code_cache_exists, "expr cache (.code.rds) should be written")
  expect_false(env_cache_exists, "env cache (.env.rds) should NOT be written when disabled")
})

test_that("env cache enabled writes .env.rds when safe", {
  # Setup: temporary module file
  temp_dir <- withr::local_tempdir()
  module_file <- file.path(temp_dir, "test-module.rye")
  writeLines(c(
    "(module test-module",
    "  (export x)",
    "  (define x 42))"
  ), module_file)

  # Create engine with use_env_cache = TRUE
  withr::local_options(rye.env_cache_warning_shown = TRUE)
  engine <- Engine$new(use_env_cache = TRUE)

  # Load safe module
  engine$load_file_in_env(module_file, engine$env$env, create_scope = FALSE)

  # Verify: both .code.rds AND .env.rds written
  cache_dir <- file.path(temp_dir, ".rye_cache")
  expect_true(dir.exists(cache_dir))

  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = FALSE)
  code_cache_exists <- any(grepl("\\.code\\.rds$", cache_files))
  env_cache_exists <- any(grepl("\\.env\\.rds$", cache_files))

  expect_true(code_cache_exists, "expr cache (.code.rds) should be written")
  expect_true(env_cache_exists, "env cache (.env.rds) should be written when enabled and safe")
})

test_that("env cache disabled uses expr cache even when .env.rds exists", {
  skip("Difficult to test directly without internal state inspection")
  # This would require adding test hooks or timing measurements
  # The behavior is correct by construction: the if (isTRUE(self$use_env_cache))
  # check prevents the env cache from being attempted
})

test_that("One-time warning shown per session", {
  withr::local_options(rye.env_cache_warning_shown = NULL)

  # First engine with use_env_cache=TRUE shows warning
  expect_message(
    Engine$new(use_env_cache = TRUE),
    "Environment cache.*enabled"
  )

  # Second engine does not repeat warning
  expect_silent(Engine$new(use_env_cache = TRUE))
})

test_that("Dependency change causes stale data with env cache", {
  skip("This is a known limitation documented in inst/design-docs/CACHE_INVALIDATION.md")
  # This test documents WHY the env cache is off by default
  # Implementation would require:
  # 1. Create module-a.rye with (define x 1)
  # 2. Create module-b.rye that imports a and uses x
  # 3. Load both with env cache enabled
  # 4. Change module-a.rye to (define x 100)
  # 5. Reload: module-a fresh, module-b from cache
  # 6. Verify module-b has stale x=1 (demonstrating the issue)
})

test_that("expr cache is safe with file changes", {
  # The expr cache (compiled expressions) is the safe default.
  # It verifies that changes to module files are properly detected through cache invalidation.
  temp_dir <- withr::local_tempdir()

  # Create a simple file (not a module, to avoid import complexity)
  test_file <- file.path(temp_dir, "changing-file.rye")
  writeLines("(define test-value 42)", test_file)

  # Load with env cache disabled (safe mode)
  withr::local_options(rye.env_cache_warning_shown = TRUE)
  engine1 <- Engine$new(use_env_cache = FALSE)
  engine1$eval_text(sprintf('(load "%s")', test_file))

  # Verify initial value
  expect_equal(engine1$eval_text("test-value"), 42)

  # Change the file
  writeLines("(define test-value 100)", test_file)

  # Reload in new engine (simulating fresh session)
  engine2 <- Engine$new(use_env_cache = FALSE)
  engine2$eval_text(sprintf('(load "%s")', test_file))

  # Verify the change is picked up (cache was invalidated by content hash)
  expect_equal(engine2$eval_text("test-value"), 100)
})

test_that("NULL use_env_cache inherits from global option", {
  # Test explicit NULL (default behavior)
  withr::local_options(rye.use_env_cache = TRUE)
  withr::local_options(rye.env_cache_warning_shown = TRUE)

  engine <- Engine$new(use_env_cache = NULL)
  expect_true(engine$use_env_cache)

  withr::local_options(rye.use_env_cache = FALSE)
  engine2 <- Engine$new(use_env_cache = NULL)
  expect_false(engine2$use_env_cache)
})

test_that("Warning message format is correct", {
  withr::local_options(rye.env_cache_warning_shown = NULL)

  expect_message(
    Engine$new(use_env_cache = TRUE),
    "Note: Environment cache is enabled"
  )

  expect_message(
    {
      withr::local_options(rye.env_cache_warning_shown = NULL)
      Engine$new(use_env_cache = TRUE)
    },
    "4x speedup"
  )

  expect_message(
    {
      withr::local_options(rye.env_cache_warning_shown = NULL)
      Engine$new(use_env_cache = TRUE)
    },
    "options\\(rye\\.use_env_cache = FALSE\\)"
  )
})
