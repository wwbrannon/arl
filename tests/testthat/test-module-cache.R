# Tests for ModuleCache (R/module-cache.R): module caching system

# Default compiler flags for cache tests (all TRUE = default compiler state)
.test_compiler_flags <- c(
  enable_tco = TRUE, enable_constant_folding = TRUE,
  enable_dead_code_elim = TRUE, enable_strength_reduction = TRUE,
  enable_identity_elim = TRUE, enable_truthiness_opt = TRUE,
  enable_begin_simplify = TRUE, enable_boolean_flatten = TRUE
)

# Cache path generation tests
test_that("get_paths() returns NULL for non-existent file", {
  cache <- arl:::ModuleCache$new()
  paths <- cache$get_paths("/nonexistent/file.arl")
  expect_null(paths)
})

test_that("get_paths() returns expected structure", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  paths <- cache$get_paths(tmp_file)

  expect_type(paths, "list")
  expect_true(all(c("cache_dir", "code_cache", "code_r", "file_hash") %in% names(paths)))
  user_cache <- tools::R_user_dir("arl", "cache")
  expect_true(startsWith(normalizePath(paths$cache_dir, mustWork = FALSE, winslash = "/"),
                         normalizePath(user_cache, mustWork = FALSE, winslash = "/")))
  expect_true(grepl("\\.code\\.rds$", paths$code_cache))
  expect_true(grepl("\\.code\\.R$", paths$code_r))
  expect_true(nchar(paths$file_hash) == 32) # MD5 hash length
})

test_that("get_paths() hash changes when file content changes", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  paths1 <- cache$get_paths(tmp_file)
  hash1 <- paths1$file_hash

  # Modify file
  writeLines("(module test (export foo bar))", tmp_file)
  paths2 <- cache$get_paths(tmp_file)
  hash2 <- paths2$file_hash

  expect_false(hash1 == hash2)
})

# expr cache tests
test_that("write_code() creates cache files", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  result <- cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash, compiler_flags = .test_compiler_flags)

  expect_true(result)
  expect_true(file.exists(paths$code_cache))
  # .code.R only written when debug_cache is TRUE
  expect_false(file.exists(paths$code_r))
})

test_that("write_code() creates human-readable .code.R file when debug_cache is TRUE", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
    options(arl.debug_cache = NULL)
  })

  compiled_body <- list(quote(foo <- 42), quote(bar <- "test"))
  paths <- cache$get_paths(tmp_file)

  options(arl.debug_cache = TRUE)
  cache$write_code("test", compiled_body, c("foo", "bar"), TRUE, FALSE, tmp_file, paths$file_hash, compiler_flags = .test_compiler_flags)

  r_code <- readLines(paths$code_r)
  expect_true(any(grepl("module: test", r_code)))
  expect_true(any(grepl("Exports:", r_code)))
  expect_true(any(grepl("Expression", r_code)))
})

test_that("write_code() includes metadata", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash, compiler_flags = .test_compiler_flags)

  cache_data <- readRDS(paths$code_cache)
  expect_equal(cache_data$version, as.character(utils::packageVersion("arl")))
  expect_equal(cache_data$file_hash, paths$file_hash)
  expect_equal(cache_data$module_name, "test")
  expect_equal(cache_data$exports, c("foo"))
  expect_equal(cache_data$export_all, FALSE)
  expect_equal(length(cache_data$compiled_body), 1)
})

test_that("load_code() returns NULL for non-existent cache", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  result <- cache$load_code("/nonexistent/cache.code.rds", tmp_file)

  expect_null(result)
})

test_that("load_code() loads cache data", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash, compiler_flags = .test_compiler_flags)

  loaded <- cache$load_code(paths$code_cache, tmp_file)

  expect_false(is.null(loaded))
  expect_equal(loaded$module_name, "test")
  expect_equal(loaded$exports, c("foo"))
  expect_equal(loaded$export_all, FALSE)
  expect_equal(length(loaded$compiled_body), 1)
})

test_that("load_code() returns NULL for version mismatch", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)

  on.exit(unlink(tmp_file), add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash, compiler_flags = .test_compiler_flags)

  # Modify version in cache
  cache_data <- readRDS(paths$code_cache)
  cache_data$version <- "0.0.0.9999"
  saveRDS(cache_data, paths$code_cache)

  result <- cache$load_code(paths$code_cache, tmp_file)

  expect_null(result)
})

test_that("load_code() returns NULL for coverage mismatch", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)

  on.exit(unlink(tmp_file), add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  # Write cache with coverage = TRUE
  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash, coverage = TRUE, compiler_flags = .test_compiler_flags)

  # Loading without coverage should reject it
  result <- cache$load_code(paths$code_cache, tmp_file, coverage = FALSE)
  expect_null(result)
})

test_that("load_code() returns NULL for missing coverage field (pre-upgrade cache)", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)

  on.exit(unlink(tmp_file), add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  # Write a cache, then strip the coverage field to simulate old format
  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash, compiler_flags = .test_compiler_flags)
  cache_data <- readRDS(paths$code_cache)
  cache_data$coverage <- NULL
  saveRDS(cache_data, paths$code_cache)

  result <- cache$load_code(paths$code_cache, tmp_file)
  expect_null(result)
})

test_that("load_code() returns NULL for hash mismatch", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)

  on.exit(unlink(tmp_file), add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths1 <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths1$file_hash, compiler_flags = .test_compiler_flags)

  # Modify file content - changes hash
  writeLines("(module test (export foo bar))", tmp_file)
  paths2 <- cache$get_paths(tmp_file)

  # Old cache with old hash should be invalid
  result <- cache$load_code(paths1$code_cache, tmp_file)

  expect_null(result)
})

# Integration test: cache hit path excludes _-prefixed names from export-all
test_that("cache hit path excludes _-prefixed names from export-all exports", {
  tmp_dir <- tempfile("cache_underscore_test")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  module_file <- file.path(tmp_dir, "undermod.arl")
  writeLines(c(
    "(module undermod",
    "  (export-all)",
    "  (define public-fn (lambda () 1))",
    "  (define _private-helper (lambda () 2))",
    "  (define __also-private (lambda () 3)))"
  ), module_file)

  eng <- make_engine()

  # First load — populates cache and module registry
  eng$load_file_in_env(module_file)
  entry1 <- engine_field(eng, "env")$module_registry$get("undermod")
  expect_true("public-fn" %in% entry1$exports)
  expect_false("_private-helper" %in% entry1$exports)
  expect_false("__also-private" %in% entry1$exports)

  # Second load with a fresh engine — should hit cache
  eng2 <- make_engine()
  eng2$load_file_in_env(module_file)
  entry2 <- engine_field(eng2, "env")$module_registry$get("undermod")
  expect_true("public-fn" %in% entry2$exports)
  expect_false("_private-helper" %in% entry2$exports,
               info = "cache hit path must exclude _-prefixed names")
  expect_false("__also-private" %in% entry2$exports,
               info = "cache hit path must exclude .__-prefixed names")
})

# --- compiler_flags tests ---

test_that("write_code() stores compiler_flags in cache data", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)
  flags <- c(enable_tco = TRUE, enable_constant_folding = FALSE,
             enable_dead_code_elim = TRUE, enable_strength_reduction = TRUE,
             enable_identity_elim = TRUE, enable_truthiness_opt = TRUE,
             enable_begin_simplify = TRUE, enable_boolean_flatten = TRUE)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file,
                   paths$file_hash, compiler_flags = flags)

  cache_data <- readRDS(paths$code_cache)
  expect_equal(cache_data$compiler_flags, flags)
})

test_that("load_code() rejects cache with mismatched compiler_flags", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)
  flags1 <- c(enable_tco = TRUE, enable_constant_folding = TRUE,
              enable_dead_code_elim = TRUE, enable_strength_reduction = TRUE,
              enable_identity_elim = TRUE, enable_truthiness_opt = TRUE,
              enable_begin_simplify = TRUE, enable_boolean_flatten = TRUE)
  flags2 <- c(enable_tco = FALSE, enable_constant_folding = TRUE,
              enable_dead_code_elim = TRUE, enable_strength_reduction = TRUE,
              enable_identity_elim = TRUE, enable_truthiness_opt = TRUE,
              enable_begin_simplify = TRUE, enable_boolean_flatten = TRUE)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file,
                   paths$file_hash, compiler_flags = flags1)

  # Load with same flags — should succeed
  result_same <- cache$load_code(paths$code_cache, tmp_file, file_hash = paths$file_hash,
                                 compiler_flags = flags1)
  expect_false(is.null(result_same))

  # Load with different flags — should reject (and deletes cache file)
  result_diff <- cache$load_code(paths$code_cache, tmp_file, file_hash = paths$file_hash,
                                 compiler_flags = flags2)
  expect_null(result_diff)
})

test_that("load_code() rejects cache with NULL compiler_flags (pre-upgrade)", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)
  flags <- c(enable_tco = TRUE, enable_constant_folding = TRUE,
             enable_dead_code_elim = TRUE, enable_strength_reduction = TRUE,
             enable_identity_elim = TRUE, enable_truthiness_opt = TRUE,
             enable_begin_simplify = TRUE, enable_boolean_flatten = TRUE)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file,
                   paths$file_hash, compiler_flags = flags)

  # Strip compiler_flags to simulate old cache format
  cache_data <- readRDS(paths$code_cache)
  cache_data$compiler_flags <- NULL
  saveRDS(cache_data, paths$code_cache)

  result <- cache$load_code(paths$code_cache, tmp_file, file_hash = paths$file_hash,
                            compiler_flags = flags)
  expect_null(result)
})

# --- default_packages NULL rejection ---

test_that("load_code() rejects cache with NULL default_packages (pre-upgrade)", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file,
                   paths$file_hash, compiler_flags = .test_compiler_flags)

  # Set default_packages to NULL to simulate old cache format
  cache_data <- readRDS(paths$code_cache)
  cache_data$default_packages <- NULL
  saveRDS(cache_data, paths$code_cache)

  result <- cache$load_code(paths$code_cache, tmp_file, file_hash = paths$file_hash)
  expect_null(result)
})

# --- stale cache cleanup ---

test_that("write_code() cleans up old cache files for same source", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  # Write cache for hash H1 (with debug_cache to test .code.R cleanup)
  options(arl.debug_cache = TRUE)
  on.exit(options(arl.debug_cache = NULL), add = TRUE)
  paths1 <- cache$get_paths(tmp_file)
  cache$write_code("test", list(quote(foo <- 42)), c("foo"), FALSE, FALSE,
                   tmp_file, paths1$file_hash)
  expect_true(file.exists(paths1$code_cache))
  expect_true(file.exists(paths1$code_r))

  # Change file, write cache for hash H2
  writeLines("(module test (export foo bar))", tmp_file)
  paths2 <- cache$get_paths(tmp_file)
  cache$write_code("test", list(quote(foo <- 42), quote(bar <- 1)), c("foo", "bar"),
                   FALSE, FALSE, tmp_file, paths2$file_hash)

  # H1 files should be gone, H2 files should exist
  expect_false(file.exists(paths1$code_cache))
  expect_false(file.exists(paths1$code_r))
  expect_true(file.exists(paths2$code_cache))
  expect_true(file.exists(paths2$code_r))
})

# --- TOCTOU: write_code uses provided cache_paths, not fresh get_paths ---

test_that("write_code() uses provided cache_paths instead of recomputing", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("content version 1", tmp_file)
  on.exit(unlink(tmp_file))

  # Get paths for the original content
  paths_v1 <- cache$get_paths(tmp_file)

  # Change file content (simulates TOCTOU: file changed between read and cache write)
  writeLines("content version 2", tmp_file)

  # Write cache with explicitly provided cache_paths from V1
  # This should write to V1's filename, not recompute a V2 hash
  result <- cache$write_code("test", list(quote(x <- 1)), c("x"), FALSE, FALSE,
                             tmp_file, paths_v1$file_hash, cache_paths = paths_v1)

  expect_true(result)
  expect_true(file.exists(paths_v1$code_cache),
              info = "cache file should be at V1 path (provided cache_paths)")

  # The V2 hash file should NOT exist (we didn't recompute)
  paths_v2 <- cache$get_paths(tmp_file)
  expect_false(file.exists(paths_v2$code_cache),
               info = "no cache file should exist at V2 path")
})

# ============================================================================
# Cache write/read options
# ============================================================================

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

  # Verify: .code.rds written under R_user_dir
  cache <- arl:::ModuleCache$new()
  paths <- cache$get_paths(module_file)
  expect_true(dir.exists(paths$cache_dir))

  cache_files <- list.files(paths$cache_dir, pattern = "\\.rds$", full.names = FALSE)
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
  engine1$eval_text(sprintf('(load "%s")', arl_path(test_file)))

  # Verify initial value
  expect_equal(engine1$eval_text("test-value"), 42)

  # Change the file
  writeLines("(define test-value 100)", test_file)

  # Reload in new engine (simulating fresh session)
  engine2 <- Engine$new()
  engine2$eval_text(sprintf('(load "%s")', arl_path(test_file)))

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

# --- library-tree redirect ---

test_that("get_paths() always uses R_user_dir for cache", {
  cache <- arl:::ModuleCache$new()

  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module tmp-test (export x) (define x 1))", tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)

  paths <- cache$get_paths(tmp_file)
  expect_false(is.null(paths))

  # Cache dir should always be under R_user_dir
  user_cache <- tools::R_user_dir("arl", "cache")
  expect_true(startsWith(normalizePath(paths$cache_dir, mustWork = FALSE, winslash = "/"),
                         normalizePath(user_cache, mustWork = FALSE, winslash = "/")),
              info = "cache_dir should be under R_user_dir")
  expect_true(grepl("/modules/", paths$cache_dir),
              info = "cache_dir should contain /modules/ subdirectory")
})
