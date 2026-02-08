# Tests for ModuleCache (R/module-cache.R): module caching system

# Cache path generation tests
test_that("get_paths() returns NULL for non-existent file", {
  cache <- rye:::ModuleCache$new()
  paths <- cache$get_paths("/nonexistent/file.rye")
  expect_null(paths)
})

test_that("get_paths() returns expected structure", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  paths <- cache$get_paths(tmp_file)

  expect_type(paths, "list")
  expect_true(all(c("cache_dir", "env_cache", "code_cache", "code_r", "file_hash") %in% names(paths)))
  expect_true(grepl("\\.rye_cache$", paths$cache_dir))
  expect_true(grepl("\\.env\\.rds$", paths$env_cache))
  expect_true(grepl("\\.code\\.rds$", paths$code_cache))
  expect_true(grepl("\\.code\\.R$", paths$code_r))
  expect_true(nchar(paths$file_hash) == 32) # MD5 hash length
})

test_that("get_paths() hash changes when file content changes", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
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

# Safety checks tests
test_that("is_safe_to_cache() returns safe for clean environment", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()
  test_env$foo <- 42
  test_env$bar <- "test"

  # Function with module environment
  test_env$baz <- function(x) x + 1
  environment(test_env$baz) <- test_env

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_true(result$safe)
  expect_equal(result$issues, character(0))
})

test_that("is_safe_to_cache() detects external pointers", {
  skip("External pointer creation requires C-level access")
  # This test would require creating an actual external pointer,
  # which is difficult without C code. The implementation is tested
  # by checking typeof(obj) == "externalptr"
})

test_that("is_safe_to_cache() detects connections", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()

  # Create a connection
  tmp_file <- tempfile()
  writeLines("test", tmp_file)
  test_env$conn <- file(tmp_file, "r")
  on.exit({
    close(test_env$conn)
    unlink(tmp_file)
  })

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_false(result$safe)
  expect_true(any(grepl("Connection", result$issues)))
})

test_that("is_safe_to_cache() allows functions with module environment", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()

  # Function with module environment is safe
  test_env$fn <- function(x) x + 1
  environment(test_env$fn) <- test_env

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_true(result$safe)
})

test_that("is_safe_to_cache() allows functions with base/empty/namespace envs", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()

  test_env$fn_base <- base::identity
  # Create function with emptyenv
  f <- function(x) x
  environment(f) <- emptyenv()
  test_env$fn_empty <- f

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_true(result$safe)
})

test_that("is_safe_to_cache() detects captured functions", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()
  other_env <- new.env()

  # Function captured from non-module environment
  other_env$x <- 42
  test_env$captured <- (function() {
    env <- other_env
    function() env$x
  })()

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_false(result$safe)
  expect_true(any(grepl("Captured function", result$issues)))
})

test_that("is_safe_to_cache() allows Rye module functions", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()

  # Create a function from another Rye module
  # The implementation checks isTRUE(get0(".rye_module", ...)), so needs TRUE value
  other_module <- new.env()
  other_module$.rye_module <- TRUE  # Must be TRUE, not just a string
  test_env$from_module <- function(x) x
  environment(test_env$from_module) <- other_module

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_true(result$safe)
})

test_that("is_safe_to_cache() detects sub-environments with non-module parent", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()
  other_env <- new.env()

  # Sub-environment with non-module parent
  test_env$sub_env <- new.env(parent = other_env)

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_false(result$safe)
  expect_true(any(grepl("Sub-environment", result$issues)))
})

test_that("is_safe_to_cache() allows sub-environments with module/base/empty parent", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()

  test_env$sub_module <- new.env(parent = test_env)
  test_env$sub_base <- new.env(parent = baseenv())
  test_env$sub_empty <- new.env(parent = emptyenv())

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_true(result$safe)
})

test_that("is_safe_to_cache() skips special internal names", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()

  # Add names that should be skipped
  test_env$.rye_module <- "test"
  test_env$.rye_exports <- c("foo")
  assign(".__rye_define_value__1", 42, envir = test_env)
  assign("__rye_helper", function() {}, envir = test_env)
  test_env$.rye_internal <- 123
  test_env$quasiquote <- function() {}

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_true(result$safe)
})

test_that("is_safe_to_cache() reports multiple issues", {
  cache <- rye:::ModuleCache$new()
  test_env <- new.env()
  engine_env <- new.env()
  other_env <- new.env()

  # Add multiple unsafe items
  tmp_file <- tempfile()
  writeLines("test", tmp_file)
  test_env$conn <- file(tmp_file, "r")

  # Add a captured function
  other_env$x <- 42
  test_env$captured <- (function() {
    env <- other_env
    function() env$x
  })()

  on.exit({
    close(test_env$conn)
    unlink(tmp_file)
  })

  result <- cache$is_safe_to_cache(test_env, engine_env)

  expect_false(result$safe)
  expect_true(length(result$issues) >= 2)
})

# Option C (env cache) tests
test_that("write_env() creates cache files", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  module_env <- new.env()
  module_env$foo <- 42
  paths <- cache$get_paths(tmp_file)

  result <- cache$write_env("test", module_env, c("foo"), tmp_file, paths$file_hash)

  expect_true(result)
  expect_true(file.exists(paths$env_cache))
})

test_that("write_env() severs and restores parent environment", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  engine_env <- new.env()
  module_env <- new.env(parent = engine_env)
  module_env$foo <- 42
  paths <- cache$get_paths(tmp_file)

  # Parent should be engine_env before write
  expect_identical(parent.env(module_env), engine_env)

  cache$write_env("test", module_env, c("foo"), tmp_file, paths$file_hash)

  # Parent should be restored after write
  expect_identical(parent.env(module_env), engine_env)
})

test_that("write_env() includes version and metadata", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  module_env <- new.env()
  module_env$foo <- 42
  paths <- cache$get_paths(tmp_file)

  cache$write_env("test", module_env, c("foo"), tmp_file, paths$file_hash)

  cache_data <- readRDS(paths$env_cache)
  expect_equal(cache_data$version, as.character(utils::packageVersion("rye")))
  expect_equal(cache_data$file_hash, paths$file_hash)
  expect_equal(cache_data$module_name, "test")
  expect_equal(cache_data$exports, c("foo"))
})

test_that("load_env() returns NULL for non-existent cache", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  engine_env <- new.env()
  result <- cache$load_env("/nonexistent/cache.env.rds", engine_env, tmp_file)

  expect_null(result)
})

test_that("load_env() loads cache and relinks parent", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  engine_env1 <- new.env()
  module_env <- new.env(parent = engine_env1)
  module_env$foo <- 42
  paths <- cache$get_paths(tmp_file)

  cache$write_env("test", module_env, c("foo"), tmp_file, paths$file_hash)

  # Load with different engine env
  engine_env2 <- new.env()
  loaded <- cache$load_env(paths$env_cache, engine_env2, tmp_file)

  expect_false(is.null(loaded))
  expect_equal(loaded$module_name, "test")
  expect_equal(loaded$exports, c("foo"))
  expect_equal(loaded$module_env$foo, 42)
  expect_identical(parent.env(loaded$module_env), engine_env2)
})

test_that("load_env() returns NULL for version mismatch", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)

  cache_dir <- file.path(dirname(tmp_file), ".rye_cache")
  on.exit({
    unlink(tmp_file)
    unlink(cache_dir, recursive = TRUE)
  }, add = TRUE)

  module_env <- new.env()
  module_env$foo <- 42
  paths <- cache$get_paths(tmp_file)

  cache$write_env("test", module_env, c("foo"), tmp_file, paths$file_hash)

  # Modify version in cache
  cache_data <- readRDS(paths$env_cache)
  cache_data$version <- "0.0.0.9999"
  saveRDS(cache_data, paths$env_cache)

  engine_env <- new.env()
  result <- cache$load_env(paths$env_cache, engine_env, tmp_file)

  expect_null(result)
  # Cache invalidation deletes files (implementation detail, not tested here)
})

test_that("load_env() returns NULL for hash mismatch", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)

  cache_dir <- file.path(dirname(tmp_file), ".rye_cache")
  on.exit({
    unlink(tmp_file)
    unlink(cache_dir, recursive = TRUE)
  }, add = TRUE)

  module_env <- new.env()
  module_env$foo <- 42
  paths1 <- cache$get_paths(tmp_file)

  cache$write_env("test", module_env, c("foo"), tmp_file, paths1$file_hash)

  # Modify file content - this changes the hash
  writeLines("(module test (export foo bar))", tmp_file)
  paths2 <- cache$get_paths(tmp_file)

  engine_env <- new.env()
  # Old cache path with old hash should be invalid
  result <- cache$load_env(paths1$env_cache, engine_env, tmp_file)

  expect_null(result)
  # Cache invalidation deletes files (implementation detail, not tested here)
})

# Option A (code cache) tests
test_that("write_code() creates cache files", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  result <- cache$write_code("test", compiled_body, c("foo"), FALSE, tmp_file, paths$file_hash)

  expect_true(result)
  expect_true(file.exists(paths$code_cache))
  expect_true(file.exists(paths$code_r))
})

test_that("write_code() creates human-readable .code.R file", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  compiled_body <- list(quote(foo <- 42), quote(bar <- "test"))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo", "bar"), TRUE, tmp_file, paths$file_hash)

  r_code <- readLines(paths$code_r)
  expect_true(any(grepl("module: test", r_code)))
  expect_true(any(grepl("Exports:", r_code)))
  expect_true(any(grepl("Expression", r_code)))
})

test_that("write_code() includes metadata", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, tmp_file, paths$file_hash)

  cache_data <- readRDS(paths$code_cache)
  expect_equal(cache_data$version, as.character(utils::packageVersion("rye")))
  expect_equal(cache_data$file_hash, paths$file_hash)
  expect_equal(cache_data$module_name, "test")
  expect_equal(cache_data$exports, c("foo"))
  expect_equal(cache_data$export_all, FALSE)
  expect_equal(length(cache_data$compiled_body), 1)
})

test_that("load_code() returns NULL for non-existent cache", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit(unlink(tmp_file))

  result <- cache$load_code("/nonexistent/cache.code.rds", tmp_file)

  expect_null(result)
})

test_that("load_code() loads cache data", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, tmp_file, paths$file_hash)

  loaded <- cache$load_code(paths$code_cache, tmp_file)

  expect_false(is.null(loaded))
  expect_equal(loaded$module_name, "test")
  expect_equal(loaded$exports, c("foo"))
  expect_equal(loaded$export_all, FALSE)
  expect_equal(length(loaded$compiled_body), 1)
})

test_that("load_code() returns NULL for version mismatch", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)

  cache_dir <- file.path(dirname(tmp_file), ".rye_cache")
  on.exit({
    unlink(tmp_file)
    unlink(cache_dir, recursive = TRUE)
  }, add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, tmp_file, paths$file_hash)

  # Modify version in cache
  cache_data <- readRDS(paths$code_cache)
  cache_data$version <- "0.0.0.9999"
  saveRDS(cache_data, paths$code_cache)

  result <- cache$load_code(paths$code_cache, tmp_file)

  expect_null(result)
  # Cache invalidation deletes files (implementation detail, not tested here)
})

test_that("load_code() returns NULL for hash mismatch", {
  cache <- rye:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".rye")
  writeLines("(module test (export foo))", tmp_file)

  cache_dir <- file.path(dirname(tmp_file), ".rye_cache")
  on.exit({
    unlink(tmp_file)
    unlink(cache_dir, recursive = TRUE)
  }, add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths1 <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, tmp_file, paths1$file_hash)

  # Modify file content - changes hash
  writeLines("(module test (export foo bar))", tmp_file)
  paths2 <- cache$get_paths(tmp_file)

  # Old cache with old hash should be invalid
  result <- cache$load_code(paths1$code_cache, tmp_file)

  expect_null(result)
  # Cache invalidation deletes files (implementation detail, not tested here)
})
