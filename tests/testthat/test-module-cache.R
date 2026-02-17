# Tests for ModuleCache (R/module-cache.R): module caching system

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
  expect_true(grepl("\\.arl_cache$", paths$cache_dir))
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

  result <- cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash)

  expect_true(result)
  expect_true(file.exists(paths$code_cache))
  expect_true(file.exists(paths$code_r))
})

test_that("write_code() creates human-readable .code.R file", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)
  on.exit({
    unlink(tmp_file)
    unlink(dirname(tmp_file), recursive = TRUE)
  })

  compiled_body <- list(quote(foo <- 42), quote(bar <- "test"))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo", "bar"), TRUE, FALSE, tmp_file, paths$file_hash)

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

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash)

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

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash)

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

  cache_dir <- file.path(dirname(tmp_file), ".arl_cache")
  on.exit({
    unlink(tmp_file)
    unlink(cache_dir, recursive = TRUE)
  }, add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash)

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

  cache_dir <- file.path(dirname(tmp_file), ".arl_cache")
  on.exit({
    unlink(tmp_file)
    unlink(cache_dir, recursive = TRUE)
  }, add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  # Write cache with coverage = TRUE
  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash, coverage = TRUE)

  # Loading without coverage should reject it
  result <- cache$load_code(paths$code_cache, tmp_file, coverage = FALSE)
  expect_null(result)
})

test_that("load_code() returns NULL for missing coverage field (pre-upgrade cache)", {
  cache <- arl:::ModuleCache$new()
  tmp_file <- tempfile(fileext = ".arl")
  writeLines("(module test (export foo))", tmp_file)

  cache_dir <- file.path(dirname(tmp_file), ".arl_cache")
  on.exit({
    unlink(tmp_file)
    unlink(cache_dir, recursive = TRUE)
  }, add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths <- cache$get_paths(tmp_file)

  # Write a cache, then strip the coverage field to simulate old format
  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths$file_hash)
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

  cache_dir <- file.path(dirname(tmp_file), ".arl_cache")
  on.exit({
    unlink(tmp_file)
    unlink(cache_dir, recursive = TRUE)
  }, add = TRUE)

  compiled_body <- list(quote(foo <- 42))
  paths1 <- cache$get_paths(tmp_file)

  cache$write_code("test", compiled_body, c("foo"), FALSE, FALSE, tmp_file, paths1$file_hash)

  # Modify file content - changes hash
  writeLines("(module test (export foo bar))", tmp_file)
  paths2 <- cache$get_paths(tmp_file)

  # Old cache with old hash should be invalid
  result <- cache$load_code(paths1$code_cache, tmp_file)

  expect_null(result)
})
