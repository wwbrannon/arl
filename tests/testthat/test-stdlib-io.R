# Comprehensive I/O operation tests: file I/O, directories, parsing, system operations

engine <- make_engine()

# ============================================================================
# File I/O Operations
# ============================================================================

test_that("read-file and write-file work with strings", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()
  on.exit(unlink(tmp))

  # Write string content
  engine$eval(
    engine$read(sprintf('(write-file "%s" "hello world")', arl_path(tmp)))[[1]], env = env)

  # Read it back
  result <- engine$eval(
    engine$read(sprintf('(read-file "%s")', arl_path(tmp)))[[1]], env = env)
  expect_equal(result, "hello world")
})

test_that("read-lines and write-lines work with lists", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()
  on.exit(unlink(tmp))

  # Write lines as list
  engine$eval(
    engine$read(sprintf('(write-lines "%s" (list "line1" "line2" "line3"))', arl_path(tmp)))[[1]], env = env)

  # Read as list
  result <- engine$eval(
    engine$read(sprintf('(read-lines "%s")', arl_path(tmp)))[[1]], env = env)
  expect_equal(result, list("line1", "line2", "line3"))
})

test_that("write-file works with lists converted to lines", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()
  on.exit(unlink(tmp))

  # Write list as lines with newline separator
  engine$eval(
    engine$read(sprintf('(write-file "%s" (list "line1" "line2"))', arl_path(tmp)))[[1]], env = env)

  # Read it back as string
  result <- engine$eval(
    engine$read(sprintf('(read-file "%s")', arl_path(tmp)))[[1]], env = env)
  expect_equal(result, "line1\nline2")
})

test_that("append-file adds content to existing file", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()
  on.exit(unlink(tmp))

  # Write initial content
  engine$eval(
    engine$read(sprintf('(write-file "%s" "first")', arl_path(tmp)))[[1]], env = env)

  # Append more content
  engine$eval(
    engine$read(sprintf('(append-file "%s" "second")', arl_path(tmp)))[[1]], env = env)

  # Read back
  result <- engine$eval(
    engine$read(sprintf('(read-file "%s")', arl_path(tmp)))[[1]], env = env)
  expect_match(result, "first.*second")
})

test_that("file-exists? checks file existence", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()
  on.exit(unlink(tmp))

  # File doesn't exist yet
  result <- engine$eval(
    engine$read(sprintf('(file-exists? "%s")', arl_path(tmp)))[[1]], env = env)
  expect_false(result)

  # Create file
  writeLines("test", tmp)

  # Now it exists
  result <- engine$eval(
    engine$read(sprintf('(file-exists? "%s")', arl_path(tmp)))[[1]], env = env)
  expect_true(result)
})

test_that("file-size returns file size in bytes", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()
  on.exit(unlink(tmp))

  # Write known content
  writeLines("hello", tmp)
  expected_size <- file.size(tmp)

  result <- engine$eval(
    engine$read(sprintf('(file-size "%s")', arl_path(tmp)))[[1]], env = env)
  expect_equal(result, expected_size)
})

test_that("file-size errors on non-existent file", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()

  expect_error(
    engine$eval(
      engine$read(sprintf('(file-size "%s")', arl_path(tmp)))[[1]], env = env),
    "file does not exist")
})

test_that("file-modified-time returns modification timestamp", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()
  on.exit(unlink(tmp))

  # Create file
  writeLines("test", tmp)

  result <- engine$eval(
    engine$read(sprintf('(file-modified-time "%s")', arl_path(tmp)))[[1]], env = env)

  # Should be numeric timestamp close to now
  expect_true(is.numeric(result))
  expect_true(result > 0)
})

test_that("file-delete removes file", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()
  writeLines("test", tmp)
  expect_true(file.exists(tmp))

  # Delete it
  result <- engine$eval(
    engine$read(sprintf('(file-delete "%s")', arl_path(tmp)))[[1]], env = env)
  expect_true(result)
  expect_false(file.exists(tmp))
})

test_that("file-delete errors on non-existent file", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- tempfile()

  expect_error(
    engine$eval(
      engine$read(sprintf('(file-delete "%s")', arl_path(tmp)))[[1]], env = env),
    "file does not exist")
})

# ============================================================================
# Directory Operations
# ============================================================================

test_that("directory-exists? checks directory existence", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- file.path(tempdir(), "test_dir")
  on.exit(unlink(tmp, recursive = TRUE))

  # Directory doesn't exist yet
  result <- engine$eval(
    engine$read(sprintf('(directory-exists? "%s")', arl_path(tmp)))[[1]], env = env)
  expect_false(result)

  # Create directory
  dir.create(tmp)

  # Now it exists
  result <- engine$eval(
    engine$read(sprintf('(directory-exists? "%s")', arl_path(tmp)))[[1]], env = env)
  expect_true(result)
})

test_that("directory-list returns list of filenames", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- file.path(tempdir(), "test_dir")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # Create some files
  writeLines("test", file.path(tmp, "file1.txt"))
  writeLines("test", file.path(tmp, "file2.txt"))

  result <- engine$eval(
    engine$read(sprintf('(directory-list "%s")', arl_path(tmp)))[[1]], env = env)

  expect_true(is.list(result))
  expect_equal(length(result), 2)
  expect_true("file1.txt" %in% unlist(result))
  expect_true("file2.txt" %in% unlist(result))
})

test_that("directory-list with full.names returns full paths", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- file.path(tempdir(), "test_dir")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  writeLines("test", file.path(tmp, "file1.txt"))

  result <- engine$eval(
    engine$read(sprintf('(directory-list "%s" #t)', arl_path(tmp)))[[1]], env = env)

  expect_true(is.list(result))
  expect_match(result[[1]], "file1.txt$")
  expect_match(result[[1]], "^(/|[A-Za-z]:)")  # Full path starts with / or drive letter
})

test_that("directory-list errors on non-existent directory", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- file.path(tempdir(), "nonexistent_dir")

  expect_error(
    engine$eval(
      engine$read(sprintf('(directory-list "%s")', arl_path(tmp)))[[1]], env = env),
    "directory does not exist")
})

test_that("directory-delete removes directory", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- file.path(tempdir(), "test_dir")
  dir.create(tmp)
  writeLines("test", file.path(tmp, "file.txt"))
  expect_true(dir.exists(tmp))

  # Delete it (recursive = #t by default)
  result <- engine$eval(
    engine$read(sprintf('(directory-delete "%s")', arl_path(tmp)))[[1]], env = env)
  expect_true(result)
  expect_false(dir.exists(tmp))
})

test_that("directory-delete errors on non-existent directory", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  tmp <- file.path(tempdir(), "nonexistent_dir")

  expect_error(
    engine$eval(
      engine$read(sprintf('(directory-delete "%s")', arl_path(tmp)))[[1]], env = env),
    "directory does not exist")
})

# ============================================================================
# Parsing Operations
# ============================================================================

test_that("read parses Arl expressions from strings", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  # Parse simple expression
  result <- engine$eval(
    engine$read('(read "(+ 1 2)")')[[1]], env = env)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")

  # Parse symbol
  result <- engine$eval(
    engine$read('(read "foo")')[[1]], env = env)
  expect_true(is.symbol(result))
  expect_equal(as.character(result), "foo")

  # Parse number
  result <- engine$eval(
    engine$read('(read "42")')[[1]], env = env)
  expect_equal(result, 42)
})

test_that("read-from-string is alias for read", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  result1 <- engine$eval(
    engine$read('(read "(+ 1 2)")')[[1]], env = env)
  result2 <- engine$eval(
    engine$read('(read-from-string "(+ 1 2)")')[[1]], env = env)

  expect_equal(result1, result2)
})

# ============================================================================
# Environment Variables
# ============================================================================

test_that("setenv and getenv work with environment variables", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  # Set variable
  engine$eval(
    engine$read('(setenv "ARL_TEST_VAR" "test_value")')[[1]], env = env)

  # Get it back
  result <- engine$eval(
    engine$read('(getenv "ARL_TEST_VAR")')[[1]], env = env)
  expect_equal(result, "test_value")

  # Clean up
  Sys.unsetenv("ARL_TEST_VAR")
})

test_that("getenv returns #nil for unset variable", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  Sys.unsetenv("ARL_NONEXISTENT_VAR")

  result <- engine$eval(
    engine$read('(getenv "ARL_NONEXISTENT_VAR")')[[1]], env = env)
  expect_null(result)
})

# ============================================================================
# System Operations
# ============================================================================

test_that("system executes commands and returns exit code", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  # Successful command (exit code 0)
  result <- engine$eval(
    engine$read('(system "true")')[[1]], env = env)
  expect_equal(result, 0)

  # Failed command (exit code non-zero)
  result <- engine$eval(
    engine$read('(system "false")')[[1]], env = env)
  expect_true(result != 0)
})

test_that("system-output captures command output", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  result <- engine$eval(
    engine$read('(system-output "echo hello")')[[1]], env = env)

  expect_true(is.character(result))
  expect_match(result, "hello")
})

# ============================================================================
# Coverage: Output functions (write-string, newline, print)
# ============================================================================

test_that("write-string outputs string", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  output <- capture.output(
    result <- engine$eval(engine$read('(write-string "hello")')[[1]], env = env))
  expect_null(result)  # write-string returns #nil
  expect_true(any(grepl("hello", output)))
})

test_that("newline outputs a newline", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  output <- capture.output(
    result <- engine$eval(engine$read("(newline)")[[1]], env = env))
  expect_null(result)  # newline returns #nil
})

test_that("print outputs value and returns it", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("io"), env = env)

  output <- capture.output(
    result <- engine$eval(engine$read("(print 42)")[[1]], env = env))
  expect_equal(result, 42)
  expect_true(any(grepl("42", output)))
})
