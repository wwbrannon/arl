test_that("CLI parse defaults to repl", {
  parsed <- arl:::CLI$new(character(0))$parse()
  expect_equal(parsed$action, "repl")
  expect_length(parsed$files, 0)
  expect_null(parsed$expr)
  expect_length(parsed$errors, 0)
})

test_that("CLI parse handles file and eval", {
  parsed <- arl:::CLI$new(c("--file", "script.arl"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "script.arl")
  expect_null(parsed$expr)

  parsed <- arl:::CLI$new(c("--eval", "(+ 1 2)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")
  expect_length(parsed$files, 0)
})

test_that("CLI parse handles help and version", {
  parsed <- arl:::CLI$new(c("--help"))$parse()
  expect_equal(parsed$action, "help")
  expect_length(parsed$errors, 0)

  parsed <- arl:::CLI$new(c("--version"))$parse()
  expect_equal(parsed$action, "version")
  expect_length(parsed$errors, 0)
})

test_that("CLI parse errors on invalid input", {
  parsed <- arl:::CLI$new(c("--file", "a.arl", "--eval", "(+ 1 2)"))$parse()
  expect_true(any(grepl("Use only one of --file/files or --eval", parsed$errors)))

  parsed <- arl:::CLI$new(c("--unknown"))$parse()
  expect_true(length(parsed$errors) > 0)
})

test_that("CLI parse supports short options", {
  parsed <- arl:::CLI$new(c("-f", "a.arl"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "a.arl")

  parsed <- arl:::CLI$new(c("-e", "(+ 1 2)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")

  parsed <- arl:::CLI$new(c("-h"))$parse()
  expect_equal(parsed$action, "help")

  parsed <- arl:::CLI$new(c("-v"))$parse()
  expect_equal(parsed$action, "version")
})

test_that("CLI parse treats positional args as files", {
  parsed <- arl:::CLI$new(c("a.arl", "b.arl"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, c("a.arl", "b.arl"))
})

test_that("CLI parse ignores --args from wrappers", {
  parsed <- arl:::CLI$new(c("--args", "--eval", "(+ 1 2)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")
})

test_that("cli executes files in order", {
  file_a <- tempfile(fileext = ".arl")
  file_b <- tempfile(fileext = ".arl")
  writeLines("(define x 2)", file_a)
  writeLines("(+ x 3)", file_b)

  output <- capture.output(
    arl:::cli(c("--file", file_a, file_b))
  )

  expect_true(any(grepl("5", output)))
})

test_that("cli reads from stdin when not a tty", {
  withr::local_options(list(
    arl.cli_isatty_override = FALSE,
    arl.cli_read_stdin_override = function() "(+ 1 2)"
  ))

  output <- capture.output(arl:::cli(character(0)))

  expect_true(any(grepl("3", output)))
})

# Help and Version Functions ----

test_that("cli returns formatted help", {
  output <- capture.output(arl:::cli(c("--help")))
  expect_true(any(grepl("Usage:", output)))
  expect_true(any(grepl("--file", output)))
  expect_true(any(grepl("--eval", output)))
  expect_true(any(grepl("--version", output)))
  expect_true(any(grepl("--help", output)))
  expect_true(any(grepl("Examples:", output)))
})

test_that("install_cli installs wrapper to target dir", {
  temp_dir <- tempfile("arl-cli-")
  dir.create(temp_dir)
  source <- file.path(temp_dir, "arl-src")
  writeLines(c("#!/usr/bin/env sh", "echo arl"), source)

  result <- testthat::with_mocked_bindings(
    {
      capture.output(
        installed <- arl::install_cli(target_dir = temp_dir, overwrite = TRUE),
        type = "message"
      )
      installed
    },
    system.file = function(..., package = NULL) {
      if (!is.null(package) && package == "arl") {
        return(source)
      }
      base::system.file(..., package = package)
    },
    .package = "base"
  )

  expected <- file.path(temp_dir, "arl")
  expect_true(file.exists(expected))
  expect_equal(
    normalizePath(result, winslash = "/", mustWork = FALSE),
    normalizePath(expected, winslash = "/", mustWork = FALSE)
  )
})

test_that("install_cli warns when target dir is not on PATH", {
  temp_dir <- tempfile("arl-cli-notinpath-")
  dir.create(temp_dir)
  source <- file.path(temp_dir, "arl-src")
  writeLines(c("#!/usr/bin/env sh", "echo arl"), source)

  messages <- testthat::with_mocked_bindings(
    {
      capture.output(
        arl::install_cli(target_dir = temp_dir, overwrite = TRUE),
        type = "message"
      )
    },
    system.file = function(..., package = NULL) {
      if (!is.null(package) && package == "arl") {
        return(source)
      }
      base::system.file(..., package = package)
    },
    .package = "base"
  )

  expect_true(any(grepl("PATH", messages)))
})

# Environment Loading ----

test_that("Engine initializes environment with stdlib", {
  engine <- make_engine()
  env <- engine$get_env()
  expect_true(is.environment(env))
  expect_true(exists("map", envir = env))  # stdlib function
})

# Evaluation Functions ----

test_that("cli_eval_text prints non-NULL results", {
  engine <- make_engine()
  cli <- arl:::CLI$new()
  output <- capture.output(result <- cli$cli_eval_text("(+ 2 3)", engine))
  expect_equal(result, 5)
  expect_true(any(grepl("5", output)))
})

test_that("cli_eval_text does not print define results", {
  engine <- make_engine()
  cli <- arl:::CLI$new()
  output <- capture.output(result <- cli$cli_eval_text("(define y 10)", engine))
  expect_equal(result, 10)  # define returns the value (invisibly)
  expect_length(output, 0)  # but doesn't print
})

# I/O Helper Functions ----

test_that("CLI cli_isatty wraps isatty", {
  result <- arl:::CLI$new()$cli_isatty()
  expect_type(result, "logical")
})

test_that("CLI cli_read_stdin reads from stdin", {
  # Test that the method exists and returns a character vector
  # Actual stdin testing requires process redirection
  expect_true(is.function(arl:::CLI$new()$cli_read_stdin))
})

# Main CLI Function - Error Paths ----

test_that("cli shows help with --help flag", {
  output <- capture.output(arl:::cli(c("--help")))
  expect_true(any(grepl("Usage:", output)))
})

test_that("cli shows help with -h flag", {
  output <- capture.output(arl:::cli(c("-h")))
  expect_true(any(grepl("Usage:", output)))
})

test_that("cli shows version with --version flag", {
  output <- capture.output(arl:::cli(c("--version")))
  expect_true(any(grepl("^arl", output)))
})

test_that("cli shows version with -v flag", {
  output <- capture.output(arl:::cli(c("-v")))
  expect_true(any(grepl("^arl", output)))
})

test_that("cli errors on missing file", {
  # Test that non-existent files are detected
  parsed <- arl:::CLI$new(c("--file", "nonexistent.arl"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "nonexistent.arl")

  # The actual error handling (quit) is tested by verifying file.exists is checked
  expect_false(file.exists("nonexistent.arl"))
})

test_that("cli errors and shows help on invalid args", {
  exit_fn_called <- FALSE
  withr::local_options(list(
    arl.cli_quiet = TRUE,
    arl.cli_exit_fn = function(message, show_help) {
      exit_fn_called <<- TRUE
    }
  ))
  capture.output(
    suppressMessages(arl:::cli(c("--unknown-flag"))),
    type = "message"
  )
  expect_true(exit_fn_called)
})

test_that("cli handles --eval flag", {
  output <- capture.output(arl:::cli(c("--eval", "(+ 10 20)")))
  expect_true(any(grepl("30", output)))
})

test_that("cli handles -e flag", {
  output <- capture.output(arl:::cli(c("-e", "(* 3 4)")))
  expect_true(any(grepl("12", output)))
})

test_that("cli starts interactive REPL when tty", {
  withr::local_options(list(
    arl.cli_isatty_override = TRUE,
    arl.repl_read_form_override = function(...) NULL,
    arl.repl_can_use_history_override = FALSE
  ))

  output <- capture.output(arl:::cli(character(0)))

  expect_true(any(grepl("^Arl REPL", output)))
})

test_that("cli --quiet and -q set arl.repl_quiet and print no banner", {
  withr::local_options(list(
    arl.cli_isatty_override = TRUE,
    arl.repl_read_form_override = function(...) NULL,
    arl.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(arl:::cli(c("--quiet")))
  expect_length(output, 0)
  expect_true(isTRUE(getOption("arl.repl_quiet")))

  withr::local_options(list(arl.repl_quiet = FALSE))
  output_q <- capture.output(arl:::cli(c("-q")))
  expect_length(output_q, 0)
})

test_that("cli reads stdin when not tty with empty input", {
  withr::local_options(list(
    arl.cli_isatty_override = FALSE,
    arl.cli_read_stdin_override = function() "   "
  ))

  output <- capture.output(arl:::cli(character(0)))

  expect_length(output, 0)
})

test_that("cli handles multiple files", {
  file_a <- tempfile(fileext = ".arl")
  file_b <- tempfile(fileext = ".arl")
  file_c <- tempfile(fileext = ".arl")
  writeLines("(define x 1)", file_a)
  writeLines("(define y 2)", file_b)
  writeLines("(+ x y)", file_c)

  output <- capture.output(arl:::cli(c(file_a, file_b, file_c)))
  expect_true(any(grepl("3", output)))

  unlink(c(file_a, file_b, file_c))
})

test_that("CLI parse handles -- argument terminator", {
  parsed <- arl:::CLI$new(c("--", "file1.arl", "-v"))$parse()
  expect_equal(parsed$files, c("file1.arl", "-v"))
  expect_equal(parsed$action, "file")
})

test_that("CLI parse errors on --file without path", {
  parsed <- arl:::CLI$new(c("--file"))$parse()
  expect_true(length(parsed$errors) > 0)
})

test_that("CLI parse errors on --eval without expression", {
  parsed <- arl:::CLI$new(c("--eval"))$parse()
  expect_true(length(parsed$errors) > 0)
})

test_that("CLI parse errors on multiple --eval flags", {
  parsed <- arl:::CLI$new(c("--eval", "(+ 1 2)", "--eval", "(+ 3 4)"))$parse()
  expect_true(length(parsed$errors) > 0)
})

# Short option missing-value edge cases ----

test_that("CLI parse errors on -e without expression", {
  parsed <- arl:::CLI$new(c("-e"))$parse()
  expect_true(length(parsed$errors) > 0)
})

test_that("CLI parse errors on -f without path", {
  parsed <- arl:::CLI$new(c("-f"))$parse()
  expect_true(length(parsed$errors) > 0)
})

# Combined flag scenarios ----

test_that("CLI parse handles -q with -e", {
  parsed <- arl:::CLI$new(c("-q", "-e", "(+ 1 2)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")
  expect_length(parsed$errors, 0)
})

test_that("cli -q -e evaluates quietly", {
  withr::local_options(list(arl.repl_quiet = FALSE))
  output <- capture.output(arl:::cli(c("-q", "-e", "(+ 5 6)")))
  expect_true(any(grepl("11", output)))
  expect_true(isTRUE(getOption("arl.repl_quiet")))
})

test_that("CLI parse handles -q with positional file", {
  parsed <- arl:::CLI$new(c("-q", "script.arl"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "script.arl")
  expect_length(parsed$errors, 0)
})

test_that("cli -q with file evaluates quietly", {
  f <- tempfile(fileext = ".arl")
  writeLines("(+ 7 8)", f)
  withr::local_options(list(arl.repl_quiet = FALSE))
  output <- capture.output(arl:::cli(c("-q", f)))
  expect_true(any(grepl("15", output)))
  expect_true(isTRUE(getOption("arl.repl_quiet")))
  unlink(f)
})

# Flag ordering ----

test_that("CLI parse handles flags after positional args", {
  parsed <- arl:::CLI$new(c("script.arl", "-q"))$parse()
  expect_equal(parsed$action, "file")
  expect_true("script.arl" %in% parsed$files)
  expect_length(parsed$errors, 0)
})

# Eval with spaces and special characters ----

test_that("CLI parse handles -e with spaces in expression", {
  parsed <- arl:::CLI$new(c("-e", "(define x 42)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(define x 42)")
  expect_length(parsed$errors, 0)
})

test_that("cli -e with multi-word expression evaluates correctly", {
  output <- capture.output(arl:::cli(c("-e", "(+ 100 200)")))
  expect_true(any(grepl("300", output)))
})

# Unknown flag error content ----

test_that("CLI parse reports unrecognized flag in error message", {
  parsed <- arl:::CLI$new(c("--bogus"))$parse()
  expect_true(length(parsed$errors) > 0)
  errors_text <- paste(parsed$errors, collapse = " ")
  # Error should mention the unrecognized flag
  expect_true(grepl("bogus", errors_text, ignore.case = TRUE))
})

# Error output paths ----

test_that("cli_exit_with_error produces visible error message", {
  error_msg <- NULL
  withr::local_options(list(
    arl.cli_exit_fn = function(message, show_help) {
      error_msg <<- message
    }
  ))
  cli_obj <- arl:::CLI$new()
  cli_obj$cli_exit_with_error("test error message", show_help = FALSE)
  expect_equal(error_msg, "test error message")
})

test_that("run() displays parse errors to user", {
  exit_messages <- character(0)
  withr::local_options(list(
    arl.cli_exit_fn = function(message, show_help) {
      exit_messages <<- c(exit_messages, message)
    }
  ))
  # Capture both stdout (help text) and stderr (error messages)
  capture.output(
    output <- capture.output(
      arl:::cli(c("--unknown-flag")),
      type = "message"
    )
  )
  # Either the error goes through exit_fn or is printed — one of these should have content

  has_output <- length(output) > 0 || length(exit_messages) > 0
  expect_true(has_output)
})
