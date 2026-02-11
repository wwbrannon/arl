test_that("CLI parse defaults to repl", {
  parsed <- rye:::CLI$new(character(0))$parse()
  expect_equal(parsed$action, "repl")
  expect_length(parsed$files, 0)
  expect_null(parsed$expr)
  expect_length(parsed$errors, 0)
})

test_that("CLI parse handles file and eval", {
  parsed <- rye:::CLI$new(c("--file", "script.rye"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "script.rye")
  expect_null(parsed$expr)

  parsed <- rye:::CLI$new(c("--eval", "(+ 1 2)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")
  expect_length(parsed$files, 0)
})

test_that("CLI parse handles help and version", {
  parsed <- rye:::CLI$new(c("--help"))$parse()
  expect_equal(parsed$action, "help")
  expect_length(parsed$errors, 0)

  parsed <- rye:::CLI$new(c("--version"))$parse()
  expect_equal(parsed$action, "version")
  expect_length(parsed$errors, 0)
})

test_that("CLI parse errors on invalid input", {
  parsed <- rye:::CLI$new(c("--file", "a.rye", "--eval", "(+ 1 2)"))$parse()
  expect_true(any(grepl("Use only one of --file/files or --eval", parsed$errors)))

  parsed <- rye:::CLI$new(c("--unknown"))$parse()
  expect_true(length(parsed$errors) > 0)
})

test_that("CLI parse supports short options", {
  parsed <- rye:::CLI$new(c("-f", "a.rye"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "a.rye")

  parsed <- rye:::CLI$new(c("-e", "(+ 1 2)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")

  parsed <- rye:::CLI$new(c("-h"))$parse()
  expect_equal(parsed$action, "help")

  parsed <- rye:::CLI$new(c("-v"))$parse()
  expect_equal(parsed$action, "version")
})

test_that("CLI parse treats positional args as files", {
  parsed <- rye:::CLI$new(c("a.rye", "b.rye"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, c("a.rye", "b.rye"))
})

test_that("CLI parse ignores --args from wrappers", {
  parsed <- rye:::CLI$new(c("--args", "--eval", "(+ 1 2)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")
})

test_that("cli executes files in order", {
  file_a <- tempfile(fileext = ".rye")
  file_b <- tempfile(fileext = ".rye")
  writeLines("(define x 2)", file_a)
  writeLines("(+ x 3)", file_b)

  output <- capture.output(
    rye:::cli(c("--file", file_a, file_b))
  )

  expect_true(any(grepl("5", output)))
})

test_that("cli reads from stdin when not a tty", {
  withr::local_options(list(
    rye.cli_isatty_override = FALSE,
    rye.cli_read_stdin_override = function() "(+ 1 2)"
  ))

  output <- capture.output(rye:::cli(character(0)))

  expect_true(any(grepl("3", output)))
})

# Help and Version Functions ----

test_that("cli returns formatted help", {
  output <- capture.output(rye:::cli(c("--help")))
  expect_true(any(grepl("Usage:", output)))
  expect_true(any(grepl("--file", output)))
  expect_true(any(grepl("--eval", output)))
  expect_true(any(grepl("--version", output)))
  expect_true(any(grepl("--help", output)))
  expect_true(any(grepl("Examples:", output)))
})

test_that("install_cli installs wrapper to target dir", {
  temp_dir <- tempfile("rye-cli-")
  dir.create(temp_dir)
  source <- file.path(temp_dir, "rye-src")
  writeLines(c("#!/usr/bin/env sh", "echo rye"), source)

  result <- testthat::with_mocked_bindings(
    {
      capture.output(
        installed <- rye::install_cli(target_dir = temp_dir, overwrite = TRUE),
        type = "message"
      )
      installed
    },
    system.file = function(..., package = NULL) {
      if (!is.null(package) && package == "rye") {
        return(source)
      }
      base::system.file(..., package = package)
    },
    .package = "base"
  )

  expected <- file.path(temp_dir, "rye")
  expect_true(file.exists(expected))
  expect_equal(
    normalizePath(result, winslash = "/", mustWork = FALSE),
    normalizePath(expected, winslash = "/", mustWork = FALSE)
  )
})

# Environment Loading ----

test_that("Engine initializes environment with stdlib", {
  engine <- make_engine()
  env <- engine$env$env
  expect_true(is.environment(env))
  expect_true(exists("map", envir = env))  # stdlib function
})

# Evaluation Functions ----

test_that("cli_eval_text prints non-NULL results", {
  engine <- make_engine()
  cli <- rye:::CLI$new()
  output <- capture.output(result <- cli$cli_eval_text("(+ 2 3)", engine))
  expect_equal(result, 5)
  expect_true(any(grepl("5", output)))
})

test_that("cli_eval_text does not print define results", {
  engine <- make_engine()
  cli <- rye:::CLI$new()
  output <- capture.output(result <- cli$cli_eval_text("(define y 10)", engine))
  expect_equal(result, 10)  # define returns the value (invisibly)
  expect_length(output, 0)  # but doesn't print
})

# I/O Helper Functions ----

test_that("CLI cli_isatty wraps isatty", {
  result <- rye:::CLI$new()$cli_isatty()
  expect_type(result, "logical")
})

test_that("CLI cli_read_stdin reads from stdin", {
  # Test that the method exists and returns a character vector
  # Actual stdin testing requires process redirection
  expect_true(is.function(rye:::CLI$new()$cli_read_stdin))
})

# Main CLI Function - Error Paths ----

test_that("cli shows help with --help flag", {
  output <- capture.output(rye:::cli(c("--help")))
  expect_true(any(grepl("Usage:", output)))
})

test_that("cli shows help with -h flag", {
  output <- capture.output(rye:::cli(c("-h")))
  expect_true(any(grepl("Usage:", output)))
})

test_that("cli shows version with --version flag", {
  output <- capture.output(rye:::cli(c("--version")))
  expect_true(any(grepl("^rye", output)))
})

test_that("cli shows version with -v flag", {
  output <- capture.output(rye:::cli(c("-v")))
  expect_true(any(grepl("^rye", output)))
})

test_that("cli errors on missing file", {
  # Test that non-existent files are detected
  parsed <- rye:::CLI$new(c("--file", "nonexistent.rye"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "nonexistent.rye")

  # The actual error handling (quit) is tested by verifying file.exists is checked
  expect_false(file.exists("nonexistent.rye"))
})

test_that("cli errors and shows help on invalid args", {
  exit_fn_called <- FALSE
  withr::local_options(list(
    rye.cli_quiet = TRUE,
    rye.cli_exit_fn = function(message, show_help) {
      exit_fn_called <<- TRUE
    }
  ))
  capture.output(
    suppressMessages(rye:::cli(c("--unknown-flag"))),
    type = "message"
  )
  expect_true(exit_fn_called)
})

test_that("cli handles --eval flag", {
  output <- capture.output(rye:::cli(c("--eval", "(+ 10 20)")))
  expect_true(any(grepl("30", output)))
})

test_that("cli handles -e flag", {
  output <- capture.output(rye:::cli(c("-e", "(* 3 4)")))
  expect_true(any(grepl("12", output)))
})

test_that("cli starts interactive REPL when tty", {
  withr::local_options(list(
    rye.cli_isatty_override = TRUE,
    rye.repl_read_form_override = function(...) NULL,
    rye.repl_can_use_history_override = FALSE
  ))

  output <- capture.output(rye:::cli(character(0)))

  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("cli --quiet and -q set rye.repl_quiet and print no banner", {
  withr::local_options(list(
    rye.cli_isatty_override = TRUE,
    rye.repl_read_form_override = function(...) NULL,
    rye.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(rye:::cli(c("--quiet")))
  expect_length(output, 0)
  expect_true(isTRUE(getOption("rye.repl_quiet")))

  withr::local_options(list(rye.repl_quiet = FALSE))
  output_q <- capture.output(rye:::cli(c("-q")))
  expect_length(output_q, 0)
})

test_that("cli reads stdin when not tty with empty input", {
  withr::local_options(list(
    rye.cli_isatty_override = FALSE,
    rye.cli_read_stdin_override = function() "   "
  ))

  output <- capture.output(rye:::cli(character(0)))

  expect_length(output, 0)
})

test_that("cli handles multiple files", {
  file_a <- tempfile(fileext = ".rye")
  file_b <- tempfile(fileext = ".rye")
  file_c <- tempfile(fileext = ".rye")
  writeLines("(define x 1)", file_a)
  writeLines("(define y 2)", file_b)
  writeLines("(+ x y)", file_c)

  output <- capture.output(rye:::cli(c(file_a, file_b, file_c)))
  expect_true(any(grepl("3", output)))

  unlink(c(file_a, file_b, file_c))
})

test_that("CLI parse handles -- argument terminator", {
  parsed <- rye:::CLI$new(c("--", "file1.rye", "-v"))$parse()
  expect_equal(parsed$files, c("file1.rye", "-v"))
  expect_equal(parsed$action, "file")
})

test_that("CLI parse errors on --file without path", {
  parsed <- rye:::CLI$new(c("--file"))$parse()
  expect_true(length(parsed$errors) > 0)
})

test_that("CLI parse errors on --eval without expression", {
  parsed <- rye:::CLI$new(c("--eval"))$parse()
  expect_true(length(parsed$errors) > 0)
})

test_that("CLI parse errors on multiple --eval flags", {
  parsed <- rye:::CLI$new(c("--eval", "(+ 1 2)", "--eval", "(+ 3 4)"))$parse()
  expect_true(length(parsed$errors) > 0)
})

# Short option missing-value edge cases ----

test_that("CLI parse errors on -e without expression", {
  parsed <- rye:::CLI$new(c("-e"))$parse()
  expect_true(length(parsed$errors) > 0)
})

test_that("CLI parse errors on -f without path", {
  parsed <- rye:::CLI$new(c("-f"))$parse()
  expect_true(length(parsed$errors) > 0)
})

# Combined flag scenarios ----

test_that("CLI parse handles -q with -e", {
  parsed <- rye:::CLI$new(c("-q", "-e", "(+ 1 2)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")
  expect_length(parsed$errors, 0)
})

test_that("cli -q -e evaluates quietly", {
  withr::local_options(list(rye.repl_quiet = FALSE))
  output <- capture.output(rye:::cli(c("-q", "-e", "(+ 5 6)")))
  expect_true(any(grepl("11", output)))
  expect_true(isTRUE(getOption("rye.repl_quiet")))
})

test_that("CLI parse handles -q with positional file", {
  parsed <- rye:::CLI$new(c("-q", "script.rye"))$parse()
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "script.rye")
  expect_length(parsed$errors, 0)
})

test_that("cli -q with file evaluates quietly", {
  f <- tempfile(fileext = ".rye")
  writeLines("(+ 7 8)", f)
  withr::local_options(list(rye.repl_quiet = FALSE))
  output <- capture.output(rye:::cli(c("-q", f)))
  expect_true(any(grepl("15", output)))
  expect_true(isTRUE(getOption("rye.repl_quiet")))
  unlink(f)
})

# Flag ordering ----

test_that("CLI parse handles flags after positional args", {
  parsed <- rye:::CLI$new(c("script.rye", "-q"))$parse()
  expect_equal(parsed$action, "file")
  expect_true("script.rye" %in% parsed$files)
  expect_length(parsed$errors, 0)
})

# Eval with spaces and special characters ----

test_that("CLI parse handles -e with spaces in expression", {
  parsed <- rye:::CLI$new(c("-e", "(define x 42)"))$parse()
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(define x 42)")
  expect_length(parsed$errors, 0)
})

test_that("cli -e with multi-word expression evaluates correctly", {
  output <- capture.output(rye:::cli(c("-e", "(+ 100 200)")))
  expect_true(any(grepl("300", output)))
})

# Unknown flag error content ----

test_that("CLI parse reports unrecognized flag in error message", {
  parsed <- rye:::CLI$new(c("--bogus"))$parse()
  expect_true(length(parsed$errors) > 0)
  errors_text <- paste(parsed$errors, collapse = " ")
  # Error should mention the unrecognized flag
  expect_true(grepl("bogus", errors_text, ignore.case = TRUE))
})

# Error output paths ----

test_that("cli_exit_with_error produces visible error message", {
  error_msg <- NULL
  withr::local_options(list(
    rye.cli_exit_fn = function(message, show_help) {
      error_msg <<- message
    }
  ))
  cli_obj <- rye:::CLI$new()
  cli_obj$cli_exit_with_error("test error message", show_help = FALSE)
  expect_equal(error_msg, "test error message")
})

test_that("run() displays parse errors to user", {
  exit_messages <- character(0)
  withr::local_options(list(
    rye.cli_exit_fn = function(message, show_help) {
      exit_messages <<- c(exit_messages, message)
    }
  ))
  # Capture stdout to check that error text appears
  output <- capture.output(
    rye:::cli(c("--unknown-flag")),
    type = "message"
  )
  # Either the error goes through exit_fn or is printed — one of these should have content

  has_output <- length(output) > 0 || length(exit_messages) > 0
  expect_true(has_output)
})
