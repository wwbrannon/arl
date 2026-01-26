test_that("parse_cli_args defaults to repl", {
  parsed <- rye:::parse_cli_args(character(0))
  expect_equal(parsed$action, "repl")
  expect_length(parsed$files, 0)
  expect_null(parsed$expr)
  expect_length(parsed$errors, 0)
})

test_that("parse_cli_args handles file and eval", {
  parsed <- rye:::parse_cli_args(c("--file", "script.rye"))
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "script.rye")
  expect_null(parsed$expr)

  parsed <- rye:::parse_cli_args(c("--eval", "(+ 1 2)"))
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")
  expect_length(parsed$files, 0)
})

test_that("parse_cli_args handles help and version", {
  parsed <- rye:::parse_cli_args(c("--help"))
  expect_equal(parsed$action, "help")
  expect_length(parsed$errors, 0)

  parsed <- rye:::parse_cli_args(c("--version"))
  expect_equal(parsed$action, "version")
  expect_length(parsed$errors, 0)
})

test_that("parse_cli_args errors on invalid input", {
  parsed <- rye:::parse_cli_args(c("--file", "a.rye", "--eval", "(+ 1 2)"))
  expect_true(any(grepl("Use only one of --file/files or --eval", parsed$errors)))

  parsed <- rye:::parse_cli_args(c("--unknown"))
  expect_true(any(grepl("Unknown argument", parsed$errors)))
})

test_that("parse_cli_args supports short options", {
  parsed <- rye:::parse_cli_args(c("-f", "a.rye", "-r"))
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, "a.rye")

  parsed <- rye:::parse_cli_args(c("-e", "(+ 1 2)"))
  expect_equal(parsed$action, "eval")
  expect_equal(parsed$expr, "(+ 1 2)")

  parsed <- rye:::parse_cli_args(c("-h"))
  expect_equal(parsed$action, "help")

  parsed <- rye:::parse_cli_args(c("-v"))
  expect_equal(parsed$action, "version")
})

test_that("parse_cli_args treats positional args as files", {
  parsed <- rye:::parse_cli_args(c("a.rye", "b.rye"))
  expect_equal(parsed$action, "file")
  expect_equal(parsed$files, c("a.rye", "b.rye"))
})

test_that("rye_cli executes files in order", {
  file_a <- tempfile(fileext = ".rye")
  file_b <- tempfile(fileext = ".rye")
  writeLines("(define x 2)", file_a)
  writeLines("(+ x 3)", file_b)

  output <- capture.output(
    rye:::rye_cli(c("--file", file_a, file_b))
  )

  expect_true(any(grepl("5", output)))
})

test_that("rye_cli reads from stdin when not a tty", {
  output <- testthat::with_mocked_bindings(
    {
      capture.output(rye:::rye_cli(character(0)))
    },
    cli_isatty = function() FALSE,
    cli_read_stdin = function() "(+ 1 2)",
    rye_repl = function(...) stop("repl should not be called"),
    .env = asNamespace("rye")
  )

  expect_true(any(grepl("3", output)))
})
