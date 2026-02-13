test_that("DocParser parses @description annotations", {
  tmp <- tempfile(fileext = ".arl")
  on.exit(unlink(tmp))
  writeLines(c(
    "(module test-mod",
    "  (export my-fn)",
    "",
    "  ;;' @description Add two numbers.",
    "  ;;' @examples",
    "  ;;' (my-fn 1 2)  ; => 3",
    "  ;;' @seealso sub-fn",
    "  (define my-fn",
    "    (lambda (a b) (+ a b)))",
    ")"
  ), tmp)

  parser <- DocParser$new()
  result <- parser$parse_file(tmp)

  expect_true("my-fn" %in% names(result$functions))
  fn <- result$functions[["my-fn"]]
  expect_equal(fn$description, "Add two numbers.")
  expect_match(fn$examples, "my-fn 1 2")
  expect_equal(fn$seealso, "sub-fn")
})

test_that("DocParser parses @section annotations", {
  tmp <- tempfile(fileext = ".arl")
  on.exit(unlink(tmp))
  writeLines(c(
    "(module test-mod",
    "  (export f1 f2)",
    "",
    "  ;;' @section Math Helpers",
    "",
    "  ;;' @description Double a number.",
    "  (define f1 (lambda (x) (* x 2)))",
    "",
    "  ;;' @description Triple a number.",
    "  (define f2 (lambda (x) (* x 3)))",
    ")"
  ), tmp)

  parser <- DocParser$new()
  result <- parser$parse_file(tmp)

  expect_equal(length(result$sections), 1)
  expect_equal(result$sections[[1]]$name, "Math Helpers")
  expect_equal(result$functions[["f1"]]$section, "Math Helpers")
  expect_equal(result$functions[["f2"]]$section, "Math Helpers")
})

test_that("DocParser parses @note and @signature", {
  tmp <- tempfile(fileext = ".arl")
  on.exit(unlink(tmp))
  writeLines(c(
    "(module test-mod",
    "  (export my-fn)",
    "",
    "  ;;' @signature (my-fn a b)",
    "  ;;' @description A function.",
    "  ;;' @note This is a note.",
    "  (define my-fn (lambda (a b) (+ a b)))",
    ")"
  ), tmp)

  parser <- DocParser$new()
  result <- parser$parse_file(tmp)

  fn <- result$functions[["my-fn"]]
  expect_equal(fn$note, "This is a note.")
  expect_equal(fn$signature, "(my-fn a b)")
})

test_that("DocParser get_exports extracts module exports", {
  tmp <- tempfile(fileext = ".arl")
  on.exit(unlink(tmp))
  writeLines(c(
    "(module test-mod",
    "  (export foo bar baz)",
    "  (define foo 1)",
    "  (define bar 2)",
    "  (define baz 3)",
    ")"
  ), tmp)

  parser <- DocParser$new()
  exports <- parser$get_exports(tmp)

  expect_true("foo" %in% exports)
  expect_true("bar" %in% exports)
  expect_true("baz" %in% exports)
})

test_that("DocParser handles defmacro definitions", {
  tmp <- tempfile(fileext = ".arl")
  on.exit(unlink(tmp))
  writeLines(c(
    "(module test-mod",
    "  (export my-macro)",
    "",
    "  ;;' @description A test macro.",
    "  ;;' @examples",
    "  ;;' (my-macro x)",
    "  (defmacro my-macro (x) x)",
    ")"
  ), tmp)

  parser <- DocParser$new()
  result <- parser$parse_file(tmp)

  expect_true("my-macro" %in% names(result$functions))
  fn <- result$functions[["my-macro"]]
  expect_equal(fn$description, "A test macro.")
  expect_match(fn$examples, "my-macro x")
})

test_that("DocParser skips standalone sections without definitions", {
  tmp <- tempfile(fileext = ".arl")
  on.exit(unlink(tmp))
  writeLines(c(
    "(module test-mod",
    "  (export f1)",
    "",
    "  ;;' @section Standalone Section",
    "  ;;' This section has prose but no following definition.",
    "",
    "  ;;' @description A function.",
    "  (define f1 (lambda () 42))",
    ")"
  ), tmp)

  parser <- DocParser$new()
  result <- parser$parse_file(tmp)

  # Standalone section should be recorded
  expect_true(length(result$sections) >= 1)
  # f1 should still be parsed
  expect_true("f1" %in% names(result$functions))
})

test_that("parse_text parses annotations from string input", {
  code <- "(module str-mod
  (export add)

  ;;' @description Add two numbers together.
  ;;' @examples
  ;;' (add 1 2)  ; => 3
  (define add (lambda (a b) (+ a b)))
)"

  parser <- DocParser$new()
  result <- parser$parse_text(code)

  expect_true("add" %in% names(result$functions))
  fn <- result$functions[["add"]]
  expect_equal(fn$description, "Add two numbers together.")
  expect_match(fn$examples, "add 1 2")
  expect_null(result$file)
})

test_that("string-input modules get annotation-based docs via eval_text", {
  code <- "(module str-ann-mod
  (export greet)

  ;;' @description Create a greeting message.
  ;;' @examples
  ;;' (greet \"world\")  ; => \"hello, world\"
  (define greet (lambda (name) (string-append \"hello, \" name)))
)"

  engine <- make_engine()
  env <- engine$get_env()
  engine$eval_text(code, env = env)
  engine$eval_text("(import str-ann-mod)", env = env)

  fn <- engine$eval_text("greet", env = env)
  doc <- attr(fn, "arl_doc", exact = TRUE)
  expect_false(is.null(doc))
  expect_equal(doc$description, "Create a greeting message.")
  expect_match(doc$examples, "greet")
})

test_that("DocParser parses @internal tag", {
  tmp <- tempfile(fileext = ".arl")
  on.exit(unlink(tmp))
  writeLines(c(
    "(module test-mod",
    "  (export-all)",
    "",
    "  ;;' @internal",
    "  ;;' @description Internal helper.",
    "  (define __helper (lambda (x) x))",
    "",
    "  ;;' @description Public function.",
    "  (define pub-fn (lambda (x) x))",
    ")"
  ), tmp)

  parser <- DocParser$new()
  result <- parser$parse_file(tmp)

  expect_true(result$functions[["__helper"]]$internal)
  expect_false(result$functions[["pub-fn"]]$internal)
})

test_that("DocParser parses @noeval tag", {
  tmp <- tempfile(fileext = ".arl")
  on.exit(unlink(tmp))
  writeLines(c(
    "(module test-mod",
    "  (export io-fn pure-fn)",
    "",
    "  ;;' @noeval",
    "  ;;' @description Reads a file.",
    "  ;;' @examples",
    "  ;;' (io-fn \"test.txt\")",
    "  (define io-fn (lambda (path) path))",
    "",
    "  ;;' @description Pure function.",
    "  ;;' @examples",
    "  ;;' (pure-fn 42)",
    "  (define pure-fn (lambda (x) x))",
    ")"
  ), tmp)

  parser <- DocParser$new()
  result <- parser$parse_file(tmp)

  expect_true(result$functions[["io-fn"]]$noeval)
  expect_false(result$functions[["pure-fn"]]$noeval)
})

test_that("annotation-based docs are available via compiler", {
  tmp <- tempfile(fileext = ".arl")
  on.exit(unlink(tmp))
  writeLines(c(
    "(module test-ann-mod",
    "  (export add)",
    "",
    "  ;;' @description Add two numbers together.",
    "  ;;' @examples",
    "  ;;' (add 1 2)  ; => 3",
    "  ;;' @seealso sub",
    "  (define add",
    "    (lambda (a b) (+ a b)))",
    ")"
  ), tmp)

  engine <- make_engine()
  env <- engine$get_env()

  # Load the module from file (triggers annotation parsing in compiler)
  engine$eval_in_env(engine$read(sprintf('(load "%s")', tmp))[[1]], env)
  engine$eval_in_env(engine$read("(import test-ann-mod)")[[1]], env)

  # The function should have annotation-based arl_doc
  fn <- engine$eval_in_env(engine$read("add")[[1]], env)
  doc <- attr(fn, "arl_doc", exact = TRUE)
  expect_false(is.null(doc))
  expect_equal(doc$description, "Add two numbers together.")
  expect_match(doc$examples, "add 1 2")
  expect_equal(doc$seealso, "sub")
})
