# Comprehensive string operation tests

engine <- make_engine()

test_that("string-slice extracts string portions", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("string-slice", envir = env)("hello", 1, 4), "ell")
  expect_equal(get("string-slice", envir = env)("world", 0, 3), "wor")

  # Full string
  expect_equal(get("string-slice", envir = env)("test", 0, 4), "test")
})

test_that("string case conversion works", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("string-upcase", envir = env)("hello"), "HELLO")
  expect_equal(get("string-downcase", envir = env)("HELLO"), "hello")

  # Mixed case
  expect_equal(get("string-upcase", envir = env)("HeLLo"), "HELLO")
  expect_equal(get("string-downcase", envir = env)("HeLLo"), "hello")

  # Empty string
  expect_equal(get("string-upcase", envir = env)(""), "")
})

test_that("char-at and string-ref access characters", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # char-at (0-indexed)
  expect_equal(get("char-at", envir = env)("hello", 0), "h")
  expect_equal(get("char-at", envir = env)("hello", 4), "o")

  # Out of bounds
  expect_error(get("char-at", envir = env)("hi", 5), "out of bounds")
})

test_that("string-length returns character count", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("string-length", envir = env)("hello"), 5)
  expect_equal(get("string-length", envir = env)(""), 0)
  expect_equal(get("string-length", envir = env)("a"), 1)
})

test_that("number->string converts with bases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Decimal (default)
  expect_equal(get("number->string", envir = env)(42), "42")

  # Hexadecimal
  expect_equal(get("number->string", envir = env)(42, 16), "2a")

  # Binary
  expect_equal(get("number->string", envir = env)(8, 2), "1000")

  # Octal
  expect_equal(get("number->string", envir = env)(64, 8), "100")
})

test_that("string->number parses numbers", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("string->number", envir = env)("42"), 42)
  expect_equal(get("string->number", envir = env)("3.14"), 3.14)
  expect_equal(get("string->number", envir = env)("-10"), -10)

  # Invalid string returns #f
  expect_false(get("string->number", envir = env)("not-a-number"))
})

test_that("string comparison operators work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # string=?
  expect_true(get("string=?", envir = env)("hello", "hello"))
  expect_false(get("string=?", envir = env)("hello", "world"))

  # string<?
  expect_true(get("string<?", envir = env)("apple", "banana"))
  expect_false(get("string<?", envir = env)("banana", "apple"))

  # string>?
  expect_true(get("string>?", envir = env)("banana", "apple"))
  expect_false(get("string>?", envir = env)("apple", "banana"))

  # string<=?
  expect_true(get("string<=?", envir = env)("apple", "apple"))
  expect_true(get("string<=?", envir = env)("apple", "banana"))

  # string>=?
  expect_true(get("string>=?", envir = env)("banana", "banana"))
  expect_true(get("string>=?", envir = env)("banana", "apple"))
})

test_that("string->list and list->string convert between representations", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # string->list
  result <- get("string->list", envir = env)("abc")
  expect_equal(length(result), 3)
  expect_equal(result[[1]], "a")
  expect_equal(result[[2]], "b")
  expect_equal(result[[3]], "c")

  # list->string
  result <- get("list->string", envir = env)(list("a", "b", "c"))
  expect_equal(result, "abc")

  # Round trip
  original <- "hello"
  chars <- get("string->list", envir = env)(original)
  reconstructed <- get("list->string", envir = env)(chars)
  expect_equal(reconstructed, original)
})

test_that("string-append concatenates strings", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("string-append", envir = env)("hello", " ", "world"), "hello world")
  expect_equal(get("string-append", envir = env)("a", "b", "c"), "abc")

  # Single string
  expect_equal(get("string-append", envir = env)("alone"), "alone")

  # Empty strings
  expect_equal(get("string-append", envir = env)("", "test", ""), "test")
})

# Note: string-copy may not be implemented
# test_that("string-copy creates copy of string", {
#   env <- new.env()
#   toplevel_env(engine, env = env)
#
#   original <- "hello"
#   copy <- get("string-copy", envir = env)(original)
#   expect_equal(copy, original)
#   expect_equal(copy, "hello")
# })

# ============================================================================
# String Helpers and I/O
# ============================================================================

test_that("string and io helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("string-concat", envir = env)("a", 1, "b"), "a1b")
  expect_equal(get("format-value", envir = env)(list(1, 2, 3)), "(1 2 3)")
  expect_equal(get("format-value", envir = env)(quote(f(a, b))), "(f a b)")
  expect_equal(get("string-join", envir = env)(list("a", "b", "c"), "-"), "a-b-c")
  expect_equal(get("string-split", envir = env)("a-b-c", "-"), list("a", "b", "c"))
  expect_equal(get("trim", envir = env)("  hi "), "hi")
  expect_equal(get("string-format", envir = env)("x=%s", "y"), "x=y")

  con <- textConnection("hello")
  old_opts <- options(arl.stdin = con)
  on.exit({
    options(old_opts)
    close(con)
  }, add = TRUE)
  expect_equal(get("read-line", envir = env)(), "hello")
})

test_that("string match helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(get("string-contains?", envir = env)("hello", "ell"))
  expect_false(get("string-contains?", envir = env)("hello", "^ell"))
  expect_true(get("string-contains?", envir = env)("hello", "^he", fixed = FALSE))

  expect_true(get("string-match?", envir = env)("hello", "^he"))
  expect_false(get("string-match?", envir = env)("hello", "ELL"))
  expect_false(get("string-match?", envir = env)("hello", "ELL", fixed = TRUE))

  expect_equal(get("string-find", envir = env)("hello", "ll"), 2)
  expect_equal(get("string-find", envir = env)("hello", "nope"), NULL)
  expect_equal(get("string-find", envir = env)("hello", "^he", fixed = FALSE), 0)

  expect_equal(get("string-replace", envir = env)("hello", "l", "L"), "heLlo")
  expect_equal(get("string-replace-all", envir = env)("hello", "l", "L"), "heLLo")

  # Regex mode via :fixed #f
  expect_equal(get("string-replace", envir = env)("abc123", "[0-9]+", "NUM", fixed = FALSE), "abcNUM")
  expect_equal(get("string-replace-all", envir = env)("a1b2c3", "[0-9]", "X", fixed = FALSE), "aXbXcX")
})

# ============================================================================
# Edge Cases: String Operations
# ============================================================================

test_that("string operations handle edge cases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # string-concat with no arguments returns empty string
  expect_equal(get("string-concat", envir = env)(), "")

  # string-concat with single argument
  expect_equal(get("string-concat", envir = env)("hello"), "hello")

  # string-concat with NULL (NULLs are skipped)
  expect_equal(get("string-concat", envir = env)(NULL, "world"), "world")

  # string-concat with numbers
  expect_equal(get("string-concat", envir = env)(1, 2, 3), "123")

  # string-join with empty list
  expect_equal(get("string-join", envir = env)(list(), "-"), "")

  # string-join with single element
  expect_equal(get("string-join", envir = env)(list("a"), "-"), "a")

  # string-split with empty string (R's strsplit("", "-", fixed=TRUE) returns character(0))
  result <- get("string-split", envir = env)("", "-")
  expect_equal(length(result), 0)
  expect_true(is.list(result))

  # string-split with delimiter not present
  expect_equal(get("string-split", envir = env)("hello", "-"), list("hello"))

  # trim with already trimmed string
  expect_equal(get("trim", envir = env)("hello"), "hello")

  # trim with only whitespace
  expect_equal(get("trim", envir = env)("   "), "")
})

# ============================================================================
# Coverage: string-find not found, substring negative start, number->string base error
# ============================================================================

test_that("string-find returns #nil when pattern not found", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read('(string-find "hello" "xyz")')[[1]], env = env)
  expect_null(result)
})

test_that("string-slice errors on negative start", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_error(
    engine$eval(engine$read('(string-slice "hello" -1 3)')[[1]], env = env),
    "start index cannot be negative")
})

test_that("number->string errors on base out of range", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_error(
    engine$eval(engine$read("(number->string 10 1)")[[1]], env = env),
    "base must be between 2 and 36")

  expect_error(
    engine$eval(engine$read("(number->string 10 37)")[[1]], env = env),
    "base must be between 2 and 36")
})

# ============================================================================
# string-prefix?
# ============================================================================

test_that("string-prefix? checks string prefix", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read('(string-prefix? "he" "hello")')[[1]], env = env)
  expect_true(result)

  result <- engine$eval(
    engine$read('(string-prefix? "wo" "hello")')[[1]], env = env)
  expect_false(result)

  # Empty prefix always matches
  result <- engine$eval(
    engine$read('(string-prefix? "" "hello")')[[1]], env = env)
  expect_true(result)

  # Exact match
  result <- engine$eval(
    engine$read('(string-prefix? "hello" "hello")')[[1]], env = env)
  expect_true(result)

  # Prefix longer than string
  result <- engine$eval(
    engine$read('(string-prefix? "hello world" "hello")')[[1]], env = env)
  expect_false(result)
})

# ============================================================================
# string-suffix?
# ============================================================================

test_that("string-suffix? checks string suffix", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read('(string-suffix? "lo" "hello")')[[1]], env = env)
  expect_true(result)

  result <- engine$eval(
    engine$read('(string-suffix? "he" "hello")')[[1]], env = env)
  expect_false(result)

  # Empty suffix always matches
  result <- engine$eval(
    engine$read('(string-suffix? "" "hello")')[[1]], env = env)
  expect_true(result)

  # Exact match
  result <- engine$eval(
    engine$read('(string-suffix? "hello" "hello")')[[1]], env = env)
  expect_true(result)
})

# ============================================================================
# string-empty?
# ============================================================================

test_that("string-empty? checks for empty string", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read('(string-empty? "")')[[1]], env = env)
  expect_true(result)

  result <- engine$eval(
    engine$read('(string-empty? "a")')[[1]], env = env)
  expect_false(result)

  result <- engine$eval(
    engine$read('(string-empty? "hello")')[[1]], env = env)
  expect_false(result)

  # Whitespace is not empty
  result <- engine$eval(
    engine$read('(string-empty? " ")')[[1]], env = env)
  expect_false(result)
})

# ============================================================================
# string-repeat
# ============================================================================

test_that("string-repeat repeats string n times", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read('(string-repeat "ab" 3)')[[1]], env = env)
  expect_equal(result, "ababab")

  # 0 repetitions
  result <- engine$eval(
    engine$read('(string-repeat "hello" 0)')[[1]], env = env)
  expect_equal(result, "")

  # 1 repetition
  result <- engine$eval(
    engine$read('(string-repeat "hello" 1)')[[1]], env = env)
  expect_equal(result, "hello")

  # Empty string
  result <- engine$eval(
    engine$read('(string-repeat "" 5)')[[1]], env = env)
  expect_equal(result, "")
})
