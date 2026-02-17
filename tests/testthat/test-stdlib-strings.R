# Comprehensive string operation tests

engine <- make_engine()

test_that("string-slice extracts string portions", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(env$`string-slice`("hello", 1, 4), "ell")
  expect_equal(env$`string-slice`("world", 0, 3), "wor")

  # Full string
  expect_equal(env$`string-slice`("test", 0, 4), "test")
})

test_that("string case conversion works", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(env$`string-upcase`("hello"), "HELLO")
  expect_equal(env$`string-downcase`("HELLO"), "hello")

  # Mixed case
  expect_equal(env$`string-upcase`("HeLLo"), "HELLO")
  expect_equal(env$`string-downcase`("HeLLo"), "hello")

  # Empty string
  expect_equal(env$`string-upcase`(""), "")
})

test_that("char-at and string-ref access characters", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # char-at (0-indexed)
  expect_equal(env$`char-at`("hello", 0), "h")
  expect_equal(env$`char-at`("hello", 4), "o")

  # Out of bounds
  expect_error(env$`char-at`("hi", 5), "out of bounds")
})

test_that("string-length returns character count", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(env$`string-length`("hello"), 5)
  expect_equal(env$`string-length`(""), 0)
  expect_equal(env$`string-length`("a"), 1)
})

test_that("number->string converts with bases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Decimal (default)
  expect_equal(env$`number->string`(42), "42")

  # Hexadecimal
  expect_equal(env$`number->string`(42, 16), "2a")

  # Binary
  expect_equal(env$`number->string`(8, 2), "1000")

  # Octal
  expect_equal(env$`number->string`(64, 8), "100")
})

test_that("string->number parses numbers", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(env$`string->number`("42"), 42)
  expect_equal(env$`string->number`("3.14"), 3.14)
  expect_equal(env$`string->number`("-10"), -10)

  # Invalid string returns #f
  expect_false(env$`string->number`("not-a-number"))
})

test_that("string comparison operators work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # string=?
  expect_true(env$`string=?`("hello", "hello"))
  expect_false(env$`string=?`("hello", "world"))

  # string<?
  expect_true(env$`string<?`("apple", "banana"))
  expect_false(env$`string<?`("banana", "apple"))

  # string>?
  expect_true(env$`string>?`("banana", "apple"))
  expect_false(env$`string>?`("apple", "banana"))

  # string<=?
  expect_true(env$`string<=?`("apple", "apple"))
  expect_true(env$`string<=?`("apple", "banana"))

  # string>=?
  expect_true(env$`string>=?`("banana", "banana"))
  expect_true(env$`string>=?`("banana", "apple"))
})

test_that("string->list and list->string convert between representations", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # string->list
  result <- env$`string->list`("abc")
  expect_equal(length(result), 3)
  expect_equal(result[[1]], "a")
  expect_equal(result[[2]], "b")
  expect_equal(result[[3]], "c")

  # list->string
  result <- env$`list->string`(list("a", "b", "c"))
  expect_equal(result, "abc")

  # Round trip
  original <- "hello"
  chars <- env$`string->list`(original)
  reconstructed <- env$`list->string`(chars)
  expect_equal(reconstructed, original)
})

test_that("string-append concatenates strings", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(env$`string-append`("hello", " ", "world"), "hello world")
  expect_equal(env$`string-append`("a", "b", "c"), "abc")

  # Single string
  expect_equal(env$`string-append`("alone"), "alone")

  # Empty strings
  expect_equal(env$`string-append`("", "test", ""), "test")
})

# Note: string-copy may not be implemented
# test_that("string-copy creates copy of string", {
#   env <- new.env()
#   toplevel_env(engine, env = env)
#
#   original <- "hello"
#   copy <- env$`string-copy`(original)
#   expect_equal(copy, original)
#   expect_equal(copy, "hello")
# })

# ============================================================================
# String Helpers and I/O
# ============================================================================

test_that("string and io helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(env$`string-concat`("a", 1, "b"), "a1b")
  expect_equal(env$`format-value`(list(1, 2, 3)), "(1 2 3)")
  expect_equal(env$`format-value`(quote(f(a, b))), "(f a b)")
  expect_equal(env$`string-join`(list("a", "b", "c"), "-"), "a-b-c")
  expect_equal(env$`string-split`("a-b-c", "-"), list("a", "b", "c"))
  expect_equal(env$trim("  hi "), "hi")
  expect_equal(env$`string-format`("x=%s", "y"), "x=y")

  con <- textConnection("hello")
  old_opts <- options(arl.stdin = con)
  on.exit({
    options(old_opts)
    close(con)
  }, add = TRUE)
  expect_equal(env$`read-line`(), "hello")
})

test_that("string match helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(env$`string-contains?`("hello", "ell"))
  expect_false(env$`string-contains?`("hello", "^ell"))
  expect_true(env$`string-contains?`("hello", "^he", fixed = FALSE))

  expect_true(env$`string-match?`("hello", "^he"))
  expect_false(env$`string-match?`("hello", "ELL"))
  expect_false(env$`string-match?`("hello", "ELL", fixed = TRUE))

  expect_equal(env$`string-find`("hello", "ll"), 2)
  expect_equal(env$`string-find`("hello", "nope"), NULL)
  expect_equal(env$`string-find`("hello", "^he", fixed = FALSE), 0)

  expect_equal(env$`string-replace`("hello", "l", "L"), "heLlo")
  expect_equal(env$`string-replace-all`("hello", "l", "L"), "heLLo")

  # Regex mode via :fixed #f
  expect_equal(env$`string-replace`("abc123", "[0-9]+", "NUM", fixed = FALSE), "abcNUM")
  expect_equal(env$`string-replace-all`("a1b2c3", "[0-9]", "X", fixed = FALSE), "aXbXcX")
})

# ============================================================================
# Edge Cases: String Operations
# ============================================================================

test_that("string operations handle edge cases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # string-concat with no arguments returns empty string
  expect_equal(env$`string-concat`(), "")

  # string-concat with single argument
  expect_equal(env$`string-concat`("hello"), "hello")

  # string-concat with NULL (NULLs are skipped)
  expect_equal(env$`string-concat`(NULL, "world"), "world")

  # string-concat with numbers
  expect_equal(env$`string-concat`(1, 2, 3), "123")

  # string-join with empty list
  expect_equal(env$`string-join`(list(), "-"), "")

  # string-join with single element
  expect_equal(env$`string-join`(list("a"), "-"), "a")

  # string-split with empty string (R's strsplit("", "-", fixed=TRUE) returns character(0))
  result <- env$`string-split`("", "-")
  expect_equal(length(result), 0)
  expect_true(is.list(result))

  # string-split with delimiter not present
  expect_equal(env$`string-split`("hello", "-"), list("hello"))

  # trim with already trimmed string
  expect_equal(env$trim("hello"), "hello")

  # trim with only whitespace
  expect_equal(env$trim("   "), "")
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
