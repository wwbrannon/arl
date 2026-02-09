# Comprehensive string operation tests

engine <- make_engine()

test_that("substring extracts string portions", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$substring("hello", 1, 4), "ell")
  expect_equal(env$substring("world", 0, 3), "wor")

  # Full string
  expect_equal(env$substring("test", 0, 4), "test")
})

test_that("string case conversion works", {
  env <- new.env()
  stdlib_env(engine, env)

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
  stdlib_env(engine, env)

  # char-at (0-indexed)
  expect_equal(env$`char-at`("hello", 0), "h")
  expect_equal(env$`char-at`("hello", 4), "o")

  # Out of bounds
  expect_error(env$`char-at`("hi", 5), "out of bounds")
})

test_that("string-length returns character count", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$`string-length`("hello"), 5)
  expect_equal(env$`string-length`(""), 0)
  expect_equal(env$`string-length`("a"), 1)
})

test_that("number->string converts with bases", {
  env <- new.env()
  stdlib_env(engine, env)

  # Decimal (default)
  expect_equal(env$`number->string`(42), "42")

  # Hexadecimal
  expect_equal(env$`number->string`(42, 16), "2a")

  # Binary
  expect_equal(env$`number->string`(8, 2), "1000")

  # Octal - returns octmode object, convert to string
  result <- env$`number->string`(64, 8)
  expect_equal(as.character(result), "100")
})

test_that("string->number parses numbers", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$`string->number`("42"), 42)
  expect_equal(env$`string->number`("3.14"), 3.14)
  expect_equal(env$`string->number`("-10"), -10)

  # Invalid string returns #f
  expect_false(env$`string->number`("not-a-number"))
})

test_that("string comparison operators work", {
  env <- new.env()
  stdlib_env(engine, env)

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
  stdlib_env(engine, env)

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
  stdlib_env(engine, env)

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
#   stdlib_env(engine, env)
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
  stdlib_env(engine, env)

  expect_equal(env$str("a", 1, "b"), "a1b")
  expect_equal(env$`format-value`(list(1, 2, 3)), "1 2 3")
  expect_equal(env$`format-value`(quote(f(a, b))), "f a b")
  expect_equal(env$`string-join`(list("a", "b", "c"), "-"), "a-b-c")
  expect_equal(env$`string-split`("a-b-c", "-"), c("a", "b", "c"))
  expect_equal(env$trim("  hi "), "hi")
  expect_equal(env$format("x=%s", "y"), "x=y")

  con <- textConnection("hello")
  old_opts <- options(rye.stdin = con)
  on.exit({
    options(old_opts)
    close(con)
  }, add = TRUE)
  expect_equal(env$`read-line`(), "hello")
})

test_that("string match helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

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
})

# ============================================================================
# Edge Cases: String Operations
# ============================================================================

test_that("string operations handle edge cases", {
  env <- new.env()
  stdlib_env(engine, env)

  # str with no arguments (returns character(0), not empty string)
  expect_equal(length(env$str()), 0)
  expect_true(is.character(env$str()))

  # str with single argument
  expect_equal(env$str("hello"), "hello")

  # str with NULL (NULLs are skipped)
  expect_equal(env$str(NULL, "world"), "world")

  # str with numbers
  expect_equal(env$str(1, 2, 3), "123")

  # string-join with empty list
  expect_equal(env$`string-join`(list(), "-"), "")

  # string-join with single element
  expect_equal(env$`string-join`(list("a"), "-"), "a")

  # string-split with empty string (returns character(0))
  result <- env$`string-split`("", "-")
  expect_equal(length(result), 0)
  expect_true(is.character(result))

  # string-split with delimiter not present
  expect_equal(env$`string-split`("hello", "-"), c("hello"))

  # trim with already trimmed string
  expect_equal(env$trim("hello"), "hello")

  # trim with only whitespace
  expect_equal(env$trim("   "), "")
})
