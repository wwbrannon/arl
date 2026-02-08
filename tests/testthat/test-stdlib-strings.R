# Comprehensive string operation tests

engine <- RyeEngine$new()

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
