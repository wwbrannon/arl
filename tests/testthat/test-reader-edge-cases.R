# Tests for reader/tokenizer edge cases
# Covers unicode, partial input, numeric edge cases, escape sequences

test_that("unicode characters in symbols", {
  engine <- make_engine()

  # Should handle unicode in symbols
  result <- engine$eval_text("(define café 42)")
  expect_no_error(result)

  result <- engine$eval_text("café")
  expect_equal(result, 42)
})

test_that("unicode in strings", {
  engine <- make_engine()

  result <- engine$eval_text('"Hello 世界"')
  expect_equal(result, "Hello 世界")

  result <- engine$eval_text('"Emoji: 🎉"')
  expect_equal(result, "Emoji: 🎉")
})

test_that("escape sequences in strings", {
  engine <- make_engine()

  result <- engine$eval_text('"Hello\\nWorld"')
  expect_equal(result, "Hello\nWorld")

  result <- engine$eval_text('"Tab\\there"')
  expect_equal(result, "Tab\there")

  result <- engine$eval_text('"Quote: \\"test\\""')
  expect_equal(result, 'Quote: "test"')

  result <- engine$eval_text('"Backslash: \\\\"')
  expect_equal(result, "Backslash: \\")
})

test_that("numeric literals - integers", {
  engine <- make_engine()

  result <- engine$eval_text("42")
  expect_equal(result, 42)

  result <- engine$eval_text("-17")
  expect_equal(result, -17)

  result <- engine$eval_text("0")
  expect_equal(result, 0)
})

test_that("numeric literals - floats", {
  engine <- make_engine()

  result <- engine$eval_text("3.14")
  expect_equal(result, 3.14)

  result <- engine$eval_text("-2.5")
  expect_equal(result, -2.5)

  result <- engine$eval_text("0.0")
  expect_equal(result, 0.0)
})

test_that("numeric literals - scientific notation", {
  engine <- make_engine()

  # Basic scientific notation
  result <- engine$eval_text("1e5")
  expect_equal(result, 1e5)

  result <- engine$eval_text("2.5e3")
  expect_equal(result, 2.5e3)

  # Uppercase E
  result <- engine$eval_text("1E5")
  expect_equal(result, 1E5)

  # Negative exponents
  result <- engine$eval_text("1e-5")
  expect_equal(result, 1e-5)

  result <- engine$eval_text("3.14e-2")
  expect_equal(result, 3.14e-2)

  # Positive exponents with explicit sign
  result <- engine$eval_text("1e+5")
  expect_equal(result, 1e+5)

  # Negative numbers with scientific notation
  result <- engine$eval_text("-2.5e3")
  expect_equal(result, -2.5e3)

  result <- engine$eval_text("-1e-5")
  expect_equal(result, -1e-5)

  result <- engine$eval_text("-3.14e+10")
  expect_equal(result, -3.14e+10)

  # Zero with exponents
  result <- engine$eval_text("0e0")
  expect_equal(result, 0)

  result <- engine$eval_text("0.0e10")
  expect_equal(result, 0)

  result <- engine$eval_text("0e-5")
  expect_equal(result, 0)

  # Integer part with exponent (no decimal)
  result <- engine$eval_text("123e2")
  expect_equal(result, 123e2)

  # Decimal without integer part is not valid in standard notation,
  # but our regex requires at least one digit before optional decimal
  result <- engine$eval_text("5.0e0")
  expect_equal(result, 5.0)

  # Works in expressions
  result <- engine$eval_text("(+ 1e2 2e2)")
  expect_equal(result, 300)

  result <- engine$eval_text("(* 2e3 3e2)")
  expect_equal(result, 2e3 * 3e2)
})

test_that("scientific notation - more edge cases", {
  engine <- make_engine()

  # Multiple digit exponents
  result <- engine$eval_text("1e10")
  expect_equal(result, 1e10)

  result <- engine$eval_text("1e100")
  expect_equal(result, 1e100)

  result <- engine$eval_text("1e-10")
  expect_equal(result, 1e-10)

  # Negative base with various exponents
  result <- engine$eval_text("-1e0")
  expect_equal(result, -1)

  result <- engine$eval_text("-9.99e99")
  expect_equal(result, -9.99e99)

  result <- engine$eval_text("-1.23e-45")
  expect_equal(result, -1.23e-45)

  # Positive sign on base
  result <- engine$eval_text("+1e5")
  expect_equal(result, 1e5)

  result <- engine$eval_text("+2.5e-3")
  expect_equal(result, 2.5e-3)
})

test_that("scientific notation - invalid formats treated as symbols", {
  engine <- make_engine()

  # These should be parsed as symbols, not numbers

  # Missing exponent digits (e with no digits after)
  # This will be tokenized as two separate tokens or fail
  expect_error(engine$eval_text("1e"))

  # Double e
  expect_error(engine$eval_text("1ee5"))

  # Decimal in exponent
  expect_error(engine$eval_text("1e5.5"))

  # Space in scientific notation
  expect_error(engine$eval_text("1e 5"))

  # Multiple signs
  expect_error(engine$eval_text("1e+-5"))
  expect_error(engine$eval_text("1e++5"))
})

test_that("numeric edge cases - very large numbers", {
  engine <- make_engine()

  # Very large numbers using scientific notation
  result <- engine$eval_text("1e100")
  expect_equal(result, 1e100)

  result <- engine$eval_text("9.99e307")
  expect_equal(result, 9.99e307)

  # Avogadro's number
  result <- engine$eval_text("6.022e23")
  expect_equal(result, 6.022e23)

  # Should work in arithmetic
  result <- engine$eval_text("(+ 1e10 1e10)")
  expect_equal(result, 2e10)

  # Comparisons
  result <- engine$eval_text("(> 1e100 1e99)")
  expect_equal(result, TRUE)

  result <- engine$eval_text("(< 1e100 1e101)")
  expect_equal(result, TRUE)
})

test_that("numeric edge cases - very small numbers", {
  engine <- make_engine()

  # Very small numbers using scientific notation
  result <- engine$eval_text("1e-100")
  expect_equal(result, 1e-100)

  result <- engine$eval_text("2.5e-308")
  expect_equal(result, 2.5e-308)

  # Planck's constant (approximately)
  result <- engine$eval_text("6.626e-34")
  expect_equal(result, 6.626e-34)

  # Should work in arithmetic
  result <- engine$eval_text("(+ 1e-10 1e-10)")
  expect_equal(result, 2e-10)

  # Multiplication with very small numbers
  result <- engine$eval_text("(* 2e-5 3e-5)")
  expect_equal(result, 2e-5 * 3e-5)

  # Division creating small numbers
  result <- engine$eval_text("(/ 1 1e10)")
  expect_equal(result, 1e-10)
})

test_that("special numeric values", {
  engine <- make_engine()

  # Inf
  result <- engine$eval_text("(/ 1 0)")
  expect_equal(result, Inf)

  # -Inf
  result <- engine$eval_text("(/ -1 0)")
  expect_equal(result, -Inf)

  # NaN (0/0)
  result <- engine$eval_text("(/ 0 0)")
  expect_true(is.nan(result))
})

test_that("empty list", {
  engine <- make_engine()

  result <- engine$eval_text("'()")
  expect_equal(length(result), 0)
})

test_that("whitespace handling", {
  engine <- make_engine()

  # Multiple spaces
  result <- engine$eval_text("(+    1    2)")
  expect_equal(result, 3)

  # Tabs
  result <- engine$eval_text("(+\t1\t2)")
  expect_equal(result, 3)

  # Mixed whitespace
  result <- engine$eval_text("(+ \t\n  1  \n\t  2)")
  expect_equal(result, 3)
})

test_that("comment handling", {
  engine <- make_engine()

  # Line comment
  result <- engine$eval_text("; comment\n(+ 1 2)")
  expect_equal(result, 3)

  # Inline comment
  result <- engine$eval_text("(+ 1 2) ; result is 3")
  expect_equal(result, 3)

  # Multiple comments
  code <- "; first comment\n; second comment\n(+ 1 2) ; inline\n; trailing"
  result <- engine$eval_text(code)
  expect_equal(result, 3)
})

test_that("partial input errors", {
  engine <- make_engine()

  # Unclosed paren - should error
  expect_error(engine$eval_text("(+ 1 2"))

  # Unclosed string - should error
  expect_error(engine$eval_text('"unclosed string'))

  # Unclosed list - should error
  expect_error(engine$eval_text("'(1 2 3"))
})

test_that("symbols with special characters", {
  engine <- make_engine()

  # Symbols can have hyphens
  engine$eval_text("(define test-var 42)")
  result <- engine$eval_text("test-var")
  expect_equal(result, 42)

  # Symbols can have question marks
  engine$eval_text("(import math :refer (%))
                    (define even? (lambda (x) (= (% x 2) 0)))")
  result <- engine$eval_text("(even? 4)")
  expect_equal(result, TRUE)

  # Symbols can have exclamation marks
  engine$eval_text("(define important! 100)")
  result <- engine$eval_text("important!")
  expect_equal(result, 100)
})

test_that("keywords", {
  engine <- make_engine()

  # Keywords start with colon and are self-evaluating
  result <- engine$eval_text(":keyword")
  expect_true(!is.null(result))

  # Quoted keywords can be compared as values
  result <- engine$eval_text("(= ':foo ':foo)")
  expect_equal(result, TRUE)

  result <- engine$eval_text("(= ':foo ':bar)")
  expect_equal(result, FALSE)

  # Bare keywords in argument position are treated as named argument syntax
  # :foo :foo becomes a named arg foo=(symbol foo), so = sees 1 arg (vacuously true)
  result <- engine$eval_text("(= :foo :foo)")
  expect_true(result)
})

test_that("dotted pairs / improper lists", {
  engine <- make_engine()

  # This may not be supported, but test that it doesn't crash
  expect_error(
    engine$eval_text("'(1 . 2)"),
    NA  # Just ensure it doesn't crash silently
  )
})

test_that("nested quotes", {
  engine <- make_engine()

  result <- engine$eval_text("''x")
  # Should be (quote x) - the outer quote is evaluated, returning the inner quote form
  expect_true(is.call(result))
})

test_that("empty string", {
  engine <- make_engine()

  result <- engine$eval_text('""')
  expect_equal(result, "")
})

test_that("string with only whitespace", {
  engine <- make_engine()

  result <- engine$eval_text('"   "')
  expect_equal(result, "   ")

  result <- engine$eval_text('"\n\t"')
  expect_equal(result, "\n\t")
})

test_that("very long symbol names", {
  engine <- make_engine()

  long_name <- paste(rep("a", 1000), collapse = "")
  code <- sprintf("(define %s 42)", long_name)
  engine$eval_text(code)

  result <- engine$eval_text(long_name)
  expect_equal(result, 42)
})

test_that("very long strings", {
  engine <- make_engine()

  long_string <- paste(rep("a", 10000), collapse = "")
  code <- sprintf('"%s"', long_string)
  result <- engine$eval_text(code)
  expect_equal(nchar(result), 10000)
})

test_that("deeply nested lists", {
  engine <- make_engine()

  # Create deeply nested list
  nested <- "'("
  for (i in 1:100) {
    nested <- paste0(nested, "(")
  }
  nested <- paste0(nested, "x")
  for (i in 1:100) {
    nested <- paste0(nested, ")")
  }
  nested <- paste0(nested, ")")

  expect_no_error({
    result <- engine$eval_text(nested)
  })
})
