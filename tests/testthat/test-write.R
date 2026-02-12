engine <- make_engine()

# =============================================================================
# Atoms
# =============================================================================

test_that("write handles integers", {
  expr <- engine$read("42L")[[1]]
  expect_equal(engine$write(expr), "42L")
})

test_that("write handles doubles", {
  expr <- engine$read("3.14")[[1]]
  expect_equal(engine$write(expr), "3.14")

  expr <- engine$read("0")[[1]]
  expect_equal(engine$write(expr), "0")
})

test_that("write handles Inf and NaN", {
  expr <- engine$read("Inf")[[1]]
  expect_equal(engine$write(expr), "Inf")

  expr <- engine$read("-Inf")[[1]]
  expect_equal(engine$write(expr), "-Inf")

  expr <- engine$read("NaN")[[1]]
  expect_equal(engine$write(expr), "NaN")
})

test_that("write handles complex numbers", {
  expr <- engine$read("2+3i")[[1]]
  expect_equal(engine$write(expr), "2+3i")

  expr <- engine$read("4i")[[1]]
  expect_equal(engine$write(expr), "0+4i")
})

test_that("write handles strings", {
  expr <- engine$read('"hello"')[[1]]
  expect_equal(engine$write(expr), '"hello"')
})

test_that("write handles strings with escapes", {
  expr <- engine$read('"line1\\nline2"')[[1]]
  expect_equal(engine$write(expr), '"line1\\nline2"')

  expr <- engine$read('"tab\\there"')[[1]]
  expect_equal(engine$write(expr), '"tab\\there"')

  expr <- engine$read('"say \\"hi\\""')[[1]]
  expect_equal(engine$write(expr), '"say \\"hi\\""')

  expr <- engine$read('"back\\\\slash"')[[1]]
  expect_equal(engine$write(expr), '"back\\\\slash"')
})

test_that("write handles booleans", {
  expr <- engine$read("#t")[[1]]
  expect_equal(engine$write(expr), "#t")

  expr <- engine$read("#f")[[1]]
  expect_equal(engine$write(expr), "#f")
})

test_that("write handles NA variants", {
  expr <- engine$read("NA")[[1]]
  expect_equal(engine$write(expr), "NA")

  expr <- engine$read("NA_integer_")[[1]]
  expect_equal(engine$write(expr), "NA_integer_")

  expr <- engine$read("NA_real_")[[1]]
  expect_equal(engine$write(expr), "NA_real_")

  expr <- engine$read("NA_complex_")[[1]]
  expect_equal(engine$write(expr), "NA_complex_")

  expr <- engine$read("NA_character_")[[1]]
  expect_equal(engine$write(expr), "NA_character_")
})

test_that("write handles #nil", {
  expr <- engine$read("#nil")[[1]]
  expect_equal(engine$write(expr), "#nil")
})

# =============================================================================
# Symbols
# =============================================================================

test_that("write handles plain symbols", {
  expr <- engine$read("foo")[[1]]
  expect_equal(engine$write(expr), "foo")

  expr <- engine$read("+")[[1]]
  expect_equal(engine$write(expr), "+")
})

test_that("write handles hyphenated symbols", {
  expr <- engine$read("my-func")[[1]]
  expect_equal(engine$write(expr), "my-func")
})

test_that("write handles predicate symbols", {
  expr <- engine$read("null?")[[1]]
  expect_equal(engine$write(expr), "null?")

  expr <- engine$read("set!")[[1]]
  expect_equal(engine$write(expr), "set!")
})

# =============================================================================
# Keywords
# =============================================================================

test_that("write handles keywords", {
  expr <- engine$read(":foo")[[1]]
  expect_equal(engine$write(expr), ":foo")

  expr <- engine$read(":bar-baz")[[1]]
  expect_equal(engine$write(expr), ":bar-baz")
})

# =============================================================================
# Lists and calls
# =============================================================================

test_that("write handles simple calls", {
  expr <- engine$read("(+ 1 2)")[[1]]
  expect_equal(engine$write(expr), "(+ 1 2)")
})

test_that("write handles nested calls", {
  expr <- engine$read("(+ 1 (* 2 3))")[[1]]
  expect_equal(engine$write(expr), "(+ 1 (* 2 3))")
})

test_that("write handles empty list", {
  expr <- engine$read("()")[[1]]
  expect_equal(engine$write(expr), "()")
})

# =============================================================================
# Sugar: quote, quasiquote, unquote, unquote-splicing
# =============================================================================

test_that("write restores quote sugar", {
  expr <- engine$read("'x")[[1]]
  expect_equal(engine$write(expr), "'x")
})

test_that("write restores quasiquote sugar", {
  expr <- engine$read("`x")[[1]]
  expect_equal(engine$write(expr), "`x")
})

test_that("write restores unquote sugar", {
  expr <- engine$read(",x")[[1]]
  expect_equal(engine$write(expr), ",x")
})

test_that("write restores unquote-splicing sugar", {
  expr <- engine$read(",@x")[[1]]
  expect_equal(engine$write(expr), ",@x")
})

# =============================================================================
# Sugar: :: and :::
# =============================================================================

test_that("write restores :: sugar", {
  expr <- engine$read("base::mean")[[1]]
  expect_equal(engine$write(expr), "base::mean")
})

test_that("write restores ::: sugar", {
  expr <- engine$read("stats:::fitted.default")[[1]]
  expect_equal(engine$write(expr), "stats:::fitted.default")
})

# =============================================================================
# Dotted pairs
# =============================================================================

test_that("write handles simple dotted pair", {
  expr <- engine$read("'(a . b)")[[1]][[2]]
  expect_equal(engine$write(expr), "(a . b)")
})

test_that("write handles improper list", {
  expr <- engine$read("'(a b . c)")[[1]][[2]]
  expect_equal(engine$write(expr), "(a b . c)")
})

# =============================================================================
# Round-trip property: read(write(read(source))) == read(source)
# =============================================================================

test_that("round-trip property holds for various inputs", {
  cases <- c(
    "42L", "3.14", '"hello"', "#t", "#f", "#nil", "NA",
    "foo", ":bar",
    "(+ 1 2)", "(define x 10)", "(lambda (x) (+ x 1))",
    "'x", "`(a ,b ,@c)",
    "base::mean"
  )
  for (src in cases) {
    original <- engine$read(src)[[1]]
    written <- engine$write(original)
    re_read <- engine$read(written)[[1]]
    expect_equal(re_read, original, info = paste("round-trip failed for:", src))
  }
})

# =============================================================================
# Arl-level builtin
# =============================================================================

test_that("write is accessible as a Arl builtin", {
  result <- engine$eval_text("(write '(+ 1 2))")
  expect_equal(result, "(+ 1 2)")
})

test_that("write works with atoms from Arl", {
  expect_equal(engine$eval_text('(write 42)'), "42")
  expect_equal(engine$eval_text('(write "hello")'), '"hello"')
  expect_equal(engine$eval_text('(write #t)'), "#t")
})
