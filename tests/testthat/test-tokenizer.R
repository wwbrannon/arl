engine <- make_engine()

test_that("tokenizer handles basic tokens", {
  tokens <- engine$tokenize("(+ 1 2)")
  expect_equal(length(tokens), 5)
  expect_equal(tokens[[1]]$type, "LPAREN")
  expect_equal(tokens[[2]]$type, "SYMBOL")
  expect_equal(tokens[[2]]$value, "+")
  expect_equal(tokens[[3]]$type, "NUMBER")
  expect_equal(tokens[[3]]$value, 1)
  expect_equal(tokens[[4]]$type, "NUMBER")
  expect_equal(tokens[[4]]$value, 2)
  expect_equal(tokens[[5]]$type, "RPAREN")
})

test_that("tokenizer handles strings", {
  tokens <- engine$tokenize('"hello world"')
  expect_equal(length(tokens), 1)
  expect_equal(tokens[[1]]$type, "STRING")
  expect_equal(tokens[[1]]$value, "hello world")
})

test_that("tokenizer handles escape sequences", {
  tokens <- engine$tokenize('"hello\\nworld"')
  expect_equal(tokens[[1]]$value, "hello\nworld")
})

test_that("tokenizer preserves unknown escapes", {
  tokens <- engine$tokenize('"C:\\\\Users\\\\runner\\\\file.rye"')
  expect_equal(tokens[[1]]$value, "C:\\Users\\runner\\file.rye")
})

test_that("tokenizer handles booleans and nil", {
  tokens <- engine$tokenize("#t #f #nil")
  expect_equal(tokens[[1]]$type, "BOOLEAN")
  expect_equal(tokens[[1]]$value, TRUE)
  expect_equal(tokens[[2]]$type, "BOOLEAN")
  expect_equal(tokens[[2]]$value, FALSE)
  expect_equal(tokens[[3]]$type, "NIL")
  expect_null(tokens[[3]]$value)
})

test_that("tokenizer handles comments", {
  tokens <- engine$tokenize("; comment\n(+ 1 2)")
  expect_equal(length(tokens), 5)
  expect_equal(tokens[[1]]$type, "LPAREN")
})

test_that("tokenizer handles quote syntax", {
  tokens <- engine$tokenize("'x")
  expect_equal(tokens[[1]]$type, "QUOTE")
  expect_equal(tokens[[2]]$type, "SYMBOL")
  expect_equal(tokens[[2]]$value, "x")
})

test_that("tokenizer handles :: operator in symbols", {
  tokens <- engine$tokenize("base::mean")
  expect_equal(length(tokens), 1)
  expect_equal(tokens[[1]]$type, "SYMBOL")
  expect_equal(tokens[[1]]$value, "base::mean")
})

test_that("tokenizer handles ::: operator in symbols", {
  tokens <- engine$tokenize("pkg:::internal")
  expect_equal(length(tokens), 1)
  expect_equal(tokens[[1]]$type, "SYMBOL")
  expect_equal(tokens[[1]]$value, "pkg:::internal")
})

test_that("keywords are tokenized correctly", {
  tokens <- engine$tokenize(":data")
  expect_equal(length(tokens), 1)
  expect_equal(tokens[[1]]$type, "KEYWORD")
  expect_equal(tokens[[1]]$value, "data")
})

test_that("keywords in expressions", {
  tokens <- engine$tokenize("(plot x y :col \"red\")")
  expect_equal(tokens[[5]]$type, "KEYWORD")
  expect_equal(tokens[[5]]$value, "col")
})

test_that("tokenizer handles integer literals", {
  tokens <- engine$tokenize("4L 42L -10L")
  expect_equal(length(tokens), 3)
  expect_equal(tokens[[1]]$type, "NUMBER")
  expect_equal(tokens[[1]]$value, 4L)
  expect_true(is.integer(tokens[[1]]$value))
  expect_equal(tokens[[2]]$type, "NUMBER")
  expect_equal(tokens[[2]]$value, 42L)
  expect_true(is.integer(tokens[[2]]$value))
  expect_equal(tokens[[3]]$type, "NUMBER")
  expect_equal(tokens[[3]]$value, -10L)
  expect_true(is.integer(tokens[[3]]$value))
})

test_that("tokenizer handles pure imaginary numbers", {
  tokens <- engine$tokenize("4i 3.14i -2.5i")
  expect_equal(length(tokens), 3)
  expect_equal(tokens[[1]]$type, "NUMBER")
  expect_equal(tokens[[1]]$value, 0+4i)
  expect_true(is.complex(tokens[[1]]$value))
  expect_equal(tokens[[2]]$type, "NUMBER")
  expect_equal(tokens[[2]]$value, 0+3.14i)
  expect_true(is.complex(tokens[[2]]$value))
  expect_equal(tokens[[3]]$type, "NUMBER")
  expect_equal(tokens[[3]]$value, 0-2.5i)
  expect_true(is.complex(tokens[[3]]$value))
})

test_that("tokenizer handles full complex number syntax", {
  tokens <- engine$tokenize("2+4i 3.14-2.5i -1+2i -5-3i")
  expect_equal(length(tokens), 4)
  # 2+4i
  expect_equal(tokens[[1]]$type, "NUMBER")
  expect_equal(tokens[[1]]$value, 2+4i)
  expect_true(is.complex(tokens[[1]]$value))
  # 3.14-2.5i
  expect_equal(tokens[[2]]$type, "NUMBER")
  expect_equal(tokens[[2]]$value, 3.14-2.5i)
  expect_true(is.complex(tokens[[2]]$value))
  # -1+2i
  expect_equal(tokens[[3]]$type, "NUMBER")
  expect_equal(tokens[[3]]$value, -1+2i)
  expect_true(is.complex(tokens[[3]]$value))
  # -5-3i
  expect_equal(tokens[[4]]$type, "NUMBER")
  expect_equal(tokens[[4]]$value, -5-3i)
  expect_true(is.complex(tokens[[4]]$value))
})

test_that("tokenizer handles NA values", {
  tokens <- engine$tokenize("NA NA_real_ NA_integer_ NA_character_ NA_complex_")
  expect_equal(length(tokens), 5)
  expect_equal(tokens[[1]]$type, "NA")
  expect_true(is.na(tokens[[1]]$value))
  expect_equal(tokens[[2]]$type, "NA")
  expect_true(is.na(tokens[[2]]$value))
  expect_equal(typeof(tokens[[2]]$value), "double")
  expect_equal(tokens[[3]]$type, "NA")
  expect_true(is.na(tokens[[3]]$value))
  expect_equal(typeof(tokens[[3]]$value), "integer")
  expect_equal(tokens[[4]]$type, "NA")
  expect_true(is.na(tokens[[4]]$value))
  expect_equal(typeof(tokens[[4]]$value), "character")
  expect_equal(tokens[[5]]$type, "NA")
  expect_true(is.na(tokens[[5]]$value))
  expect_equal(typeof(tokens[[5]]$value), "complex")
})

# =============================================================================
# Dotted-pair (standalone dot) tokenizer tests
# =============================================================================

test_that("standalone dot in dotted-pair syntax yields DOT token", {
  tokens <- engine$tokenize("(a . b)")
  expect_equal(length(tokens), 5)
  expect_equal(tokens[[1]]$type, "LPAREN")
  expect_equal(tokens[[2]]$type, "SYMBOL")
  expect_equal(tokens[[2]]$value, "a")
  expect_equal(tokens[[3]]$type, "DOT")
  expect_equal(tokens[[3]]$value, ".")
  expect_equal(tokens[[4]]$type, "SYMBOL")
  expect_equal(tokens[[4]]$value, "b")
  expect_equal(tokens[[5]]$type, "RPAREN")
})

test_that("dot with no surrounding space is part of symbol", {
  tokens <- engine$tokenize("(a.b)")
  expect_equal(length(tokens), 3)
  expect_equal(tokens[[1]]$type, "LPAREN")
  expect_equal(tokens[[2]]$type, "SYMBOL")
  expect_equal(tokens[[2]]$value, "a.b")
  expect_equal(tokens[[3]]$type, "RPAREN")
})

test_that("dot at start of list yields DOT token", {
  tokens <- engine$tokenize("( . b)")
  expect_equal(length(tokens), 4)
  expect_equal(tokens[[2]]$type, "DOT")
  expect_equal(tokens[[2]]$value, ".")
})

test_that("dot before closing paren yields DOT token", {
  tokens <- engine$tokenize("(a . )")
  expect_equal(length(tokens), 4)
  expect_equal(tokens[[3]]$type, "DOT")
  expect_equal(tokens[[4]]$type, "RPAREN")
})
