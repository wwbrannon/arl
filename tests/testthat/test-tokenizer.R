engine <- RyeEngine$new()

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
