test_that("tokenizer handles basic tokens", {
  tokens <- rye_tokenize("(+ 1 2)")
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
  tokens <- rye_tokenize('"hello world"')
  expect_equal(length(tokens), 1)
  expect_equal(tokens[[1]]$type, "STRING")
  expect_equal(tokens[[1]]$value, "hello world")
})

test_that("tokenizer handles escape sequences", {
  tokens <- rye_tokenize('"hello\\nworld"')
  expect_equal(tokens[[1]]$value, "hello\nworld")
})

test_that("tokenizer handles booleans and nil", {
  tokens <- rye_tokenize("#t #f #nil")
  expect_equal(tokens[[1]]$type, "BOOLEAN")
  expect_equal(tokens[[1]]$value, TRUE)
  expect_equal(tokens[[2]]$type, "BOOLEAN")
  expect_equal(tokens[[2]]$value, FALSE)
  expect_equal(tokens[[3]]$type, "NIL")
  expect_null(tokens[[3]]$value)
})

test_that("tokenizer handles comments", {
  tokens <- rye_tokenize("; comment\n(+ 1 2)")
  expect_equal(length(tokens), 5)
  expect_equal(tokens[[1]]$type, "LPAREN")
})

test_that("tokenizer handles quote syntax", {
  tokens <- rye_tokenize("'x")
  expect_equal(tokens[[1]]$type, "QUOTE")
  expect_equal(tokens[[2]]$type, "SYMBOL")
  expect_equal(tokens[[2]]$value, "x")
})

test_that("parser handles simple expressions", {
  exprs <- rye_read("(+ 1 2)")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  expect_equal(as.character(exprs[[1]][[1]]), "+")
})

test_that("parser handles nested expressions", {
  exprs <- rye_read("(+ 1 (* 2 3))")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  expect_true(is.call(exprs[[1]][[3]]))
})

test_that("parser handles quote sugar", {
  exprs <- rye_read("'x")
  expect_equal(length(exprs), 1)
  expect_true(is.call(exprs[[1]]))
  expect_equal(as.character(exprs[[1]][[1]]), "quote")
  expect_equal(as.character(exprs[[1]][[2]]), "x")
})

test_that("Phase 1 calculator test", {
  result <- rye_eval(rye_read("(+ 1 (* 2 3))")[[1]])
  expect_equal(result, 7)
})

test_that("evaluator handles simple arithmetic", {
  expect_equal(rye_eval(rye_read("(+ 1 2)")[[1]]), 3)
  expect_equal(rye_eval(rye_read("(- 5 3)")[[1]]), 2)
  expect_equal(rye_eval(rye_read("(* 4 5)")[[1]]), 20)
  expect_equal(rye_eval(rye_read("(/ 10 2)")[[1]]), 5)
})

test_that("evaluator handles R functions", {
  result <- rye_eval(rye_read("(mean (c 1 2 3 4 5))")[[1]])
  expect_equal(result, 3)
})

test_that("evaluator handles nested calls", {
  result <- rye_eval(rye_read("(+ (* 2 3) (* 4 5))")[[1]])
  expect_equal(result, 26)
})
