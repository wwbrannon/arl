# Tests for new strings.arl additions:
# string-prefix?, string-suffix?, string-empty?, string-repeat

engine <- make_engine()

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
