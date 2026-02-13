# Remaining edge case tests for Arl standard library
# List operation edge cases moved to test-stdlib-edge-list.R
# Higher-order function edge cases moved to test-stdlib-edge-functions.R
# Sequence operation edge cases moved to test-stdlib-edge-sequences.R
# Predicates edge cases moved to test-stdlib-edge-predicates.R
# String edge cases moved to test-stdlib-strings.R
# Math boundary conditions moved to test-stdlib-math.R

engine <- make_engine()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  toplevel_env(engine, env) # nolint: object_usage_linter.
  env
}

# ============================================================================
# Performance Tests (Large Lists)
# ============================================================================

test_that("stdlib handles large lists efficiently", {
  env <- setup_env()

  # Large list (1000 elements)
  large_list <- as.list(1:1000)

  # map should handle large lists
  result <- env$map(function(x) x * 2, large_list)
  expect_equal(length(result), 1000)
  expect_equal(result[[1]], 2)
  expect_equal(result[[1000]], 2000)

  # filter should handle large lists
  result <- env$filter(function(x) x %% 2 == 0, large_list)
  expect_equal(length(result), 500)

  # reduce should handle large lists
  result <- env$reduce(`+`, large_list)
  expect_equal(result, sum(1:1000))

  # reverse should handle large lists
  result <- env$reverse(large_list)
  expect_equal(result[[1]], 1000)
  expect_equal(result[[1000]], 1)
})

# ============================================================================
# Mixed Type Tests
# ============================================================================

test_that("stdlib handles mixed types correctly", {
  env <- setup_env()

  # List with mixed types
  mixed <- list(1, "two", 3.0, TRUE, NULL, list(5))

  # map should work with mixed types
  result <- env$map(function(x) is.null(x), mixed)
  expect_equal(result[[5]], TRUE)

  # filter should work with mixed types
  result <- env$filter(function(x) is.numeric(x), mixed)
  expect_equal(length(result), 2)  # 1 and 3.0

  # str should convert all types
  result <- env$str(1, "two", TRUE, NULL)
  expect_true(is.character(result))
})

# ============================================================================
# Coverage: Dict edge cases
# ============================================================================

test_that("dict-set with symbol key works", {
  env <- setup_env()

  result <- engine$eval(
    engine$read("(dict-set (dict) 'sym-key \"val\")")[[1]], env = env)
  val <- engine$eval(engine$read("(dict-get result \"sym-key\")")[[1]],
    env = local({
      e <- new.env(parent = env)
      e$result <- result
      e
    }))
  expect_equal(val, "val")
})

test_that("dict-set errors on invalid key type", {
  env <- setup_env()

  expect_error(
    engine$eval(engine$read("(dict-set (dict) 42 \"val\")")[[1]], env = env),
    "requires a string, symbol, or keyword key")
})

test_that("dict-set errors on non-dict", {
  env <- setup_env()

  expect_error(
    engine$eval(engine$read("(dict-set 42 \"key\" \"val\")")[[1]], env = env),
    "requires a dict")
})

test_that("dict-keys on empty dict returns empty list", {
  env <- setup_env()

  result <- engine$eval(engine$read("(dict-keys (dict))")[[1]], env = env)
  expect_equal(length(result), 0)
})

test_that("dict-remove removes last key leaving empty dict", {
  env <- setup_env()

  result <- engine$eval(
    engine$read('(dict-remove (dict :a 1) "a")')[[1]], env = env)
  keys <- engine$eval(engine$read("(dict-keys d)")[[1]],
    env = local({
      e <- new.env(parent = env)
      e$d <- result
      e
    }))
  expect_equal(length(keys), 0)
})

test_that("dict-has? on non-dict returns #f", {
  env <- setup_env()

  result <- engine$eval(
    engine$read('(dict-has? 42 "key")')[[1]], env = env)
  expect_false(result)
})

# ============================================================================
# Coverage: Set edge cases
# ============================================================================

test_that("set-add errors on non-set", {
  env <- setup_env()

  expect_error(
    engine$eval(engine$read('(set-add 42 "x")')[[1]], env = env),
    "requires a set")
})

# ============================================================================
# Coverage: Display edge cases
# ============================================================================

test_that("__format-seq with empty list returns empty string", {
  env <- setup_env()

  result <- engine$eval(
    engine$read("(__format-seq (list))")[[1]], env = env)
  expect_equal(result, "")
})

# ============================================================================
# Coverage: Equality edge cases
# ============================================================================

test_that("eq? errors as not implementable in R", {
  env <- setup_env()

  expect_error(
    engine$eval(engine$read("(eq? 1 1)")[[1]], env = env),
    "cannot be properly implemented")
})

test_that("eqv? errors as not implementable in R", {
  env <- setup_env()

  expect_error(
    engine$eval(engine$read("(eqv? 1 1)")[[1]], env = env),
    "cannot be properly implemented")
})

test_that("equal? list vs non-list returns #f", {
  env <- setup_env()

  result <- engine$eval(
    engine$read("(equal? (list 1) 42)")[[1]], env = env)
  expect_false(result)
})

# ============================================================================
# Coverage: Types edge cases
# ============================================================================

test_that("empty? works on lists", {
  env <- setup_env()

  expect_true(engine$eval(engine$read("(empty? (list))")[[1]], env = env))
  expect_false(engine$eval(engine$read("(empty? (list 1))")[[1]], env = env))
})

# ============================================================================
# Coverage: Dict non-dict fallbacks
# ============================================================================

test_that("dict-keys on non-dict returns empty list", {
  env <- setup_env()

  result <- engine$eval(engine$read("(dict-keys 42)")[[1]], env = env)
  expect_equal(length(result), 0)
})

test_that("dict-values on non-dict returns empty list", {
  env <- setup_env()

  result <- engine$eval(engine$read("(dict-values 42)")[[1]], env = env)
  expect_equal(length(result), 0)
})

# ============================================================================
# Coverage: Set non-set fallback
# ============================================================================

test_that("set-contains? on non-set returns #f", {
  env <- setup_env()

  result <- engine$eval(engine$read('(set-contains? 42 "x")')[[1]], env = env)
  expect_false(result)
})

# ============================================================================
# Coverage: Equality type mismatch paths
# ============================================================================

test_that("equal? on environment vs non-environment returns #f", {
  env <- setup_env()

  result <- engine$eval(
    engine$read("(equal? (r/call \"new.env\" (list)) 42)")[[1]], env = env)
  expect_false(result)
})

# ============================================================================
# Coverage: Math predicate non-matching types
# ============================================================================

test_that("integer? on non-numeric returns #f", {
  env <- setup_env()

  expect_false(engine$eval(engine$read('(integer? "hi")')[[1]], env = env))
})

test_that("natural? on non-integer returns #f", {
  env <- setup_env()

  expect_false(engine$eval(engine$read('(natural? "hi")')[[1]], env = env))
})

test_that("rational? on non-real returns #f", {
  env <- setup_env()

  expect_false(engine$eval(engine$read('(rational? "hi")')[[1]], env = env))
})

test_that("max with arguments works", {
  env <- setup_env()

  expect_equal(engine$eval(engine$read("(max 1 5 3)")[[1]], env = env), 5)
})

# ============================================================================
# Coverage: string->list on empty string, char-at negative index
# ============================================================================

test_that("string->list on empty string returns empty list", {
  env <- setup_env()

  result <- engine$eval(
    engine$read('(string->list "")')[[1]], env = env)
  expect_equal(result, list())
})

test_that("char-at errors on negative index", {
  env <- setup_env()

  expect_error(
    engine$eval(engine$read('(char-at "hello" -1)')[[1]], env = env),
    "out of bounds")
})

# ============================================================================
# Coverage: functional foldl/foldr with init value, repeatedly
# ============================================================================

test_that("foldl and foldr with init value from Arl", {
  env <- new.env()
  toplevel_env(engine, env)
  import_stdlib_modules(engine, c("functional"), env)

  result <- engine$eval(
    engine$read("(foldl + (list 1 2 3) 10)")[[1]], env = env)
  expect_equal(result, 16)

  result <- engine$eval(
    engine$read("(foldr + (list 1 2 3) 10)")[[1]], env = env)
  expect_equal(result, 16)
})

test_that("repeatedly from Arl code (sequences version, n fn order)", {
  env <- new.env()
  toplevel_env(engine, env)

  engine$eval(engine$read("(define counter 0)")[[1]], env = env)
  result <- engine$eval(
    engine$read("(repeatedly 3 (lambda () (set! counter (+ counter 1)) counter))")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))
})

# ============================================================================
# Coverage: display/println
# ============================================================================

test_that("display outputs formatted value", {
  env <- setup_env()

  # Make sure ARL_QUIET is not set
  old <- Sys.getenv("ARL_QUIET")
  Sys.unsetenv("ARL_QUIET")
  on.exit(if (nzchar(old)) Sys.setenv(ARL_QUIET = old) else Sys.unsetenv("ARL_QUIET"))

  output <- capture.output(
    engine$eval(engine$read("(display 42)")[[1]], env = env))
  expect_true(any(grepl("42", output)))
})

test_that("println outputs formatted value", {
  env <- setup_env()

  old <- Sys.getenv("ARL_QUIET")
  Sys.unsetenv("ARL_QUIET")
  on.exit(if (nzchar(old)) Sys.setenv(ARL_QUIET = old) else Sys.unsetenv("ARL_QUIET"))

  output <- capture.output(
    engine$eval(engine$read("(println 42)")[[1]], env = env))
  expect_true(any(grepl("42", output)))
})
