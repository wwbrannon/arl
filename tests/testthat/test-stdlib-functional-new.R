# Tests for new functional.arl additions:
# multi-list map, for-each, count, map-indexed, group-by, frequencies

engine <- make_engine()

# ============================================================================
# Multi-list map
# ============================================================================

test_that("map works with multiple lists", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Two lists with +
  result <- engine$eval(
    engine$read("(map + '(1 2 3) '(4 5 6))")[[1]], env = env)
  expect_equal(result, list(5, 7, 9))

  # Three lists
  result <- engine$eval(
    engine$read("(map + '(1 2 3) '(4 5 6) '(7 8 9))")[[1]], env = env)
  expect_equal(result, list(12, 15, 18))

  # Single list still works (backward compat)
  result <- engine$eval(
    engine$read("(map (lambda (x) (* x 2)) '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(2, 4, 6))

  # Multi-list with lambda
  result <- engine$eval(
    engine$read("(map (lambda (a b) (list a b)) '(1 2 3) '(x y z))")[[1]], env = env)
  expect_equal(result, list(list(1, quote(x)), list(2, quote(y)), list(3, quote(z))))
})

test_that("multi-list map with empty lists", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(map + '() '())")[[1]], env = env)
  expect_equal(result, list())
})

# ============================================================================
# for-each
# ============================================================================

test_that("for-each applies side effects and returns nil", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Side effects happen, returns nil
  result <- engine$eval(engine$read("
    (begin
      (define acc (list))
      (for-each (lambda (x) (set! acc (c acc (list (* x 2))))) '(1 2 3))
      acc)
  ")[[1]], env = env)
  expect_equal(result, list(2, 4, 6))

  # Returns nil
  result <- engine$eval(engine$read("
    (for-each (lambda (x) x) '(1 2 3))
  ")[[1]], env = env)
  expect_null(result)
})

test_that("for-each works with multiple lists", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(engine$read("
    (begin
      (define acc (list))
      (for-each (lambda (a b) (set! acc (c acc (list (+ a b))))) '(1 2 3) '(10 20 30))
      acc)
  ")[[1]], env = env)
  expect_equal(result, list(11, 22, 33))
})

# ============================================================================
# count
# ============================================================================

test_that("count counts matching elements", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(count even? '(1 2 3 4 5 6))")[[1]], env = env)
  expect_equal(result, 3)

  # None match
  result <- engine$eval(
    engine$read("(count even? '(1 3 5))")[[1]], env = env)
  expect_equal(result, 0)

  # All match
  result <- engine$eval(
    engine$read("(count even? '(2 4 6))")[[1]], env = env)
  expect_equal(result, 3)

  # Empty list
  result <- engine$eval(
    engine$read("(count even? '())")[[1]], env = env)
  expect_equal(result, 0)
})

# ============================================================================
# map-indexed
# ============================================================================

test_that("map-indexed passes index and element", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(map-indexed list '(a b c))")[[1]], env = env)
  expect_equal(result, list(list(0, quote(a)), list(1, quote(b)), list(2, quote(c))))

  # Empty list
  result <- engine$eval(
    engine$read("(map-indexed list '())")[[1]], env = env)
  expect_equal(result, list())
})

test_that("map-indexed with computation", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(map-indexed (lambda (i x) (+ i x)) '(10 20 30))")[[1]], env = env)
  expect_equal(result, list(10, 21, 32))
})

# ============================================================================
# group-by
# ============================================================================

test_that("group-by groups by key function", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("(group-by even? '(1 2 3 4 5 6))")[[1]], env = env)

  # Result should be a dict
  expect_true(get("dict?", envir = env)(result))

  # Check grouped values
  true_group <- get("dict-get", envir = env)(result, "TRUE")
  false_group <- get("dict-get", envir = env)(result, "FALSE")
  expect_equal(true_group, list(2, 4, 6))
  expect_equal(false_group, list(1, 3, 5))
})

test_that("group-by with string key function", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict", "strings"), env = env)

  result <- engine$eval(engine$read("
    (group-by string-length '(\"hi\" \"hey\" \"ok\" \"foo\" \"a\"))
  ")[[1]], env = env)

  expect_true(get("dict?", envir = env)(result))
  expect_equal(get("dict-get", envir = env)(result, "2"), list("hi", "ok"))
  expect_equal(get("dict-get", envir = env)(result, "3"), list("hey", "foo"))
  expect_equal(get("dict-get", envir = env)(result, "1"), list("a"))
})

test_that("group-by with empty list", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("(group-by even? '())")[[1]], env = env)
  expect_true(get("dict?", envir = env)(result))
  expect_equal(get("dict-keys", envir = env)(result), list())
})

# ============================================================================
# frequencies
# ============================================================================

test_that("frequencies counts element occurrences", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("(frequencies '(a b a c b a))")[[1]], env = env)

  expect_true(get("dict?", envir = env)(result))
  expect_equal(get("dict-get", envir = env)(result, "a"), 3)
  expect_equal(get("dict-get", envir = env)(result, "b"), 2)
  expect_equal(get("dict-get", envir = env)(result, "c"), 1)
})

test_that("frequencies with empty list", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("(frequencies '())")[[1]], env = env)
  expect_true(get("dict?", envir = env)(result))
  expect_equal(get("dict-keys", envir = env)(result), list())
})

test_that("frequencies with numbers", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("(frequencies '(1 2 1 1 3 2))")[[1]], env = env)
  expect_true(get("dict?", envir = env)(result))
  expect_equal(get("dict-get", envir = env)(result, "1"), 3)
  expect_equal(get("dict-get", envir = env)(result, "2"), 2)
  expect_equal(get("dict-get", envir = env)(result, "3"), 1)
})
