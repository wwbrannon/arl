# Dict operation tests

engine <- make_engine()

# ============================================================================
# Basic dict operations (from collections)
# ============================================================================

test_that("basic dict operations work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  dict <- get("dict", envir = env)(a = 1, b = 2)
  expect_equal(get("dict-get", envir = env)(dict, "a"), 1)
  expect_equal(get("dict-get", envir = env)(dict, "missing", 99), 99)
  expect_true(get("dict-has?", envir = env)(dict, "b"))
  expect_false(get("dict-has?", envir = env)(dict, "c"))
  expect_equal(get("dict-keys", envir = env)(dict), list("a", "b"))
  expect_equal(get("dict-values", envir = env)(dict), list(1, 2))

  updated <- get("dict-set", envir = env)(dict, "c", 3)
  expect_equal(get("dict-get", envir = env)(updated, "c"), 3)
  removed <- get("dict-remove", envir = env)(updated, "a")
  expect_false(get("dict-has?", envir = env)(removed, "a"))

  merged <- get("dict-merge", envir = env)(get("dict", envir = env)(a = 1, b = 2), get("dict", envir = env)(b = 3, c = 4))
  expect_equal(get("dict-get", envir = env)(merged, "a"), 1)
  expect_equal(get("dict-get", envir = env)(merged, "b"), 3)
  expect_equal(get("dict-get", envir = env)(merged, "c"), 4)
})

# ============================================================================
# dict-update
# ============================================================================

test_that("dict-update applies function to existing value", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict", "math"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define d (dict :x 1 :y 2))
      (dict-update d \"x\" inc)
      (dict-get d \"x\"))
  ")[[1]], env = env)
  expect_equal(result, 2)
})

test_that("dict-update uses default for missing key", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict", "math"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define d (dict :x 1))
      (dict-update d \"z\" inc 0)
      (dict-get d \"z\"))
  ")[[1]], env = env)
  expect_equal(result, 1)  # inc(0) = 1
})

test_that("dict-update returns the dict", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (dict? (dict-update (dict :x 1) \"x\" (lambda (v) (* v 10))))
  ")[[1]], env = env)
  expect_true(result)
})

# ============================================================================
# dict-map
# ============================================================================

test_that("dict-map transforms values", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define d (dict :x 1 :y 2 :z 3))
      (define d2 (dict-map (lambda (k v) (* v 10)) d))
      (list (dict-get d2 \"x\") (dict-get d2 \"y\") (dict-get d2 \"z\")))
  ")[[1]], env = env)
  expect_equal(result, list(10, 20, 30))
})

test_that("dict-map returns new dict", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define d (dict :x 1))
      (define d2 (dict-map (lambda (k v) (* v 10)) d))
      ;; Original unchanged
      (dict-get d \"x\"))
  ")[[1]], env = env)
  expect_equal(result, 1)
})

test_that("dict-map on empty dict", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define d2 (dict-map (lambda (k v) v) (dict)))
      (dict-keys d2))
  ")[[1]], env = env)
  expect_equal(result, list())
})

# ============================================================================
# dict-filter
# ============================================================================

test_that("dict-filter keeps matching entries", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define d (dict :x 1 :y 2 :z 3))
      (define d2 (dict-filter (lambda (k v) (> v 1)) d))
      (dict-keys d2))
  ")[[1]], env = env)
  expect_equal(sort(unlist(result)), c("y", "z"))
})

test_that("dict-filter returns new dict", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (dict? (dict-filter (lambda (k v) #t) (dict :x 1)))
  ")[[1]], env = env)
  expect_true(result)
})

# ============================================================================
# dict-for-each
# ============================================================================

test_that("dict-for-each iterates for side effects", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define acc (list))
      (dict-for-each (lambda (k v) (set! acc (c acc (list v)))) (dict :x 1 :y 2))
      acc)
  ")[[1]], env = env)
  expect_equal(sort(unlist(result)), c(1, 2))
})

test_that("dict-for-each returns nil", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (dict-for-each (lambda (k v) v) (dict :x 1))
  ")[[1]], env = env)
  expect_null(result)
})

# ============================================================================
# dict->alist / alist->dict
# ============================================================================

test_that("dict->alist converts dict to association list", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (dict->alist (dict :x 1 :y 2))
  ")[[1]], env = env)

  # Each element should be a (key value) pair
  expect_equal(length(result), 2)
  # Check that we have the right pairs (order should match insertion)
  expect_equal(result[[1]], list("x", 1))
  expect_equal(result[[2]], list("y", 2))
})

test_that("alist->dict converts association list to dict", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define d (alist->dict (list (list \"x\" 1) (list \"y\" 2))))
      (list (dict-get d \"x\") (dict-get d \"y\")))
  ")[[1]], env = env)
  expect_equal(result, list(1, 2))
})

test_that("dict->alist round-trips with alist->dict", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define d (dict :a 10 :b 20))
      (define d2 (alist->dict (dict->alist d)))
      (list (dict-get d2 \"a\") (dict-get d2 \"b\")))
  ")[[1]], env = env)
  expect_equal(result, list(10, 20))
})

test_that("dict->alist on empty dict", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("dict"), env = env)

  result <- engine$eval(engine$read("(dict->alist (dict))")[[1]], env = env)
  expect_equal(result, list())
})
