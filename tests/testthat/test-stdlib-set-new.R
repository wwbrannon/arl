# Tests for new set.arl additions:
# set->list, list->set, set-size, set-map, set-filter

engine <- make_engine()

# ============================================================================
# set->list
# ============================================================================

test_that("set->list extracts elements as a list", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(
    engine$read("(set->list (set 1 2 3))")[[1]], env = env)
  expect_equal(sort(unlist(result)), c(1, 2, 3))
})

test_that("set->list on empty set", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(
    engine$read("(set->list (set))")[[1]], env = env)
  expect_equal(result, list())
})

# ============================================================================
# list->set
# ============================================================================

test_that("list->set creates set from list", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define s (list->set '(1 2 3 2 1)))
      (list (set? s) (set-contains? s 1) (set-contains? s 2) (set-contains? s 3)))
  ")[[1]], env = env)
  expect_equal(result, list(TRUE, TRUE, TRUE, TRUE))
})

test_that("list->set deduplicates", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (set-size (list->set '(1 1 1 2 2 3)))
  ")[[1]], env = env)
  expect_equal(result, 3)
})

# ============================================================================
# set-size
# ============================================================================

test_that("set-size returns number of elements", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(
    engine$read("(set-size (set 1 2 3))")[[1]], env = env)
  expect_equal(result, 3)

  result <- engine$eval(
    engine$read("(set-size (set))")[[1]], env = env)
  expect_equal(result, 0)

  # Duplicates don't count
  result <- engine$eval(
    engine$read("(set-size (set 1 1 2))")[[1]], env = env)
  expect_equal(result, 2)
})

# ============================================================================
# set-map
# ============================================================================

test_that("set-map transforms elements", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set", "math"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define s (set-map inc (set 1 2 3)))
      (list (set-contains? s 2) (set-contains? s 3) (set-contains? s 4)
            (set-contains? s 1)))
  ")[[1]], env = env)
  expect_equal(result, list(TRUE, TRUE, TRUE, FALSE))
})

test_that("set-map returns a set", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (set? (set-map (lambda (x) (* x 2)) (set 1 2 3)))
  ")[[1]], env = env)
  expect_true(result)
})

test_that("set-map on empty set", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (set-size (set-map (lambda (x) x) (set)))
  ")[[1]], env = env)
  expect_equal(result, 0)
})

# ============================================================================
# set-filter
# ============================================================================

test_that("set-filter keeps matching elements", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (begin
      (define s (set-filter even? (set 1 2 3 4)))
      (list (set-contains? s 2) (set-contains? s 4)
            (set-contains? s 1) (set-contains? s 3)))
  ")[[1]], env = env)
  expect_equal(result, list(TRUE, TRUE, FALSE, FALSE))
})

test_that("set-filter returns a set", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (set? (set-filter even? (set 1 2 3)))
  ")[[1]], env = env)
  expect_true(result)
})

test_that("set-filter with no matches", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (set-size (set-filter even? (set 1 3 5)))
  ")[[1]], env = env)
  expect_equal(result, 0)
})
