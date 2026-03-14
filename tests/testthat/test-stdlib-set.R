# Set operation tests

engine <- make_engine()

# ============================================================================
# Basic set operations (from collections)
# ============================================================================

thin <- make_cran_thinner()

test_that("basic set operations work", {
  thin()
  env <- new.env()
  toplevel_env(engine, env = env)

  set_values <- function(set) {
    keys <- ls(envir = set, all.names = TRUE, sorted = FALSE)
    if (length(keys) == 0) {
      return(list())
    }
    as.list(mget(keys, envir = set, inherits = FALSE))
  }
  set <- get("set", envir = env)(1, 2, 2, 3)
  expect_true(get("set?", envir = env)(set))
  expect_true(get("set-contains?", envir = env)(set, 2))
  expect_false(get("set-contains?", envir = env)(set, 4))

  updated_set <- get("set-add", envir = env)(set, 4)
  expect_true(get("set-contains?", envir = env)(updated_set, 4))

  removed_set <- get("set-remove", envir = env)(set, 2)
  expect_false(get("set-contains?", envir = env)(removed_set, 2))

  union_set <- get("set-union", envir = env)(get("set", envir = env)(1, 2), get("set", envir = env)(2, 3))
  expect_true(get("set-contains?", envir = env)(union_set, 1))
  expect_true(get("set-contains?", envir = env)(union_set, 2))
  expect_true(get("set-contains?", envir = env)(union_set, 3))

  intersection_set <- get("set-intersection", envir = env)(get("set", envir = env)(1, 2), get("set", envir = env)(2, 3))
  expect_true(get("set-contains?", envir = env)(intersection_set, 2))
  expect_false(get("set-contains?", envir = env)(intersection_set, 1))

  difference_set <- get("set-difference", envir = env)(get("set", envir = env)(1, 2), get("set", envir = env)(2, 3))
  expect_true(get("set-contains?", envir = env)(difference_set, 1))
  expect_false(get("set-contains?", envir = env)(difference_set, 2))
  expect_equal(length(set_values(difference_set)), 1)
})

# ============================================================================
# set->list
# ============================================================================

test_that("set->list extracts elements as a list", {
  thin()
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(
    engine$read("(set->list (set 1 2 3))")[[1]], env = env)
  expect_equal(sort(unlist(result)), c(1, 2, 3))
})

test_that("set->list on empty set", {
  thin()
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
  thin()
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
  thin()
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
  thin()
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
  thin()
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
  thin()
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (set? (set-map (lambda (x) (* x 2)) (set 1 2 3)))
  ")[[1]], env = env)
  expect_true(result)
})

test_that("set-map on empty set", {
  thin()
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
  thin()
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
  thin()
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (set? (set-filter even? (set 1 2 3)))
  ")[[1]], env = env)
  expect_true(result)
})

test_that("set-filter with no matches", {
  thin()
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("set"), env = env)

  result <- engine$eval(engine$read("
    (set-size (set-filter even? (set 1 3 5)))
  ")[[1]], env = env)
  expect_equal(result, 0)
})
