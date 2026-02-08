# Comprehensive dict and set collection tests

engine <- RyeEngine$new()

# ============================================================================
# Dict Operations
# ============================================================================

test_that("dict and set helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  dict <- env$dict(a = 1, b = 2)
  expect_equal(env$`dict-get`(dict, "a"), 1)
  expect_equal(env$`dict-get`(dict, "missing", 99), 99)
  expect_true(env$`dict-has?`(dict, "b"))
  expect_false(env$`dict-has?`(dict, "c"))
  expect_equal(env$`dict-keys`(dict), list("a", "b"))
  expect_equal(env$`dict-values`(dict), list(1, 2))

  updated <- env$`dict-set`(dict, "c", 3)
  expect_equal(env$`dict-get`(updated, "c"), 3)
  removed <- env$`dict-remove`(updated, "a")
  expect_false(env$`dict-has?`(removed, "a"))

  merged <- env$`dict-merge`(env$dict(a = 1, b = 2), env$dict(b = 3, c = 4))
  expect_equal(env$`dict-get`(merged, "a"), 1)
  expect_equal(env$`dict-get`(merged, "b"), 3)
  expect_equal(env$`dict-get`(merged, "c"), 4)

  # ============================================================================
  # Set Operations
  # ============================================================================

  set_values <- function(set) {
    keys <- ls(envir = set, all.names = TRUE, sorted = FALSE)
    if (length(keys) == 0) {
      return(list())
    }
    as.list(mget(keys, envir = set, inherits = FALSE))
  }
  set <- env$set(1, 2, 2, 3)
  expect_true(env$`set?`(set))
  expect_true(env$`set-contains?`(set, 2))
  expect_false(env$`set-contains?`(set, 4))

  updated_set <- env$`set-add`(set, 4)
  expect_true(env$`set-contains?`(updated_set, 4))

  removed_set <- env$`set-remove`(set, 2)
  expect_false(env$`set-contains?`(removed_set, 2))

  union_set <- env$`set-union`(env$set(1, 2), env$set(2, 3))
  expect_true(env$`set-contains?`(union_set, 1))
  expect_true(env$`set-contains?`(union_set, 2))
  expect_true(env$`set-contains?`(union_set, 3))

  intersection_set <- env$`set-intersection`(env$set(1, 2), env$set(2, 3))
  expect_true(env$`set-contains?`(intersection_set, 2))
  expect_false(env$`set-contains?`(intersection_set, 1))

  difference_set <- env$`set-difference`(env$set(1, 2), env$set(2, 3))
  expect_true(env$`set-contains?`(difference_set, 1))
  expect_false(env$`set-contains?`(difference_set, 2))
  expect_equal(length(set_values(difference_set)), 1)
})
