# Comprehensive dict and set collection tests

engine <- make_engine()

# ============================================================================
# Dict Operations
# ============================================================================

test_that("dict and set helpers work", {
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
