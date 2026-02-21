# Sequence helper tests: take, drop, partition, flatten, member, contains?

engine <- make_engine()

test_that("sequence helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("take", envir = env)(2, list(1, 2, 3)), list(1, 2))
  expect_equal(get("drop", envir = env)(2, list(1, 2, 3)), list(3))
  expect_equal(get("take-while", envir = env)(function(x) x < 3, list(1, 2, 3, 1)), list(1, 2))
  expect_equal(get("drop-while", envir = env)(function(x) x < 3, list(1, 2, 3, 1)), list(3, 1))
  expect_equal(get("partition", envir = env)(2, list(1, 2, 3, 4)), list(list(1, 2), list(3, 4)))
  expect_equal(get("flatten", envir = env)(list(1, list(2, list(3)), 4)), list(1, 2, 3, 4))
})

test_that("member and contains? sequence helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("member", envir = env)(2, list(1, 2, 3)), list(2, 3))
  expect_false(get("member", envir = env)(5, list(1, 2, 3)))

  # member uses equal? by default (structural equality)
  expect_equal(get("member", envir = env)(list(1, 2), list(list(3, 4), list(1, 2), list(5, 6))),
               list(list(1, 2), list(5, 6)))

  # use-identical keyword for identity comparison (2 is double, identical match)
  expect_equal(get("member", envir = env)(2, list(1, 2, 3), `use-identical` = TRUE), list(2, 3))
  # 2L (integer) is not identical to 2 (double) in R
  expect_false(get("member", envir = env)(2L, list(1, 2, 3), `use-identical` = TRUE))

  expect_true(get("contains?", envir = env)(2, list(1, 2, 3)))
  expect_false(get("contains?", envir = env)(5, list(1, 2, 3)))
})

test_that("length= checks exact length", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length= '(1 2 3) 3)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length= '(1 2) 3)")[[1]], env = env))
})

test_that("length> checks greater length", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length> '(1 2 3) 2)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length> '(1 2) 2)")[[1]], env = env))
})

test_that("length< checks less length", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length< '(1 2) 3)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length< '(1 2 3) 3)")[[1]], env = env))
})

test_that("length predicates work with empty sequences", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length= '() 0)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(length< '() 1)")[[1]], env = env))
})

test_that("length predicates work with vectors", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # Note: length("hello") is 1 in R (vector length), not 5 (character count)
  # Use nchar() for string character count
  expect_true(engine$eval(engine$read("(length= \"hello\" 1)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length> \"hello\" 1)")[[1]], env = env))
})

test_that("length predicates handle boundaries", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(length= '(1 2 3) 3)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(length> '(1 2 3) 3)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(length> '(1 2 3) 2)")[[1]], env = env))
})

# ============================================================================
# Coverage: partition zero-n error, flatten recursive
# ============================================================================

test_that("partition errors when n is zero", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_error(
    engine$eval(engine$read("(partition 0 (list 1 2 3))")[[1]], env = env),
    "requires positive n and step")
})

test_that("flatten handles deeply nested lists", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(flatten (list 1 (list 2 (list 3))))")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))
})

test_that("zip with no arguments returns empty list", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  result <- engine$eval(engine$read("(zip)")[[1]], env = env)
  expect_equal(result, list())
})

# ============================================================================
# find
# ============================================================================

test_that("find returns first matching element", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(find even? '(1 3 4 5 6))")[[1]], env = env)
  expect_equal(result, 4)

  # Not found returns #f
  result <- engine$eval(
    engine$read("(find even? '(1 3 5))")[[1]], env = env)
  expect_false(result)

  # Empty list returns #f
  result <- engine$eval(
    engine$read("(find even? '())")[[1]], env = env)
  expect_false(result)

  # First element matches
  result <- engine$eval(
    engine$read("(find even? '(2 3 4))")[[1]], env = env)
  expect_equal(result, 2)
})

# ============================================================================
# distinct
# ============================================================================

test_that("distinct removes duplicates preserving order", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(distinct '(1 2 1 3 2))")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))

  # Empty list
  result <- engine$eval(
    engine$read("(distinct '())")[[1]], env = env)
  expect_equal(result, list())

  # No duplicates
  result <- engine$eval(
    engine$read("(distinct '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))

  # All same
  result <- engine$eval(
    engine$read("(distinct '(1 1 1))")[[1]], env = env)
  expect_equal(result, list(1))
})

test_that("distinct works with symbols", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(distinct '(a b a c b))")[[1]], env = env)
  expect_equal(result, list(quote(a), quote(b), quote(c)))
})

# ============================================================================
# split-at
# ============================================================================

test_that("split-at splits list at index", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(split-at 2 '(a b c d))")[[1]], env = env)
  expect_equal(result, list(list(quote(a), quote(b)), list(quote(c), quote(d))))

  # Split at 0
  result <- engine$eval(
    engine$read("(split-at 0 '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(list(), list(1, 2, 3)))

  # Split beyond length
  result <- engine$eval(
    engine$read("(split-at 10 '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(list(1, 2, 3), list()))

  # Empty list
  result <- engine$eval(
    engine$read("(split-at 2 '())")[[1]], env = env)
  expect_equal(result, list(list(), list()))
})

# ============================================================================
# split-with
# ============================================================================

test_that("split-with splits at first non-matching element", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(split-with even? '(2 4 1 3))")[[1]], env = env)
  expect_equal(result, list(list(2, 4), list(1, 3)))

  # No match at start
  result <- engine$eval(
    engine$read("(split-with even? '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(list(), list(1, 2, 3)))

  # All match
  result <- engine$eval(
    engine$read("(split-with even? '(2 4 6))")[[1]], env = env)
  expect_equal(result, list(list(2, 4, 6), list()))

  # Empty list
  result <- engine$eval(
    engine$read("(split-with even? '())")[[1]], env = env)
  expect_equal(result, list(list(), list()))
})

# ============================================================================
# interpose
# ============================================================================

test_that("interpose inserts separator between elements", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(interpose 0 '(1 2 3))")[[1]], env = env)
  expect_equal(result, list(1, 0, 2, 0, 3))

  # Single element
  result <- engine$eval(
    engine$read("(interpose 0 '(1))")[[1]], env = env)
  expect_equal(result, list(1))

  # Empty list
  result <- engine$eval(
    engine$read("(interpose 0 '())")[[1]], env = env)
  expect_equal(result, list())

  # String separator
  result <- engine$eval(
    engine$read("(interpose \",\" '(\"a\" \"b\" \"c\"))")[[1]], env = env)
  expect_equal(result, list("a", ",", "b", ",", "c"))
})

# ============================================================================
# partition-by
# ============================================================================

test_that("partition-by splits into runs by key", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- engine$eval(
    engine$read("(partition-by even? '(2 4 1 3 6))")[[1]], env = env)
  expect_equal(result, list(list(2, 4), list(1, 3), list(6)))

  # All same key
  result <- engine$eval(
    engine$read("(partition-by even? '(2 4 6))")[[1]], env = env)
  expect_equal(result, list(list(2, 4, 6)))

  # Empty list
  result <- engine$eval(
    engine$read("(partition-by even? '())")[[1]], env = env)
  expect_equal(result, list())

  # Single element
  result <- engine$eval(
    engine$read("(partition-by even? '(1))")[[1]], env = env)
  expect_equal(result, list(list(1)))

  # Alternating
  result <- engine$eval(
    engine$read("(partition-by even? '(1 2 3 4 5))")[[1]], env = env)
  expect_equal(result, list(list(1), list(2), list(3), list(4), list(5)))
})

# ============================================================================
# Edge cases: sequence operations
# ============================================================================

test_that("take handles edge cases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # n = 0
  expect_equal(get("take", envir = env)(0, list(1, 2, 3)), list())

  # n = 1
  expect_equal(get("take", envir = env)(1, list(1, 2, 3)), list(1))

  # n equals list length
  expect_equal(get("take", envir = env)(3, list(1, 2, 3)), list(1, 2, 3))

  # n > list length (should return whole list)
  expect_equal(get("take", envir = env)(10, list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(get("take", envir = env)(5, list()), list())
})

test_that("drop handles edge cases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # n = 0
  expect_equal(get("drop", envir = env)(0, list(1, 2, 3)), list(1, 2, 3))

  # n = 1
  expect_equal(get("drop", envir = env)(1, list(1, 2, 3)), list(2, 3))

  # n equals list length
  expect_equal(get("drop", envir = env)(3, list(1, 2, 3)), list())

  # n > list length (should return empty list)
  expect_equal(get("drop", envir = env)(10, list(1, 2, 3)), list())

  # Empty list
  expect_equal(get("drop", envir = env)(5, list()), list())
})

test_that("take-while handles edge cases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Always true predicate
  expect_equal(get("take-while", envir = env)(function(x) TRUE, list(1, 2, 3)), list(1, 2, 3))

  # Always false predicate
  expect_equal(get("take-while", envir = env)(function(x) FALSE, list(1, 2, 3)), list())

  # Empty list
  expect_equal(get("take-while", envir = env)(function(x) TRUE, list()), list())

  # First element fails
  expect_equal(get("take-while", envir = env)(function(x) x > 5, list(1, 2, 3)), list())

  # All but last pass
  expect_equal(get("take-while", envir = env)(function(x) x < 3, list(1, 2, 3)), list(1, 2))
})

test_that("drop-while handles edge cases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Always true predicate
  expect_equal(get("drop-while", envir = env)(function(x) TRUE, list(1, 2, 3)), list())

  # Always false predicate
  expect_equal(get("drop-while", envir = env)(function(x) FALSE, list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(get("drop-while", envir = env)(function(x) TRUE, list()), list())

  # First element fails
  expect_equal(get("drop-while", envir = env)(function(x) x > 5, list(1, 2, 3)), list(1, 2, 3))
})

test_that("partition handles edge cases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # n = 1
  expect_equal(get("partition", envir = env)(1, list(1, 2, 3)), list(list(1), list(2), list(3)))

  # n equals list length
  expect_equal(get("partition", envir = env)(3, list(1, 2, 3)), list(list(1, 2, 3)))

  # n > list length (returns empty list - incomplete partition is dropped)
  expect_equal(get("partition", envir = env)(10, list(1, 2, 3)), list())

  # List length not divisible by n (incomplete partition is dropped)
  result <- get("partition", envir = env)(2, list(1, 2, 3, 4, 5))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], list(3, 4))

  # Empty list
  expect_equal(get("partition", envir = env)(2, list()), list())

  # Single element (incomplete partition is dropped)
  expect_equal(get("partition", envir = env)(2, list(42)), list())
})

test_that("partition with step parameter handles edge cases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # step < n (overlapping partitions)
  result <- get("partition", envir = env)(3, list(1, 2, 3, 4, 5), 1)
  expect_equal(length(result), 3)
  expect_equal(result[[1]], list(1, 2, 3))
  expect_equal(result[[2]], list(2, 3, 4))
  expect_equal(result[[3]], list(3, 4, 5))

  # step > n (gaps between partitions)
  result <- get("partition", envir = env)(2, list(1, 2, 3, 4, 5, 6), 3)
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], list(4, 5))

  # step = n (non-overlapping, same as default)
  result <- get("partition", envir = env)(2, list(1, 2, 3, 4), 2)
  expect_equal(result, list(list(1, 2), list(3, 4)))
})

test_that("flatten handles edge cases", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Already flat
  expect_equal(get("flatten", envir = env)(list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(get("flatten", envir = env)(list()), list())

  # Single nested list
  expect_equal(get("flatten", envir = env)(list(list(1, 2, 3))), list(1, 2, 3))

  # Deeply nested (3 levels)
  deep <- list(1, list(2, list(3, list(4))))
  result <- get("flatten", envir = env)(deep)
  expect_equal(result, list(1, 2, 3, 4))

  # Mixed nesting
  mixed <- list(1, list(2, 3), 4, list(list(5, 6), 7))
  result <- get("flatten", envir = env)(mixed)
  expect_equal(result, list(1, 2, 3, 4, 5, 6, 7))

  # Empty nested lists
  expect_equal(get("flatten", envir = env)(list(list(), list(), list())), list())
})
