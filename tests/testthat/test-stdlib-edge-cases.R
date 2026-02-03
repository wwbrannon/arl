# Edge case tests for Rye standard library
# Tests boundary conditions, empty inputs, and unusual scenarios

engine <- RyeEngine$new()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  stdlib_env(engine, env)
  env
}

# ============================================================================
# List Operations Edge Cases
# ============================================================================

test_that("car handles edge cases", {
  env <- setup_env()

  # Empty list should return NULL
  expect_null(env$car(list()))

  # Single element
  expect_equal(env$car(list(42)), 42)

  # Nested lists
  expect_equal(env$car(list(list(1, 2), 3)), list(1, 2))

  # NULL as element
  expect_null(env$car(list(NULL, 2)))
})

test_that("cdr handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$cdr(list()), list())

  # Single element (should return empty list)
  expect_equal(env$cdr(list(42)), list())

  # Two elements
  expect_equal(env$cdr(list(1, 2)), list(2))

  # Nested structures
  result <- env$cdr(list(1, list(2, 3), 4))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(2, 3))
})

test_that("cons handles edge cases", {
  env <- setup_env()

  # Cons to empty list
  expect_equal(env$cons(1, list()), list(1))

  # Cons NULL
  expect_equal(env$cons(NULL, list(1, 2)), list(NULL, 1, 2))

  # Cons nested list
  result <- env$cons(list(1, 2), list(3, 4))
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], 3)
})

test_that("append handles edge cases", {
  env <- setup_env()

  # Both empty
  expect_equal(env$append(list(), list()), list())

  # First empty
  expect_equal(env$append(list(), list(1, 2)), list(1, 2))

  # Second empty
  expect_equal(env$append(list(1, 2), list()), list(1, 2))

  # Single elements
  expect_equal(env$append(list(1), list(2)), list(1, 2))
})

test_that("reverse handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$reverse(list()), list())

  # Single element
  expect_equal(env$reverse(list(42)), list(42))

  # Nested lists (should reverse top level only)
  result <- env$reverse(list(list(1, 2), list(3, 4)))
  expect_equal(result[[1]], list(3, 4))
  expect_equal(result[[2]], list(1, 2))
})

test_that("list* handles edge cases", {
  env <- setup_env()

  # Single element with empty list
  expect_equal(env$`list*`(1, list()), list(1))

  # Multiple elements
  expect_equal(env$`list*`(1, 2, list(3)), list(1, 2, 3))

  # With NULL
  expect_equal(env$`list*`(NULL, list(1)), list(NULL, 1))
})

# ============================================================================
# Higher-Order Functions Edge Cases
# ============================================================================

test_that("map handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$map(function(x) x * 2, list()), list())

  # Single element
  expect_equal(env$map(function(x) x * 2, list(5)), list(10))

  # Function returning NULL
  result <- env$map(function(x) NULL, list(1, 2, 3))
  expect_equal(length(result), 3)
  expect_null(result[[1]])

  # Identity function
  expect_equal(env$map(function(x) x, list(1, 2, 3)), list(1, 2, 3))
})

test_that("filter handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$filter(function(x) TRUE, list()), list())

  # All match
  expect_equal(env$filter(function(x) TRUE, list(1, 2, 3)), list(1, 2, 3))

  # None match
  expect_equal(env$filter(function(x) FALSE, list(1, 2, 3)), list())

  # Single element matches
  expect_equal(env$filter(function(x) x == 2, list(1, 2, 3)), list(2))
})

test_that("reduce handles edge cases", {
  env <- setup_env()

  # Single element
  expect_equal(env$reduce(`+`, list(42)), 42)

  # Two elements
  expect_equal(env$reduce(`+`, list(1, 2)), 3)

  # String concatenation
  concat <- function(a, b) paste0(a, b)
  expect_equal(env$reduce(concat, list("a", "b", "c")), "abc")
})

test_that("mapcat handles edge cases", {
  env <- setup_env()

  # Empty list
  expect_equal(env$mapcat(function(x) list(x), list()), list())

  # Function returning empty lists
  expect_equal(env$mapcat(function(x) list(), list(1, 2, 3)), list())

  # Function returning single element
  expect_equal(env$mapcat(function(x) list(x), list(1, 2)), list(1, 2))

  # Mixed result sizes
  result <- env$mapcat(function(x) if (x == 1) list(x) else list(x, x), list(1, 2, 3))
  expect_equal(result, list(1, 2, 2, 3, 3))
})

test_that("every? and any? handle edge cases", {
  env <- setup_env()

  # Empty list - every? should be TRUE (vacuous truth)
  expect_true(env$`every?`(function(x) FALSE, list()))

  # Empty list - any? should be FALSE
  expect_false(env$`any?`(function(x) TRUE, list()))

  # Single element
  expect_true(env$`every?`(function(x) x > 0, list(5)))
  expect_true(env$`any?`(function(x) x > 0, list(5)))

  # All same value
  expect_true(env$`every?`(function(x) x == 1, list(1, 1, 1)))
})

# ============================================================================
# Sequence Helpers Edge Cases
# ============================================================================

test_that("take handles edge cases", {
  env <- setup_env()

  # n = 0
  expect_equal(env$take(0, list(1, 2, 3)), list())

  # n = 1
  expect_equal(env$take(1, list(1, 2, 3)), list(1))

  # n equals list length
  expect_equal(env$take(3, list(1, 2, 3)), list(1, 2, 3))

  # n > list length (should return whole list)
  expect_equal(env$take(10, list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(env$take(5, list()), list())
})

test_that("drop handles edge cases", {
  env <- setup_env()

  # n = 0
  expect_equal(env$drop(0, list(1, 2, 3)), list(1, 2, 3))

  # n = 1
  expect_equal(env$drop(1, list(1, 2, 3)), list(2, 3))

  # n equals list length
  expect_equal(env$drop(3, list(1, 2, 3)), list())

  # n > list length (should return empty list)
  expect_equal(env$drop(10, list(1, 2, 3)), list())

  # Empty list
  expect_equal(env$drop(5, list()), list())
})

test_that("take-while handles edge cases", {
  env <- setup_env()

  # Always true predicate
  expect_equal(env$`take-while`(function(x) TRUE, list(1, 2, 3)), list(1, 2, 3))

  # Always false predicate
  expect_equal(env$`take-while`(function(x) FALSE, list(1, 2, 3)), list())

  # Empty list
  expect_equal(env$`take-while`(function(x) TRUE, list()), list())

  # First element fails
  expect_equal(env$`take-while`(function(x) x > 5, list(1, 2, 3)), list())

  # All but last pass
  expect_equal(env$`take-while`(function(x) x < 3, list(1, 2, 3)), list(1, 2))
})

test_that("drop-while handles edge cases", {
  env <- setup_env()

  # Always true predicate
  expect_equal(env$`drop-while`(function(x) TRUE, list(1, 2, 3)), list())

  # Always false predicate
  expect_equal(env$`drop-while`(function(x) FALSE, list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(env$`drop-while`(function(x) TRUE, list()), list())

  # First element fails
  expect_equal(env$`drop-while`(function(x) x > 5, list(1, 2, 3)), list(1, 2, 3))
})

test_that("partition handles edge cases", {
  env <- setup_env()

  # n = 1
  expect_equal(env$partition(1, list(1, 2, 3)), list(list(1), list(2), list(3)))

  # n equals list length
  expect_equal(env$partition(3, list(1, 2, 3)), list(list(1, 2, 3)))

  # n > list length (returns empty list - incomplete partition is dropped)
  expect_equal(env$partition(10, list(1, 2, 3)), list())

  # List length not divisible by n (incomplete partition is dropped)
  result <- env$partition(2, list(1, 2, 3, 4, 5))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], list(3, 4))

  # Empty list
  expect_equal(env$partition(2, list()), list())

  # Single element (incomplete partition is dropped)
  expect_equal(env$partition(2, list(42)), list())
})

test_that("partition with step parameter handles edge cases", {
  env <- setup_env()

  # step < n (overlapping partitions)
  result <- env$partition(3, list(1, 2, 3, 4, 5), 1)
  expect_equal(length(result), 3)
  expect_equal(result[[1]], list(1, 2, 3))
  expect_equal(result[[2]], list(2, 3, 4))
  expect_equal(result[[3]], list(3, 4, 5))

  # step > n (gaps between partitions)
  result <- env$partition(2, list(1, 2, 3, 4, 5, 6), 3)
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, 2))
  expect_equal(result[[2]], list(4, 5))

  # step = n (non-overlapping, same as default)
  result <- env$partition(2, list(1, 2, 3, 4), 2)
  expect_equal(result, list(list(1, 2), list(3, 4)))
})

test_that("flatten handles edge cases", {
  env <- setup_env()

  # Already flat
  expect_equal(env$flatten(list(1, 2, 3)), list(1, 2, 3))

  # Empty list
  expect_equal(env$flatten(list()), list())

  # Single nested list
  expect_equal(env$flatten(list(list(1, 2, 3))), list(1, 2, 3))

  # Deeply nested (3 levels)
  deep <- list(1, list(2, list(3, list(4))))
  result <- env$flatten(deep)
  expect_equal(result, list(1, 2, 3, 4))

  # Mixed nesting
  mixed <- list(1, list(2, 3), 4, list(list(5, 6), 7))
  result <- env$flatten(mixed)
  expect_equal(result, list(1, 2, 3, 4, 5, 6, 7))

  # Empty nested lists
  expect_equal(env$flatten(list(list(), list(), list())), list())
})

# ============================================================================
# Predicates Edge Cases
# ============================================================================

test_that("predicates handle edge cases", {
  env <- setup_env()

  # null? with various falsy values
  expect_true(env$`null?`(NULL))
  expect_false(env$`null?`(FALSE))
  expect_false(env$`null?`(0))
  expect_false(env$`null?`(""))
  expect_true(env$`null?`(list()))  # empty list is considered null

  # list? with various types
  expect_true(env$`list?`(list()))
  expect_true(env$`list?`(list(1, 2, 3)))
  expect_false(env$`list?`(c(1, 2, 3)))  # vector, not list
  expect_false(env$`list?`(NULL))

  # list-or-pair? edge cases (non-empty list or dotted pair)
  expect_false(env$`list-or-pair?`(list()))
  expect_true(env$`list-or-pair?`(list(1)))
  expect_true(env$`list-or-pair?`(list(1, 2)))

  # vector? with various types (only numeric vectors)
  expect_true(env$`vector?`(c(1, 2, 3)))
  expect_false(env$`vector?`(c("a", "b")))  # character vectors are not considered vectors
  expect_true(env$`vector?`(1:10))
  expect_false(env$`vector?`(list(1, 2, 3)))

  # number? with various numeric types
  expect_true(env$`number?`(42))
  expect_true(env$`number?`(3.14))
  expect_true(env$`number?`(1L))  # integer
  expect_false(env$`number?`("42"))

  # string? edge cases
  expect_true(env$`string?`(""))
  expect_true(env$`string?`("hello"))
  expect_false(env$`string?`(NULL))

  # fn? and callable? with various types
  expect_true(env$`fn?`(function(x) x))
  expect_true(env$`callable?`(function(x) x))
  expect_true(env$`callable?`(`+`))
  expect_false(env$`fn?`(NULL))
  expect_false(env$`callable?`(42))
})

# ============================================================================
# String Operations Edge Cases
# ============================================================================

test_that("string operations handle edge cases", {
  env <- setup_env()

  # str with no arguments (returns character(0), not empty string)
  expect_equal(length(env$str()), 0)
  expect_true(is.character(env$str()))

  # str with single argument
  expect_equal(env$str("hello"), "hello")

  # str with NULL (NULLs are skipped)
  expect_equal(env$str(NULL, "world"), "world")

  # str with numbers
  expect_equal(env$str(1, 2, 3), "123")

  # string-join with empty list
  expect_equal(env$`string-join`(list(), "-"), "")

  # string-join with single element
  expect_equal(env$`string-join`(list("a"), "-"), "a")

  # string-split with empty string (returns character(0))
  result <- env$`string-split`("", "-")
  expect_equal(length(result), 0)
  expect_true(is.character(result))

  # string-split with delimiter not present
  expect_equal(env$`string-split`("hello", "-"), c("hello"))

  # trim with already trimmed string
  expect_equal(env$trim("hello"), "hello")

  # trim with only whitespace
  expect_equal(env$trim("   "), "")
})

# ============================================================================
# Apply and Call Edge Cases
# ============================================================================

test_that("apply handles edge cases", {
  env <- setup_env()

  # Single argument
  expect_equal(env$funcall(identity, list(42)), 42)

  # Empty list (should work if function accepts no args)
  zero_arg_fn <- function() 42
  expect_equal(env$funcall(zero_arg_fn, list()), 42)

  # Many arguments
  many_sum <- function(...) sum(...)
  expect_equal(env$funcall(many_sum, as.list(1:100)), 5050)
})

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
# Nested Structure Tests
# ============================================================================

test_that("stdlib handles deeply nested structures", {
  env <- setup_env()

  # Create deeply nested list (10 levels)
  deep <- list(1)
  for (i in 2:10) {
    deep <- list(i, deep)
  }

  # flatten should handle deep nesting
  result <- env$flatten(deep)
  expect_equal(length(result), 10)
  expect_equal(result[[1]], 10)
  expect_equal(result[[10]], 1)

  # car/cdr should navigate nested structures
  expect_equal(env$car(deep), 10)
  expect_equal(env$car(env$cdr(deep)), list(9, list(8, list(7, list(6, list(5, list(4, list(3, list(2, list(1))))))))))
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
# Boundary Conditions for Numeric Operations
# ============================================================================

test_that("numeric operations handle boundary conditions", {
  env <- setup_env()

  # Large numbers
  large <- 1e100
  expect_equal(env$`=`(large, large), TRUE)

  # Negative numbers
  expect_equal(env$`%`(-10, 3), -10 %% 3)

  # Zero
  expect_equal(env$`%`(0, 5), 0)

  # Floating point (uses exact equality, not all.equal)
  expect_false(env$`=`(0.1 + 0.2, 0.3))  # floating point precision issues
  expect_true(env$`=`(1.0, 1.0))
})
