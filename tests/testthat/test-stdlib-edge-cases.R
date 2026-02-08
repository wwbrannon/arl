# Remaining edge case tests for Rye standard library
# List operation edge cases moved to test-stdlib-edge-list.R
# Higher-order function edge cases moved to test-stdlib-edge-functions.R
# Sequence operation edge cases moved to test-stdlib-edge-sequences.R

engine <- RyeEngine$new()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  stdlib_env(engine, env)
  env
}

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
  expect_true(env$`vector?`(c("a", "b")))
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
