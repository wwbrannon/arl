# Edge case tests for type predicates

engine <- make_engine()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  stdlib_env(engine, env)
  env
}

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
