# Edge case tests for type predicates

engine <- make_engine()

# Helper to create test env with stdlib
setup_env <- function() {
  env <- new.env()
  toplevel_env(engine, env) # nolint: object_usage_linter.
  env
}

test_that("predicates handle edge cases", {
  env <- setup_env()

  # null? with various falsy values
  expect_true(get("null?", envir = env)(NULL))
  expect_false(get("null?", envir = env)(FALSE))
  expect_false(get("null?", envir = env)(0))
  expect_false(get("null?", envir = env)(""))
  expect_true(get("null?", envir = env)(list()))  # empty list is considered null

  # list? with various types
  expect_true(get("list?", envir = env)(list()))
  expect_true(get("list?", envir = env)(list(1, 2, 3)))
  expect_false(get("list?", envir = env)(c(1, 2, 3)))  # vector, not list
  expect_false(get("list?", envir = env)(NULL))

  # list-or-pair? edge cases (non-empty list or dotted pair)
  expect_false(get("list-or-pair?", envir = env)(list()))
  expect_true(get("list-or-pair?", envir = env)(list(1)))
  expect_true(get("list-or-pair?", envir = env)(list(1, 2)))

  # vector? with various types (only numeric vectors)
  expect_true(get("vector?", envir = env)(c(1, 2, 3)))
  expect_true(get("vector?", envir = env)(c("a", "b")))
  expect_true(get("vector?", envir = env)(1:10))
  expect_false(get("vector?", envir = env)(list(1, 2, 3)))

  # number? with various numeric types
  expect_true(get("number?", envir = env)(42))
  expect_true(get("number?", envir = env)(3.14))
  expect_true(get("number?", envir = env)(1L))  # integer
  expect_false(get("number?", envir = env)("42"))

  # string? edge cases
  expect_true(get("string?", envir = env)(""))
  expect_true(get("string?", envir = env)("hello"))
  expect_false(get("string?", envir = env)(NULL))

  # fn? and callable? with various types
  expect_true(get("fn?", envir = env)(function(x) x))
  expect_true(get("callable?", envir = env)(function(x) x))
  expect_true(get("callable?", envir = env)(`+`))
  expect_false(get("fn?", envir = env)(NULL))
  expect_false(get("callable?", envir = env)(42))
})
