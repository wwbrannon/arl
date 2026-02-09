# Comprehensive threading macro tests: -> (thread-first) and ->> (thread-last)

engine <- make_engine()

test_that("-> threads value as first argument", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("threading"), env)

  # Simple threading
  result <- engine$eval_in_env(
    engine$read("(-> 5 (+ 3) (* 2))")[[1]], env)
  expect_equal(result, 16)  # ((5 + 3) * 2) = 16

  # Thread with single argument functions
  result <- engine$eval_in_env(
    engine$read("(-> 10 (- 5) (/ 2) (+ 1))")[[1]], env)
  expect_equal(result, 3.5)  # (((10 - 5) / 2) + 1) = 3.5

  # Thread through multiple operations
  result <- engine$eval_in_env(
    engine$read("(-> 100 (/ 10) (+ 5) (* 2))")[[1]], env)
  expect_equal(result, 30)  # (((100 / 10) + 5) * 2) = 30
})

test_that("->> threads value as last argument", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("threading", "list", "functional"), env)

  # Thread through list operations
  result <- engine$eval_in_env(
    engine$read("(->> (list 1 2 3) (map (lambda (x) (* x 2))) (filter (lambda (x) (> x 2))))")[[1]], env)
  expect_equal(result, list(4, 6))

  # Thread with reduce
  result <- engine$eval_in_env(
    engine$read("(->> (list 1 2 3 4) (map (lambda (x) (* x x))) (reduce +))")[[1]], env)
  expect_equal(result, 30)  # 1 + 4 + 9 + 16 = 30
})

test_that("threading macros work with nested forms", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("threading"), env)

  # Nested threading with ->
  result <- engine$eval_in_env(
    engine$read("(-> 10 (- 5) (/ 2) (+ 1))")[[1]], env)
  expect_equal(result, 3.5)

  # Thread through complex expression
  result <- engine$eval_in_env(
    engine$read("(-> 5 (+ 10) (- 3) (* 2))")[[1]], env)
  expect_equal(result, 24)  # (((5 + 10) - 3) * 2) = 24
})

test_that("threading macros handle single operations", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("threading"), env)

  # Single operation with ->
  result <- engine$eval_in_env(
    engine$read("(-> 5 (+ 3))")[[1]], env)
  expect_equal(result, 8)

  # Single operation with ->>
  result <- engine$eval_in_env(
    engine$read("(->> 5 (* 2))")[[1]], env)
  expect_equal(result, 10)
})

test_that("threading can be combined with other macros", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("threading", "binding"), env)

  # Combine -> with let
  result <- engine$eval_in_env(
    engine$read("(let ((x 10)) (-> x (+ 5) (* 2)))")[[1]], env)
  expect_equal(result, 30)

  # Thread result into let binding
  result <- engine$eval_in_env(
    engine$read("(let ((x (-> 5 (+ 3) (* 2)))) x)")[[1]], env)
  expect_equal(result, 16)
})
