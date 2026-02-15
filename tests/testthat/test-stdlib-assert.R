# Comprehensive assertion function tests

engine <- make_engine()

test_that("assert passes on true condition", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # Should return #t
  result <- engine$eval(
    engine$read("(assert #t)")[[1]], env = env)
  expect_true(result)

  # Truthy value
  result <- engine$eval(
    engine$read("(assert 1)")[[1]], env = env)
  expect_true(result)

  # Non-zero number
  result <- engine$eval(
    engine$read("(assert 42)")[[1]], env = env)
  expect_true(result)
})

test_that("assert fails on false condition", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  expect_error(
    engine$eval(engine$read("(assert #f)")[[1]], env = env),
    "Assertion failed")

  expect_error(
    engine$eval(engine$read("(assert #nil)")[[1]], env = env),
    "Assertion failed")
})

test_that("assert accepts custom error message", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  expect_error(
    engine$eval(
      engine$read('(assert #f "Custom failure message")')[[1]], env = env),
    "Custom failure message")
})

test_that("assert-equal compares values with equal?", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # Equal numbers
  result <- engine$eval(
    engine$read("(assert-equal 42 42)")[[1]], env = env)
  expect_true(result)

  # Equal strings
  result <- engine$eval(
    engine$read('(assert-equal "hello" "hello")')[[1]], env = env)
  expect_true(result)

  # Equal lists
  result <- engine$eval(
    engine$read("(assert-equal (list 1 2 3) (list 1 2 3))")[[1]], env = env)
  expect_true(result)
})

test_that("assert-equal fails on unequal values", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  expect_error(
    engine$eval(engine$read("(assert-equal 42 43)")[[1]], env = env),
    "Expected.*Got")

  expect_error(
    engine$eval(
      engine$read('(assert-equal "hello" "world")')[[1]], env = env),
    "Expected.*Got")

  expect_error(
    engine$eval(
      engine$read("(assert-equal (list 1 2) (list 1 3))")[[1]], env = env),
    "Expected.*Got")
})

test_that("assert-true passes on truthy values", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # #t
  result <- engine$eval(
    engine$read("(assert-true #t)")[[1]], env = env)
  expect_true(result)

  # Non-zero number
  result <- engine$eval(
    engine$read("(assert-true 1)")[[1]], env = env)
  expect_true(result)

  # Non-empty string
  result <- engine$eval(
    engine$read('(assert-true "yes")')[[1]], env = env)
  expect_true(result)
})

test_that("assert-true fails on falsy values", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  expect_error(
    engine$eval(engine$read("(assert-true #f)")[[1]], env = env),
    "Expected truthy value")

  expect_error(
    engine$eval(engine$read("(assert-true #nil)")[[1]], env = env),
    "Expected truthy value")
})

test_that("assert-false passes on falsy values", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # #f
  result <- engine$eval(
    engine$read("(assert-false #f)")[[1]], env = env)
  expect_true(result)

  # #nil
  result <- engine$eval(
    engine$read("(assert-false #nil)")[[1]], env = env)
  expect_true(result)
})

test_that("assert-false fails on truthy values", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  expect_error(
    engine$eval(engine$read("(assert-false #t)")[[1]], env = env),
    "Expected falsy value")

  expect_error(
    engine$eval(engine$read("(assert-false 1)")[[1]], env = env),
    "Expected falsy value")

  expect_error(
    engine$eval(engine$read('(assert-false "yes")')[[1]], env = env),
    "Expected falsy value")
})

test_that("assert-eq compares identity with identical?", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # Identical numbers
  result <- engine$eval(
    engine$read("(assert-eq 42 42)")[[1]], env = env)
  expect_true(result)

  # Identical booleans
  result <- engine$eval(
    engine$read("(assert-eq #t #t)")[[1]], env = env)
  expect_true(result)

  # Same symbol reference
  engine$eval(engine$read("(define x 'foo)")[[1]], env = env)
  result <- engine$eval(
    engine$read("(assert-eq x x)")[[1]], env = env)
  expect_true(result)
})

test_that("assert-eq passes for structurally identical lists", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # R's identical() returns TRUE for structurally identical lists
  result <- engine$eval(
    engine$read("(assert-eq (list 1 2) (list 1 2))")[[1]], env = env)
  expect_true(result)

  # Also works for nested lists
  result <- engine$eval(
    engine$read("(assert-eq (list 1 (list 2 3)) (list 1 (list 2 3)))")[[1]], env = env)
  expect_true(result)
})

test_that("assert-eq fails on non-identical values", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # Different list contents
  expect_error(
    engine$eval(
      engine$read("(assert-eq (list 1 2) (list 1 3))")[[1]], env = env),
    "Expected.*identical")

  # Different types
  expect_error(
    engine$eval(
      engine$read("(assert-eq 42 \"42\")")[[1]], env = env),
    "Expected.*identical")
})

test_that("assert-error passes when function throws error", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # Function that throws error
  result <- engine$eval(
    engine$read('(assert-error (lambda () (error "boom")))')[[1]], env = env)
  expect_true(result)

  # Function that calls stop
  result <- engine$eval(
    engine$read('(assert-error (lambda () (stop "error")))')[[1]], env = env)
  expect_true(result)
})

test_that("assert-error fails when function doesn't throw", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  expect_error(
    engine$eval(
      engine$read("(assert-error (lambda () 42))")[[1]], env = env),
    "Expected an error to be thrown")

  expect_error(
    engine$eval(
      engine$read("(assert-error (lambda () #t))")[[1]], env = env),
    "Expected an error to be thrown")
})

test_that("assert-no-error passes when function doesn't throw", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # Simple value
  result <- engine$eval(
    engine$read("(assert-no-error (lambda () 42))")[[1]], env = env)
  expect_true(result)

  # Expression
  result <- engine$eval(
    engine$read("(assert-no-error (lambda () (+ 1 2)))")[[1]], env = env)
  expect_true(result)

  # Returns #t
  result <- engine$eval(
    engine$read("(assert-no-error (lambda () #t))")[[1]], env = env)
  expect_true(result)
})

test_that("assert-no-error fails when function throws error", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  expect_error(
    engine$eval(
      engine$read('(assert-no-error (lambda () (error "boom")))')[[1]], env = env),
    "Expected no error to be thrown")

  expect_error(
    engine$eval(
      engine$read('(assert-no-error (lambda () (stop "fail")))')[[1]], env = env),
    "Expected no error to be thrown")
})

test_that("assert functions work with expressions", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # assert with expression
  result <- engine$eval(
    engine$read("(assert (= (+ 1 2) 3))")[[1]], env = env)
  expect_true(result)

  # assert-equal with expressions
  result <- engine$eval(
    engine$read("(assert-equal (* 2 3) (+ 3 3))")[[1]], env = env)
  expect_true(result)

  # assert-true with comparison
  result <- engine$eval(
    engine$read("(assert-true (> 10 5))")[[1]], env = env)
  expect_true(result)

  # assert-false with comparison
  result <- engine$eval(
    engine$read("(assert-false (< 10 5))")[[1]], env = env)
  expect_true(result)
})

test_that("assert functions work in combination", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # Multiple assertions in sequence
  result <- engine$eval(
    engine$read("(begin
      (assert-equal 1 1)
      (assert-true #t)
      (assert-false #f)
      42)")[[1]], env = env)
  expect_equal(result, 42)
})

test_that("assert functions short-circuit on first failure", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("assert"), env = env)

  # First assertion fails, second never evaluated
  engine$eval(engine$read("(define counter 0)")[[1]], env = env)

  expect_error(
    engine$eval(
      engine$read("(begin
        (assert #f)
        (set! counter 1))")[[1]], env = env),
    "Assertion failed")

  # Counter should still be 0
  expect_equal(env$counter, 0)
})
