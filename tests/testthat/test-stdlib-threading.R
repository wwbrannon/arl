# Comprehensive threading macro tests: -> (thread-first) and ->> (thread-last)

engine <- make_engine()

test_that("-> threads value as first argument", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # Simple threading
  result <- engine$eval(
    engine$read("(-> 5 (+ 3) (* 2))")[[1]], env = env)
  expect_equal(result, 16)  # ((5 + 3) * 2) = 16

  # Thread with single argument functions
  result <- engine$eval(
    engine$read("(-> 10 (- 5) (/ 2) (+ 1))")[[1]], env = env)
  expect_equal(result, 3.5)  # (((10 - 5) / 2) + 1) = 3.5

  # Thread through multiple operations
  result <- engine$eval(
    engine$read("(-> 100 (/ 10) (+ 5) (* 2))")[[1]], env = env)
  expect_equal(result, 30)  # (((100 / 10) + 5) * 2) = 30
})

test_that("->> threads value as last argument", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading", "list", "functional"), env = env)

  # Thread through list operations
  result <- engine$eval(
    engine$read("(->> (list 1 2 3) (map (lambda (x) (* x 2))) (filter (lambda (x) (> x 2))))")[[1]], env = env)
  expect_equal(result, list(4, 6))

  # Thread with reduce
  result <- engine$eval(
    engine$read("(->> (list 1 2 3 4) (map (lambda (x) (* x x))) (reduce +))")[[1]], env = env)
  expect_equal(result, 30)  # 1 + 4 + 9 + 16 = 30
})

test_that("threading macros work with nested forms", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # Nested threading with ->
  result <- engine$eval(
    engine$read("(-> 10 (- 5) (/ 2) (+ 1))")[[1]], env = env)
  expect_equal(result, 3.5)

  # Thread through complex expression
  result <- engine$eval(
    engine$read("(-> 5 (+ 10) (- 3) (* 2))")[[1]], env = env)
  expect_equal(result, 24)  # (((5 + 10) - 3) * 2) = 24
})

test_that("threading macros handle single operations", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # Single operation with ->
  result <- engine$eval(
    engine$read("(-> 5 (+ 3))")[[1]], env = env)
  expect_equal(result, 8)

  # Single operation with ->>
  result <- engine$eval(
    engine$read("(->> 5 (* 2))")[[1]], env = env)
  expect_equal(result, 10)
})

test_that("threading can be combined with other macros", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading", "binding"), env = env)

  # Combine -> with let
  result <- engine$eval(
    engine$read("(let ((x 10)) (-> x (+ 5) (* 2)))")[[1]], env = env)
  expect_equal(result, 30)

  # Thread result into let binding
  result <- engine$eval(
    engine$read("(let ((x (-> 5 (+ 3) (* 2)))) x)")[[1]], env = env)
  expect_equal(result, 16)
})

# ============================================================================
# as->
# ============================================================================

test_that("as-> threads with named binding", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # Simple case
  result <- engine$eval(
    engine$read("(as-> 1 x (+ x 1) (* x 3))")[[1]], env = env)
  expect_equal(result, 6)  # (1+1)=2, 2*3=6

  # Value in different positions
  result <- engine$eval(
    engine$read("(as-> 5 x (- 10 x) (* x 2))")[[1]], env = env)
  expect_equal(result, 10)  # (10-5)=5, 5*2=10

  # Single form
  result <- engine$eval(
    engine$read("(as-> 42 x (+ x 0))")[[1]], env = env)
  expect_equal(result, 42)
})

# ============================================================================
# some->
# ============================================================================

test_that("some-> threads through truthy values", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # All truthy - works like ->
  result <- engine$eval(
    engine$read("(some-> 5 (+ 3) (* 2))")[[1]], env = env)
  expect_equal(result, 16)
})

test_that("some-> short-circuits on nil", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # Short-circuits on nil
  result <- engine$eval(
    engine$read("(some-> #nil (+ 3) (* 2))")[[1]], env = env)
  expect_null(result)
})

test_that("some-> short-circuits on false", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # Short-circuits on #f
  result <- engine$eval(
    engine$read("(some-> #f (+ 3))")[[1]], env = env)
  expect_false(result)
})

test_that("some-> short-circuits on 0 (falsy in Arl)", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # 0 is falsy in Arl, so some-> should short-circuit
  result <- engine$eval(
    engine$read("(some-> 0 (+ 3))")[[1]], env = env)
  expect_equal(result, 0)
})

# ============================================================================
# some->>
# ============================================================================

test_that("some->> threads last with short-circuit", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # All truthy - works like ->>
  result <- engine$eval(
    engine$read("(some->> 5 (- 10))")[[1]], env = env)
  expect_equal(result, 5)  # (- 10 5) = 5

  # Short-circuits on nil
  result <- engine$eval(
    engine$read("(some->> #nil (- 10))")[[1]], env = env)
  expect_null(result)
})

test_that("some->> short-circuits on 0 (falsy in Arl)", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # 0 is falsy in Arl, so some->> should short-circuit
  result <- engine$eval(
    engine$read("(some->> 0 (- 10))")[[1]], env = env)
  expect_equal(result, 0)
})

# ============================================================================
# cond->
# ============================================================================

test_that("cond-> conditionally applies steps", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # Both conditions true
  result <- engine$eval(
    engine$read("(cond-> 1 (#t (+ 1)) (#t (* 3)))")[[1]], env = env)
  expect_equal(result, 6)  # 1+1=2, 2*3=6

  # First false, second true
  result <- engine$eval(
    engine$read("(cond-> 1 (#f (+ 1)) (#t (* 3)))")[[1]], env = env)
  expect_equal(result, 3)  # skip +1, 1*3=3

  # Both false
  result <- engine$eval(
    engine$read("(cond-> 42 (#f (+ 1)) (#f (* 3)))")[[1]], env = env)
  expect_equal(result, 42)  # unchanged
})

test_that("cond-> with computed conditions", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  result <- engine$eval(
    engine$read("(cond-> 10 ((> 3 2) (+ 5)) ((< 3 2) (* 100)))")[[1]], env = env)
  expect_equal(result, 15)  # 10+5=15, skip *100
})

# ============================================================================
# cond->>
# ============================================================================

test_that("cond->> conditionally applies steps (thread-last)", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("threading"), env = env)

  # Thread-last variant
  result <- engine$eval(
    engine$read("(cond->> 5 (#t (- 10)) (#f (* 100)))")[[1]], env = env)
  expect_equal(result, 5)  # (- 10 5) = 5, skip *100

  # All conditions false
  result <- engine$eval(
    engine$read("(cond->> 42 (#f (+ 1)) (#f (- 100)))")[[1]], env = env)
  expect_equal(result, 42)
})
