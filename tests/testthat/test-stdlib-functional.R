# Functional programming tests: map, filter, reduce, foldl/foldr, every?, any?, mapcat, remove

engine <- make_engine()

test_that("map applies function to list", {
  env <- new.env()
  toplevel_env(engine, env = env)

  double <- function(x) x * 2
  result <- get("map", envir = env)(double, list(1, 2, 3))

  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("map works from Arl code", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Define a doubling function in Arl
  engine$eval(engine$read("(define double (lambda (x) (* x 2)))")[[1]], env = env)

  # Use map with the Arl function
  result <- engine$eval(engine$read("(map double (list 1 2 3))")[[1]], env = env)

  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("filter selects matching elements", {
  env <- new.env()
  toplevel_env(engine, env = env)

  is_even <- function(x) x %% 2 == 0
  result <- get("filter", envir = env)(is_even, list(1, 2, 3, 4, 5, 6))

  expect_equal(length(result), 3)
  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("filter works from Arl code", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Define a predicate in Arl
  engine$eval(engine$read("(define even? (lambda (x) (= (% x 2) 0)))")[[1]], env = env)

  # Use filter
  result <- engine$eval(engine$read("(filter even? (list 1 2 3 4 5 6))")[[1]], env = env)

  expect_equal(length(result), 3)
})

test_that("reduce combines list elements", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- get("reduce", envir = env)(`+`, list(1, 2, 3, 4))
  expect_equal(result, 10)

  result <- get("reduce", envir = env)(`*`, list(1, 2, 3, 4))
  expect_equal(result, 24)
})

test_that("foldl and foldr work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("foldl", envir = env)(`-`, list(1, 2, 3)), -4)
  expect_equal(get("foldr", envir = env)(`-`, list(1, 2, 3)), 2)
})

test_that("every? checks all elements match predicate", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(get("every?", envir = env)(function(x) x > 0, list(1, 2, 3)))
  expect_false(get("every?", envir = env)(function(x) x > 1, list(1, 2, 3)))
})

test_that("any? checks if any element matches predicate", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(get("any?", envir = env)(function(x) x > 2, list(1, 2, 3)))
  expect_false(get("any?", envir = env)(function(x) x > 5, list(1, 2, 3)))
})

test_that("mapcat maps and concatenates results", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- get("mapcat", envir = env)(function(x) list(x, x + 10), list(1, 2))
  expect_equal(result, list(1, 11, 2, 12))
})

test_that("remove filters out matching elements", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- get("remove", envir = env)(function(x) x %% 2 == 0, list(1, 2, 3, 4))
  expect_equal(result, list(1, 3))
})

test_that("complement negates predicate", {
  env <- new.env()
  toplevel_env(engine, env = env)

  is_even <- function(x) x %% 2 == 0
  is_odd <- get("complement", envir = env)(is_even)

  expect_true(is_odd(1))
  expect_false(is_odd(2))
  expect_true(is_odd(3))
  expect_false(is_odd(4))

  # Use with filter
  result <- get("filter", envir = env)(is_odd, list(1, 2, 3, 4, 5, 6))
  expect_equal(result, list(1, 3, 5))
})

test_that("compose combines functions", {
  env <- new.env()
  toplevel_env(engine, env = env)

  double <- function(x) x * 2
  add_one <- function(x) x + 1

  # compose applies right-to-left: f(g(x))
  double_then_add_one <- get("compose", envir = env)(add_one, double)
  expect_equal(double_then_add_one(5), 11)  # (5 * 2) + 1

  # Multiple compositions
  add_ten <- function(x) x + 10
  complex_fn <- get("compose", envir = env)(double, get("compose", envir = env)(add_one, add_ten))
  expect_equal(complex_fn(5), 32)  # ((5 + 10) + 1) * 2
})

test_that("partial applies arguments partially", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Partial application
  add <- function(a, b) a + b
  add_five <- get("partial", envir = env)(add, 5)

  expect_equal(add_five(3), 8)
  expect_equal(add_five(10), 15)

  # Multiple arguments
  multiply <- function(a, b, c) a * b * c
  multiply_by_2_3 <- get("partial", envir = env)(multiply, 2, 3)

  expect_equal(multiply_by_2_3(4), 24)  # 2 * 3 * 4
  expect_equal(multiply_by_2_3(5), 30)  # 2 * 3 * 5
})

test_that("repeat creates list with repeated value", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- get("repeat", envir = env)(5, "x")
  expect_equal(length(result), 5)
  expect_equal(result[[1]], "x")
  expect_equal(result[[5]], "x")

  # With number
  result <- get("repeat", envir = env)(3, 42)
  expect_equal(result, list(42, 42, 42))

  # With NULL
  result <- get("repeat", envir = env)(2, NULL)
  expect_equal(length(result), 2)
  expect_null(result[[1]])
})

test_that("zip combines lists element-wise", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Two lists
  result <- get("zip", envir = env)(list(1, 2, 3), list("a", "b", "c"))
  expect_equal(length(result), 3)
  expect_equal(result[[1]], list(1, "a"))
  expect_equal(result[[2]], list(2, "b"))
  expect_equal(result[[3]], list(3, "c"))

  # Three lists
  result <- get("zip", envir = env)(list(1, 2), list("a", "b"), list(TRUE, FALSE))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, "a", TRUE))
  expect_equal(result[[2]], list(2, "b", FALSE))

  # Different lengths (zip to shortest)
  result <- get("zip", envir = env)(list(1, 2, 3, 4), list("a", "b"))
  expect_equal(length(result), 2)

  # Empty list
  result <- get("zip", envir = env)(list(), list(1, 2))
  expect_equal(length(result), 0)
})

# ============================================================================
# NEW: High-priority functional helper tests
# ============================================================================

test_that("curry creates curried functions", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env = env)

  # Curry a function with 2 arguments
  engine$eval(engine$read("(define add (lambda (a b) (+ a b)))")[[1]], env = env)
  engine$eval(engine$read("(define add-curried (curry add))")[[1]], env = env)

  # Call with one argument returns partially applied function
  engine$eval(engine$read("(define add5 (add-curried 5))")[[1]], env = env)
  result <- engine$eval(engine$read("(add5 3)")[[1]], env = env)
  expect_equal(result, 8)

  # Call with all arguments at once works
  result <- engine$eval(engine$read("(add-curried 10 20)")[[1]], env = env)
  expect_equal(result, 30)

  # Curry with initial arguments
  engine$eval(engine$read("(define add10 (curry add 10))")[[1]], env = env)
  result <- engine$eval(engine$read("(add10 7)")[[1]], env = env)
  expect_equal(result, 17)
})

test_that("memoize caches function results", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env = env)

  # Create a function that counts how many times it's called
  engine$eval(engine$read("
    (begin
      (define call-count 0)
      (define expensive (lambda (x)
        (begin
          (set! call-count (+ call-count 1))
          (* x x))))
      (define memoized (memoize expensive)))
  ")[[1]], env = env)

  # First call - should execute function
  result1 <- engine$eval(engine$read("(memoized 5)")[[1]], env = env)
  expect_equal(result1, 25)
  expect_equal(engine$eval(engine$read("call-count")[[1]], env = env), 1)

  # Second call with same argument - should use cache
  result2 <- engine$eval(engine$read("(memoized 5)")[[1]], env = env)
  expect_equal(result2, 25)
  expect_equal(engine$eval(engine$read("call-count")[[1]], env = env), 1)  # Still 1

  # Call with different argument - should execute function again
  result3 <- engine$eval(engine$read("(memoized 7)")[[1]], env = env)
  expect_equal(result3, 49)
  expect_equal(engine$eval(engine$read("call-count")[[1]], env = env), 2)
})

test_that("juxt applies multiple functions to same arguments", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("functional", "list"), env = env)

  # Create juxtaposition of + and *
  engine$eval(engine$read("(define add-and-mult (juxt + *))")[[1]], env = env)

  result <- engine$eval(engine$read("(add-and-mult 3 4)")[[1]], env = env)
  expect_equal(result, list(7, 12))  # (+ 3 4) and (* 3 4)

  # Juxt with more functions
  engine$eval(engine$read("(define trio (juxt car cadr caddr))")[[1]], env = env)
  result <- engine$eval(engine$read("(trio (list 1 2 3 4 5))")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))
})

test_that("constantly returns function that always returns same value", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env = env)

  engine$eval(engine$read("(define always-42 (constantly 42))")[[1]], env = env)

  # No matter what arguments, always returns 42
  expect_equal(engine$eval(engine$read("(always-42)")[[1]], env = env), 42)
  expect_equal(engine$eval(engine$read("(always-42 1)")[[1]], env = env), 42)
  expect_equal(engine$eval(engine$read("(always-42 1 2 3)")[[1]], env = env), 42)

  # Use with map
  result <- engine$eval(engine$read("(map (constantly 'x) '(1 2 3))")[[1]], env = env)
  expect_equal(length(result), 3)
  expect_equal(as.character(result[[1]]), "x")
})

test_that("iterate applies function n times", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env = env)

  # Double a number 3 times
  engine$eval(engine$read("(define double (lambda (x) (* x 2)))")[[1]], env = env)
  result <- engine$eval(engine$read("(iterate double 3 5)")[[1]], env = env)
  expect_equal(result, 40)  # 5 * 2 * 2 * 2 = 40

  # Zero iterations returns initial value
  result <- engine$eval(engine$read("(iterate double 0 5)")[[1]], env = env)
  expect_equal(result, 5)

  # Single iteration
  result <- engine$eval(engine$read("(iterate double 1 10)")[[1]], env = env)
  expect_equal(result, 20)
})

test_that("iterate-until collects values until predicate is true", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env = env)

  # Double until value > 100
  engine$eval(engine$read("(define double (lambda (x) (* x 2)))")[[1]], env = env)
  result <- engine$eval(
    engine$read("(iterate-until double 5 (lambda (x) (> x 100)))")[[1]], env = env)

  # Should be: 5, 10, 20, 40, 80 (stops before 160)
  expect_equal(result, list(5, 10, 20, 40, 80))

  # Immediate termination
  result <- engine$eval(
    engine$read("(iterate-until (lambda (x) (* x 2)) 200 (lambda (x) (> x 100)))")[[1]], env = env)
  expect_equal(result, list(200))  # First value before checking next
})

# ============================================================================
# Coverage: mapcat with empty result, foldl/foldr no-init in Arl, curry 3-arg
# ============================================================================

test_that("mapcat with empty results returns empty list", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("functional"), env = env)

  result <- engine$eval(
    engine$read("(mapcat (lambda (x) (list)) (list 1 2 3))")[[1]], env = env)
  expect_equal(result, list())
})

test_that("foldl and foldr with no init value from Arl code", {
  env <- new.env()
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("functional"), env = env)

  # foldl with no init (uses Arl's + which is variadic)
  result <- engine$eval(
    engine$read("(foldl + (list 1 2 3))")[[1]], env = env)
  expect_equal(result, 6)

  # foldr with no init
  result <- engine$eval(
    engine$read("(foldr + (list 1 2 3))")[[1]], env = env)
  expect_equal(result, 6)
})

test_that("curry with 3-arg function enables multi-step partial application", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env = env)

  engine$eval(engine$read("(define add3 (curry (lambda (a b c) (+ a b c))))")[[1]], env = env)
  result <- engine$eval(engine$read("(((add3 1) 2) 3)")[[1]], env = env)
  expect_equal(result, 6)
})
