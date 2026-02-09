# Functional programming tests: map, filter, reduce, foldl/foldr, every?, any?, mapcat, remove

engine <- make_engine()

test_that("map applies function to list", {
  env <- new.env()
  stdlib_env(engine, env)

  double <- function(x) x * 2
  result <- env$map(double, list(1, 2, 3))

  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("map works from Rye code", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define a doubling function in Rye
  engine$eval_in_env(engine$read("(define double (lambda (x) (* x 2)))")[[1]], env)

  # Use map with the Rye function
  result <- engine$eval_in_env(engine$read("(map double (list 1 2 3))")[[1]], env)

  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("filter selects matching elements", {
  env <- new.env()
  stdlib_env(engine, env)

  is_even <- function(x) x %% 2 == 0
  result <- env$filter(is_even, list(1, 2, 3, 4, 5, 6))

  expect_equal(length(result), 3)
  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("filter works from Rye code", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define a predicate in Rye
  engine$eval_in_env(engine$read("(define even? (lambda (x) (= (% x 2) 0)))")[[1]], env)

  # Use filter
  result <- engine$eval_in_env(engine$read("(filter even? (list 1 2 3 4 5 6))")[[1]], env)

  expect_equal(length(result), 3)
})

test_that("reduce combines list elements", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- env$reduce(`+`, list(1, 2, 3, 4))
  expect_equal(result, 10)

  result <- env$reduce(`*`, list(1, 2, 3, 4))
  expect_equal(result, 24)
})

test_that("foldl and foldr work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$foldl(`-`, list(1, 2, 3)), -4)
  expect_equal(env$foldr(`-`, list(1, 2, 3)), 2)
})

test_that("every? checks all elements match predicate", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_true(env$`every?`(function(x) x > 0, list(1, 2, 3)))
  expect_false(env$`every?`(function(x) x > 1, list(1, 2, 3)))
})

test_that("any? checks if any element matches predicate", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_true(env$`any?`(function(x) x > 2, list(1, 2, 3)))
  expect_false(env$`any?`(function(x) x > 5, list(1, 2, 3)))
})

test_that("mapcat maps and concatenates results", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- env$mapcat(function(x) list(x, x + 10), list(1, 2))
  expect_equal(result, list(1, 11, 2, 12))
})

test_that("remove filters out matching elements", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- env$remove(function(x) x %% 2 == 0, list(1, 2, 3, 4))
  expect_equal(result, list(1, 3))
})

test_that("complement negates predicate", {
  env <- new.env()
  stdlib_env(engine, env)

  is_even <- function(x) x %% 2 == 0
  is_odd <- env$complement(is_even)

  expect_true(is_odd(1))
  expect_false(is_odd(2))
  expect_true(is_odd(3))
  expect_false(is_odd(4))

  # Use with filter
  result <- env$filter(is_odd, list(1, 2, 3, 4, 5, 6))
  expect_equal(result, list(1, 3, 5))
})

test_that("compose combines functions", {
  env <- new.env()
  stdlib_env(engine, env)

  double <- function(x) x * 2
  add_one <- function(x) x + 1

  # compose applies right-to-left: f(g(x))
  double_then_add_one <- env$compose(add_one, double)
  expect_equal(double_then_add_one(5), 11)  # (5 * 2) + 1

  # Multiple compositions
  add_ten <- function(x) x + 10
  complex_fn <- env$compose(double, env$compose(add_one, add_ten))
  expect_equal(complex_fn(5), 32)  # ((5 + 10) + 1) * 2
})

test_that("partial applies arguments partially", {
  env <- new.env()
  stdlib_env(engine, env)

  # Partial application
  add <- function(a, b) a + b
  add_five <- env$partial(add, 5)

  expect_equal(add_five(3), 8)
  expect_equal(add_five(10), 15)

  # Multiple arguments
  multiply <- function(a, b, c) a * b * c
  multiply_by_2_3 <- env$partial(multiply, 2, 3)

  expect_equal(multiply_by_2_3(4), 24)  # 2 * 3 * 4
  expect_equal(multiply_by_2_3(5), 30)  # 2 * 3 * 5
})

test_that("repeatedly calls function n times", {
  env <- new.env()
  stdlib_env(engine, env)

  counter <- 0
  increment <- function() {
    counter <<- counter + 1
    counter
  }

  result <- env$repeatedly(5, increment)
  expect_equal(length(result), 5)
  expect_equal(result[[1]], 1)
  expect_equal(result[[5]], 5)
})

test_that("repeat creates list with repeated value", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- env$`repeat`(5, "x")
  expect_equal(length(result), 5)
  expect_equal(result[[1]], "x")
  expect_equal(result[[5]], "x")

  # With number
  result <- env$`repeat`(3, 42)
  expect_equal(result, list(42, 42, 42))

  # With NULL
  result <- env$`repeat`(2, NULL)
  expect_equal(length(result), 2)
  expect_null(result[[1]])
})

test_that("zip combines lists element-wise", {
  env <- new.env()
  stdlib_env(engine, env)

  # Two lists
  result <- env$zip(list(1, 2, 3), list("a", "b", "c"))
  expect_equal(length(result), 3)
  expect_equal(result[[1]], list(1, "a"))
  expect_equal(result[[2]], list(2, "b"))
  expect_equal(result[[3]], list(3, "c"))

  # Three lists
  result <- env$zip(list(1, 2), list("a", "b"), list(TRUE, FALSE))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, "a", TRUE))
  expect_equal(result[[2]], list(2, "b", FALSE))

  # Different lengths (zip to shortest)
  result <- env$zip(list(1, 2, 3, 4), list("a", "b"))
  expect_equal(length(result), 2)

  # Empty list
  result <- env$zip(list(), list(1, 2))
  expect_equal(length(result), 0)
})

# ============================================================================
# NEW: High-priority functional helper tests
# ============================================================================

test_that("curry creates curried functions", {
  env <- stdlib_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env)

  # Curry a function with 2 arguments
  engine$eval_in_env(engine$read("(define add (lambda (a b) (+ a b)))")[[1]], env)
  engine$eval_in_env(engine$read("(define add-curried (curry add))")[[1]], env)

  # Call with one argument returns partially applied function
  engine$eval_in_env(engine$read("(define add5 (add-curried 5))")[[1]], env)
  result <- engine$eval_in_env(engine$read("(add5 3)")[[1]], env)
  expect_equal(result, 8)

  # Call with all arguments at once works
  result <- engine$eval_in_env(engine$read("(add-curried 10 20)")[[1]], env)
  expect_equal(result, 30)

  # Curry with initial arguments
  engine$eval_in_env(engine$read("(define add10 (curry add 10))")[[1]], env)
  result <- engine$eval_in_env(engine$read("(add10 7)")[[1]], env)
  expect_equal(result, 17)
})

test_that("memoize caches function results", {
  env <- stdlib_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env)

  # Create a function that counts how many times it's called
  engine$eval_in_env(engine$read("
    (begin
      (define call-count 0)
      (define expensive (lambda (x)
        (begin
          (set! call-count (+ call-count 1))
          (* x x))))
      (define memoized (memoize expensive)))
  ")[[1]], env)

  # First call - should execute function
  result1 <- engine$eval_in_env(engine$read("(memoized 5)")[[1]], env)
  expect_equal(result1, 25)
  expect_equal(engine$eval_in_env(engine$read("call-count")[[1]], env), 1)

  # Second call with same argument - should use cache
  result2 <- engine$eval_in_env(engine$read("(memoized 5)")[[1]], env)
  expect_equal(result2, 25)
  expect_equal(engine$eval_in_env(engine$read("call-count")[[1]], env), 1)  # Still 1

  # Call with different argument - should execute function again
  result3 <- engine$eval_in_env(engine$read("(memoized 7)")[[1]], env)
  expect_equal(result3, 49)
  expect_equal(engine$eval_in_env(engine$read("call-count")[[1]], env), 2)
})

test_that("juxt applies multiple functions to same arguments", {
  env <- stdlib_env(engine, new.env())
  import_stdlib_modules(engine, c("functional", "list"), env)

  # Create juxtaposition of + and *
  engine$eval_in_env(engine$read("(define add-and-mult (juxt + *))")[[1]], env)

  result <- engine$eval_in_env(engine$read("(add-and-mult 3 4)")[[1]], env)
  expect_equal(result, list(7, 12))  # (+ 3 4) and (* 3 4)

  # Juxt with more functions
  engine$eval_in_env(engine$read("(define trio (juxt car cadr caddr))")[[1]], env)
  result <- engine$eval_in_env(engine$read("(trio (list 1 2 3 4 5))")[[1]], env)
  expect_equal(result, list(1, 2, 3))
})

test_that("constantly returns function that always returns same value", {
  env <- stdlib_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env)

  engine$eval_in_env(engine$read("(define always-42 (constantly 42))")[[1]], env)

  # No matter what arguments, always returns 42
  expect_equal(engine$eval_in_env(engine$read("(always-42)")[[1]], env), 42)
  expect_equal(engine$eval_in_env(engine$read("(always-42 1)")[[1]], env), 42)
  expect_equal(engine$eval_in_env(engine$read("(always-42 1 2 3)")[[1]], env), 42)

  # Use with map
  result <- engine$eval_in_env(engine$read("(map (constantly 'x) '(1 2 3))")[[1]], env)
  expect_equal(length(result), 3)
  expect_equal(as.character(result[[1]]), "x")
})

test_that("iterate applies function n times", {
  env <- stdlib_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env)

  # Double a number 3 times
  engine$eval_in_env(engine$read("(define double (lambda (x) (* x 2)))")[[1]], env)
  result <- engine$eval_in_env(engine$read("(iterate double 3 5)")[[1]], env)
  expect_equal(result, 40)  # 5 * 2 * 2 * 2 = 40

  # Zero iterations returns initial value
  result <- engine$eval_in_env(engine$read("(iterate double 0 5)")[[1]], env)
  expect_equal(result, 5)

  # Single iteration
  result <- engine$eval_in_env(engine$read("(iterate double 1 10)")[[1]], env)
  expect_equal(result, 20)
})

test_that("iterate-until collects values until predicate is true", {
  env <- stdlib_env(engine, new.env())
  import_stdlib_modules(engine, c("functional"), env)

  # Double until value > 100
  engine$eval_in_env(engine$read("(define double (lambda (x) (* x 2)))")[[1]], env)
  result <- engine$eval_in_env(
    engine$read("(iterate-until double 5 (lambda (x) (> x 100)))")[[1]], env)

  # Should be: 5, 10, 20, 40, 80 (stops before 160)
  expect_equal(result, list(5, 10, 20, 40, 80))

  # Immediate termination
  result <- engine$eval_in_env(
    engine$read("(iterate-until (lambda (x) (* x 2)) 200 (lambda (x) (> x 100)))")[[1]], env)
  expect_equal(result, list(200))  # First value before checking next
})
