test_that("stdlib loads successfully", {
  env <- new.env()
  rye_load_stdlib(env)

  expect_true(exists("car", envir = env))
  expect_true(exists("cdr", envir = env))
  expect_true(exists("map", envir = env))
  expect_true(exists("filter", envir = env))
  expect_true(exists("reduce", envir = env))
})

test_that("car returns first element", {
  env <- new.env()
  rye_load_stdlib(env)

  # Test with R list
  expect_equal(env$car(list(1, 2, 3)), 1)

  # Test with parsed expression
  expr <- rye_read("(+ 1 2)")[[1]]
  expect_equal(as.character(env$car(expr)), "+")
})

test_that("cdr returns rest of list", {
  env <- new.env()
  rye_load_stdlib(env)

  # Test with R list
  result <- env$cdr(list(1, 2, 3))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 3)
})

test_that("cons adds element to front", {
  env <- new.env()
  rye_load_stdlib(env)

  result <- env$cons(1, list(2, 3))
  expect_equal(result[[1]], 1)
  expect_equal(result[[2]], 2)
  expect_equal(result[[3]], 3)
})

test_that("map applies function to list", {
  env <- new.env()
  rye_load_stdlib(env)

  double <- function(x) x * 2
  result <- env$map(double, list(1, 2, 3))

  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("filter selects matching elements", {
  env <- new.env()
  rye_load_stdlib(env)

  is_even <- function(x) x %% 2 == 0
  result <- env$filter(is_even, list(1, 2, 3, 4, 5, 6))

  expect_equal(length(result), 3)
  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("reduce combines list elements", {
  env <- new.env()
  rye_load_stdlib(env)

  result <- env$reduce(`+`, list(1, 2, 3, 4))
  expect_equal(result, 10)

  result <- env$reduce(`*`, list(1, 2, 3, 4))
  expect_equal(result, 24)
})

test_that("map works from Rye code", {
  env <- new.env()
  rye_load_stdlib(env)

  # Define a doubling function in Rye
  rye_eval(rye_read("(define double (lambda (x) (* x 2)))")[[1]], env)

  # Use map with the Rye function
  result <- rye_eval(rye_read("(map double (list 1 2 3))")[[1]], env)

  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("filter works from Rye code", {
  env <- new.env()
  rye_load_stdlib(env)

  # Define a predicate in Rye
  rye_eval(rye_read("(define even? (lambda (x) (= (% x 2) 0)))")[[1]], env)

  # Use filter
  result <- rye_eval(rye_read("(filter even? (list 1 2 3 4 5 6))")[[1]], env)

  expect_equal(length(result), 3)
})

test_that("predicates work correctly", {
  env <- new.env()
  rye_load_stdlib(env)

  expect_true(rye_eval(rye_read("(number? 42)")[[1]], env))
  expect_false(rye_eval(rye_read("(number? \"hello\")")[[1]], env))

  expect_true(rye_eval(rye_read("(string? \"hello\")")[[1]], env))
  expect_false(rye_eval(rye_read("(string? 42)")[[1]], env))

  expect_true(rye_eval(rye_read("(null? #nil)")[[1]], env))
  expect_false(rye_eval(rye_read("(null? 42)")[[1]], env))
})

test_that("and macro works", {
  env <- new.env()

  # Define and macro
  rye_eval(rye_read("(defmacro and (first second) `(if ,first ,second #f))")[[1]], env)

  result <- rye_eval(rye_read("(and #t #t)")[[1]], env)
  expect_true(result)

  result <- rye_eval(rye_read("(and #t #f)")[[1]], env)
  expect_false(result)

  result <- rye_eval(rye_read("(and #f #t)")[[1]], env)
  expect_false(result)
})

test_that("or macro works", {
  env <- new.env()

  # Define or macro
  rye_eval(rye_read("(defmacro or (first second) `(if ,first #t ,second))")[[1]], env)

  result <- rye_eval(rye_read("(or #t #f)")[[1]], env)
  expect_true(result)

  result <- rye_eval(rye_read("(or #f #t)")[[1]], env)
  expect_true(result)

  result <- rye_eval(rye_read("(or #f #f)")[[1]], env)
  expect_false(result)
})

test_that("not function works", {
  env <- new.env()
  rye_load_stdlib(env)

  expect_false(rye_eval(rye_read("(not #t)")[[1]], env))
  expect_true(rye_eval(rye_read("(not #f)")[[1]], env))
  expect_false(rye_eval(rye_read("(not 42)")[[1]], env))
})
