# Comprehensive binding form tests: let, let*, letrec, destructuring-bind, when-let, if-let

engine <- make_engine()

test_that("let binds variables in parallel", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  # Simple parallel bindings
  result <- engine$eval(
    engine$read("(let ((x 1) (y 2)) (+ x y))")[[1]], env = env)
  expect_equal(result, 3)

  # Variables don't see each other (parallel binding)
  engine$eval(engine$read("(define x 10)")[[1]], env = env)
  result <- engine$eval(
    engine$read("(let ((x 1) (y x)) y)")[[1]], env = env)
  expect_equal(result, 10)  # y sees outer x, not the new binding

  # Empty bindings
  result <- engine$eval(
    engine$read("(let () 42)")[[1]], env = env)
  expect_equal(result, 42)

  # Multiple expressions in body
  result <- engine$eval(
    engine$read("(let ((a 5)) (define b 10) (+ a b))")[[1]], env = env)
  expect_equal(result, 15)
})

test_that("let* binds variables sequentially", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  # Sequential bindings - later bindings see earlier ones
  result <- engine$eval(
    engine$read("(let* ((x 1) (y (+ x 1))) y)")[[1]], env = env)
  expect_equal(result, 2)

  # Chained sequential bindings
  result <- engine$eval(
    engine$read("(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c)")[[1]], env = env)
  expect_equal(result, 3)

  # Empty bindings
  result <- engine$eval(
    engine$read("(let* () 42)")[[1]], env = env)
  expect_equal(result, 42)

  # Works with complex expressions
  result <- engine$eval(
    engine$read("(let* ((x 5) (y (* x x)) (z (+ y x))) z)")[[1]], env = env)
  expect_equal(result, 30)  # (5 * 5) + 5
})

test_that("letrec allows recursive bindings", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  # Recursive factorial
  result <- engine$eval(
    engine$read("(letrec ((fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))) (fact 5))")[[1]], env = env)
  expect_equal(result, 120)

  # Mutually recursive functions
  result <- engine$eval(
    engine$read("
      (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
               (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
        (list (even? 4) (odd? 4) (even? 5) (odd? 5)))
    ")[[1]], env = env)
  expect_equal(result, list(TRUE, FALSE, FALSE, TRUE))

  # Simple recursive binding
  result <- engine$eval(
    engine$read("(letrec ((x 10)) x)")[[1]], env = env)
  expect_equal(result, 10)
})

test_that("destructuring-bind unpacks structures", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  # Simple list destructuring
  result <- engine$eval(
    engine$read("(destructuring-bind (a b c) (list 1 2 3) (+ a b c))")[[1]], env = env)
  expect_equal(result, 6)

  # Nested destructuring
  result <- engine$eval(
    engine$read("(destructuring-bind (a (b c)) (list 1 (list 2 3)) (+ a b c))")[[1]], env = env)
  expect_equal(result, 6)

  # Single variable
  result <- engine$eval(
    engine$read("(destructuring-bind x 42 x)")[[1]], env = env)
  expect_equal(result, 42)
})

test_that("pattern-symbols extracts symbols from patterns", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  # Single symbol
  result <- engine$eval(
    engine$read("(pattern-symbols 'x)")[[1]], env = env)
  expect_equal(length(result), 1)
  expect_equal(as.character(result[[1]]), "x")

  # List pattern
  result <- engine$eval(
    engine$read("(pattern-symbols '(a b c))")[[1]], env = env)
  expect_equal(length(result), 3)
  expect_equal(as.character(result[[1]]), "a")
  expect_equal(as.character(result[[2]]), "b")
  expect_equal(as.character(result[[3]]), "c")

  # Nested pattern
  result <- engine$eval(
    engine$read("(pattern-symbols '(a (b c)))")[[1]], env = env)
  expect_equal(length(result), 3)
})

test_that("binding macros when-let and if-let work", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  result <- engine$eval(engine$read("(when-let (x 10) (+ x 1))")[[1]], env = env)
  expect_equal(result, 11)

  result <- engine$eval(engine$read("(when-let (x #f) (+ x 1))")[[1]], env = env)
  expect_null(result)

  result <- engine$eval(engine$read("(when-let ((a b) (list 1 2)) (+ a b))")[[1]], env = env)
  expect_equal(result, 3)

  result <- engine$eval(engine$read("(if-let (x 5) (+ x 1) 0)")[[1]], env = env)
  expect_equal(result, 6)

  result <- engine$eval(engine$read("(if-let (x #nil) 1 2)")[[1]], env = env)
  expect_equal(result, 2)

  result <- engine$eval(engine$read("(if-let (x #f) 1 2)")[[1]], env = env)
  expect_equal(result, 2)

  result <- engine$eval(engine$read("(if-let (x #f) 1)")[[1]], env = env)
  expect_null(result)

  result <- engine$eval(engine$read("(if-let ((a b) (list 3 4)) (+ a b) 0)")[[1]], env = env)
  expect_equal(result, 7)
})
