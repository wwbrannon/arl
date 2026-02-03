engine <- RyeEngine$new()

test_that("quote returns unevaluated expression", {
  result <- engine$eval(engine$read("(quote x)")[[1]])
  expect_true(is.symbol(result))
  expect_equal(as.character(result), "x")

  result <- engine$eval(engine$read("(quote (+ 1 2))")[[1]])
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")
})

test_that("quote sugar works", {
  result <- engine$eval(engine$read("'x")[[1]])
  expect_true(is.symbol(result))
  expect_equal(as.character(result), "x")

  result <- engine$eval(engine$read("'(+ 1 2)")[[1]])
  expect_true(is.call(result))
})

test_that("delay creates a promise", {
  result <- engine$eval(engine$read("(delay (+ 1 2))")[[1]])
  expect_true(r6_isinstance(result, "RyePromise"))
})

test_that("promise? detects promises", {
  env <- stdlib_env(engine)
  result <- engine$eval_in_env(engine$read("(promise? (delay 1))")[[1]], env)
  expect_true(isTRUE(result))

  non_promise <- engine$eval_in_env(engine$read("(promise? 1)")[[1]], env)
  expect_false(isTRUE(non_promise))
})

test_that("delay is lazy until forced", {
  env <- stdlib_env(engine)
  engine$eval_in_env(
    engine$read("(begin (define counter 0)\n  (define p (delay (begin (set! counter (+ counter 1)) counter)))\n  counter)")[[1]],
    env
  )
  expect_equal(env$counter, 0)

  engine$eval_in_env(engine$read("(force p)")[[1]], env)
  expect_equal(env$counter, 1)
})


test_that("if evaluates conditionally", {
  result <- engine$eval(engine$read("(if #t 1 2)")[[1]])
  expect_equal(result, 1)

  result <- engine$eval(engine$read("(if #f 1 2)")[[1]])
  expect_equal(result, 2)

  result <- engine$eval(engine$read("(if #nil 1 2)")[[1]])
  expect_equal(result, 2)
})

test_that("if with no else branch returns NULL", {
  result <- engine$eval(engine$read("(if #f 1)")[[1]])
  expect_null(result)
})

test_that("if evaluates test expression", {
  result <- engine$eval(engine$read("(if (> 5 3) 'yes 'no)")[[1]])
  expect_equal(as.character(result), "yes")

  result <- engine$eval(engine$read("(if (< 5 3) 'yes 'no)")[[1]])
  expect_equal(as.character(result), "no")
})

test_that("define creates variables", {
  env <- new.env()
  engine$eval_in_env(engine$read("(define x 42)")[[1]], env)
  expect_equal(env$x, 42)

  engine$eval_in_env(engine$read("(define y (+ 1 2))")[[1]], env)
  expect_equal(env$y, 3)
})

test_that("lambda creates functions", {
  env <- new.env()
  engine$eval_in_env(engine$read("(define add (lambda (a b) (+ a b)))")[[1]], env)
  expect_true(is.function(env$add))

  result <- engine$eval_in_env(engine$read("(add 3 4)")[[1]], env)
  expect_equal(result, 7)
})

test_that("lambda with no arguments", {
  env <- new.env()
  engine$eval_in_env(engine$read("(define get-five (lambda () 5))")[[1]], env)
  result <- engine$eval_in_env(engine$read("(get-five)")[[1]], env)
  expect_equal(result, 5)
})

test_that("lambda supports default arguments", {
  env <- new.env()
  engine$eval_in_env(engine$read("(define add-default (lambda ((x 10) (y 5)) (+ x y)))")[[1]], env)
  expect_equal(engine$eval_in_env(engine$read("(add-default)")[[1]], env), 15)
  expect_equal(engine$eval_in_env(engine$read("(add-default 2)")[[1]], env), 7)
  expect_equal(engine$eval_in_env(engine$read("(add-default 2 3)")[[1]], env), 5)
})

test_that("lambda supports destructuring arguments", {
  env <- new.env()
  engine$eval_in_env(engine$read("(define sum-pair (lambda ((pattern (a b))) (+ a b)))")[[1]], env)
  expect_equal(engine$eval_in_env(engine$read("(sum-pair (list 2 3))")[[1]], env), 5)

  engine$eval_in_env(engine$read("(define nested (lambda ((pattern (a (b c)))) (+ a (+ b c))))")[[1]], env)
  expect_equal(engine$eval_in_env(engine$read("(nested (list 1 (list 2 3)))")[[1]], env), 6)

  engine$eval_in_env(engine$read("(define sum-default (lambda ((pattern (a b) (list 4 5))) (+ a b)))")[[1]], env)
  expect_equal(engine$eval_in_env(engine$read("(sum-default)")[[1]], env), 9)
  expect_equal(engine$eval_in_env(engine$read("(sum-default (list 1 2))")[[1]], env), 3)

  engine$eval_in_env(engine$read("(define rest-pair (lambda (x . (pattern (a b))) (list x a b)))")[[1]], env)
  expect_equal(engine$eval_in_env(engine$read("(rest-pair 1 2 3)")[[1]], env), list(1, 2, 3))
})

test_that("lambda supports dotted rest arguments", {
  env <- new.env()
  engine$eval_in_env(engine$read("(define count-rest (lambda (x . rest) (length rest)))")[[1]], env)
  expect_equal(engine$eval_in_env(engine$read("(count-rest 1)")[[1]], env), 0)
  expect_equal(engine$eval_in_env(engine$read("(count-rest 1 2 3 4)")[[1]], env), 3)
})

test_that("lambda with multiple body expressions", {
  env <- new.env()
  engine$eval_in_env(engine$read("(define f (lambda (x) (define y 10) (+ x y)))")[[1]], env)
  result <- engine$eval_in_env(engine$read("(f 5)")[[1]], env)
  expect_equal(result, 15)
})

test_that("lambda captures environment", {
  env <- new.env()
  engine$eval_in_env(engine$read("(define x 10)")[[1]], env)
  engine$eval_in_env(engine$read("(define add-x (lambda (y) (+ x y)))")[[1]], env)
  result <- engine$eval_in_env(engine$read("(add-x 5)")[[1]], env)
  expect_equal(result, 15)
})

test_that("begin evaluates expressions in sequence", {
  env <- new.env()
  result <- engine$eval_in_env(engine$read("(begin (define x 1) (define y 2) (+ x y))")[[1]], env)
  expect_equal(result, 3)
  expect_equal(env$x, 1)
  expect_equal(env$y, 2)
})

test_that("begin returns last expression", {
  result <- engine$eval(engine$read("(begin 1 2 3)")[[1]])
  expect_equal(result, 3)
})

test_that("set! modifies existing bindings", {
  env <- new.env()

  # Define a variable
  engine$eval_in_env(engine$read("(define x 10)")[[1]], env)
  expect_equal(env$x, 10)

  # Modify it with set!
  engine$eval_in_env(engine$read("(set! x 20)")[[1]], env)
  expect_equal(env$x, 20)

  # set! should error on undefined variable
  expect_error(
    engine$eval_in_env(engine$read("(set! undefined-var 42)")[[1]], env),
    "variable 'undefined-var' is not defined"
  )

  # set! in a lambda should modify parent scope
  engine$eval_in_env(engine$read("(define y 5)")[[1]], env)
  engine$eval_in_env(engine$read("(define f (lambda () (set! y 15)))")[[1]], env)
  expect_equal(env$y, 5)
  env$f()
  expect_equal(env$y, 15)
})
