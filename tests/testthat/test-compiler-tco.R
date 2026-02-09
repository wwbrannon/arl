# Self-Tail-Call Optimization Tests

engine <- RyeEngine$new()

# ==============================================================================
# Core correctness tests
# ==============================================================================

test_that("TCO: factorial with accumulator", {
  engine$eval_text("
    (define fact (lambda (n acc)
      (if (<= n 1)
        acc
        (fact (- n 1) (* acc n)))))
  ")
  result <- engine$eval_text("(fact 10 1)")
  expect_equal(result, 3628800)
})

test_that("TCO: iterate pattern (fn applied n times)", {
  engine$eval_text("
    (define my-iterate (lambda (fn n init)
      (if (<= n 0)
        init
        (my-iterate fn (- n 1) (fn init)))))
  ")
  result <- engine$eval_text("(my-iterate (lambda (x) (+ x 1)) 5 0)")
  expect_equal(result, 5)
})

test_that("TCO: deep recursion does not stack overflow", {
  engine$eval_text("
    (define count-down (lambda (n)
      (if (<= n 0)
        0
        (count-down (- n 1)))))
  ")
  result <- engine$eval_text("(count-down 100000)")
  expect_equal(result, 0)
})

test_that("TCO: GCD (swap pattern tests temp correctness)", {
  engine$eval_text("
    (define gcd (lambda (a b)
      (if (== b 0)
        a
        (gcd b (modulo a b)))))
  ")
  result <- engine$eval_text("(gcd 12 8)")
  expect_equal(result, 4)
  result2 <- engine$eval_text("(gcd 100 75)")
  expect_equal(result2, 25)
})

test_that("TCO: works with cond (macro-expands to nested if)", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("control"), env)
  engine$eval_in_env(
    engine$read("
      (define classify (lambda (n)
        (cond
          ((< n 0) (quote negative))
          ((== n 0) (quote zero))
          (#t (classify (- n 1))))))
    ")[[1]],
    env
  )
  result <- engine$eval_in_env(engine$read("(classify 5)")[[1]], env)
  expect_equal(result, quote(zero))
})

test_that("TCO: single-param self-call (no temp needed)", {
  engine$eval_text("
    (define sum-to (lambda (n acc)
      (if (<= n 0)
        acc
        (sum-to (- n 1) (+ acc n)))))
  ")
  result <- engine$eval_text("(sum-to 100 0)")
  expect_equal(result, 5050)
})

test_that("TCO: unchanged param is not reassigned", {
  # fn stays the same in recursive call - should be optimized away
  engine$eval_text("
    (define apply-n (lambda (fn n val)
      (if (<= n 0)
        val
        (apply-n fn (- n 1) (fn val)))))
  ")
  result <- engine$eval_text("(apply-n (lambda (x) (+ x 1)) 10 0)")
  expect_equal(result, 10)
})

# ==============================================================================
# Non-interference tests
# ==============================================================================

test_that("TCO: non-recursive function is not affected", {
  out <- engine$inspect_compilation("(define add (lambda (a b) (+ a b)))")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_false(grepl("while", deparsed, fixed = TRUE))
})

test_that("TCO: non-tail-recursive function (fib) is not TCO'd", {
  engine$eval_text("
    (define fib (lambda (n)
      (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))
  ")
  out <- engine$inspect_compilation("
    (define fib (lambda (n)
      (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_false(grepl("while", deparsed, fixed = TRUE))
  # Still works for small inputs
  result <- engine$eval_text("(fib 10)")
  expect_equal(result, 55)
})

test_that("TCO: rest-param function is not TCO'd", {
  engine$eval_text("
    (define my-sum (lambda (. args)
      (if (null? args)
        0
        (+ (car args) (apply my-sum (cdr args))))))
  ")
  out <- engine$inspect_compilation("
    (define my-sum (lambda (. args)
      (if (null? args)
        0
        (+ (car args) (apply my-sum (cdr args))))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_false(grepl("while", deparsed, fixed = TRUE))
})

# ==============================================================================
# Compiled output verification
# ==============================================================================

test_that("VERIFY: TCO'd function has repeat and return, no self-call", {
  out <- engine$inspect_compilation("
    (define fact (lambda (n acc)
      (if (<= n 1)
        acc
        (fact (- n 1) (* acc n)))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_true(grepl("while", deparsed, fixed = TRUE))
  expect_true(grepl("return", deparsed, fixed = TRUE))
  # Should not contain a call to fact() in the compiled output
  expect_false(grepl("fact(", deparsed, fixed = TRUE))
})

test_that("VERIFY: non-TCO'd function has no repeat", {
  out <- engine$inspect_compilation("(define add (lambda (a b) (+ a b)))")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_false(grepl("while", deparsed, fixed = TRUE))
})

test_that("VERIFY: TCO with temp variables for multi-param swap", {
  out <- engine$inspect_compilation("
    (define gcd (lambda (a b)
      (if (== b 0)
        a
        (gcd b (modulo a b)))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_true(grepl("while", deparsed, fixed = TRUE))
  expect_true(grepl(".tco_", deparsed, fixed = TRUE))
})

test_that("TCO: begin in tail position", {
  engine$eval_text("
    (define count-with-side-effect (lambda (n acc)
      (if (<= n 0)
        acc
        (begin
          (+ 1 1)
          (count-with-side-effect (- n 1) (+ acc 1))))))
  ")
  result <- engine$eval_text("(count-with-side-effect 100 0)")
  expect_equal(result, 100)
})
