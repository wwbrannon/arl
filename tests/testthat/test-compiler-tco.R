# Self-Tail-Call Optimization Tests

engine <- make_engine()

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

test_that("TCO: sum-to with two-param self-call", {
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

test_that("TCO: rest-param function with apply (not direct self-call) is not TCO'd", {
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

test_that("VERIFY: TCO'd function has while-loop and return, no self-call", {
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
  expect_true(grepl(".__tco_", deparsed, fixed = TRUE))
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

# ==============================================================================
# Destructuring params
# ==============================================================================

test_that("TCO: destructuring params with self-tail-call", {
  engine$eval_text("
    (define sum-pairs (lambda ((pattern (a b)) n acc)
      (if (<= n 0)
        acc
        (sum-pairs (list (+ a 1) (+ b 1)) (- n 1) (+ acc a b)))))
  ")
  result <- engine$eval_text("(sum-pairs (list 1 2) 3 0)")
  # Iteration 1: a=1, b=2 -> acc=0+1+2=3, next (2,3) n=2
  # Iteration 2: a=2, b=3 -> acc=3+2+3=8, next (3,4) n=1
  # Iteration 3: a=3, b=4 -> acc=8+3+4=15, next (4,5) n=0
  # Return 15
  expect_equal(result, 15)
})

test_that("TCO: deep recursion with destructuring does not stack overflow", {
  engine$eval_text("
    (define count-pair (lambda ((pattern (a b)) n)
      (if (<= n 0)
        (+ a b)
        (count-pair (list (+ a 1) b) (- n 1)))))
  ")
  result <- engine$eval_text("(count-pair (list 0 0) 100000)")
  expect_equal(result, 100000)
})

test_that("VERIFY: TCO'd destructuring has while and .__assign_pattern inside loop", {
  out <- engine$inspect_compilation("
    (define sum-pairs (lambda ((pattern (a b)) n acc)
      (if (<= n 0)
        acc
        (sum-pairs (list (+ a 1) (+ b 1)) (- n 1) (+ acc a b)))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_true(grepl("while", deparsed, fixed = TRUE))
  expect_true(grepl(".__assign_pattern", deparsed, fixed = TRUE))
  # Should not contain a call to sum-pairs in the compiled output
  expect_false(grepl("sum.pairs(", deparsed, fixed = TRUE))
})

# ==============================================================================
# Keyword args in self-calls
# ==============================================================================

test_that("TCO: keyword args in self-tail-call", {
  engine$eval_text("
    (define kw-sum (lambda (x y acc)
      (if (<= x 0)
        acc
        (kw-sum :y (- y 1) :x (- x 1) :acc (+ acc x y)))))
  ")
  result <- engine$eval_text("(kw-sum 3 3 0)")
  # Iteration 1: x=3, y=3, acc=0+3+3=6
  # Iteration 2: x=2, y=2, acc=6+2+2=10
  # Iteration 3: x=1, y=1, acc=10+1+1=12
  # Iteration 4: x=0, return 12
  expect_equal(result, 12)
})

test_that("TCO: mixed positional + keyword self-tail-call", {
  engine$eval_text("
    (define mixed-fn (lambda (x y)
      (if (<= x 0)
        y
        (mixed-fn (- x 1) :y (+ y x)))))
  ")
  result <- engine$eval_text("(mixed-fn 5 0)")
  # sum of 5+4+3+2+1 = 15
  expect_equal(result, 15)
})

test_that("TCO: unknown keyword in self-call bails to normal call", {
  # Self-call uses :z which doesn't match any param — compile_self_tail_call
  # bails, so the self-call remains as a normal recursive call in the output
  out <- engine$inspect_compilation("
    (define bad-kw-fn (lambda (x y)
      (if (<= x 0)
        y
        (bad-kw-fn (- x 1) :z (+ y x)))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  # while loop is present (detector found the self-call)
  expect_true(grepl("while", deparsed, fixed = TRUE))
  # but the self-call is still in the output (bail means not actually optimized)
  expect_true(grepl("bad-kw-fn", deparsed, fixed = TRUE))
})

test_that("TCO: deep recursion with keyword args does not stack overflow", {
  engine$eval_text("
    (define kw-count (lambda (n acc)
      (if (<= n 0)
        acc
        (kw-count :acc (+ acc 1) :n (- n 1)))))
  ")
  result <- engine$eval_text("(kw-count 100000 0)")
  expect_equal(result, 100000)
})

# ==============================================================================
# Rest params
# ==============================================================================

test_that("TCO: rest param with direct self-tail-call", {
  engine$eval_text("
    (define rest-count (lambda (n . rest)
      (if (<= n 0)
        (length rest)
        (rest-count (- n 1) 1 2 3))))
  ")
  result <- engine$eval_text("(rest-count 5 1 2 3)")
  expect_equal(result, 3)
})

test_that("TCO: rest param with varying arg counts in self-calls", {
  engine$eval_text("
    (define collect-loop (lambda (n . args)
      (if (<= n 0)
        args
        (collect-loop (- n 1) n))))
  ")
  result <- engine$eval_text("(collect-loop 3)")
  # Iteration 1: n=3, args=() -> (collect-loop 2 3)
  # Iteration 2: n=2, args=(3) -> (collect-loop 1 2)
  # Iteration 3: n=1, args=(2) -> (collect-loop 0 1)
  # Return (1)
  expect_equal(result, list(1))
})

test_that("TCO: deep recursion with rest params does not stack overflow", {
  engine$eval_text("
    (define rest-loop (lambda (n . rest)
      (if (<= n 0)
        (length rest)
        (rest-loop (- n 1)))))
  ")
  result <- engine$eval_text("(rest-loop 100000)")
  expect_equal(result, 0L)
})

test_that("VERIFY: TCO'd rest-param function has while, no self-call", {
  out <- engine$inspect_compilation("
    (define rest-loop (lambda (n . rest)
      (if (<= n 0)
        (length rest)
        (rest-loop (- n 1)))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_true(grepl("while", deparsed, fixed = TRUE))
  expect_false(grepl("rest.loop(", deparsed, fixed = TRUE))
})

# ==============================================================================
# Pattern rest params (now TCO'd)
# ==============================================================================

test_that("TCO: pattern rest params are TCO'd", {
  out <- engine$inspect_compilation("
    (define pat-rest-fn (lambda (n . (pattern (a b)))
      (if (<= n 0)
        (+ a b)
        (pat-rest-fn (- n 1) 10 20))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_true(grepl("while", deparsed, fixed = TRUE))
  expect_true(grepl(".__assign_pattern", deparsed, fixed = TRUE))
  expect_false(grepl("pat.rest.fn(", deparsed, fixed = TRUE))
})

test_that("TCO: deep recursion with pattern rest params does not stack overflow", {
  engine$eval_text("
    (define pat-rest-loop (lambda (n . (pattern (a b)))
      (if (<= n 0)
        (+ a b)
        (pat-rest-loop (- n 1) (+ a 1) b))))
  ")
  result <- engine$eval_text("(pat-rest-loop 100000 0 0)")
  expect_equal(result, 100000)
})

test_that("TCO: pattern rest destructuring produces correct values through iterations", {
  engine$eval_text("
    (define pat-rest-acc (lambda (n acc . (pattern (a b)))
      (if (<= n 0)
        acc
        (pat-rest-acc (- n 1) (+ acc a b) (+ a 1) (+ b 1)))))
  ")
  result <- engine$eval_text("(pat-rest-acc 3 0 1 2)")
  # Iteration 1: n=3, a=1, b=2 -> acc=0+1+2=3, next a=2,b=3
  # Iteration 2: n=2, a=2, b=3 -> acc=3+2+3=8, next a=3,b=4
  # Iteration 3: n=1, a=3, b=4 -> acc=8+3+4=15, next a=4,b=5
  # Return 15
  expect_equal(result, 15)
})

# ==============================================================================
# let/let*/letrec in tail position (IIFE inlining)
# ==============================================================================

test_that("TCO: basic let in tail position", {
  engine$eval_text("
    (define let-count (lambda (n)
      (let ((m (- n 1)))
        (if (<= m 0) 0 (let-count m)))))
  ")
  result <- engine$eval_text("(let-count 10)")
  expect_equal(result, 0)
})

test_that("TCO: deep recursion with let does not stack overflow", {
  engine$eval_text("
    (define let-loop (lambda (n)
      (let ((m (- n 1)))
        (if (<= m 0) 0 (let-loop m)))))
  ")
  result <- engine$eval_text("(let-loop 100000)")
  expect_equal(result, 0)
})

test_that("TCO: let* with sequential bindings + self-tail-call", {
  engine$eval_text("
    (define letstar-fn (lambda (n acc)
      (let* ((m (- n 1))
             (new-acc (+ acc n)))
        (if (<= m 0) new-acc (letstar-fn m new-acc)))))
  ")
  result <- engine$eval_text("(letstar-fn 10 0)")
  # Sum of 10+9+8+...+1 = 55
  expect_equal(result, 55)
})

test_that("TCO: nested let* (multiple bindings) + deep recursion", {
  engine$eval_text("
    (define letstar-loop (lambda (n acc)
      (let* ((m (- n 1))
             (new-acc (+ acc 1)))
        (if (<= m 0) new-acc (letstar-loop m new-acc)))))
  ")
  result <- engine$eval_text("(letstar-loop 100000 0)")
  expect_equal(result, 100000)
})

test_that("VERIFY: let in tail position compiles to while, no self-call", {
  out <- engine$inspect_compilation("
    (define let-count (lambda (n)
      (let ((m (- n 1)))
        (if (<= m 0) 0 (let-count m)))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_true(grepl("while", deparsed, fixed = TRUE))
  expect_false(grepl("let.count(", deparsed, fixed = TRUE))
})

test_that("TCO: letrec in tail position", {
  engine$eval_text("
    (define letrec-fn (lambda (n acc)
      (letrec ((x n))
        (if (<= x 0) acc (letrec-fn (- x 1) (+ acc x))))))
  ")
  result <- engine$eval_text("(letrec-fn 10 0)")
  # Sum of 10+9+...+1 = 55
  expect_equal(result, 55)
})

test_that("TCO: deep recursion with letrec does not stack overflow", {
  engine$eval_text("
    (define letrec-loop (lambda (n acc)
      (letrec ((x n))
        (if (<= x 0) acc (letrec-loop (- x 1) (+ acc 1))))))
  ")
  result <- engine$eval_text("(letrec-loop 100000 0)")
  expect_equal(result, 100000)
})

# ==============================================================================
# Non-interference: IIFE edge cases
# ==============================================================================

test_that("TCO: let where self-call is NOT in tail position is not TCO'd", {
  out <- engine$inspect_compilation("
    (define not-tail (lambda (n)
      (let ((m (- n 1)))
        (+ 1 (not-tail m)))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  expect_false(grepl("while", deparsed, fixed = TRUE))
})

test_that("TCO: IIFE with complex params bails on inlining", {
  # Hand-written IIFE with rest param in tail position — compile_tail_iife
  # should bail, leaving the self-call in compiled output
  out <- engine$inspect_compilation("
    (define iife-rest-fn (lambda (n)
      ((lambda (m . rest) (if (<= m 0) 0 (iife-rest-fn m))) (- n 1))))
  ")
  deparsed <- paste(out$compiled_deparsed, collapse = "\n")
  # while loop is present (detector found the self-call inside IIFE)
  expect_true(grepl("while", deparsed, fixed = TRUE))
  # but self-call is still present (IIFE inlining bailed due to rest param)
  expect_true(grepl("iife-rest-fn", deparsed, fixed = TRUE))
})

test_that("TCO: IIFE with complex params still works at runtime", {
  engine$eval_text("
    (define iife-rest-fn2 (lambda (n)
      ((lambda (m . rest) (if (<= m 0) 0 (iife-rest-fn2 m))) (- n 1))))
  ")
  result <- engine$eval_text("(iife-rest-fn2 10)")
  expect_equal(result, 0)
})

# ==============================================================================
# Error reporting in TCO-optimized functions
# ==============================================================================

test_that("TCO: error in base case includes source location", {
  err <- tryCatch(
    engine$eval_text("
      (define tco-err-base (lambda (n)
        (if (<= n 0)
          (+ 1 nope)
          (tco-err-base (- n 1)))))
      (tco-err-base 3)
    ", source_name = "tco-base.rye"),
    error = function(e) e
  )
  expect_s3_class(err, "rye_error")
  formatted <- engine$source_tracker$format_error(err)
  expect_match(formatted, "tco-base\\.rye")
})

test_that("TCO: error in tail-call argument includes source location", {
  err <- tryCatch(
    engine$eval_text("
      (define tco-err-arg (lambda (n acc)
        (if (<= n 0)
          acc
          (tco-err-arg (- n 1) (+ acc nope)))))
      (tco-err-arg 3 0)
    ", source_name = "tco-arg.rye"),
    error = function(e) e
  )
  expect_s3_class(err, "rye_error")
  formatted <- engine$source_tracker$format_error(err)
  expect_match(formatted, "tco-arg\\.rye")
})

test_that("TCO: error inside let body includes source location", {
  err <- tryCatch(
    engine$eval_text("
      (define tco-err-let (lambda (n)
        (let ((m (- n 1)))
          (if (<= m 0)
            (+ 1 nope)
            (tco-err-let m)))))
      (tco-err-let 3)
    ", source_name = "tco-let.rye"),
    error = function(e) e
  )
  expect_s3_class(err, "rye_error")
  formatted <- engine$source_tracker$format_error(err)
  expect_match(formatted, "tco-let\\.rye")
})
