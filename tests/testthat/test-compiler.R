engine <- make_engine()

test_that("compiled eval handles simple arithmetic", {
  expect_equal(engine$eval(engine$read("(+ 1 2)")[[1]]), 3)
  expect_equal(engine$eval(engine$read("(- 5 3)")[[1]]), 2)
  expect_equal(engine$eval(engine$read("(* 4 5)")[[1]]), 20)
  expect_equal(engine$eval(engine$read("(/ 10 2)")[[1]]), 5)
})

test_that("compiled eval handles R functions", {
  result <- engine$eval(engine$read("(mean (c 1 2 3 4 5))")[[1]])
  expect_equal(result, 3)
})

test_that("compiled eval handles nested calls", {
  result <- engine$eval(engine$read("(+ (* 2 3) (* 4 5))")[[1]])
  expect_equal(result, 26)
})

test_that("compiled eval evaluates arguments left-to-right", {
  env <- new.env(parent = baseenv())
  engine$eval(engine$read("(define x 0)")[[1]], env = env)
  engine$eval(engine$read("(define collect (lambda (a b) (list a b)))")[[1]], env = env)

  result <- engine$eval(
    engine$read("(collect (begin (set! x (+ x 1)) x) (begin (set! x (+ x 1)) x))")[[1]],
    env = env
  )

  expect_equal(result, list(1, 2))
  expect_equal(env$x, 2)
})

test_that("compiled eval handles :: sugar", {
  result <- engine$eval(engine$read("(base::mean (c 1 2 3))")[[1]])
  expect_equal(result, 2)
})

test_that("calculator with nested expressions", {
  result <- engine$eval(engine$read("(+ 1 (* 2 3))")[[1]])
  expect_equal(result, 7)
})

test_that("compiled eval validates special form arity and types", {
  expect_error(engine$eval(engine$read("(quote 1 2)")[[1]]), "quote requires exactly 1 argument")
  expect_error(engine$eval(engine$read("(quasiquote)")[[1]]), "quasiquote requires exactly 1 argument")
  expect_error(engine$eval(engine$read("(if 1)")[[1]]), "if requires 2 or 3 arguments")
  expect_error(engine$eval(engine$read("(define 1 2)")[[1]]), "define requires a symbol")
  expect_error(engine$eval(engine$read("(set! 1 2)")[[1]]), "set! requires a symbol")
})

test_that("compiled eval handles set! scoping and missing bindings", {
  env <- new.env(parent = emptyenv())
  env$x <- 1
  child <- new.env(parent = env)

  engine$eval(engine$read("(set! x 2)")[[1]], env = child)
  expect_equal(env$x, 2)
  expect_error(engine$eval(engine$read("(set! y 1)")[[1]], env = child), "variable 'y' is not defined")
})

test_that("define/set! reject reserved .__* names", {
  expect_error(engine$eval_text('(define .__foo 1)'), "reserved name.*internal")
  expect_error(engine$eval_text('(define .__env 1)'), "reserved name.*internal")
  expect_error(engine$eval_text('(set! .__bar 1)'), "reserved name.*internal")
})

test_that("compiled eval validates load arguments and missing files", {
  expect_error(engine$eval(engine$read("(load 1)")[[1]]), "load requires a single file path string")
  expect_error(engine$eval(engine$read('(load "missing-file.arl")')[[1]]), "File not found")
})

test_that("compiled eval builds formulas without evaluating arguments", {
  env <- new.env(parent = baseenv())
  env$x <- 10
  result <- engine$eval(engine$read("(~ x y)")[[1]], env = env)
  expect_s3_class(result, "formula")
  expect_equal(as.character(result)[2], "x")
  expect_equal(as.character(result)[3], "y")
})

test_that("compiled eval validates package accessor arguments", {
  expect_error(engine$eval(engine$read("(:: base mean extra)")[[1]]), "requires exactly 2 arguments")
  expect_error(engine$eval(engine$read("(:: 1 mean)")[[1]]), "Package name must be a symbol or string")
  expect_error(engine$eval(engine$read("(:: base 1)")[[1]]), "Function/object name must be a symbol or string")
})

test_that("compiled eval validates keyword usage", {
  expect_error(engine$eval(engine$read("(mean :trim)")[[1]]), "requires a value")
})

test_that("compiled eval validates lambda argument lists", {
  expect_error(engine$eval(engine$read("(lambda 1 2)")[[1]]), "lambda arguments must be a list")
  expect_error(
    engine$eval(engine$read("(lambda (1) 2)")[[1]]),
    "lambda arguments must be symbols, \\(name default\\) pairs, or \\(pattern <pat> \\[default\\]\\)"
  )
  expect_error(
    engine$eval(engine$read("(lambda (a .) a)")[[1]]),
    "Dotted parameter list must have exactly one parameter after '\\.'"
  )
})

test_that("eval text errors include source and stack context", {
  env <- new.env(parent = baseenv())
  err <- tryCatch(
    engine$eval_text("(+ 1 nope)", env = env, source_name = "test.arl"),
    error = function(e) e
  )
  expect_s3_class(err, "arl_error")

  formatted <- engine_field(engine, "source_tracker")$format_error(err)
  expect_match(formatted, "test\\.arl:1:1-1:10")
  expect_match(formatted, "R stack:")
  expect_match(formatted, "eval_text")
})

# =============================================================================
# current-env and r/eval (per-engine env stack, no global state)
# =============================================================================

test_that("current-env returns the active evaluation environment", {
  engine$eval(engine$read("(define _ce_test 123)")[[1]])
  curr <- engine$eval(engine$read("(current-env)")[[1]])
  expect_true(is.environment(curr))
  expect_equal(get("_ce_test", envir = curr, inherits = FALSE), 123)
})

test_that("r/eval with no env uses current environment", {
  # + is in the env (from stdlib); r/eval (quote +) should return it
  result <- engine$eval(engine$read("(r/eval (quote +))")[[1]])
  expect_true(is.function(result))
})

test_that("r/eval with no env sees bindings from same evaluation context", {
  # current-env returns the active env (with bindings from previous evals in same engine)
  eng <- make_engine()
  eng$eval(eng$read("(define _reval_secret 99)")[[1]])
  curr <- eng$eval(eng$read("(current-env)")[[1]])
  expect_equal(get("_reval_secret", envir = curr, inherits = FALSE), 99)
  # r/eval (quote x) looks up x in current env when called in same eval
  result <- eng$eval(eng$read("(r/eval (quote +))")[[1]])
  expect_true(is.function(result))
})

test_that("multiple engines have independent current-env", {
  engine_a <- make_engine()
  engine_b <- make_engine()
  engine_a$eval(engine_a$read("(define _eng_x 1)")[[1]])
  engine_b$eval(engine_b$read("(define _eng_x 2)")[[1]])
  curr_a <- engine_a$eval(engine_a$read("(current-env)")[[1]])
  curr_b <- engine_b$eval(engine_b$read("(current-env)")[[1]])
  expect_equal(get("_eng_x", envir = curr_a, inherits = FALSE), 1)
  expect_equal(get("_eng_x", envir = curr_b, inherits = FALSE), 2)
  # r/eval (quote +) works in each engine (each has its own current-env closure)
  result_a <- engine_a$eval(engine_a$read("(r/eval (quote +))")[[1]])
  result_b <- engine_b$eval(engine_b$read("(r/eval (quote +))")[[1]])
  expect_true(is.function(result_a))
  expect_true(is.function(result_b))
})

make_env <- function(engine, init = NULL) {
  env <- new.env()
  toplevel_env(engine, env) # nolint: object_usage_linter.
  if (is.function(init)) {
    init(env)
  }
  env = env
}

eval_compiled_in_env <- function(engine, expr, env) {
  expanded <- engine$macroexpand(expr, env = env, preserve_src = TRUE)
  compiled <- engine_field(engine, "compiler")$compile(expanded, env, strict = TRUE)
  expect_false(is.null(compiled)) # nolint: object_usage_linter.
  result <- withVisible(engine_field(engine, "compiled_runtime")$eval_compiled(compiled, env))
  result$value <- engine_field(engine, "source_tracker")$strip_src(result$value)
  list(
    compiled = compiled,
    result = result
  )
}

test_that("compiler conformance for core constructs", {
  cases <- list(
    list(
      name = "quote",
      expr = "'(a b)"
    ),
    list(
      name = "quasiquote",
      expr = "`(1 ,(+ 1 1) 3)"
    ),
    list(
      name = "if",
      expr = "(if #t 1 2)"
    ),
    list(
      name = "begin",
      expr = "(begin (define x 1) (+ x 1))"
    ),
    list(
      name = "lambda",
      expr = "((lambda (x) (+ x 1)) 2)"
    ),
    list(
      name = "set!",
      expr = "(begin (define x 1) (set! x 2) x)"
    ),
    list(
      name = "define",
      expr = "(begin (define x 10) x)"
    ),
    list(
      name = "and",
      expr = "(and #t 1 2)"
    ),
    list(
      name = "or",
      expr = "(or #f 1 2)"
    ),
    list(
      name = "import",
      expr = "(begin (import list) (caddr (list 1 2 3 4)))"
    ),
    list(
      name = "load",
      expr = "(begin (load load_path) loaded_value)",
      init = function(env) {
        module_path <- tempfile("arl-load-", fileext = ".arl")
        writeLines(
          c("(define loaded_value 42)"),
          module_path
        )
        assign("load_path", module_path, envir = env)
      }
    ),
    list(
      name = "package access",
      expr = "(base::mean (c 1 2 3))"
    ),
    list(
      name = "keyword args",
      expr = "(seq :from 1 :to 5 :by 2)"
    )
  )

  for (case in cases) {
    env_eval <- make_env(engine, case$init)
    env_compiled <- make_env(engine, case$init)
    expr <- engine$read(case$expr)[[1]]

    expected <- withVisible(engine$eval(expr, env = env_eval))
    compiled_out <- eval_compiled_in_env(engine, expr, env = env_compiled)

    expect_equal(compiled_out$result$value, expected$value, info = case$name)
    expect_identical(compiled_out$result$visible, expected$visible, info = case$name)
  }
})

test_that("compiler output is pure R code (no evaluator references)", {
  env <- make_env(engine)
  exprs <- list(
    engine$read("(begin (define x 1) (+ x 2))")[[1]],
    engine$read("((lambda (x) (* x 2)) 3)")[[1]],
    engine$read("(and #t 1 2)")[[1]]
  )
  for (expr in exprs) {
    expanded <- engine$macroexpand(expr, env = env, preserve_src = TRUE)
    compiled <- engine_field(engine, "compiler")$compile(expanded, env, strict = TRUE)
    expect_false(is.null(compiled))
    text <- paste(deparse(compiled), collapse = " ")
    expect_false(grepl("Evaluator", text, fixed = TRUE))
    expect_false(grepl("evaluator", text, fixed = TRUE))
    expect_false(grepl("\\.arl_eval", text))
  }
})

test_that("compiled visibility contract matches engine eval", {
  env_eval <- make_env(engine)
  env_compiled <- make_env(engine)

  expr_define <- engine$read("(define x 1)")[[1]]
  expected_define <- withVisible(engine$eval(expr_define, env = env_eval))
  compiled_define <- eval_compiled_in_env(engine, expr_define, env = env_compiled)
  expect_false(expected_define$visible)
  expect_false(compiled_define$result$visible)

  expr_begin <- engine$read("(begin (define x 1) x)")[[1]]
  expected_begin <- withVisible(engine$eval(expr_begin, env = env_eval))
  compiled_begin <- eval_compiled_in_env(engine, expr_begin, env = env_compiled)
  expect_true(expected_begin$visible)
  expect_true(compiled_begin$result$visible)

  expr_empty <- engine$read("(begin)")[[1]]
  expected_empty <- withVisible(engine$eval(expr_empty, env = env_eval))
  compiled_empty <- eval_compiled_in_env(engine, expr_empty, env = env_compiled)
  expect_false(expected_empty$visible)
  expect_false(compiled_empty$result$visible)
  expect_null(expected_empty$value)
  expect_null(compiled_empty$result$value)
})

test_that("macro pipeline matches engine eval", {
  env_eval <- make_env(engine)
  env_compiled <- make_env(engine)

  engine$eval(engine$read("(defmacro my-when (test body) `(if ,test ,body #nil))")[[1]], env = env_eval)
  engine$eval(engine$read("(defmacro my-inc (x) `(+ ,x 1))")[[1]], env = env_eval)
  engine$eval(engine$read("(defmacro my-when (test body) `(if ,test ,body #nil))")[[1]], env = env_compiled)
  engine$eval(engine$read("(defmacro my-inc (x) `(+ ,x 1))")[[1]], env = env_compiled)

  exprs <- list(
    engine$read("(my-inc 2)")[[1]],
    engine$read("(my-when #t (my-inc 1))")[[1]]
  )

  for (expr in exprs) {
    expected <- withVisible(engine$eval(expr, env = env_eval))

    expanded <- engine$macroexpand(expr, env = env_compiled, preserve_src = TRUE)
    compiled <- engine_field(engine, "compiler")$compile(expanded, env_compiled, strict = TRUE)
    expect_false(is.null(compiled))
    actual <- withVisible(engine_field(engine, "compiled_runtime")$eval_compiled(compiled, env_compiled))
    actual$value <- engine_field(engine, "source_tracker")$strip_src(actual$value)

    expect_equal(actual$value, expected$value)
    expect_identical(actual$visible, expected$visible)
  }
})

# Optimization Tests: Constant Folding
test_that("compiler performs constant folding for arithmetic operations", {
  engine <- make_engine()

  # Test that pure arithmetic with literals gets folded
  # We verify by checking the result is correct (semantic test)
  expect_equal(engine$eval(engine$read("(+ 1 2)")[[1]]), 3)
  expect_equal(engine$eval(engine$read("(- 10 3)")[[1]]), 7)
  expect_equal(engine$eval(engine$read("(* 4 5)")[[1]]), 20)
  expect_equal(engine$eval(engine$read("(/ 20 4)")[[1]]), 5)

  # Test nested constant expressions
  expect_equal(engine$eval(engine$read("(+ (* 2 3) (* 4 5))")[[1]]), 26)
  expect_equal(engine$eval(engine$read("(- (+ 10 5) (* 2 3))")[[1]]), 9)
})

test_that("compiler performs constant folding for comparison operations", {
  engine <- make_engine()

  # Comparison operators should fold
  expect_true(engine$eval(engine$read("(< 1 2)")[[1]]))
  expect_false(engine$eval(engine$read("(> 1 2)")[[1]]))
  expect_true(engine$eval(engine$read("(== 5 5)")[[1]]))
  expect_false(engine$eval(engine$read("(!= 5 5)")[[1]]))
  expect_true(engine$eval(engine$read("(<= 2 2)")[[1]]))
  expect_true(engine$eval(engine$read("(>= 3 3)")[[1]]))
})

test_that("compiler performs constant folding for logical operations", {
  engine <- make_engine()

  # Logical operators should fold
  expect_true(engine$eval(engine$read("(& #t #t)")[[1]]))
  expect_false(engine$eval(engine$read("(& #t #f)")[[1]]))
  expect_true(engine$eval(engine$read("(| #t #f)")[[1]]))
  expect_false(engine$eval(engine$read("(| #f #f)")[[1]]))
  expect_true(engine$eval(engine$read("(! #f)")[[1]]))
  expect_false(engine$eval(engine$read("(! #t)")[[1]]))
})

test_that("compiler does NOT fold when arguments have side effects", {
  engine <- make_engine()
  env <- new.env(parent = baseenv())

  # Define a function with side effects
  engine$eval(engine$read("(define counter 0)")[[1]], env = env)
  engine$eval(engine$read("(define inc! (lambda () (set! counter (+ counter 1)) counter))")[[1]], env = env)

  # This should NOT be folded - inc! has side effects
  result <- engine$eval(engine$read("(+ (inc!) (inc!))")[[1]], env = env)
  expect_equal(result, 3)  # 1 + 2 = 3
  expect_equal(env$counter, 2)  # Counter incremented twice
})

test_that("compiler does NOT fold when operators are not pure", {
  engine <- make_engine()

  # Non-literal arguments should not fold
  env <- new.env(parent = baseenv())
  env$x <- 10
  result <- engine$eval(engine$read("(+ x 5)")[[1]], env = env)
  expect_equal(result, 15)

  # Mixed literal and variable should not fold
  env$y <- 3
  result <- engine$eval(engine$read("(* 2 y)")[[1]], env = env)
  expect_equal(result, 6)
})

test_that("compiler performs constant folding for math functions", {
  engine <- make_engine()

  # Math functions with literal arguments should fold
  expect_equal(engine$eval(engine$read("(abs -5)")[[1]]), 5)
  expect_equal(engine$eval(engine$read("(sqrt 16)")[[1]]), 4)
  expect_equal(engine$eval(engine$read("(floor 3.7)")[[1]]), 3)
  expect_equal(engine$eval(engine$read("(ceiling 3.2)")[[1]]), 4)
  expect_equal(engine$eval(engine$read("(round 3.5)")[[1]]), 4)
})

test_that("compiler handles constant folding edge cases", {
  engine <- make_engine()

  # Division by zero produces Inf (R behavior)
  expect_equal(engine$eval(engine$read("(/ 1 0)")[[1]]), Inf)

  # NA/NaN propagation (NULL in Arl is NULL in R, not NA)
  result <- engine$eval(engine$read("(+ 1 2)")[[1]])
  expect_false(is.na(result))

  # Empty list operations that are pure
  expect_equal(engine$eval(engine$read("(length (list))")[[1]]), 0)
})

# Optimization Tests: Truthiness Optimization
test_that("compiler optimizes truthiness checks for literal booleans", {
  engine <- make_engine()

  # Literal TRUE/FALSE should work without .__true_p wrapper
  expect_equal(engine$eval(engine$read("(if #t 1 2)")[[1]]), 1)
  expect_equal(engine$eval(engine$read("(if #f 1 2)")[[1]]), 2)
  expect_equal(engine$eval(engine$read("(if #nil 1 2)")[[1]]), 2)
})

test_that("compiler optimizes truthiness checks for comparison operators", {
  engine <- make_engine()

  # Comparison operators return proper R logicals - no wrapper needed
  expect_equal(engine$eval(engine$read("(if (< 1 2) 1 2)")[[1]]), 1)
  expect_equal(engine$eval(engine$read("(if (> 1 2) 1 2)")[[1]]), 2)
  expect_equal(engine$eval(engine$read("(if (== 5 5) 1 2)")[[1]]), 1)
  expect_equal(engine$eval(engine$read("(if (!= 5 5) 1 2)")[[1]]), 2)
  expect_equal(engine$eval(engine$read("(if (<= 2 2) 1 2)")[[1]]), 1)
  expect_equal(engine$eval(engine$read("(if (>= 3 3) 1 2)")[[1]]), 1)
})

test_that("compiler optimizes truthiness checks for logical operators", {
  engine <- make_engine()

  # Logical operators return proper R logicals - no wrapper needed
  expect_equal(engine$eval(engine$read("(if (& #t #t) 1 2)")[[1]]), 1)
  expect_equal(engine$eval(engine$read("(if (| #f #t) 1 2)")[[1]]), 1)
  expect_equal(engine$eval(engine$read("(if (! #f) 1 2)")[[1]]), 1)
})

test_that("compiler preserves Arl truthiness semantics", {
  engine <- make_engine()

  # #f, #nil, and 0 are false in Arl (0 follows R semantics)
  # Strings, empty lists, etc. are truthy
  expect_equal(engine$eval(engine$read("(if 0 1 2)")[[1]]), 2)  # 0 is falsy
  expect_equal(engine$eval(engine$read('(if "" 1 2)')[[1]]), 1)  # empty string is truthy
  expect_equal(engine$eval(engine$read("(if (list) 1 2)")[[1]]), 1)  # empty list is truthy

  # But #f and #nil are falsy
  expect_equal(engine$eval(engine$read("(if #f 1 2)")[[1]]), 2)
  expect_equal(engine$eval(engine$read("(if #nil 1 2)")[[1]]), 2)
})

test_that("compiler handles constant-folded boolean tests", {
  engine <- make_engine()

  # When constant folding produces a boolean literal, skip wrapper
  expect_equal(engine$eval(engine$read("(if (< 1 2) 1 2)")[[1]]), 1)
  expect_equal(engine$eval(engine$read("(if (> 1 2) 1 2)")[[1]]), 2)
})

# Optimization Tests: Dead Code Elimination
test_that("compiler eliminates dead branches for constant true test", {
  engine <- make_engine()

  # When test is literally TRUE, only then-branch should remain
  expect_equal(engine$eval(engine$read("(if #t 42 99)")[[1]]), 42)
  expect_equal(engine$eval(engine$read("(if #t 100 200)")[[1]]), 100)

  # With constant-folded true condition
  expect_equal(engine$eval(engine$read("(if (< 1 2) 100 200)")[[1]]), 100)
})

test_that("compiler eliminates dead branches for constant false test", {
  engine <- make_engine()

  # When test is literally FALSE, only else-branch should remain
  expect_equal(engine$eval(engine$read("(if #f 42 99)")[[1]]), 99)

  # With constant-folded false condition
  expect_equal(engine$eval(engine$read("(if (> 1 2) 100 200)")[[1]]), 200)
})

test_that("compiler eliminates dead branches for null test", {
  engine <- make_engine()

  # NULL is falsy in Arl, so else-branch is taken
  expect_equal(engine$eval(engine$read("(if #nil 42 99)")[[1]]), 99)
})

test_that("compiler handles missing else branch with constant test", {
  engine <- make_engine()

  # (if #t a) should become just a
  expect_equal(engine$eval(engine$read("(if #t 42)")[[1]]), 42)

  # (if #f a) should become NULL (no else branch)
  expect_null(engine$eval(engine$read("(if #f 42)")[[1]]))
})

test_that("dead code elimination preserves side effects in taken branch", {
  engine <- make_engine()
  env <- new.env(parent = baseenv())
  env$x <- 0

  # Side effects in then-branch should execute
  engine$eval(engine$read("(if #t (set! x 10) (set! x 20))")[[1]], env = env)
  expect_equal(env$x, 10)

  # Reset
  env$x <- 0

  # Side effects in else-branch should execute
  engine$eval(engine$read("(if #f (set! x 10) (set! x 20))")[[1]], env = env)
  expect_equal(env$x, 20)
})

test_that("dead code elimination does NOT eliminate for variable tests", {
  engine <- make_engine()
  env <- new.env(parent = baseenv())
  env$x <- TRUE

  # Variable test - both branches should be compiled (not eliminated)
  result <- engine$eval(engine$read("(if x 1 2)")[[1]], env = env)
  expect_equal(result, 1)

  env$x <- FALSE
  result <- engine$eval(engine$read("(if x 1 2)")[[1]], env = env)
  expect_equal(result, 2)
})

# Optimization Tests: Begin Simplification
test_that("compiler simplifies single-expression begin blocks", {
  engine <- make_engine()

  # Single expression should not have block wrapper
  expect_equal(engine$eval(engine$read("(begin 42)")[[1]]), 42)
  expect_equal(engine$eval(engine$read("(begin (+ 1 2))")[[1]]), 3)
})

test_that("compiler preserves multi-expression begin blocks", {
  engine <- make_engine()
  env <- new.env(parent = baseenv())
  env$x <- 0

  # Multiple expressions need block wrapper
  engine$eval(engine$read("(begin (set! x 10) (set! x 20) x)")[[1]], env = env)
  expect_equal(env$x, 20)
})

test_that("compiler handles empty begin", {
  engine <- make_engine()

  # Empty begin should return NULL (invisible)
  result <- engine$eval(engine$read("(begin)")[[1]])
  expect_null(result)
})

# Optimization Tests: Identity Elimination
test_that("compiler eliminates simple identity lambda", {
  engine <- make_engine()

  # ((lambda (x) x) value) should become just value
  expect_equal(engine$eval(engine$read("((lambda (x) x) 42)")[[1]]), 42)
  expect_equal(engine$eval(engine$read("((lambda (x) x) (+ 1 2))")[[1]]), 3)
})

test_that("compiler eliminates identity lambda selecting first arg", {
  engine <- make_engine()

  # ((lambda (a b) a) v1 v2) should become just v1
  expect_equal(engine$eval(engine$read("((lambda (a b) a) 10 20)")[[1]]), 10)
  expect_equal(engine$eval(engine$read("((lambda (x y z) x) 1 2 3)")[[1]]), 1)
})

test_that("compiler does NOT eliminate non-identity lambdas", {
  engine <- make_engine()

  # These are not identity functions - should not be optimized away
  expect_equal(engine$eval(engine$read("((lambda (x) (+ x 1)) 5)")[[1]]), 6)
  expect_equal(engine$eval(engine$read("((lambda (a b) (+ a b)) 3 4)")[[1]]), 7)
})

test_that("identity elimination preserves evaluation order", {
  engine <- make_engine()
  env <- new.env(parent = baseenv())
  env$counter <- 0
  engine$eval(engine$read("(define inc! (lambda () (set! counter (+ counter 1)) counter))")[[1]], env = env)

  # Arguments should still be evaluated even if lambda is eliminated
  result <- engine$eval(engine$read("((lambda (x) x) (inc!))")[[1]], env = env)
  expect_equal(result, 1)
  expect_equal(env$counter, 1)
})

# Note: Optimization verification tests moved to test-compiler-optimizations.R
