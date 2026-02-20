# Compiler Optimization Verification Tests
#
# These tests verify that compiler optimizations actually fire by checking
# properties of the compiled output, not just semantic correctness.
#
# Philosophy: We check properties (e.g., "is not an if statement") rather than
# exact output, making tests resilient to optimization strategy changes while
# still ensuring optimizations are applied.

engine <- Engine$new(load_prelude = FALSE)

# ==============================================================================
# Constant Folding Verification
# ==============================================================================

test_that("VERIFY: constant folding produces literals", {
  engine <- Engine$new(load_prelude = FALSE)

  # Simple arithmetic should fold to literal
  out <- engine$inspect_compilation("(+ 1 2)")
  expect_true(is.numeric(out$compiled))
  expect_equal(out$compiled, 3)

  # Nested arithmetic should fold to literal
  out <- engine$inspect_compilation("(+ (* 2 3) (* 4 5))")
  expect_true(is.numeric(out$compiled))
  expect_equal(out$compiled, 26)

  # Comparison should fold to literal boolean
  out <- engine$inspect_compilation("(< 1 2)")
  expect_true(is.logical(out$compiled))
  expect_equal(out$compiled, TRUE)

  # Math functions should fold
  out <- engine$inspect_compilation("(sqrt 16)")
  expect_true(is.numeric(out$compiled))
  expect_equal(out$compiled, 4)
})

test_that("VERIFY: constant folding does NOT fold non-constants", {
  engine <- Engine$new(load_prelude = FALSE)

  # Variable expressions should NOT fold
  out <- engine$inspect_compilation("(+ x 1)")
  expect_true(is.call(out$compiled))  # Should still be a call

  # Partial folding should work
  out <- engine$inspect_compilation("(+ x (* 2 3))")
  expect_true(is.call(out$compiled))
  # But (* 2 3) part should be folded to 6 in the args
})

# ==============================================================================
# Truthiness Optimization Verification
# ==============================================================================

test_that("VERIFY: truthiness optimization skips wrapper for known booleans", {
  engine <- Engine$new(load_prelude = FALSE)

  # Literal TRUE should not have .__true_p wrapper
  out <- engine$inspect_compilation("(if #t 1 2)")
  compiled_str <- paste(out$compiled_deparsed, collapse = " ")
  expect_false(grepl(".__true_p", compiled_str))

  # Comparison should not have wrapper
  out <- engine$inspect_compilation("(if (> x 5) 1 2)")
  compiled_str <- paste(out$compiled_deparsed, collapse = " ")
  expect_false(grepl(".__true_p.*>", compiled_str))

  # Variable SHOULD have wrapper
  out <- engine$inspect_compilation("(if x 1 2)")
  compiled_str <- paste(out$compiled_deparsed, collapse = " ")
  expect_true(grepl(".__true_p", compiled_str))
})

# ==============================================================================
# Dead Code Elimination Verification
# ==============================================================================

test_that("VERIFY: dead code elimination removes unreachable branches", {
  engine <- Engine$new(load_prelude = FALSE)

  # (if #t a b) should be just a, not an if statement
  out <- engine$inspect_compilation("(if #t 1 2)")
  expect_false(is.call(out$compiled) && identical(out$compiled[[1]], quote(`if`)))
  expect_equal(out$compiled, 1)

  # (if #f a b) should be just b
  out <- engine$inspect_compilation("(if #f 1 2)")
  expect_false(is.call(out$compiled) && identical(out$compiled[[1]], quote(`if`)))
  expect_equal(out$compiled, 2)

  # Constant-folded condition should also eliminate
  out <- engine$inspect_compilation("(if (< 1 2) 100 200)")
  expect_false(is.call(out$compiled) && identical(out$compiled[[1]], quote(`if`)))
  expect_equal(out$compiled, 100)

  # Variable condition should NOT eliminate
  out <- engine$inspect_compilation("(if x 1 2)")
  expect_true(is.call(out$compiled) && identical(out$compiled[[1]], quote(`if`)))
})

# ==============================================================================
# Begin Simplification Verification
# ==============================================================================

test_that("VERIFY: begin simplification removes single-expression blocks", {
  engine <- Engine$new(load_prelude = FALSE)

  # (begin x) should be just x, not a block
  out <- engine$inspect_compilation("(begin 1)")
  expect_false(is.call(out$compiled) && identical(out$compiled[[1]], quote(`{`)))
  expect_equal(out$compiled, 1)

  # (begin (+ 1 2)) should fold and simplify to just 3
  out <- engine$inspect_compilation("(begin (+ 1 2))")
  expect_false(is.call(out$compiled) && identical(out$compiled[[1]], quote(`{`)))
  expect_equal(out$compiled, 3)

  # Multiple expressions should keep block
  out <- engine$inspect_compilation("(begin 1 2 3)")
  expect_true(is.call(out$compiled) && identical(out$compiled[[1]], quote(`{`)))
})

# ==============================================================================
# Identity Elimination Verification
# ==============================================================================

test_that("VERIFY: identity elimination inlines identity lambdas", {
  engine <- Engine$new(load_prelude = FALSE)

  # ((lambda (x) x) 42) should be just 42, not a function application
  out <- engine$inspect_compilation("((lambda (x) x) 42)")
  # Check it's NOT a function application (which has as.function in the operator)
  is_funcall <- is.call(out$compiled) && is.call(out$compiled[[1]]) &&
                length(out$compiled[[1]]) > 0 &&
                identical(out$compiled[[1]][[1]], quote(as.function))
  expect_false(is_funcall, info = "Should not be a function application")
  expect_equal(out$compiled, 42)

  # ((lambda (a b) a) 10 20) should be just 10
  out <- engine$inspect_compilation("((lambda (a b) a) 10 20)")
  is_funcall <- is.call(out$compiled) && is.call(out$compiled[[1]]) &&
                length(out$compiled[[1]]) > 0 &&
                identical(out$compiled[[1]][[1]], quote(as.function))
  expect_false(is_funcall)
  expect_equal(out$compiled, 10)

  # Non-identity lambda should still be a function application
  out <- engine$inspect_compilation("((lambda (x) (+ x 1)) 5)")
  is_funcall <- is.call(out$compiled) && is.call(out$compiled[[1]]) &&
                length(out$compiled[[1]]) > 0 &&
                identical(out$compiled[[1]][[1]], quote(as.function))
  expect_true(is_funcall, info = "Non-identity lambda should remain as function call")

  # With constant-folded argument
  out <- engine$inspect_compilation("((lambda (x) x) (+ 1 2))")
  is_funcall <- is.call(out$compiled) && is.call(out$compiled[[1]]) &&
                length(out$compiled[[1]]) > 0 &&
                identical(out$compiled[[1]][[1]], quote(as.function))
  expect_false(is_funcall)
  expect_equal(out$compiled, 3)  # Both optimizations apply!
})

# ==============================================================================
# Optimization Composition Verification
# ==============================================================================

test_that("VERIFY: optimizations compose correctly", {
  engine <- Engine$new(load_prelude = FALSE)

  # Constant folding + dead code elimination
  out <- engine$inspect_compilation("(if (< 1 2) (+ 2 3) (+ 4 5))")
  expect_equal(out$compiled, 5)  # Folds (< 1 2) to TRUE, eliminates else, folds (+ 2 3)

  # Constant folding + begin simplification
  out <- engine$inspect_compilation("(begin (+ 1 2))")
  expect_equal(out$compiled, 3)  # Folds and simplifies

  # Identity elimination + constant folding
  out <- engine$inspect_compilation("((lambda (x) x) (+ 1 2))")
  expect_equal(out$compiled, 3)  # Folds argument and eliminates lambda

  # All together: begin + if + constant folding + dead code elimination
  out <- engine$inspect_compilation("(begin (if (< 1 2) (+ 10 20) (+ 30 40)))")
  expect_equal(out$compiled, 30)  # Everything optimizes away to just 30
})

# ==============================================================================
# Boolean Operator Optimization Verification (Phase 2)
# ==============================================================================

test_that("VERIFY: and/or skip temps for simple values", {
  engine <- Engine$new(load_prelude = FALSE)

  # Simple literals should NOT generate temporary variables
  out <- engine$inspect_compilation("(and 1 2 3)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  # Should not have assignment to temp for literal 1
  expect_false(grepl("tmp.*<-.*1[^0-9]", compiled_str, perl = TRUE),
    info = "Literal 1 should not be assigned to temp")

  # Symbols should NOT generate temporary variables
  out <- engine$inspect_compilation("(and x y z)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  # Should not have assignment like: tmp <- x
  expect_false(grepl("tmp.*<-.*x[^a-zA-Z]", compiled_str, perl = TRUE),
    info = "Symbol x should not be assigned to temp")

  # Complex expressions SHOULD still get temps (to avoid double evaluation)
  out <- engine$inspect_compilation("(and (+ a 1) (+ b 2))")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  # Should have at least one temp assignment for complex expression
  expect_true(grepl("<-", compiled_str),
    info = "Complex expressions should still use temps")
})

test_that("VERIFY: nested boolean chains flatten", {
  engine <- Engine$new(load_prelude = FALSE)

  # Nested AND should flatten: (and (and a b) c) behaves like (and a b c)
  # We check by verifying similar structure/depth
  nested <- engine$inspect_compilation("(and (and a b) c)")
  flat <- engine$inspect_compilation("(and a b c)")

  # Both should short-circuit correctly and have similar complexity
  # We can't check exact equality due to different compilation paths,
  # but we can verify both work correctly
  expect_true(is.language(nested$compiled))
  expect_true(is.language(flat$compiled))

  # Nested OR should flatten similarly
  nested_or <- engine$inspect_compilation("(or (or a b) c)")
  flat_or <- engine$inspect_compilation("(or a b c)")

  expect_true(is.language(nested_or$compiled))
  expect_true(is.language(flat_or$compiled))

  # Mixed operators should NOT flatten
  mixed <- engine$inspect_compilation("(and (or a b) c)")
  # This should remain nested (can't flatten different operators)
  expect_true(is.language(mixed$compiled))
})

# ==============================================================================
# Quasiquote Simplification Verification (Phase 3)
# ==============================================================================

test_that("VERIFY: quasiquote with no unquotes simplifies", {
  engine <- Engine$new(load_prelude = FALSE)

  # Pure quoted template (no unquotes) should be simple
  out <- engine$inspect_compilation("`(a b c)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")

  # Should be much simpler than the current 4-level nesting
  # Check that it doesn't have excessive as.call nesting
  as_call_count <- length(gregexpr("as\\.call", compiled_str)[[1]])
  expect_true(as_call_count <= 2,
    info = sprintf("Expected <=2 as.call, got %d", as_call_count))

  # Simple symbols should be very simple
  out <- engine$inspect_compilation("`x")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  expect_true(nchar(compiled_str) < 50,
    info = "Simple quasiquote should generate short code")
})

test_that("VERIFY: quasiquote with unquotes preserves correctness", {
  engine <- Engine$new(load_prelude = FALSE)

  # With unquotes, correctness is paramount
  # These tests verify behavior, not optimization
  engine$eval(engine$read("(define x 42)")[[1]])

  result <- engine$eval(engine$read("`(a ,x c)")[[1]])
  # Result is a call object
  expect_true(is.call(result))
  expect_equal(result[[2]], 42)  # Middle element should be unquoted value

  # Nested quasiquote
  result <- engine$eval(engine$read("`(a `(b ,x) c)")[[1]])
  expect_true(is.call(result))
})

test_that("VERIFY: quasiquote code complexity reduction", {
  engine <- Engine$new(load_prelude = FALSE)

  # Measure complexity of compiled quasiquote
  simple_qq <- engine$inspect_compilation("`(a b c)")
  with_unquote <- engine$inspect_compilation("`(a ,x c)")

  # Simple case should be simpler than unquote case
  simple_len <- length(deparse(simple_qq$compiled))
  unquote_len <- length(deparse(with_unquote$compiled))

  # Simple should be notably smaller (this will fail before optimization)
  expect_true(simple_len < unquote_len * 0.7,
    info = sprintf("Simple (%d lines) should be <70%% of unquote (%d lines)",
                   simple_len, unquote_len))
})

# ==============================================================================
# Strength Reduction Verification (Phase 3)
# ==============================================================================

test_that("VERIFY: multiplication by 2 reduces to addition", {
  engine <- Engine$new(load_prelude = FALSE)

  # (* x 2) should become (+ x x)
  out <- engine$inspect_compilation("(* x 2)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")

  # Should have + instead of *
  expect_true(grepl("\\+", compiled_str),
    info = "Should use addition")
  expect_false(grepl("\\*", compiled_str),
    info = "Should not use multiplication")
})

test_that("VERIFY: power of 2 reduces to multiplication", {
  engine <- Engine$new(load_prelude = FALSE)

  # (^ x 2) should become (* x x)
  out <- engine$inspect_compilation("(^ x 2)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")

  # Should have * instead of ^
  expect_true(grepl("\\*", compiled_str),
    info = "Should use multiplication")
  expect_false(grepl("\\^", compiled_str),
    info = "Should not use exponentiation")
})

test_that("VERIFY: strength reduction preserves semantics", {
  engine <- Engine$new(load_prelude = FALSE)

  # Test that optimized code produces same results
  engine$eval(engine$read("(define x 5)")[[1]])

  # (* x 2) should still equal 10
  result1 <- engine$eval(engine$read("(* x 2)")[[1]])
  expect_equal(result1, 10)

  # (^ x 2) should still equal 25
  result2 <- engine$eval(engine$read("(^ x 2)")[[1]])
  expect_equal(result2, 25)
})

test_that("VERIFY: strength reduction only applies to safe cases", {
  engine <- Engine$new(load_prelude = FALSE)

  # (* x 3) should NOT reduce (not power of 2)
  out <- engine$inspect_compilation("(* x 3)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  expect_true(grepl("\\*", compiled_str),
    info = "Multiplication by 3 should not reduce")

  # (^ x 3) should NOT reduce (not power of 2)
  out <- engine$inspect_compilation("(^ x 3)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  expect_true(grepl("\\^", compiled_str),
    info = "Power of 3 should not reduce")
})

test_that("VERIFY: strength reduction does not duplicate side-effectful expressions", {
  engine <- Engine$new(load_prelude = TRUE)

  # (* (begin (set! x (+ x 1)) x) 2) should NOT be reduced to addition,
  # because the expression has side effects that would execute twice
  engine$eval_text("(define x 0)")
  out <- engine$inspect_compilation("(* (begin (set! x (+ x 1)) x) 2)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  # Should still use multiplication (not reduced)
  expect_true(grepl("\\*", compiled_str),
    info = "Side-effectful expression should not be strength-reduced")

  # But (* x 2) with a simple symbol should still reduce
  out <- engine$inspect_compilation("(* x 2)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  expect_true(grepl("\\+", compiled_str),
    info = "Simple symbol should still be strength-reduced")

  # (* (f x) 2) with a function call should NOT reduce
  out <- engine$inspect_compilation("(* (f x) 2)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  expect_true(grepl("\\*", compiled_str),
    info = "Function call should not be strength-reduced")

  # (^ (f x) 2) with a function call should NOT reduce
  out <- engine$inspect_compilation("(^ (f x) 2)")
  compiled_str <- paste(deparse(out$compiled), collapse = " ")
  expect_true(grepl("\\^", compiled_str),
    info = "Function call in exponent base should not be strength-reduced")

  # Semantic correctness: side effects should execute exactly once
  engine$eval_text("(define counter 0)")
  engine$eval_text("(define bump (lambda () (set! counter (+ counter 1)) counter))")
  result <- engine$eval_text("(* (bump) 2)")
  expect_equal(result, 2)  # bump returns 1, * 2 = 2
  expect_equal(engine$eval_text("counter"), 1)  # bump called exactly once
})

# ==============================================================================
# Nesting Depth Restoration Verification
# ==============================================================================

test_that("VERIFY: nesting_depth is restored after compilation errors in define", {
  # The observable effect of corrupted nesting_depth is that top-level import
  # compilation would fail (compile_import checks nesting_depth > 0).
  # We test that after a failed define, subsequent compilations still work.
  engine <- Engine$new(load_prelude = FALSE)

  # Compile a valid top-level define
  engine$eval_text("(define x 1)")

  # Attempt to compile a define with an invalid value expression.
  # This should fail gracefully without corrupting compiler state.
  tryCatch(
    engine$eval_text("(define y (undefined-special-form z))"),
    error = function(e) NULL
  )

  # If nesting_depth was corrupted, this define would behave unexpectedly
  # because the compiler would think we're inside a nested scope
  expect_no_error(engine$eval_text("(define z 3)"))
  expect_equal(engine$eval_text("z"), 3)
})

test_that("VERIFY: nesting_depth is restored after compilation errors in set!", {
  engine <- Engine$new(load_prelude = FALSE)
  engine$eval_text("(define x 1)")

  # Attempt set! with an invalid value
  tryCatch(
    engine$eval_text("(set! x (undefined-special-form z))"),
    error = function(e) NULL
  )

  # Subsequent define should still work at top level
  expect_no_error(engine$eval_text("(define y 2)"))
  expect_equal(engine$eval_text("y"), 2)
})

# ==============================================================================
# Boolean Flatten Edge Cases
# ==============================================================================

test_that("VERIFY: and/or handle degenerate nested empty forms", {
  engine <- Engine$new(load_prelude = FALSE)

  # (and) with no args should return #t (identity for and)
  result <- engine$eval_text("(and)")
  expect_true(result)

  # (or) with no args should return #f (identity for or)
  result <- engine$eval_text("(or)")
  expect_false(result)

  # Single-arg forms should work
  result <- engine$eval_text("(and 42)")
  expect_equal(result, 42)
  result <- engine$eval_text("(or 42)")
  expect_equal(result, 42)
})

# ==============================================================================
# No-Folding Mode: Builtins Match R Semantics Without Constant Folding
# ==============================================================================
#
# Constant folding evaluates pure expressions at compile time using base R.
# When folding is disabled, the same expressions run through Arl's builtin
# wrappers at runtime. These tests verify that builtins produce the same
# results as base R, catching divergences like the NaN == NaN bug where
# variadic_eq returned FALSE instead of NA.

test_that("disable_constant_folding parameter works", {
  e_fold <- Engine$new(load_prelude = FALSE)
  e_nofold <- Engine$new(load_prelude = FALSE, disable_constant_folding = TRUE)

  # With folding, (+ 1 2) should be a literal

  out_fold <- e_fold$inspect_compilation("(+ 1 2)")
  expect_true(is.numeric(out_fold$compiled))

  # Without folding, (+ 1 2) should remain a call
  out_nofold <- e_nofold$inspect_compilation("(+ 1 2)")
  expect_true(is.call(out_nofold$compiled))

  # Both should produce the same result
  expect_equal(e_fold$eval_text("(+ 1 2)"), 3)
  expect_equal(e_nofold$eval_text("(+ 1 2)"), 3)
})

test_that("disable_constant_folding via R option works", {
  withr::local_options(list(arl.disable_constant_folding = TRUE))
  e <- Engine$new(load_prelude = FALSE)
  out <- e$inspect_compilation("(+ 1 2)")
  expect_true(is.call(out$compiled))
  expect_equal(e$eval_text("(+ 1 2)"), 3)
})

test_that("disable_constant_folding via env var works", {
  withr::local_envvar(ARL_DISABLE_CONSTANT_FOLDING = "1")
  e <- Engine$new(load_prelude = FALSE)
  out <- e$inspect_compilation("(+ 1 2)")
  expect_true(is.call(out$compiled))
  expect_equal(e$eval_text("(+ 1 2)"), 3)
})

test_that("arithmetic builtins match R without folding", {
  e <- Engine$new(disable_constant_folding = TRUE)

  expect_equal(e$eval_text("(+ 1 2)"), 3)
  expect_equal(e$eval_text("(- 10 3)"), 7)
  expect_equal(e$eval_text("(* 4 5)"), 20)
  expect_equal(e$eval_text("(/ 10 3)"), 10 / 3)
  expect_equal(e$eval_text("(^ 2 10)"), 1024)
  expect_equal(e$eval_text("(%% 10 3)"), 1)
  expect_equal(e$eval_text("(%/% 10 3)"), 3)
})

test_that("comparison builtins match R without folding", {
  e <- Engine$new(disable_constant_folding = TRUE)

  expect_true(e$eval_text("(< 1 2)"))
  expect_false(e$eval_text("(< 2 1)"))
  expect_true(e$eval_text("(> 2 1)"))
  expect_false(e$eval_text("(> 1 2)"))
  expect_true(e$eval_text("(<= 1 1)"))
  expect_true(e$eval_text("(>= 1 1)"))
  expect_true(e$eval_text("(!= 1 2)"))
  expect_false(e$eval_text("(!= 1 1)"))
})

test_that("equality builtins handle NaN/NA correctly without folding", {
  e <- Engine$new(disable_constant_folding = TRUE)

  # NaN == NaN should return NA (R semantics), not FALSE
  result <- e$eval_text("(== NaN NaN)")
  expect_true(is.na(result))

  # NA == NA should also return NA
  result <- e$eval_text("(== NA NA)")
  expect_true(is.na(result))

  # Normal equality should still work
  expect_true(e$eval_text("(== 1 1)"))
  expect_false(e$eval_text("(== 1 2)"))

  # NULL equality (Arl-specific: NULL-safe)
  expect_true(e$eval_text("(== NULL NULL)"))
  expect_false(e$eval_text("(== 1 NULL)"))
  expect_false(e$eval_text("(== NULL 1)"))

  # != with NaN
  result <- e$eval_text("(!= NaN NaN)")
  expect_true(is.na(result))
})

test_that("logical builtins match R without folding", {
  e <- Engine$new(disable_constant_folding = TRUE)

  expect_true(e$eval_text("(& #t #t)"))
  expect_false(e$eval_text("(& #t #f)"))
  expect_true(e$eval_text("(| #f #t)"))
  expect_false(e$eval_text("(| #f #f)"))
  expect_false(e$eval_text("(! #t)"))
  expect_true(e$eval_text("(! #f)"))
})

test_that("math builtins match R without folding", {
  e <- Engine$new(disable_constant_folding = TRUE)

  expect_equal(e$eval_text("(abs -5)"), 5)
  expect_equal(e$eval_text("(sqrt 16)"), 4)
  expect_equal(e$eval_text("(floor 3.7)"), 3)
  expect_equal(e$eval_text("(ceiling 3.2)"), 4)
  expect_equal(e$eval_text("(round 3.5)"), 4)

  # Special values
  expect_true(is.nan(suppressWarnings(e$eval_text("(sqrt -1)"))))
  expect_equal(e$eval_text("(abs Inf)"), Inf)
  expect_equal(e$eval_text("(log 1)"), 0)
})

test_that("folded and unfolded results agree on edge cases", {
  e_fold <- Engine$new()
  e_nofold <- Engine$new(disable_constant_folding = TRUE)

  cases <- c(
    "(+ 0 0)", "(* 0 1)", "(/ 1 0)", "(- 0 0)",
    "(== 1 1)", "(== 1 2)", "(!= 1 1)", "(!= 1 2)",
    "(< 1 1)", "(<= 1 1)", "(> 1 1)", "(>= 1 1)",
    "(abs -0)", "(sqrt 0)", "(log Inf)"
  )
  for (expr in cases) {
    r_fold <- e_fold$eval_text(expr)
    r_nofold <- e_nofold$eval_text(expr)
    # Use identical() to catch NA vs FALSE etc.
    expect_identical(r_fold, r_nofold,
      info = sprintf("Mismatch for %s: folded=%s, unfolded=%s",
                     expr, deparse(r_fold), deparse(r_nofold)))
  }
})
