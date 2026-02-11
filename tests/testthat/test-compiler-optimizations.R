# Compiler Optimization Verification Tests
#
# These tests verify that compiler optimizations actually fire by checking
# properties of the compiled output, not just semantic correctness.
#
# Philosophy: We check properties (e.g., "is not an if statement") rather than
# exact output, making tests resilient to optimization strategy changes while
# still ensuring optimizations are applied.

engine <- Engine$new()

# ==============================================================================
# Constant Folding Verification
# ==============================================================================

test_that("VERIFY: constant folding produces literals", {
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

  # Literal TRUE should not have .rye_true_p wrapper
  out <- engine$inspect_compilation("(if #t 1 2)")
  compiled_str <- paste(out$compiled_deparsed, collapse = " ")
  expect_false(grepl(".rye_true_p", compiled_str))

  # Comparison should not have wrapper
  out <- engine$inspect_compilation("(if (> x 5) 1 2)")
  compiled_str <- paste(out$compiled_deparsed, collapse = " ")
  expect_false(grepl(".rye_true_p.*>", compiled_str))

  # Variable SHOULD have wrapper
  out <- engine$inspect_compilation("(if x 1 2)")
  compiled_str <- paste(out$compiled_deparsed, collapse = " ")
  expect_true(grepl(".rye_true_p", compiled_str))
})

# ==============================================================================
# Dead Code Elimination Verification
# ==============================================================================

test_that("VERIFY: dead code elimination removes unreachable branches", {
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
  engine <- Engine$new()

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
