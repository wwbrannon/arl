# Compiler Optimization Verification Tests
#
# These tests verify that compiler optimizations actually fire by checking
# properties of the compiled output, not just semantic correctness.
#
# Philosophy: We check properties (e.g., "is not an if statement") rather than
# exact output, making tests resilient to optimization strategy changes while
# still ensuring optimizations are applied.

engine <- RyeEngine$new()

# ==============================================================================
# Constant Folding Verification
# ==============================================================================

test_that("VERIFY: constant folding produces literals", {
  engine <- RyeEngine$new()

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
  engine <- RyeEngine$new()

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
  engine <- RyeEngine$new()

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
  engine <- RyeEngine$new()

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
  engine <- RyeEngine$new()

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
  engine <- RyeEngine$new()

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
  engine <- RyeEngine$new()

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
