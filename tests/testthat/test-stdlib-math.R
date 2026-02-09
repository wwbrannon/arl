# Comprehensive math and numeric helper tests

engine <- make_engine()

# ============================================================================
# Numeric Helpers
# ============================================================================

test_that("numeric helpers inc/dec/clamp/within? work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$inc(5), 6)
  expect_equal(env$inc(5, 2), 7)
  expect_equal(env$dec(5), 4)
  expect_equal(env$dec(5, 2), 3)

  expect_equal(env$clamp(5, 1, 10), 5)
  expect_equal(env$clamp(-1, 0, 10), 0)
  expect_equal(env$clamp(11, 0, 10), 10)

  expect_true(env$`within?`(5, 1, 10))
  expect_false(env$`within?`(0, 1, 10))
  expect_false(env$`within?`(11, 1, 10))
})

# ============================================================================
# Complex Numbers
# ============================================================================

test_that("type coercion functions work", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(exact->inexact 5)")[[1]], env), 5.0)
  expect_equal(engine$eval_in_env(engine$read("(inexact->exact 5.7)")[[1]], env), 6L)
  expect_equal(engine$eval_in_env(engine$read("(->integer \"42\")")[[1]], env), 42L)
  expect_equal(engine$eval_in_env(engine$read("(->double 5)")[[1]], env), 5.0)
})

test_that("complex number utilities work", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  z <- engine$eval_in_env(engine$read("(make-rectangular 3 4)")[[1]], env)
  expect_equal(Re(z), 3.0)
  expect_equal(Im(z), 4.0)

  expect_equal(engine$eval_in_env(engine$read("(real-part (make-rectangular 3 4))")[[1]], env), 3.0)
  expect_equal(engine$eval_in_env(engine$read("(imag-part (make-rectangular 3 4))")[[1]], env), 4.0)
  expect_equal(engine$eval_in_env(engine$read("(magnitude (make-rectangular 3 4))")[[1]], env), 5.0)
})

# ============================================================================
# Edge Cases: Boundary Conditions for Numeric Operations
# ============================================================================

test_that("numeric operations handle boundary conditions", {
  env <- new.env()
  stdlib_env(engine, env)

  # Large numbers
  large <- 1e100
  expect_equal(env$`=`(large, large), TRUE)

  # Negative numbers
  expect_equal(env$`%`(-10, 3), -10 %% 3)

  # Zero
  expect_equal(env$`%`(0, 5), 0)

  # Floating point (uses exact equality, not all.equal)
  expect_false(env$`=`(0.1 + 0.2, 0.3))  # floating point precision issues
  expect_true(env$`=`(1.0, 1.0))
})
