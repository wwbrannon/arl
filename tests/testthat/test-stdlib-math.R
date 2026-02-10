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

test_that("division by zero returns Inf", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  # In R, division by zero returns Inf, not an error
  expect_equal(engine$eval_in_env(engine$read("(/ 1 0)")[[1]], env), Inf)
  expect_equal(engine$eval_in_env(engine$read("(/ -10 0)")[[1]], env), -Inf)
})

# ============================================================================
# Coverage: Variadic comparison operators with 1 arg (error paths)
# ============================================================================

test_that("variadic comparison operators return #t with 0 or 1 arguments (vacuously true)", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  # 1 argument: vacuously true
  expect_true(engine$eval_in_env(engine$read("(< 1)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(> 1)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(<= 1)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(>= 1)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(== 1)")[[1]], env))

  # 0 arguments: vacuously true
  expect_true(engine$eval_in_env(engine$read("(<)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(>)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(<=)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(>=)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(==)")[[1]], env))
})

# ============================================================================
# Coverage: Arithmetic/stats with 0 args (error paths)
# ============================================================================

test_that("variadic arithmetic operators error with 0 arguments", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_error(engine$eval_in_env(engine$read("(-)")[[1]], env), "requires at least one argument")
  expect_error(engine$eval_in_env(engine$read("(/)")[[1]], env), "requires at least one argument")
  expect_error(engine$eval_in_env(engine$read("(min)")[[1]], env), "requires at least one argument")
  expect_error(engine$eval_in_env(engine$read("(max)")[[1]], env), "requires at least one argument")
  # gcd with 0 args returns 0 (identity element)
  expect_equal(engine$eval_in_env(engine$read("(gcd)")[[1]], env), 0)
  # lcm with 0 args returns 1 (identity element)
  expect_equal(engine$eval_in_env(engine$read("(lcm)")[[1]], env), 1)
})

# ============================================================================
# Coverage: Number predicate edge cases
# ============================================================================

test_that("number predicate edge cases cover remaining lines", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  # integer? with 3.0 (finite, == as.integer) -> #t
  expect_true(engine$eval_in_env(engine$read("(integer? 3.0)")[[1]], env))

  # rational? with finite real -> #t
  expect_true(engine$eval_in_env(engine$read("(rational? 1.5)")[[1]], env))

  # rational? with infinite -> #f
  expect_false(engine$eval_in_env(engine$read("(rational? Inf)")[[1]], env))

  # inexact? with double -> #t
  expect_true(engine$eval_in_env(engine$read("(inexact? 3.14)")[[1]], env))

  # inexact? with non-number -> #f
  expect_false(engine$eval_in_env(engine$read("(inexact? \"hi\")")[[1]], env))
})

# ============================================================================
# Coverage: Uncalled math functions (expt, atan2)
# ============================================================================

test_that("expt and atan2 work", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(expt 2 10)")[[1]], env), 1024)
  expect_equal(
    engine$eval_in_env(engine$read("(atan2 1.0 1.0)")[[1]], env),
    atan2(1.0, 1.0)
  )
})
