# Comprehensive struct definition tests

engine <- RyeEngine$new()

test_that("defstruct macro defines constructor and accessors", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("struct"), env)

  engine$eval_in_env(engine$read("(defstruct Point (x y))")[[1]], env)
  engine$eval_in_env(engine$read("(define p (make-Point 1 2))")[[1]], env)

  expect_true(engine$eval_in_env(engine$read("(Point? p)")[[1]], env))
  expect_equal(engine$eval_in_env(engine$read("(Point-x p)")[[1]], env), 1)
  expect_equal(engine$eval_in_env(engine$read("(Point-y p)")[[1]], env), 2)
})
