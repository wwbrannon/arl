# Comprehensive struct definition tests

engine <- make_engine()

test_that("defstruct macro defines constructor and accessors", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("struct"), env = env)

  engine$eval(engine$read("(defstruct Point (x y))")[[1]], env = env)
  engine$eval(engine$read("(define p (make-Point 1 2))")[[1]], env = env)

  expect_true(engine$eval(engine$read("(Point? p)")[[1]], env = env))
  expect_equal(engine$eval(engine$read("(Point-x p)")[[1]], env = env), 1)
  expect_equal(engine$eval(engine$read("(Point-y p)")[[1]], env = env), 2)
})
