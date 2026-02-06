test_that("rye_do_call quotes symbols except for accessors", {
  engine <- RyeEngine$new()
  a <- 10
  result <- engine$compiled_runtime$do_call(base::list, list(as.symbol("a")))
  expect_true(is.symbol(result[[1]]))
  expect_equal(as.character(result[[1]]), "a")

  obj <- list(a = 1)
  expect_equal(engine$compiled_runtime$do_call(base::`$`, list(obj, as.symbol("a"))), 1)
  expect_equal(engine$compiled_runtime$do_call(base::`[`, list(c(10, 20), 2)), 20)
  expect_equal(engine$compiled_runtime$do_call(base::`[[`, list(list(1, 2), 2)), 2)
})

test_that("RyeEnv$assign creates binding in current environment (lexical scoping)", {
  root <- new.env(parent = emptyenv())
  root$x <- 1
  child <- new.env(parent = root)

  # assign (used by define) should create a NEW binding in the current env,
  # not modify the parent env - this is proper lexical scoping
  RyeEnv$new(child)$assign("x", 2)
  expect_equal(root$x, 1)  # parent unchanged
  expect_true(exists("x", envir = child, inherits = FALSE))  # new binding in child
  expect_equal(child$x, 2)  # child has value 2
})

test_that("RyeEnv$assign falls back to current env when binding not found", {
  parent_env <- new.env(parent = emptyenv())
  child <- new.env(parent = parent_env)

  RyeEnv$new(child)$assign("z", 3)
  expect_true(exists("z", envir = child, inherits = FALSE))
  expect_false(exists("z", envir = parent_env, inherits = FALSE))
})
