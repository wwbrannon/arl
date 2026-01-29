test_that("rye_do_call quotes symbols except for accessors", {
  a <- 10
  result <- rye:::rye_do_call(base::list, list(as.symbol("a")))
  expect_true(is.symbol(result[[1]]))
  expect_equal(as.character(result[[1]]), "a")

  obj <- list(a = 1)
  expect_equal(rye:::rye_do_call(base::`$`, list(obj, as.symbol("a"))), 1)
  expect_equal(rye:::rye_do_call(base::`[`, list(c(10, 20), 2)), 20)
  expect_equal(rye:::rye_do_call(base::`[[`, list(list(1, 2), 2)), 2)
})

test_that("RyeEnv$assign targets nearest rye environment", {
  root <- new.env(parent = emptyenv())
  assign(".rye_env", TRUE, envir = root)
  root$x <- 1
  child <- new.env(parent = root)
  assign(".rye_env", TRUE, envir = child)

  RyeEnv$new(child)$assign("x", 2)
  expect_equal(root$x, 2)
  expect_false(exists("x", envir = child, inherits = FALSE))
})

test_that("RyeEnv$assign falls back to current env", {
  parent_env <- new.env(parent = emptyenv())
  child <- new.env(parent = parent_env)
  assign(".rye_env", TRUE, envir = child)

  RyeEnv$new(child)$assign("z", 3)
  expect_true(exists("z", envir = child, inherits = FALSE))
  expect_false(exists("z", envir = parent_env, inherits = FALSE))
})
