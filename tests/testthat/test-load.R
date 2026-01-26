test_that("rye_load_file evaluates source into environment", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

  path <- tempfile(fileext = ".rye")
  writeLines(c("(define foo 42)", "(define bar (+ foo 1))"), path)
  on.exit(unlink(path), add = TRUE)

  rye_load_file(path, env)

  expect_equal(get("foo", envir = env), 42)
  expect_equal(get("bar", envir = env), 43)
})

test_that("(load ...) evaluates file in current environment", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

  path <- tempfile(fileext = ".rye")
  writeLines("(define foo 7)", path)
  on.exit(unlink(path), add = TRUE)

  exprs <- rye_read(paste0("(load \"", path, "\")"))
  rye_eval(exprs[[1]], env)

  expect_equal(get("foo", envir = env), 7)
})

test_that("rye_load_prelude registers prelude macros", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

  rye_load_prelude(env)

  expect_true(rye:::is_macro(as.symbol("when")))
  expect_true(rye:::is_macro(as.symbol("unless")))
})
