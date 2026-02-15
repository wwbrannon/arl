test_that("Engine$import_stdlib loads stdlib after load_stdlib = FALSE", {
  engine <- make_engine(load_stdlib = FALSE)
  env <- engine$get_env()

  expect_false(exists("map", envir = env, inherits = FALSE))
  expect_silent(engine$import_stdlib())
  expect_true(exists("map", envir = env, inherits = FALSE))
})

test_that("Arl import-stdlib builtin loads stdlib into current eval env", {
  engine <- make_engine(load_stdlib = FALSE)
  env <- engine$get_env()

  expect_false(exists("map", envir = env, inherits = FALSE))
  expect_silent(engine$eval_text("(import-stdlib)", env = env))
  expect_true(exists("map", envir = env, inherits = FALSE))
})

test_that("help includes builtins-docs entry for import-stdlib", {
  engine <- make_engine(load_stdlib = FALSE)
  env <- engine$get_env()

  output <- capture.output(engine$eval_text("(help import-stdlib)", env = env))
  expect_true(any(grepl("Topic: import-stdlib", output)))
  expect_true(any(grepl("\\(import-stdlib\\)", output)))
})
