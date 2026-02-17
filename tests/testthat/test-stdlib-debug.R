# Comprehensive error and debug helper tests

engine <- make_engine()

test_that("error and debug helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_error(get("error", envir = env)("boom"), "boom")
  expect_warning(get("warn", envir = env)("warn"))
  expect_error(get("assert", envir = env)(FALSE, "nope"), "nope")
  expect_true(get("assert", envir = env)(TRUE, "nope"))

  output <- capture.output(get("trace", envir = env)("hi", "label"))
  expect_true(any(grepl("label", output)))
})
