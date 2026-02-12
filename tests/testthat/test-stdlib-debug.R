# Comprehensive error and debug helper tests

engine <- make_engine()

test_that("error and debug helpers work", {
  env <- new.env()
  toplevel_env(engine, env)

  expect_error(env$error("boom"), "boom")
  expect_warning(env$warn("warn"))
  expect_error(env$assert(FALSE, "nope"), "nope")
  expect_true(env$assert(TRUE, "nope"))

  output <- capture.output(env$trace("hi", "label"))
  expect_true(any(grepl("label", output)))
})
