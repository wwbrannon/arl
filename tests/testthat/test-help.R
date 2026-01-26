test_that("help accepts symbol and string topics", {
  env <- rye_load_stdlib_base()
  expect_error(rye_eval_text("(help mean)", env), NA)
  expect_error(rye_eval_text("(help \"mean\")", env), NA)
})

test_that("help shows Rye special-form docs", {
  env <- rye_load_stdlib_base()
  output <- capture.output(rye_eval_text("(help if)", env))
  expect_true(any(grepl("Topic: if", output)))
  expect_true(any(grepl("Usage: (if test", output, fixed = TRUE)))

})

test_that("help shows Rye stdlib docs via attributes", {
  env <- rye_load_stdlib_base()
  output <- capture.output(rye_eval_text("(help map)", env))
  expect_true(any(grepl("\\(map fn lst\\)", output)))
})

test_that("help shows Rye macro docs from stdlib files", {
  env <- rye_load_stdlib(load_files = TRUE)
  output <- capture.output(rye_eval_text("(help when)", env))
  expect_true(any(grepl("Topic: when", output)))
  expect_true(any(grepl("\\(when test body\\)", output)))
})
