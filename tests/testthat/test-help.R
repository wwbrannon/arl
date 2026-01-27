test_that("help accepts symbol and string topics", {
  env <- rye_load_stdlib_base()
  expect_error(capture.output(rye_eval_text("(help mean)", env)), NA)
  expect_error(capture.output(rye_eval_text("(help \"mean\")", env)), NA)
})

test_that("help shows Rye special-form docs", {
  env <- rye_load_stdlib_base()
  output <- capture.output(rye_eval_text("(help if)", env))
  expect_true(any(grepl("Topic: if", output)))
  expect_true(any(grepl("Usage: (if test", output, fixed = TRUE)))

})

test_that("help shows Rye stdlib docs via attributes", {
  env <- rye_load_stdlib_base()
  output <- capture.output(rye_eval_text("(help apply)", env))
  expect_true(any(grepl("\\(apply fn args\\)", output)))
})

test_that("help shows Rye macro docs from stdlib files", {
  env <- rye_load_stdlib()
  output <- capture.output(rye_eval_text("(help when)", env))
  expect_true(any(grepl("Topic: when", output)))
  expect_true(any(grepl("\\(when test body\\)", output)))
})

test_that("help reads lambda docstrings", {
  env <- rye_load_stdlib()
  rye_eval_text("(define add (lambda (x y) \"Add x and y.\" (+ x y)))", env)
  output <- capture.output(rye_eval_text("(help add)", env))
  expect_true(any(grepl("Usage: \\(add x y\\)", output)))
  expect_true(any(grepl("Add x and y\\.", output)))
})
