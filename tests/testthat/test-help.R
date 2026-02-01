test_that("help accepts symbol and string topics", {
  engine <- RyeEngine$new()
  env <- engine$env$env
  expect_error(capture.output(engine$eval_text_in_env("(help mean)", env)), NA)
  expect_error(capture.output(engine$eval_text_in_env("(help \"mean\")", env)), NA)
})

test_that("help shows Rye special-form docs", {
  engine <- RyeEngine$new()
  env <- engine$env$env
  output <- capture.output(engine$eval_text_in_env("(help if)", env))
  expect_true(any(grepl("Topic: if", output)))
  expect_true(any(grepl("Usage: (if test", output, fixed = TRUE)))

})

test_that("help shows Rye stdlib docs via attributes", {
  engine <- RyeEngine$new()
  env <- engine$env$env
  output <- capture.output(engine$eval_text_in_env("(help funcall)", env))
  expect_true(any(grepl("\\(funcall fn args\\)", output)))
})

test_that("help shows Rye macro docs from stdlib files", {
  engine <- RyeEngine$new()
  env <- engine$env$env
  import_stdlib_modules(engine, c("control"))
  output <- capture.output(engine$eval_text_in_env("(help when)", env))
  expect_true(any(grepl("Topic: when", output)))
  expect_true(any(grepl("\\(when test body\\)", output)))
})

test_that("help reads lambda docstrings", {
  engine <- RyeEngine$new()
  env <- engine$env$env
  engine$eval_text_in_env("(define add (lambda (x y) \"Add x and y.\" (+ x y)))", env)
  output <- capture.output(engine$eval_text_in_env("(help add)", env))
  expect_true(any(grepl("Usage: \\(add x y\\)", output)))
  expect_true(any(grepl("Add x and y\\.", output)))
})
