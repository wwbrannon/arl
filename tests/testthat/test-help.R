test_that("help accepts symbol and string topics", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)
  expect_error(capture.output(engine$eval_text("(help mean)", env)), NA)
  expect_error(capture.output(engine$eval_text("(help \"mean\")", env)), NA)
})

test_that("help shows Rye special-form docs", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)
  output <- capture.output(engine$eval_text("(help if)", env))
  expect_true(any(grepl("Topic: if", output)))
  expect_true(any(grepl("Usage: (if test", output, fixed = TRUE)))

})

test_that("help shows Rye stdlib docs via attributes", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)
  output <- capture.output(engine$eval_text("(help apply)", env))
  expect_true(any(grepl("\\(apply fn args\\)", output)))
})

test_that("help shows Rye macro docs from stdlib files", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)
  import_stdlib_modules(engine, c("control"))
  output <- capture.output(engine$eval_text("(help when)", env))
  expect_true(any(grepl("Topic: when", output)))
  expect_true(any(grepl("\\(when test body\\)", output)))
})

test_that("help reads lambda docstrings", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)
  engine$eval_text("(define add (lambda (x y) \"Add x and y.\" (+ x y)))", env)
  output <- capture.output(engine$eval_text("(help add)", env))
  expect_true(any(grepl("Usage: \\(add x y\\)", output)))
  expect_true(any(grepl("Add x and y\\.", output)))
})
