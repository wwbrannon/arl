test_that("help accepts symbol and string topics", {
  engine <- make_engine()
  env <- engine$env$raw()
  expect_error(capture.output(engine$eval_text("(help mean)", env = env)), NA)
  expect_error(capture.output(engine$eval_text("(help \"mean\")", env = env)), NA)
})

test_that("help shows Rye special-form docs", {
  engine <- make_engine()
  env <- engine$env$raw()
  output <- capture.output(engine$eval_text("(help if)", env = env))
  expect_true(any(grepl("Topic: if", output)))
  expect_true(any(grepl("Usage: (if test", output, fixed = TRUE)))

})

test_that("help shows Rye stdlib docs via attributes", {
  engine <- make_engine()
  env <- engine$env$raw()
  output <- capture.output(engine$eval_text("(help funcall)", env = env))
  expect_true(any(grepl("\\(funcall fn args\\)", output)))
})

test_that("help shows Rye macro docs from stdlib files", {
  engine <- make_engine()
  env <- engine$env$raw()
  import_stdlib_modules(engine, c("control"))
  output <- capture.output(engine$eval_text("(help when)", env = env))
  expect_true(any(grepl("Topic: when", output)))
  expect_true(any(grepl("\\(when test \\. body\\)", output)))
})

test_that("help reads lambda docstrings", {
  engine <- make_engine()
  env <- engine$env$raw()
  engine$eval_text("(define add (lambda (x y) \"Add x and y.\" (+ x y)))", env = env)
  output <- capture.output(engine$eval_text("(help add)", env = env))
  expect_true(any(grepl("Usage: \\(add x y\\)", output)))
  expect_true(any(grepl("Add x and y\\.", output)))
})

# Initialization tests
test_that("HelpSystem initialization requires RyeEnv and MacroExpander", {
  env <- new.env()
  rye_env <- rye:::RyeEnv$new(env)

  # Need both rye_env and macro_expander
  expect_true(r6_isinstance(rye_env, "RyeEnv"))

  # Create full engine to get macro_expander
  engine <- make_engine()
  expect_true(r6_isinstance(engine$help_system, "HelpSystem"))
})

test_that("HelpSystem builds special forms help on init", {
  engine <- make_engine()
  help_sys <- engine$help_system

  # Should have help for all special forms
  specials_help <- environment(help_sys$help_in_env)$private$specials_help
  expect_true("if" %in% names(specials_help))
  expect_true("lambda" %in% names(specials_help))
  expect_true("define" %in% names(specials_help))
  expect_true("quote" %in% names(specials_help))
})

# Help lookup chain tests
test_that("help shows all special forms", {
  engine <- make_engine()
  env <- engine$env$raw()

  # Test a few key special forms
  specials <- c("quote", "if", "lambda", "define", "set!", "begin", "module", "import")

  for (special in specials) {
    output <- capture.output(engine$eval_text(sprintf("(help %s)", special), env = env))
    expect_true(any(grepl(paste0("Topic: ", special), output)),
                info = sprintf("Help for %s should show topic", special))
  }
})

test_that("help shows macro documentation", {
  engine <- make_engine()
  env <- engine$env$raw()

  # Define a macro with docstring
  engine$eval_text('(defmacro test-macro (x) "Test macro docstring" x)', env = env)
  output <- capture.output(engine$eval_text("(help test-macro)", env = env))

  expect_true(any(grepl("Topic: test-macro", output)))
  expect_true(any(grepl("Test macro docstring", output)))
})

test_that("help shows R function signatures", {
  engine <- make_engine()
  env <- engine$env$raw()

  # R function with formals
  env$test_fn <- function(x, y = 10) x + y
  output <- capture.output(engine$eval_text("(help test_fn)", env = env))

  expect_true(any(grepl("Usage:", output)))
  expect_true(any(grepl("test_fn", output)))
})

test_that("help handles functions without formals", {
  engine <- make_engine()
  env <- engine$env$raw()

  # Primitive functions have no formals
  env$primitive_fn <- base::`+`
  output <- capture.output(engine$eval_text("(help primitive_fn)", env = env))

  # Should show topic even if usage is minimal
  expect_true(any(grepl("Topic: primitive_fn", output)))
})

test_that("help shows rye_doc attribute on functions", {
  engine <- make_engine()
  env <- engine$env$raw()

  # Function with rye_doc
  env$documented <- function(x) x * 2
  attr(env$documented, "rye_doc") <- list(
    description = "Double the input value.",
    usage = "(documented value)"
  )

  output <- capture.output(engine$eval_text("(help documented)", env = env))
  expect_true(any(grepl("Double the input value", output)))
  expect_true(any(grepl("\\(documented value\\)", output)))
})

test_that("help falls back to utils::help for unknown topics", {
  engine <- make_engine()
  env <- engine$env$raw()

  # Topic not in specials, macros, or env should fall back to R help
  # We can't easily test utils::help output, but we can verify no error
  expect_silent(capture.output(engine$eval_text("(help mean)", env = env)))
})

test_that("help prioritizes specials over macros", {
  engine <- make_engine()
  env <- engine$env$raw()

  # Try to define a macro with same name as special (won't override special help)
  # 'quote' is a special form
  output <- capture.output(engine$eval_text("(help quote)", env = env))

  # Should show special form help
  expect_true(any(grepl("Return expr without evaluation", output)))
})

test_that("help handles rye_closure with param_specs", {
  engine <- make_engine()
  env <- engine$env$raw()

  # Lambda creates rye_closure
  engine$eval_text("(define adder (lambda (x y) (+ x y)))", env = env)
  output <- capture.output(engine$eval_text("(help adder)", env = env))

  expect_true(any(grepl("\\(adder x y\\)", output)))
})

test_that("help handles rye_closure with defaults", {
  engine <- make_engine()
  env <- engine$env$raw()

  # Lambda with default argument
  engine$eval_text("(define greet (lambda ((name \"World\")) (string-append \"Hello, \" name)))", env = env)
  output <- capture.output(engine$eval_text("(help greet)", env = env))

  # Should show default in usage
  expect_true(any(grepl("greet", output)))
})

test_that("help handles rye_closure with rest params", {
  engine <- make_engine()
  env <- engine$env$raw()

  # Lambda with rest parameter
  engine$eval_text("(define variadic (lambda (x . rest) x))", env = env)
  output <- capture.output(engine$eval_text("(help variadic)", env = env))

  # Should show rest param in usage
  expect_true(any(grepl("variadic", output)))
  expect_true(any(grepl("\\.", output)))
})

# Usage generation tests
test_that("usage_from_closure handles empty params", {
  engine <- make_engine()
  env <- engine$env$raw()

  engine$eval_text("(define no_args (lambda () 42))", env = env)
  output <- capture.output(engine$eval_text("(help no_args)", env = env))

  expect_true(any(grepl("\\(no_args\\)", output)))
})

test_that("usage_from_formals formats R function signatures", {
  engine <- make_engine()
  env <- engine$env$raw()

  env$complex_fn <- function(a, b = 1, c = NULL, ...) a + b
  output <- capture.output(engine$eval_text("(help complex_fn)", env = env))

  expect_true(any(grepl("complex_fn", output)))
  expect_true(any(grepl("Usage:", output)))
})

test_that("usage_from_macro shows macro parameters", {
  engine <- make_engine()
  env <- engine$env$raw()

  engine$eval_text("(defmacro my-macro (x y z) (list x y z))", env = env)
  output <- capture.output(engine$eval_text("(help my-macro)", env = env))

  expect_true(any(grepl("\\(my-macro x y z\\)", output)))
})
