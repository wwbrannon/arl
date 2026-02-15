test_that("help accepts symbol and string topics", {
  engine <- make_engine()
  env <- engine$get_env()
  expect_error(capture.output(engine$eval_text("(help mean)", env = env)), NA)
  expect_error(capture.output(engine$eval_text("(help \"mean\")", env = env)), NA)
})

test_that("help shows Arl special-form docs", {
  engine <- make_engine()
  env <- engine$get_env()
  output <- capture.output(engine$eval_text("(help if)", env = env))
  expect_true(any(grepl("Topic: if", output)))
  expect_true(any(grepl("Usage: (if test", output, fixed = TRUE)))

})

test_that("help shows Arl stdlib docs via attributes", {
  engine <- make_engine()
  env <- engine$get_env()
  output <- capture.output(engine$eval_text("(help funcall)", env = env))
  expect_true(any(grepl("\\(funcall fn args\\)", output)))
})

test_that("help shows builtin load docs via attributes", {
  engine <- make_engine()
  env <- engine$get_env()
  output <- capture.output(engine$eval_text("(help load)", env = env))
  expect_true(any(grepl("Topic: load", output)))
  expect_true(any(grepl("Usage: (load path env = NULL)", output, fixed = TRUE)))
  expect_true(any(grepl("Defaults to the current environment.", output, fixed = TRUE)))
})

test_that("help shows Arl macro docs from stdlib files", {
  engine <- make_engine()
  env <- engine$get_env()
  import_stdlib_modules(engine, c("control"))
  output <- capture.output(engine$eval_text("(help when)", env = env))
  expect_true(any(grepl("Topic: when", output)))
  expect_true(any(grepl("\\(when test \\. body\\)", output)))
})

test_that("help shows usage for lambda without docstring", {
  engine <- make_engine()
  env <- engine$get_env()
  engine$eval_text("(define add (lambda (x y) (+ x y)))", env = env)
  output <- capture.output(engine$eval_text("(help add)", env = env))
  expect_true(any(grepl("Usage: \\(add x y\\)", output)))
})

# Initialization tests
test_that("HelpSystem initialization requires Env and MacroExpander", {
  env <- new.env()
  arl_env <- arl:::Env$new(env)

  # Need both arl_env and macro_expander
  expect_true(r6_isinstance(arl_env, "Env"))

  # Create full engine to get macro_expander
  engine <- make_engine()
  expect_true(r6_isinstance(engine_field(engine, "help_system"), "HelpSystem"))
})

test_that("HelpSystem builds special forms help on init", {
  engine <- make_engine()
  help_sys <- engine_field(engine, "help_system")

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
  env <- engine$get_env()

  # Test a few key special forms
  specials <- c("quote", "if", "lambda", "define", "set!", "begin", "module", "import")

  for (special in specials) {
    output <- capture.output(engine$eval_text(sprintf("(help %s)", special), env = env))
    expect_true(any(grepl(paste0("Topic: ", special), output)),
                info = sprintf("Help for %s should show topic", special))
  }
})

test_that("help shows macro usage", {
  engine <- make_engine()
  env <- engine$get_env()

  # Define a macro without inline docstring (inline docstrings no longer detected)
  engine$eval_text("(defmacro test-macro (x y) (list x y))", env = env)
  output <- capture.output(engine$eval_text("(help test-macro)", env = env))

  expect_true(any(grepl("Topic: test-macro", output)))
  expect_true(any(grepl("\\(test-macro x y\\)", output)))
})

test_that("help shows R function signatures", {
  engine <- make_engine()
  env <- engine$get_env()

  # R function with formals
  env$test_fn <- function(x, y = 10) x + y
  output <- capture.output(engine$eval_text("(help test_fn)", env = env))

  expect_true(any(grepl("Usage:", output)))
  expect_true(any(grepl("test_fn", output)))
})

test_that("help handles functions without formals", {
  engine <- make_engine()
  env <- engine$get_env()

  # Primitive functions have no formals
  env$primitive_fn <- base::`+`
  output <- capture.output(engine$eval_text("(help primitive_fn)", env = env))

  # Should show topic even if usage is minimal
  expect_true(any(grepl("Topic: primitive_fn", output)))
})

test_that("help shows arl_doc attribute on functions", {
  engine <- make_engine()
  env <- engine$get_env()

  # Function with arl_doc
  env$documented <- function(x) x * 2
  attr(env$documented, "arl_doc") <- list(
    description = "Double the input value.",
    usage = "(documented value)"
  )

  output <- capture.output(engine$eval_text("(help documented)", env = env))
  expect_true(any(grepl("Double the input value", output)))
  expect_true(any(grepl("\\(documented value\\)", output)))
})

test_that("help shows structured R docs for non-Arl topics", {
  engine <- make_engine()
  env <- engine$get_env()

  output <- capture.output(engine$eval_text("(help \"c\")", env = env))
  expect_true(any(grepl("Topic: c", output)))
  expect_true(any(grepl("^Usage:", output)))
  expect_true(any(grepl("^Description:", output)))
})

test_that("help supports package-qualified R help", {
  engine <- make_engine()
  env <- engine$get_env()

  output <- capture.output(engine$eval_text("(help \"writeLines\" :package \"base\")", env = env))
  expect_true(any(grepl("Topic: writeLines", output)))
  expect_true(any(grepl("R package: base", output)))
  expect_true(any(grepl("^Description:", output)))
})

test_that("help package-qualified form validates keyword syntax", {
  engine <- make_engine()
  env <- engine$get_env()

  expect_error(
    engine$eval_text("(help \"writeLines\" :pkg \"base\")", env = env),
    "help keyword form requires :package"
  )
})

test_that("help prints explicit message for unknown topics", {
  engine <- make_engine()
  env <- engine$get_env()

  output <- capture.output(engine$eval_text("(help \"nonexistent_topic_arl_foo\")", env = env))
  expect_true(any(grepl("No help found for topic: nonexistent_topic_arl_foo", output)))
})

test_that("help notes other package matches when available", {
  candidates <- c("lag", "filter", "time", "plot", "window")
  counts <- vapply(candidates, function(topic) {
    length(suppressWarnings(utils::help(topic, help_type = "text", try.all.packages = TRUE)))
  }, integer(1))
  topic <- candidates[which(counts > 1)[1]]
  if (is.na(topic) || !nzchar(topic)) {
    skip("No multi-package help topic available in this R library set")
  }

  engine <- make_engine()
  env <- engine$get_env()
  output <- capture.output(engine$eval_text(sprintf("(help \"%s\")", topic), env = env))

  expect_true(any(grepl("Also found in packages:", output)))
})

test_that("help R fallback is robust to user bindings named like base helpers", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_text("(define capture.output 123)", env = env)
  engine$eval_text("(define tail 456)", env = env)
  output <- capture.output(engine$eval_text("(help \"sum\")", env = env))

  expect_true(any(grepl("Topic: sum", output)))
  expect_true(any(grepl("^Description:", output)))
})

test_that("help package argument accepts symbol package names", {
  engine <- make_engine()
  env <- engine$get_env()

  output <- capture.output(engine$eval_text("(help \"sum\" :package base)", env = env))
  expect_true(any(grepl("Topic: sum", output)))
  expect_true(any(grepl("R package: base", output)))
})

test_that("help R doc rendering does not emit warnings", {
  engine <- make_engine()
  env <- engine$get_env()
  warning_count <- 0L

  output <- withCallingHandlers(
    capture.output(engine$eval_text("(help \"sum\")", env = env)),
    warning = function(w) {
      warning_count <<- warning_count + 1L
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Topic: sum", output)))
  expect_equal(warning_count, 0L)
})

test_that("help prioritizes specials over macros", {
  engine <- make_engine()
  env <- engine$get_env()

  # Try to define a macro with same name as special (won't override special help)
  # 'quote' is a special form
  output <- capture.output(engine$eval_text("(help quote)", env = env))

  # Should show special form help
  expect_true(any(grepl("Return expr without evaluation", output)))
})

test_that("help handles arl_closure with param_specs", {
  engine <- make_engine()
  env <- engine$get_env()

  # Lambda creates arl_closure
  engine$eval_text("(define adder (lambda (x y) (+ x y)))", env = env)
  output <- capture.output(engine$eval_text("(help adder)", env = env))

  expect_true(any(grepl("\\(adder x y\\)", output)))
})

test_that("help handles arl_closure with defaults", {
  engine <- make_engine()
  env <- engine$get_env()

  # Lambda with default argument
  engine$eval_text("(define greet (lambda ((name \"World\")) (string-append \"Hello, \" name)))", env = env)
  output <- capture.output(engine$eval_text("(help greet)", env = env))

  # Should show default in usage
  expect_true(any(grepl("greet", output)))
})

test_that("help handles arl_closure with rest params", {
  engine <- make_engine()
  env <- engine$get_env()

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
  env <- engine$get_env()

  engine$eval_text("(define no_args (lambda () 42))", env = env)
  output <- capture.output(engine$eval_text("(help no_args)", env = env))

  expect_true(any(grepl("\\(no_args\\)", output)))
})

test_that("usage_from_formals formats R function signatures", {
  engine <- make_engine()
  env <- engine$get_env()

  env$complex_fn <- function(a, b = 1, c = NULL, ...) a + b
  output <- capture.output(engine$eval_text("(help complex_fn)", env = env))

  expect_true(any(grepl("complex_fn", output)))
  expect_true(any(grepl("Usage:", output)))
})

test_that("usage_from_macro shows macro parameters", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval_text("(defmacro my-macro (x y z) (list x y z))", env = env)
  output <- capture.output(engine$eval_text("(help my-macro)", env = env))

  expect_true(any(grepl("\\(my-macro x y z\\)", output)))
})
