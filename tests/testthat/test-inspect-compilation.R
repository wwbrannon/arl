engine <- RyeEngine$new()

test_that("inspect_compilation returns a list with expected names", {
  out <- engine$inspect_compilation("(+ 1 2)")
  expect_named(out, c("parsed", "expanded", "compiled", "compiled_deparsed"))
})

test_that("inspect_compilation on compilable expression returns right-shaped results", {
  out <- engine$inspect_compilation("(+ 1 2)")
  expect_true(is.language(out$parsed))
  expect_true(is.language(out$expanded))
  # Compiled can be a language object or a literal (if constant-folded)
  expect_true(is.language(out$compiled) || is.atomic(out$compiled))
  expect_type(out$compiled_deparsed, "character")
  expect_true(length(out$compiled_deparsed) >= 1L)
  expect_false(any(is.na(out$compiled_deparsed)))
})

test_that("inspect_compilation keeps compiled and compiled_deparsed in sync", {
  # By design: no expression -> no compiled form. When there is one, both are set or both NULL.
  out_empty <- engine$inspect_compilation("")
  expect_null(out_empty$compiled)
  expect_null(out_empty$compiled_deparsed)
  out_simple <- engine$inspect_compilation("(+ 1 2)")
  if (!is.null(out_simple$compiled)) {
    expect_false(is.null(out_simple$compiled_deparsed))
  } else {
    expect_true(is.null(out_simple$compiled_deparsed))
  }
})

test_that("inspect_compilation on empty text returns all NULL", {
  out <- engine$inspect_compilation("")
  expect_null(out$parsed)
  expect_null(out$expanded)
  expect_null(out$compiled)
  expect_null(out$compiled_deparsed)
})

test_that("inspect_compilation accepts env and uses it for expansion", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("control"), env)
  # With control loaded, (and 1 2) expands (macro); without env would use engine env
  out <- engine$inspect_compilation("(and 1 2)", env = env)
  expect_named(out, c("parsed", "expanded", "compiled", "compiled_deparsed"))
  expect_true(is.language(out$parsed))
  expect_true(is.language(out$expanded))
  # Expanded form may differ from parsed when macros are involved
  expect_true(is.language(out$compiled) || is.null(out$compiled))
  if (!is.null(out$compiled_deparsed)) {
    expect_type(out$compiled_deparsed, "character")
    expect_true(length(out$compiled_deparsed) >= 1L)
  }
})

test_that("inspect_compilation with env = NULL uses engine environment", {
  out <- engine$inspect_compilation("(* 2 3)", env = NULL)
  # Compiled can be a language object or a literal (if constant-folded)
  expect_true(is.language(out$compiled) || is.atomic(out$compiled))
  expect_type(out$compiled_deparsed, "character")
})

test_that("compiled_deparsed when present is parseable R code", {
  out <- engine$inspect_compilation("(if #t 1 2)")
  skip_if(is.null(out$compiled), "Compiler returned NULL for this expression")
  parsed_back <- tryCatch(parse(text = out$compiled_deparsed), error = function(e) NULL)
  expect_true(is.language(parsed_back), info = "compiled_deparsed should parse as valid R")
})
