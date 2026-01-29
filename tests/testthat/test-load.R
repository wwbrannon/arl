test_that("engine$load_file evaluates source into environment", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)

  path <- tempfile(fileext = ".rye")
  writeLines(c("(define foo 42)", "(define bar (+ foo 1))"), path)
  on.exit(unlink(path), add = TRUE)

  engine$load_file(path, env)

  expect_equal(get("foo", envir = env), 42)
  expect_equal(get("bar", envir = env), 43)
})

test_that("(load ...) evaluates file in current environment", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)

  path <- tempfile(fileext = ".rye")
  writeLines("(define foo 7)", path)
  on.exit(unlink(path), add = TRUE)

  path_for_rye <- normalizePath(path, winslash = "/", mustWork = FALSE)
  exprs <- engine$read(paste0("(load \"", path_for_rye, "\")"))
  engine$eval(exprs[[1]], env)

  expect_equal(get("foo", envir = env), 7)
})

test_that("(load ...) resolves stdlib entries by name", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)

  exprs <- engine$read("(load \"control\")")

  expect_silent(engine$eval(exprs[[1]], env))
  expect_true(engine$macro_expander$is_macro(as.symbol("when"), env = env))
  expect_true(engine$macro_expander$is_macro(as.symbol("unless"), env = env))
})

test_that("engine$load_stdlib registers stdlib macros", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)
  import_stdlib_modules(engine, c("control", "binding", "threading", "error"))

  expect_true(engine$macro_expander$is_macro(as.symbol("when"), env = env))
  expect_true(engine$macro_expander$is_macro(as.symbol("unless"), env = env))
  expect_true(engine$macro_expander$is_macro(as.symbol("let"), env = env))
  expect_true(engine$macro_expander$is_macro(as.symbol("->"), env = env))
  expect_true(engine$macro_expander$is_macro(as.symbol("try"), env = env))
})

test_that("(import ...) loads module exports into environment", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)

  module_name <- paste0("math", sample.int(100000, 1))
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  module_file <- file.path(tmp_dir, paste0(module_name, ".rye"))
  writeLines(c(
    sprintf("(module %s", module_name),
    "  (export square inc)",
    "  (define square (lambda (x) (* x x)))",
    "  (define inc (lambda (x) (+ x 1))))"
  ), module_file)

  exprs <- engine$read(sprintf("(import %s)", module_name))
  engine$eval(exprs[[1]], env)

  exprs <- engine$read("(square 3)")
  expect_equal(engine$eval(exprs[[1]], env), 9)
})

test_that("(import ...) does not re-evaluate loaded modules", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)

  module_name <- paste0("counter", sample.int(100000, 1))
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  module_file <- file.path(tmp_dir, paste0(module_name, ".rye"))
  writeLines(c(
    sprintf("(module %s", module_name),
    "  (export tick)",
    "  (define counter 0)",
    "  (define tick (lambda () (begin (set! counter (+ counter 1)) counter))))"
  ), module_file)

  exprs <- engine$read(sprintf("(import %s)", module_name))
  engine$eval(exprs[[1]], env)

  exprs <- engine$read("(tick)")
  expect_equal(engine$eval(exprs[[1]], env), 1)
  expect_equal(engine$eval(exprs[[1]], env), 2)

  exprs <- engine$read(sprintf("(import %s)", module_name))
  engine$eval(exprs[[1]], env)
  expect_equal(engine$eval(engine$read("(tick)")[[1]], env), 3)
})

test_that("(import ...) errors on missing modules and exports", {
  engine <- new_engine(load_stdlib = TRUE)
  env <- engine_env(engine)

  missing_name <- paste0("missing", sample.int(100000, 1))
  exprs <- engine$read(sprintf("(import %s)", missing_name))
  expect_error(engine$eval(exprs[[1]], env), "Module not found")

  module_name <- paste0("bad", sample.int(100000, 1))
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  module_file <- file.path(tmp_dir, paste0(module_name, ".rye"))
  writeLines(c(
    sprintf("(module %s", module_name),
    "  (export missing-value))"
  ), module_file)

  exprs <- engine$read(sprintf("(import %s)", module_name))
  expect_error(engine$eval(exprs[[1]], env), "does not export")
})
