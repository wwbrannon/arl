test_that("rye_load_file evaluates source into environment", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

  path <- tempfile(fileext = ".rye")
  writeLines(c("(define foo 42)", "(define bar (+ foo 1))"), path)
  on.exit(unlink(path), add = TRUE)

  rye_load_file(path, env)

  expect_equal(get("foo", envir = env), 42)
  expect_equal(get("bar", envir = env), 43)
})

test_that("(load ...) evaluates file in current environment", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

  path <- tempfile(fileext = ".rye")
  writeLines("(define foo 7)", path)
  on.exit(unlink(path), add = TRUE)

  path_for_rye <- normalizePath(path, winslash = "/", mustWork = FALSE)
  exprs <- rye_read(paste0("(load \"", path_for_rye, "\")"))
  rye_eval(exprs[[1]], env)

  expect_equal(get("foo", envir = env), 7)
})

test_that("(load ...) resolves stdlib entries by name", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

  exprs <- rye_read("(load \"control\")")

  expect_silent(rye_eval(exprs[[1]], env))
  expect_true(rye:::is_macro(as.symbol("when")))
  expect_true(rye:::is_macro(as.symbol("unless")))
})

test_that("rye_load_stdlib registers stdlib macros", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

  expect_true(rye:::is_macro(as.symbol("when")))
  expect_true(rye:::is_macro(as.symbol("unless")))
  expect_true(rye:::is_macro(as.symbol("let")))
  expect_true(rye:::is_macro(as.symbol("->")))
  expect_true(rye:::is_macro(as.symbol("try")))
})

test_that("(import ...) loads module exports into environment", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

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

  exprs <- rye_read(sprintf("(import %s)", module_name))
  rye_eval(exprs[[1]], env)

  exprs <- rye_read("(square 3)")
  expect_equal(rye_eval(exprs[[1]], env), 9)
})

test_that("(import ...) does not re-evaluate loaded modules", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

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

  exprs <- rye_read(sprintf("(import %s)", module_name))
  rye_eval(exprs[[1]], env)

  exprs <- rye_read("(tick)")
  expect_equal(rye_eval(exprs[[1]], env), 1)
  expect_equal(rye_eval(exprs[[1]], env), 2)

  exprs <- rye_read(sprintf("(import %s)", module_name))
  rye_eval(exprs[[1]], env)
  expect_equal(rye_eval(rye_read("(tick)")[[1]], env), 3)
})

test_that("(import ...) errors on missing modules and exports", {
  env <- new.env(parent = baseenv())
  rye_load_stdlib(env)

  missing_name <- paste0("missing", sample.int(100000, 1))
  exprs <- rye_read(sprintf("(import %s)", missing_name))
  expect_error(rye_eval(exprs[[1]], env), "Module not found")

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

  exprs <- rye_read(sprintf("(import %s)", module_name))
  expect_error(rye_eval(exprs[[1]], env), "does not export")
})
