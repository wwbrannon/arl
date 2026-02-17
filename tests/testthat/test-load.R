test_that("engine$load_file_in_env evaluates source into environment", {
  engine <- make_engine()
  env <- engine$get_env()

  path <- tempfile(fileext = ".arl")
  writeLines(c("(define foo 42)", "(define bar (+ foo 1))"), path)
  on.exit(unlink(path), add = TRUE)

  engine$load_file_in_env(path)

  expect_equal(get("foo", envir = env), 42)
  expect_equal(get("bar", envir = env), 43)
})

test_that("(load ...) evaluates file in current environment", {
  engine <- make_engine()
  env <- engine$get_env()

  path <- tempfile(fileext = ".arl")
  writeLines("(define foo 7)", path)
  on.exit(unlink(path), add = TRUE)

  path_for_arl <- normalizePath(path, winslash = "/", mustWork = FALSE)
  exprs <- engine$read(paste0("(load \"", path_for_arl, "\")"))
  engine$eval(exprs[[1]], env = env)

  expect_equal(get("foo", envir = env), 7)
})

test_that("(load path env) evaluates file in the provided environment", {
  engine <- make_engine()
  caller_env <- engine$get_env()
  target_env <- new.env(parent = caller_env)

  path <- tempfile(fileext = ".arl")
  writeLines("(define scoped-value 11)", path)
  on.exit(unlink(path), add = TRUE)

  path_for_arl <- normalizePath(path, winslash = "/", mustWork = FALSE)
  exprs <- engine$read(paste0("(load \"", path_for_arl, "\" target-env)"))
  caller_env$`target-env` <- target_env
  engine$eval(exprs[[1]], env = caller_env)

  expect_equal(get("scoped-value", envir = target_env), 11)
  expect_false(exists("scoped-value", envir = caller_env, inherits = FALSE))
})

test_that("(load ...) does not resolve stdlib by name", {
  engine <- make_engine()
  env <- engine$get_env()

  # load should NOT do stdlib lookup; that's import's job
  expect_error(
    engine$eval(engine$read("(load \"control\")")[[1]], env = env),
    "File not found"
  )
})

test_that("(load ...) re-evaluates on each call", {
  engine <- make_engine()
  env <- engine$get_env()

  path <- tempfile(fileext = ".arl")
  writeLines("(set! counter (+ counter 1))", path)
  on.exit(unlink(path), add = TRUE)

  # Initialize counter
  assign("counter", 0L, envir = env)

  path_for_arl <- normalizePath(path, winslash = "/", mustWork = FALSE)
  expr <- engine$read(paste0("(load \"", path_for_arl, "\")"))[[1]]

  engine$eval(expr, env = env)
  expect_equal(get("counter", envir = env), 1L)

  engine$eval(expr, env = env)
  expect_equal(get("counter", envir = env), 2L)
})

test_that("stdlib modules register macros", {
  engine <- make_engine()
  env <- engine$get_env()
  import_stdlib_modules(engine, c("control", "binding", "threading"))

  expect_true(engine_field(engine, "macro_expander")$is_macro(as.symbol("when"), env = env))
  expect_true(engine_field(engine, "macro_expander")$is_macro(as.symbol("unless"), env = env))
  expect_true(engine_field(engine, "macro_expander")$is_macro(as.symbol("let"), env = env))
  expect_true(engine_field(engine, "macro_expander")$is_macro(as.symbol("->"), env = env))
  expect_true(engine_field(engine, "macro_expander")$is_macro(as.symbol("try-catch"), env = env))
})

test_that("(import ...) loads module exports into environment", {
  engine <- make_engine()
  env <- engine$get_env()

  module_name <- paste0("math", sample.int(100000, 1))
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  module_file <- file.path(tmp_dir, paste0(module_name, ".arl"))
  writeLines(c(
    sprintf("(module %s", module_name),
    "  (export square inc)",
    "  (define square (lambda (x) (* x x)))",
    "  (define inc (lambda (x) (+ x 1))))"
  ), module_file)

  exprs <- engine$read(sprintf("(import %s :refer :all)", module_name))
  engine$eval(exprs[[1]], env = env)

  exprs <- engine$read("(square 3)")
  expect_equal(engine$eval(exprs[[1]], env = env), 9)
})

test_that("(import ...) does not re-evaluate loaded modules", {
  engine <- make_engine()
  env <- engine$get_env()

  module_name <- paste0("counter", sample.int(100000, 1))
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  module_file <- file.path(tmp_dir, paste0(module_name, ".arl"))
  writeLines(c(
    sprintf("(module %s", module_name),
    "  (export tick)",
    "  (define counter 0)",
    "  (define tick (lambda () (begin (set! counter (+ counter 1)) counter))))"
  ), module_file)

  exprs <- engine$read(sprintf("(import %s :refer :all)", module_name))
  engine$eval(exprs[[1]], env = env)

  exprs <- engine$read("(tick)")
  expect_equal(engine$eval(exprs[[1]], env = env), 1)
  expect_equal(engine$eval(exprs[[1]], env = env), 2)

  exprs <- engine$read(sprintf("(import %s :refer :all)", module_name))
  engine$eval(exprs[[1]], env = env)
  expect_equal(engine$eval(engine$read("(tick)")[[1]], env = env), 3)
})

test_that("(import ...) errors on missing modules and exports", {
  engine <- make_engine()
  env <- engine$get_env()

  missing_name <- paste0("missing", sample.int(100000, 1))
  exprs <- engine$read(sprintf("(import %s)", missing_name))
  expect_error(engine$eval(exprs[[1]], env = env), "Module not found")

  module_name <- paste0("bad", sample.int(100000, 1))
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  module_file <- file.path(tmp_dir, paste0(module_name, ".arl"))
  writeLines(c(
    sprintf("(module %s", module_name),
    "  (export missing-value))"
  ), module_file)

  exprs <- engine$read(sprintf("(import %s)", module_name))
  expect_error(engine$eval(exprs[[1]], env = env), "not defined or imported")
})

test_that("(import \"path\") loads module by path and attaches exports", {
  engine <- make_engine()
  env <- engine$get_env()

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  module_file <- file.path(tmp_dir, "pathmod.arl")
  writeLines(c(
    "(module pathmod",
    "  (export double)",
    "  (define double (lambda (x) (* x 2))))"
  ), module_file)

  exprs <- engine$read(sprintf('(import "%s" :refer :all)', module_file))
  engine$eval(exprs[[1]], env = env)

  exprs <- engine$read("(double 7)")
  expect_equal(engine$eval(exprs[[1]], env = env), 14)
})

test_that("second (import \"path\") does not reload module", {
  engine <- make_engine()
  env <- engine$get_env()

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  module_file <- file.path(tmp_dir, "countmod.arl")
  writeLines(c(
    "(module countmod",
    "  (export getn)",
    "  (define n 0)",
    "  (define getn (lambda () (begin (set! n (+ n 1)) n))))"
  ), module_file)

  path_abs <- normalizePath(module_file, winslash = "/", mustWork = TRUE)
  engine$eval(engine$read(sprintf('(import "%s" :refer :all)', path_abs))[[1]], env = env)
  expect_equal(engine$eval(engine$read("(getn)")[[1]], env = env), 1)

  engine$eval(engine$read(sprintf('(import "%s" :refer :all)', path_abs))[[1]], env = env)
  expect_equal(engine$eval(engine$read("(getn)")[[1]], env = env), 2)
})

test_that("relative import paths resolve from importing file's directory", {
  engine <- make_engine()
  env <- engine$get_env()

  # Create nested directory structure
  tmp_dir <- tempfile()
  dir.create(file.path(tmp_dir, "lib"), recursive = TRUE)

  # lib/helper.arl - a module in a subdirectory
  writeLines(c(
    "(module helper",
    "  (export helper-fn)",
    "  (define helper-fn (lambda () 99)))"
  ), file.path(tmp_dir, "lib", "helper.arl"))

  # lib/main.arl - imports sibling via relative path
  writeLines(c(
    '(import "helper.arl" :refer :all)',
    "(define main-val (helper-fn))"
  ), file.path(tmp_dir, "lib", "main.arl"))

  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # CWD is NOT the lib directory — relative path must resolve from file, not CWD
  main_path <- normalizePath(file.path(tmp_dir, "lib", "main.arl"),
                             winslash = "/", mustWork = TRUE)
  engine$load_file_in_env(main_path)
  expect_equal(get("main-val", envir = env), 99)
})

test_that("(import symbol) is module name, (import \"string\") is path", {
  engine <- make_engine()
  env <- engine$get_env()

  expect_silent(engine$eval(engine$read("(import control :refer :all)")[[1]], env = env))
  expect_true(engine_field(engine, "macro_expander")$is_macro(as.symbol("when"), env = env))

  missing_path <- tempfile(fileext = ".arl")
  expect_false(file.exists(missing_path))
  expect_error(
    engine$eval(engine$read(sprintf('(import "%s")', missing_path))[[1]], env = env),
    "Module not found"
  )
})
