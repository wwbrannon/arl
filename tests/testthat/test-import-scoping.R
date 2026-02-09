# Import scoping, load (source-like), and run (isolated) tests.
# Written first (TDD); implementation follows.

test_that("imports are not visible in a different file (two load_file calls)", {
  # Each engine$load_file runs in its own scope; file B must not see file A's imports.
  engine <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  module_name <- paste0("mymod", sample.int(100000, 1))
  module_file <- file.path(tmp_dir, paste0(module_name, ".rye"))
  writeLines(c(
    sprintf("(module %s", module_name),
    "  (export myfn)",
    "  (define myfn (lambda (x) (* x 2))))"
  ), module_file)

  file_a <- file.path(tmp_dir, "file_a.rye")
  writeLines(c(
    sprintf("(import %s)", module_name),
    "(myfn 5)"
  ), file_a)
  file_b <- file.path(tmp_dir, "file_b.rye")
  writeLines("(myfn 5)", file_b)

  engine$load_file(file_a)
  expect_error(engine$load_file(file_b), regexp = "myfn|not found|object")
})

test_that("imports are visible in the same file", {
  engine <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  # Use stdlib 'list' so we don't need a temp module
  path <- file.path(tmp_dir, "single.rye")
  writeLines(c("(import list)", "(cadr (list 1 2 3))"), path)
  result <- engine$load_file(path)
  expect_equal(result, 2)
})

test_that("(load path) runs in caller env - definitions visible", {
  engine <- make_engine()
  env <- engine$env$env
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  defs_path <- file.path(tmp_dir, "defs.rye")
  writeLines("(define shared 99)", defs_path)
  main_code <- sprintf('(begin (load "%s") shared)', normalizePath(defs_path, winslash = "/"))
  exprs <- engine$read(main_code)
  result <- engine$eval_in_env(exprs[[1]], env)
  expect_equal(result, 99)
})

test_that("(load path) runs in caller env - imports visible to caller", {
  engine <- make_engine()
  env <- engine$env$env
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  with_import_path <- file.path(tmp_dir, "with_import.rye")
  writeLines(c("(import list)", "(define x (cadr (list 1 2 3)))"), with_import_path)
  main_code <- sprintf('(begin (load "%s") x)', normalizePath(with_import_path, winslash = "/"))
  exprs <- engine$read(main_code)
  result <- engine$eval_in_env(exprs[[1]], env)
  expect_equal(result, 2)
})

test_that("(run path) runs in child env - definitions not visible in caller", {
  engine <- make_engine()
  env <- engine$env$env
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  defs_path <- file.path(tmp_dir, "secret.rye")
  writeLines("(define secret 42)", defs_path)
  main_code <- sprintf('(begin (run "%s") secret)', normalizePath(defs_path, winslash = "/"))
  exprs <- engine$read(main_code)
  expect_error(engine$eval_in_env(exprs[[1]], env), regexp = "secret|not found|object")
})

test_that("(run path) runs in child env - imports not visible in caller", {
  # Use a custom module with a unique export; the engine env already has stdlib (e.g. cadr).
  engine <- make_engine()
  env <- engine$env$env
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  mod_name <- paste0("runmod", sample.int(100000, 1))
  mod_file <- file.path(tmp_dir, paste0(mod_name, ".rye"))
  writeLines(c(
    sprintf("(module %s", mod_name),
    "  (export uniquefn)",
    "  (define uniquefn (lambda (x) (+ x 1))))"
  ), mod_file)
  run_import_path <- file.path(tmp_dir, "run_import.rye")
  writeLines(sprintf("(import %s)", mod_name), run_import_path)
  main_code <- sprintf('(begin (run "%s") (uniquefn 1))', normalizePath(run_import_path, winslash = "/"))
  exprs <- engine$read(main_code)
  expect_error(engine$eval_in_env(exprs[[1]], env), regexp = "uniquefn|not found|object")
})

test_that("global module cache: same module loaded once per engine, shared across files", {
  # Module with side effect (counter); two files each import it and call tick.
  # With global cache, module runs once so counter is shared: file_a returns 1, file_b returns 2.
  engine <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  old_dir <- getwd()
  setwd(tmp_dir)
  on.exit({
    setwd(old_dir)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  mod_name <- paste0("cached", sample.int(100000, 1))
  mod_file <- file.path(tmp_dir, paste0(mod_name, ".rye"))
  writeLines(c(
    sprintf("(module %s", mod_name),
    "  (export tick)",
    "  (define counter 0)",
    "  (define tick (lambda () (begin (set! counter (+ counter 1)) counter))))"
  ), mod_file)

  file_a <- file.path(tmp_dir, "file_a.rye")
  writeLines(c(sprintf("(import %s)", mod_name), "(tick)"), file_a)
  file_b <- file.path(tmp_dir, "file_b.rye")
  writeLines(c(sprintf("(import %s)", mod_name), "(tick)"), file_b)

  expect_equal(engine$load_file(file_a), 1)
  expect_equal(engine$load_file(file_b), 2)
})
