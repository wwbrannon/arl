# Tests for nameless module forms (name derived from file path or expected_module_name)

write_module <- function(path, name, body) {
  lines <- c(
    sprintf("(module %s", name),
    body,
    ")"
  )
  writeLines(lines, path)
}

write_nameless_module <- function(path, body) {
  lines <- c(
    "(module",
    body,
    ")"
  )
  writeLines(lines, path)
}

test_that("file-backed nameless module derives name from path", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_nameless_module(file.path(tmp_dir, "mymod.arl"), c(
    "  (export x)",
    "  (define x 42)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "mymod.arl"))
  eng$eval_text("(import mymod)")
  expect_equal(eng$eval_text("x"), 42)
})

test_that("named module in eval_text works as before", {
  eng <- make_engine()
  eng$eval_text("(module test-named (export x) (define x 1))")
  eng$eval_text("(import test-named)")
  expect_equal(eng$eval_text("x"), 1)
})

test_that("nameless module in eval_text with no file context errors", {
  eng <- make_engine()
  expect_error(
    eng$eval_text("(module (export x) (define x 1))"),
    "nameless module"
  )
})

test_that("nameless module with hierarchical file path derives correct name", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  sub_dir <- file.path(tmp_dir, "sub")
  dir.create(sub_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_nameless_module(file.path(sub_dir, "deep.arl"), c(
    "  (export y)",
    "  (define y 99)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(sub_dir, "deep.arl"))
  # Name derived from basename "deep"
  eng$eval_text("(import deep)")
  expect_equal(eng$eval_text("y"), 99)
})

test_that("nameless module with export-all works", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_nameless_module(file.path(tmp_dir, "ea-nameless.arl"), c(
    "  (export-all)",
    "  (define a 10)",
    "  (define b 20)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "ea-nameless.arl"))
  eng$eval_text("(import ea-nameless)")
  expect_equal(eng$eval_text("a"), 10)
  expect_equal(eng$eval_text("b"), 20)
})
