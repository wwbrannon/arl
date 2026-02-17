# Tests for define/set!/doc! interactions with proxy-imported active bindings
# Covers gaps in the reference-based proxy import system where user code
# overwrites or mutates names that came from imports.

# Helper: create a temp module file with known exports
make_binding_module <- function(name = "bindmod", body = NULL) {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE)
  if (is.null(body)) {
    body <- c(
      sprintf("(module %s", name),
      "  (export x add1)",
      "  (define x 42)",
      "  (define add1 (lambda (n) (+ n 1))))"
    )
  }
  module_file <- file.path(tmp_dir, paste0(name, ".arl"))
  writeLines(body, module_file)
  list(dir = tmp_dir, file = module_file, name = name)
}

# --- Gap 1: define overwriting a proxy-imported binding ---

test_that("define shadows a proxy-imported value binding", {
  m <- make_binding_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import bindmod)")
  # Verify the import works

  expect_equal(engine$eval_text("x"), 42L)

  # define should shadow the active binding with a local binding
  engine$eval_text("(define x 99)")
  expect_equal(engine$eval_text("x"), 99L)
})

test_that("define shadows a proxy-imported function binding", {
  m <- make_binding_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import bindmod)")
  expect_equal(engine$eval_text("(add1 10)"), 11L)

  # Replace the imported function with a different one
  engine$eval_text("(define add1 (lambda (n) (+ n 100)))")
  expect_equal(engine$eval_text("(add1 10)"), 110L)
})

test_that("define on imported name does not mutate the module", {
  m <- make_binding_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import bindmod)")
  engine$eval_text("(define x 99)")

  # The module's original binding should be untouched
  arl_env <- arl:::Env$new(engine$get_env())
  registry <- arl_env$module_registry
  entry <- registry$get("bindmod")
  expect_equal(get("x", envir = entry$env, inherits = FALSE), 42L)
})

# --- Gap 2: set! on a proxy-imported binding (local shadowing) ---

test_that("set! on a proxy-imported binding creates a local shadow", {
  m <- make_binding_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import bindmod)")
  expect_equal(engine$eval_text("x"), 42L)

  # set! should create a local shadow, not mutate the proxy
  engine$eval_text("(set! x 999)")
  expect_equal(engine$eval_text("x"), 999L)
})

test_that("set! on a proxy-imported binding does not mutate the module", {
  m <- make_binding_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import bindmod)")
  engine$eval_text("(set! x 999)")

  # The module's original binding should be untouched
  arl_env <- arl:::Env$new(engine$get_env())
  registry <- arl_env$module_registry
  entry <- registry$get("bindmod")
  expect_equal(get("x", envir = entry$env, inherits = FALSE), 42L)
})

test_that("set! local shadow is visible on subsequent reads", {
  m <- make_binding_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import bindmod)")
  engine$eval_text("(set! x 1)")
  engine$eval_text("(set! x (+ x 1))")
  expect_equal(engine$eval_text("x"), 2L)
})

# --- Gap 3: set! on a squash-mode active binding ---

test_that("set! on a squash-mode (prelude) binding replaces it in place", {
  engine <- make_engine()

  # map is a prelude import (squash mode) — it should be accessible
  expect_true(is.function(engine$eval_text("map")))

  # set! should replace the squash binding with a regular value
  engine$eval_text("(set! map 123)")
  expect_equal(engine$eval_text("map"), 123L)
})

test_that("set! on a squash-mode binding allows subsequent set!", {
  engine <- make_engine()

  # filter is a prelude import (squash mode)
  expect_true(is.function(engine$eval_text("filter")))

  # Replace it, then set! again — should work since it's now a regular binding
  engine$eval_text("(set! filter 1)")
  engine$eval_text("(set! filter 2)")
  expect_equal(engine$eval_text("filter"), 2L)
})

# --- Gap 4: doc! interaction with active bindings ---

test_that("doc! on a proxy-imported function works", {
  m <- make_binding_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import bindmod)")
  expect_equal(engine$eval_text("(add1 10)"), 11L)

  # doc! should remove the active binding, attach docs, and assign the value
  engine$eval_text('(doc! add1 "Adds one to its argument")')

  # Function should still work
  expect_equal(engine$eval_text("(add1 10)"), 11L)

  # Documentation should be retrievable
  expect_equal(engine$eval_text("(doc add1)"), "Adds one to its argument")
})

test_that("doc! on a proxy-imported function does not mutate the module", {
  m <- make_binding_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import bindmod)")
  engine$eval_text('(doc! add1 "docs")')

  # The module's original function should have no documentation
  arl_env <- arl:::Env$new(engine$get_env())
  registry <- arl_env$module_registry
  entry <- registry$get("bindmod")
  original_fn <- get("add1", envir = entry$env, inherits = FALSE)
  expect_null(attr(original_fn, "arl_doc", exact = TRUE))
})
