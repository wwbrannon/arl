# Tests for assignment edge cases: destructuring set!, locked bindings,
# and parity between fast path (__set_impl) and slow path (Env$assign_existing).
#
# These tests lock down behavior before deduplicating the assignment code paths.

# Helper: create a temp module with locked bindings for testing
make_locked_module <- function(name = "lockmod", exports = c("a", "b", "c")) {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE)
  export_clause <- paste0("(export ", paste(exports, collapse = " "), ")")
  defines <- paste0("  (define ", exports, " ", seq_along(exports), ")")
  body <- c(
    sprintf("(module %s", name),
    paste0("  ", export_clause),
    defines,
    ")"
  )
  module_file <- file.path(tmp_dir, paste0(name, ".arl"))
  writeLines(body, module_file)
  list(dir = tmp_dir, file = module_file, name = name)
}


# --- Destructuring set! (no existing coverage) ---

test_that("basic destructuring set! assigns multiple variables", {
  engine <- make_engine()
  engine$eval_text("(define a 0)")
  engine$eval_text("(define b 0)")
  engine$eval_text("(set! (a b) (list 1 2))")
  expect_equal(engine$eval_text("a"), 1L)
  expect_equal(engine$eval_text("b"), 2L)
})

test_that("destructuring set! with rest pattern", {
  engine <- make_engine()
  engine$eval_text("(define a 0)")
  engine$eval_text("(define b 0)")
  engine$eval_text("(define rest '())")
  engine$eval_text("(set! (a b . rest) (list 1 2 3 4))")
  expect_equal(engine$eval_text("a"), 1L)
  expect_equal(engine$eval_text("b"), 2L)
  expect_equal(engine$eval_text("rest"), list(3L, 4L))
})

test_that("destructuring set! errors on undefined variable", {
  engine <- make_engine()
  engine$eval_text("(define a 0)")
  # 'nonexistent' is not defined, so set! should error
  expect_error(
    engine$eval_text("(set! (a nonexistent) (list 1 2))"),
    "not defined"
  )
})

test_that("destructuring set! on proxy-imported binding creates local shadow", {
  m <- make_locked_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import lockmod :refer :all)")
  expect_equal(engine$eval_text("a"), 1L)
  expect_equal(engine$eval_text("b"), 2L)

  # Destructuring set! should shadow locally, not mutate the module
  engine$eval_text("(define x 0)")
  engine$eval_text("(set! (x a) (list 99 100))")
  expect_equal(engine$eval_text("x"), 99L)
  expect_equal(engine$eval_text("a"), 100L)

  # Module's original binding should be untouched
  arl_env <- arl:::Env$new(engine$get_env())
  registry <- arl_env$module_registry
  entry <- registry$get("lockmod")
  expect_equal(get("a", envir = entry$env, inherits = FALSE), 1L)
})

test_that("destructuring set! on locked binding succeeds", {
  # This exercises the bug #2 path: destructuring set! goes through
  # Env$assign_existing which does NOT handle locked bindings.
  # After the fix, this should pass.
  engine <- make_engine()

  # Create a binding and lock it at the R level
  eng_env <- engine$get_env()
  assign("locked_a", 10L, envir = eng_env)
  assign("locked_b", 20L, envir = eng_env)
  lockBinding(as.symbol("locked_a"), eng_env)
  lockBinding(as.symbol("locked_b"), eng_env)

  # Destructuring set! should unlock, assign, and re-lock
  engine$eval_text("(set! (locked_a locked_b) (list 100 200))")
  expect_equal(engine$eval_text("locked_a"), 100L)
  expect_equal(engine$eval_text("locked_b"), 200L)
})


# --- Locked binding interactions (no existing coverage) ---

test_that("simple set! on a locked binding succeeds", {
  engine <- make_engine()

  eng_env <- engine$get_env()
  assign("locked_var", 42L, envir = eng_env)
  lockBinding(as.symbol("locked_var"), eng_env)

  # The fast path (__set_impl) handles locked bindings correctly
  engine$eval_text("(set! locked_var 999)")
  expect_equal(engine$eval_text("locked_var"), 999L)

  # Binding should still be locked after mutation
  expect_true(bindingIsLocked("locked_var", eng_env))
})

test_that("define over a locked binding replaces it", {
  engine <- make_engine()

  eng_env <- engine$get_env()
  assign("locked_def", 42L, envir = eng_env)
  lockBinding(as.symbol("locked_def"), eng_env)

  # define creates a new binding in the current env; since the binding
  # is locked but not an active binding, define should still work
  # (define goes through __assign_pattern → direct assign in env)
  engine$eval_text("(define locked_def 999)")
  expect_equal(engine$eval_text("locked_def"), 999L)
})


# --- Parity tests: fast path vs slow path ---

test_that("simple set! and destructuring set! produce identical results on regular bindings", {
  # Fast path: simple set!
  eng1 <- make_engine()
  eng1$eval_text("(define x 0)")
  eng1$eval_text("(set! x 42)")

  # Slow path: destructuring set! with single element
  eng2 <- make_engine()
  eng2$eval_text("(define x 0)")
  eng2$eval_text("(set! (x) (list 42))")

  expect_equal(eng1$eval_text("x"), eng2$eval_text("x"))
})

test_that("simple set! and destructuring set! produce identical results on proxy bindings", {
  m <- make_locked_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  # Fast path
  eng1 <- make_engine()
  old_wd1 <- setwd(m$dir)
  on.exit(setwd(old_wd1), add = TRUE)
  eng1$eval_text("(import lockmod :refer :all)")
  eng1$eval_text("(set! a 99)")

  # Slow path (destructuring)
  eng2 <- make_engine()
  eng2$eval_text("(import lockmod :refer :all)")
  eng2$eval_text("(set! (a) (list 99))")

  expect_equal(eng1$eval_text("a"), eng2$eval_text("a"))

  # Both should have left the module untouched
  for (eng in list(eng1, eng2)) {
    arl_env <- arl:::Env$new(eng$get_env())
    registry <- arl_env$module_registry
    entry <- registry$get("lockmod")
    expect_equal(get("a", envir = entry$env, inherits = FALSE), 1L)
  }
})

test_that("simple set! and destructuring set! produce identical results on locked bindings", {
  # Fast path
  eng1 <- make_engine()
  env1 <- eng1$get_env()
  assign("lk", 10L, envir = env1)
  lockBinding(as.symbol("lk"), env1)
  eng1$eval_text("(set! lk 99)")

  # Slow path (destructuring)
  eng2 <- make_engine()
  env2 <- eng2$get_env()
  assign("lk", 10L, envir = env2)
  lockBinding(as.symbol("lk"), env2)
  eng2$eval_text("(set! (lk) (list 99))")

  expect_equal(eng1$eval_text("lk"), eng2$eval_text("lk"))
})

test_that("simple set! and destructuring set! produce identical results on squash bindings", {
  # Fast path: set! on a prelude squash binding
  eng1 <- make_engine()
  eng1$eval_text("(set! map 123)")

  # Slow path: destructuring set! on a prelude squash binding
  eng2 <- make_engine()
  eng2$eval_text("(set! (map) (list 123))")

  expect_equal(eng1$eval_text("map"), eng2$eval_text("map"))
})
