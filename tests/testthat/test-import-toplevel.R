# Tests for top-level-only import enforcement
# import should only be allowed at the top level of a program or module body,
# not nested inside lambda, if, while, define value, etc.

test_that("import at top level works", {
  engine <- make_engine()

  # Top-level import should succeed (stdlib modules are available)
  expect_no_error(engine$eval_text("(import list)"))
})

test_that("import inside module body works", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c(
    "(module toplevel-import-test",
    "  (export x)",
    "  (import list)",
    "  (define x 42))"
  ), file.path(temp_dir, "toplevel-import-test.arl"))

  engine <- make_engine()
  expect_no_error(engine$eval_text("(import toplevel-import-test)"))
})

test_that("import inside lambda is a compile error", {
  engine <- make_engine()

  expect_error(
    engine$eval_text("(define f (lambda () (import list)))"),
    "import.*top.level|top.level.*import"
  )
})

test_that("import inside if condition is a compile error", {
  engine <- make_engine()

  expect_error(
    engine$eval_text("(if (import list) 1 2)"),
    "import.*top.level|top.level.*import"
  )
})

test_that("import inside if consequent is a compile error", {
  engine <- make_engine()

  expect_error(
    engine$eval_text("(if #t (import list) 2)"),
    "import.*top.level|top.level.*import"
  )
})

test_that("import inside if alternate is a compile error", {
  engine <- make_engine()

  expect_error(
    engine$eval_text("(if #t 1 (import list))"),
    "import.*top.level|top.level.*import"
  )
})

test_that("import inside while body is a compile error", {
  engine <- make_engine()

  expect_error(
    engine$eval_text("(while #f (import list))"),
    "import.*top.level|top.level.*import"
  )
})

test_that("import inside define value position is a compile error", {
  engine <- make_engine()

  expect_error(
    engine$eval_text("(define x (begin (import list) 1))"),
    "import.*top.level|top.level.*import"
  )
})

test_that("import inside nested begin (inside lambda) is a compile error", {
  engine <- make_engine()

  expect_error(
    engine$eval_text("(define f (lambda () (begin (import list) 1)))"),
    "import.*top.level|top.level.*import"
  )
})

test_that("import inside top-level begin is allowed", {
  engine <- make_engine()

  # A top-level begin is just sequencing, import should be fine
  expect_no_error(engine$eval_text("(begin (import list))"))
})

test_that("import inside set! value is a compile error", {
  engine <- make_engine()

  expect_error(
    engine$eval_text("(define x 1) (set! x (import list))"),
    "import.*top.level|top.level.*import"
  )
})
