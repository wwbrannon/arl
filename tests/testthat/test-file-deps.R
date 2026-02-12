# Tests for FileDeps (R/file-deps.R): module/import extraction and load order

# Find package root (DESCRIPTION lives there). In R CMD check, cwd is pkg/tests/testthat.
pkg_root <- function() {
  wd <- getwd()
  for (root in c(wd, file.path(wd, ".."), file.path(wd, "..", ".."))) {
    if (file.exists(file.path(root, "DESCRIPTION")))
      return(normalizePath(root, winslash = "/"))
  }
  wd
}

test_that("FileDeps is loadable and returns structure", {
  stdlib_dir <- system.file("arl", package = "arl")
  skip_if_not(dir.exists(stdlib_dir))
  d <- arl:::FileDeps$new(dir = stdlib_dir)
  expect_true(is.environment(d))
  expect_true(is.function(d$get_load_order))
  expect_true(is.function(d$get_modules))
  expect_true(is.function(d$get_graph))
})

test_that("stdlib modules are discovered and have valid topsort", {
  pkg <- pkg_root()
  stdlib_dir <- file.path(pkg, "inst", "arl")
  skip_if_not(dir.exists(stdlib_dir))
  d <- arl:::FileDeps$new(dir = stdlib_dir)
  modules <- d$get_modules()
  load_order <- d$get_load_order()
  expect_type(modules, "list")
  expect_type(load_order, "character")
  expect_true(length(load_order) >= 1)
  expect_true("core" %in% load_order)
  expect_true("list" %in% load_order)
  expect_true("_r" %in% load_order)
  g <- d$get_graph()
  pos <- setNames(seq_along(load_order), load_order)
  for (e in g$edges) {
    expect_true(
      pos[[e$to]] < pos[[e$from]],
      info = sprintf("Dependency %s -> %s: %s should load before %s",
        e$from, e$to, e$to, e$from)
    )
  }
})

test_that("no cycle in stdlib dependency graph", {
  pkg <- pkg_root()
  stdlib_dir <- file.path(pkg, "inst", "arl")
  skip_if_not(dir.exists(stdlib_dir))
  d <- arl:::FileDeps$new(dir = stdlib_dir)
  load_order <- d$get_load_order()
  g <- d$get_graph()
  expect_length(load_order, length(g$vertices))
})

# Error handling tests
test_that("FileDeps errors on non-existent directory", {
  expect_error(
    arl:::FileDeps$new(dir = "/nonexistent/path/xyz"),
    "Directory not found"
  )
})

test_that("FileDeps handles empty directory", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Empty directory causes vertices to be NULL, which errors in topsort
  expect_error(
    arl:::FileDeps$new(dir = tmp_dir),
    "vertices must be a character vector"
  )
})

test_that("FileDeps skips files without module declaration", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # File without (module ...) form
  writeLines(c(
    "(define foo 42)",
    "(define bar 'test')"
  ), file.path(tmp_dir, "no-module.arl"))

  # File without module causes empty vertices, which errors
  expect_error(
    arl:::FileDeps$new(dir = tmp_dir),
    "vertices must be a character vector"
  )
})

test_that("FileDeps handles malformed module declarations", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Malformed module (missing closing paren)
  writeLines("(module incomplete", file.path(tmp_dir, "bad.arl"))

  # Should not crash, just skip the malformed file
  d <- arl:::FileDeps$new(dir = tmp_dir)
  modules <- d$get_modules()
  expect_true(is.list(modules))
})

test_that("FileDeps excludes specified files", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(module test1 (export foo))", file.path(tmp_dir, "test1.arl"))
  writeLines("(module test2 (export bar))", file.path(tmp_dir, "test2.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir, exclude = c("test2.arl"))
  modules <- d$get_modules()

  expect_true("test1" %in% names(modules))
  expect_false("test2" %in% names(modules))
})

test_that("FileDeps handles custom pattern", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(module test1 (export foo))", file.path(tmp_dir, "test1.arl"))
  writeLines("(module test2 (export bar))", file.path(tmp_dir, "test2.lisp"))

  # Default pattern only matches .arl
  d <- arl:::FileDeps$new(dir = tmp_dir)
  expect_true("test1" %in% names(d$get_modules()))
  expect_false("test2" %in% names(d$get_modules()))

  # Custom pattern matches .lisp
  d2 <- arl:::FileDeps$new(dir = tmp_dir, pattern = "\\.lisp$")
  expect_false("test1" %in% names(d2$get_modules()))
  expect_true("test2" %in% names(d2$get_modules()))
})

# Parsing edge cases tests
test_that("FileDeps parses quoted module names", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines('(module "quoted-name" (export foo))', file.path(tmp_dir, "quoted.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  modules <- d$get_modules()
  expect_true("quoted-name" %in% names(modules))
})

test_that("FileDeps parses quoted export names", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines('(module test (export "foo-bar" "baz-qux"))', file.path(tmp_dir, "test.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  modules <- d$get_modules()
  exports <- modules$test$exports

  expect_true("foo-bar" %in% exports)
  expect_true("baz-qux" %in% exports)
})

test_that("FileDeps handles modules with no exports", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(module no-exports)", file.path(tmp_dir, "empty.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  modules <- d$get_modules()

  expect_true("no-exports" %in% names(modules))
  expect_equal(modules$`no-exports`$exports, character(0))
})

test_that("FileDeps handles comments in module forms", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    "; This is a comment",
    "(module test ; inline comment",
    "  (export foo bar) ; export comment",
    "  )"
  ), file.path(tmp_dir, "comments.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  modules <- d$get_modules()

  expect_true("test" %in% names(modules))
  expect_equal(modules$test$exports, c("foo", "bar"))
})

test_that("FileDeps handles string content with parentheses", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    '(module test (export foo))',
    '(define str "This has (parens) in it")'
  ), file.path(tmp_dir, "strings.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  modules <- d$get_modules()

  expect_true("test" %in% names(modules))
})

test_that("FileDeps handles nested parentheses in exports", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(module test (export (foo bar baz)))", file.path(tmp_dir, "nested.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  modules <- d$get_modules()

  # Should parse the export form correctly
  expect_true("test" %in% names(modules))
})

test_that("FileDeps extracts multiple imports", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    "(module test (export result))",
    "(import list)",
    "(import math)",
    "(import core)"
  ), file.path(tmp_dir, "multi-import.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  modules <- d$get_modules()
  imports <- modules$test$imports

  expect_true("list" %in% imports)
  expect_true("math" %in% imports)
  expect_true("core" %in% imports)
})

test_that("FileDeps handles quoted import names", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    '(module test (export foo))',
    '(import "some-module")',
    '(import "another-module")'
  ), file.path(tmp_dir, "quoted-imports.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  modules <- d$get_modules()
  imports <- modules$test$imports

  expect_true("some-module" %in% imports)
  expect_true("another-module" %in% imports)
})

# Dependency graph tests
test_that("FileDeps builds correct dependency edges", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(module base (export x))", file.path(tmp_dir, "base.arl"))
  writeLines(c(
    "(module derived (export y))",
    "(import base)"
  ), file.path(tmp_dir, "derived.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  g <- d$get_graph()

  # Should have edge from derived to base
  edge_found <- FALSE
  for (e in g$edges) {
    if (e$from == "derived" && e$to == "base") {
      edge_found <- TRUE
      break
    }
  }
  expect_true(edge_found)
})

test_that("FileDeps detects cycles from self-imports", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    "(module self-ref (export foo))",
    "(import self-ref)"  # Self-import creates cycle
  ), file.path(tmp_dir, "self.arl"))

  # Self-import creates a cycle, which topsort detects
  expect_error(
    arl:::FileDeps$new(dir = tmp_dir),
    "Cycle detected in dependency graph"
  )
})

test_that("FileDeps ignores imports of non-existent modules", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    "(module test (export foo))",
    "(import nonexistent)",  # Module not in directory
    "(import also-missing)"
  ), file.path(tmp_dir, "test.arl"))

  d <- arl:::FileDeps$new(dir = tmp_dir)
  g <- d$get_graph()

  # Should not create edges for non-existent modules
  expect_equal(length(g$edges), 0)
})
