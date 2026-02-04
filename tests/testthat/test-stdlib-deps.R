# Tests for StdlibDeps (R/stdlib-deps.R): module/import extraction and load order

# Find package root (DESCRIPTION lives there). In R CMD check, cwd is pkg/tests/testthat.
pkg_root <- function() {
  wd <- getwd()
  for (root in c(wd, file.path(wd, ".."), file.path(wd, "..", ".."))) {
    if (file.exists(file.path(root, "DESCRIPTION")))
      return(normalizePath(root, winslash = "/"))
  }
  wd
}

test_that("StdlibDeps is loadable and returns structure", {
  stdlib_dir <- system.file("rye", package = "rye")
  skip_if_not(dir.exists(stdlib_dir))
  d <- rye:::StdlibDeps$new(stdlib_dir = stdlib_dir)
  expect_true(is.environment(d))
  expect_true(is.function(d$get_load_order))
  expect_true(is.function(d$get_modules))
  expect_true(is.function(d$get_graph))
  expect_true(is.function(d$check_undeclared))
})

test_that("stdlib modules are discovered and have valid topsort", {
  pkg <- pkg_root()
  stdlib_dir <- file.path(pkg, "inst", "rye")
  skip_if_not(dir.exists(stdlib_dir))
  d <- rye:::StdlibDeps$new(stdlib_dir = stdlib_dir)
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
  stdlib_dir <- file.path(pkg, "inst", "rye")
  skip_if_not(dir.exists(stdlib_dir))
  d <- rye:::StdlibDeps$new(stdlib_dir = stdlib_dir)
  load_order <- d$get_load_order()
  g <- d$get_graph()
  expect_length(load_order, length(g$vertices))
})
