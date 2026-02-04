# Tests for tools/stdlib-deps.R: module/import extraction and load order

# Find package root (DESCRIPTION lives there). In R CMD check, cwd is pkg/tests/testthat.
pkg_root <- function() {
  wd <- getwd()
  for (root in c(wd, file.path(wd, ".."), file.path(wd, "..", ".."))) {
    if (file.exists(file.path(root, "DESCRIPTION")))
      return(normalizePath(root, winslash = "/"))
  }
  wd
}

test_that("stdlib-deps script is loadable and returns structure", {
  skip_if_not(file.exists(file.path(pkg_root(), "tools", "stdlib-deps.R")))
  script_path <- file.path(pkg_root(), "tools", "stdlib-deps.R")
  expect_true(file.exists(script_path))
  sys_path <- Sys.getenv("RYE_PKG_ROOT", unset = NA)
  if (!is.na(sys_path)) on.exit(Sys.setenv(RYE_PKG_ROOT = sys_path), add = TRUE)
  Sys.setenv(RYE_PKG_ROOT = pkg_root())
  on.exit(Sys.unsetenv("RYE_PKG_ROOT"), add = TRUE)
  env <- new.env()
  source(script_path, local = env)
  expect_true(exists("main", envir = env, mode = "function"))
  expect_true(exists("extract_module", envir = env, mode = "function"))
  expect_true(exists("extract_imports", envir = env, mode = "function"))
  expect_true(exists("topsort_deps", envir = env, mode = "function"))
  expect_true(exists("build_graph", envir = env, mode = "function"))
})

test_that("stdlib modules are discovered and have valid topsort", {
  pkg <- pkg_root()
  stdlib_dir <- file.path(pkg, "inst", "rye")
  skip_if_not(dir.exists(stdlib_dir))
  script_path <- file.path(pkg, "tools", "stdlib-deps.R")
  skip_if_not(file.exists(script_path))
  sys_path <- Sys.getenv("RYE_PKG_ROOT", unset = NA)
  if (!is.na(sys_path)) on.exit(Sys.setenv(RYE_PKG_ROOT = sys_path), add = TRUE)
  Sys.setenv(RYE_PKG_ROOT = pkg)
  on.exit(Sys.unsetenv("RYE_PKG_ROOT"), add = TRUE)
  env <- new.env()
  source(script_path, local = env)
  result <- env$main(stdlib_dir = stdlib_dir, check_undeclared = FALSE)
  expect_type(result$modules, "list")
  expect_type(result$load_order, "character")
  expect_true(length(result$load_order) >= 1)
  # Loader excludes _stdlib_loader.rye; we expect at least core, list, etc.
  expect_true("core" %in% result$load_order)
  expect_true("list" %in% result$load_order)
  expect_true("_aliases" %in% result$load_order)
  # Topological order: for every edge (from -> to), to must appear before from
  g <- result$graph
  pos <- setNames(seq_along(result$load_order), result$load_order)
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
  script_path <- file.path(pkg, "tools", "stdlib-deps.R")
  skip_if_not(file.exists(script_path))
  sys_path <- Sys.getenv("RYE_PKG_ROOT", unset = NA)
  if (!is.na(sys_path)) on.exit(Sys.setenv(RYE_PKG_ROOT = sys_path), add = TRUE)
  Sys.setenv(RYE_PKG_ROOT = pkg)
  on.exit(Sys.unsetenv("RYE_PKG_ROOT"), add = TRUE)
  env <- new.env()
  source(script_path, local = env)
  result <- env$main(stdlib_dir = stdlib_dir, check_undeclared = FALSE)
  expect_length(result$load_order, length(result$graph$vertices))
})
