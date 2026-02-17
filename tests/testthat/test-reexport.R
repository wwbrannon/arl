# Tests for re-export functionality

write_module <- function(path, name, body) {
  lines <- c(
    sprintf("(module %s", name),
    body,
    ")"
  )
  writeLines(lines, path)
}

test_that("explicit re-export: imported symbol available to consumers", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Module B exports foo
  write_module(file.path(tmp_dir, "b.arl"), "b", c(
    "  (export foo bar)",
    "  (define foo 42)",
    "  (define bar 99)"
  ))

  # Module A imports from B, re-exports foo along with own binding
  write_module(file.path(tmp_dir, "a.arl"), "a", c(
    "  (export foo baz)",
    '  (import "b.arl")',
    "  (define baz 7)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "a.arl"))

  # Module C imports from A
  eng$eval_text('(import a)')
  expect_equal(eng$eval_text("foo"), 42)
  expect_equal(eng$eval_text("baz"), 7)
})

test_that("export-all :re-export includes imported symbols", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "provider.arl"), "provider", c(
    "  (export val)",
    "  (define val 100)"
  ))

  # Facade module re-exports everything
  write_module(file.path(tmp_dir, "facade.arl"), "facade", c(
    "  (export-all :re-export)",
    '  (import "provider.arl")',
    "  (define own-val 200)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "facade.arl"))

  entry <- engine_field(eng, "env")$module_registry$get("facade")
  expect_true("val" %in% entry$exports)
  expect_true("own-val" %in% entry$exports)

  eng$eval_text('(import facade)')
  expect_equal(eng$eval_text("val"), 100)
  expect_equal(eng$eval_text("own-val"), 200)
})

test_that("export-all without :re-export excludes imports (unchanged behavior)", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "src.arl"), "src", c(
    "  (export x)",
    "  (define x 1)"
  ))

  write_module(file.path(tmp_dir, "consumer.arl"), "consumer", c(
    "  (export-all)",
    '  (import "src.arl")',
    "  (define own 2)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "consumer.arl"))

  entry <- engine_field(eng, "env")$module_registry$get("consumer")
  expect_true("own" %in% entry$exports)
  expect_false("x" %in% entry$exports)
})

test_that("re-export with :only modifier", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "b.arl"), "b", c(
    "  (export x y)",
    "  (define x 10)",
    "  (define y 20)"
  ))

  write_module(file.path(tmp_dir, "a.arl"), "a", c(
    "  (export x y own)",
    '  (import "b.arl")',
    "  (define own 30)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "a.arl"))

  # Import only x from facade a
  eng$eval_text('(import a :refer (x))')
  expect_equal(eng$eval_text("x"), 10)
  expect_error(eng$eval_text("y"))
})

test_that("re-export with :as modifier for qualified access", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "b.arl"), "b", c(
    "  (export val)",
    "  (define val 42)"
  ))

  write_module(file.path(tmp_dir, "a.arl"), "a", c(
    "  (export val)",
    '  (import "b.arl")'
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "a.arl"))
  eng$eval_text('(import a :as aa)')
  expect_equal(eng$eval_text("aa/val"), 42)
})

test_that("re-export with :rename modifier", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "b.arl"), "b", c(
    "  (export val)",
    "  (define val 42)"
  ))

  write_module(file.path(tmp_dir, "a.arl"), "a", c(
    "  (export val)",
    '  (import "b.arl")'
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "a.arl"))
  eng$eval_text('(import a :rename ((val renamed-val)))')
  expect_equal(eng$eval_text("renamed-val"), 42)
})

test_that("macro re-export: consumer can use macro from transitive module", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Module B defines a macro
  write_module(file.path(tmp_dir, "b.arl"), "b", c(
    "  (export my-when)",
    "  (defmacro my-when (test . body)",
    "    `(if ,test (begin ,@body) #f))"
  ))

  # Module A re-exports macro from B
  write_module(file.path(tmp_dir, "a.arl"), "a", c(
    "  (export my-when)",
    '  (import "b.arl")'
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "a.arl"))
  eng$eval_text('(import a)')
  expect_equal(eng$eval_text("(my-when #t 42)"), 42)
  expect_equal(eng$eval_text("(my-when #f 42)"), FALSE)
})

test_that("transitive re-export: A -> B -> C -> D", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "d.arl"), "d", c(
    "  (export deep-val)",
    "  (define deep-val 999)"
  ))

  write_module(file.path(tmp_dir, "c.arl"), "c", c(
    "  (export deep-val)",
    '  (import "d.arl")'
  ))

  write_module(file.path(tmp_dir, "b.arl"), "b", c(
    "  (export deep-val)",
    '  (import "c.arl")'
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "b.arl"))
  eng$eval_text('(import b)')
  expect_equal(eng$eval_text("deep-val"), 999)
})

test_that("reload value change reflected through re-export (live forwarding)", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "src.arl"), "src", c(
    "  (export val)",
    "  (define val 1)"
  ))

  write_module(file.path(tmp_dir, "facade.arl"), "facade", c(
    "  (export val)",
    '  (import "src.arl")'
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "facade.arl"))
  eng$eval_text('(import facade)')
  expect_equal(eng$eval_text("val"), 1)

  # Change the source module's value and reload
  write_module(file.path(tmp_dir, "src.arl"), "src", c(
    "  (export val)",
    "  (define val 999)"
  ))

  src_path <- normalizePath(file.path(tmp_dir, "src.arl"), winslash = "/")
  eng$eval_text(sprintf('(import "%s" :reload)', src_path))

  # Re-export forwarding should reflect new value
  expect_equal(eng$eval_text("val"), 999)
})

test_that("reload does not cascade: facade export set unchanged after source reload", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "src.arl"), "src", c(
    "  (export x)",
    "  (define x 1)"
  ))

  write_module(file.path(tmp_dir, "facade.arl"), "facade", c(
    "  (export-all :re-export)",
    '  (import "src.arl")'
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "facade.arl"))

  entry_before <- engine_field(eng, "env")$module_registry$get("facade")
  exports_before <- sort(entry_before$exports)

  # Reload source with a new export added
  write_module(file.path(tmp_dir, "src.arl"), "src", c(
    "  (export x y)",
    "  (define x 1)",
    "  (define y 2)"
  ))
  src_path <- normalizePath(file.path(tmp_dir, "src.arl"), winslash = "/")
  eng$eval_text(sprintf('(import "%s" :reload)', src_path))

  # Facade's export set should NOT have changed (no cascade)
  entry_after <- engine_field(eng, "env")$module_registry$get("facade")
  exports_after <- sort(entry_after$exports)
  expect_equal(exports_before, exports_after)
})

test_that("error: exporting undefined/unimported name", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "bad.arl"), "bad", c(
    "  (export nonexistent)",
    "  (define something-else 1)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  expect_error(
    eng$load_file_in_env(file.path(tmp_dir, "bad.arl")),
    "not defined or imported"
  )
})

test_that("export-all excludes _-prefixed names (private convention)", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "mod.arl"), "mod", c(
    "  (export-all)",
    "  (define public-fn 1)",
    "  (define _private-helper 2)",
    "  (define __also-private 3)",
    "  (define _another 4)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "mod.arl"))

  entry <- engine_field(eng, "env")$module_registry$get("mod")
  expect_true("public-fn" %in% entry$exports)
  expect_false("_private-helper" %in% entry$exports)
  expect_false("__also-private" %in% entry$exports)
  expect_false("_another" %in% entry$exports)
})

test_that("export-all :re-export still excludes _-prefixed own bindings", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "provider.arl"), "provider", c(
    "  (export val)",
    "  (define val 100)"
  ))

  write_module(file.path(tmp_dir, "facade.arl"), "facade", c(
    "  (export-all :re-export)",
    '  (import "provider.arl")',
    "  (define own-public 200)",
    "  (define _own-private 300)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "facade.arl"))

  entry <- engine_field(eng, "env")$module_registry$get("facade")
  expect_true("val" %in% entry$exports)
  expect_true("own-public" %in% entry$exports)
  expect_false("_own-private" %in% entry$exports)
})

test_that("explicit export of _-prefixed name still works", {
  eng <- make_engine()
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  write_module(file.path(tmp_dir, "mod.arl"), "mod", c(
    "  (export _intentionally-public helper)",
    "  (define _intentionally-public 1)",
    "  (define helper 2)"
  ))

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  eng$load_file_in_env(file.path(tmp_dir, "mod.arl"))

  eng$eval_text('(import mod)')
  expect_equal(eng$eval_text("_intentionally-public"), 1)
  expect_equal(eng$eval_text("helper"), 2)
})

test_that("compile error: export-all with bad modifier", {
  eng <- make_engine()
  expect_error(
    eng$eval_text("(module bad (export-all :bad-modifier) (define x 1))"),
    "only accepts :re-export"
  )
})
