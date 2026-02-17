# Tests for module reloading via (import mod :reload)

write_module <- function(path, name, body) {
  lines <- c(
    sprintf("(module %s", name),
    body,
    ")"
  )
  writeLines(lines, path)
}

test_that("basic reload: value updated after file change", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "test-mod", c(
    "  (export x)",
    "  (define x 1)"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  eng$eval_text(sprintf('(import "%s")', path_arl))
  expect_equal(eng$eval_text("x"), 1)

  write_module(path, "test-mod", c(
    "  (export x)",
    "  (define x 2)"
  ))

  eng$eval_text(sprintf('(import "%s" :reload)', path_arl))
  expect_equal(eng$eval_text("x"), 2)
})

test_that("reference semantics: existing proxy sees new value after reload", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "ref-mod", c(
    "  (export get-val)",
    "  (define val 10)",
    "  (define get-val (lambda () val))"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  eng$eval_text(sprintf('(import "%s")', path_arl))
  expect_equal(eng$eval_text("(get-val)"), 10)

  write_module(path, "ref-mod", c(
    "  (export get-val)",
    "  (define val 20)",
    "  (define get-val (lambda () val))"
  ))

  eng$eval_text(sprintf('(import "%s" :reload)', path_arl))
  # The proxy active binding picks up the new get-val
  expect_equal(eng$eval_text("(get-val)"), 20)
})

test_that("env identity preserved after reload", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "id-mod", c(
    "  (export x)",
    "  (define x 1)"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  eng$eval_text(sprintf('(import "%s")', path_arl))
  arl_env <- arl:::Env$new(eng$get_env())
  registry <- arl_env$module_registry
  entry_before <- registry$get("id-mod")
  env_before <- entry_before$env

  write_module(path, "id-mod", c(
    "  (export x)",
    "  (define x 2)"
  ))

  eng$eval_text(sprintf('(import "%s" :reload)', path_arl))
  entry_after <- registry$get("id-mod")
  env_after <- entry_after$env

  expect_identical(env_before, env_after)
})

test_that("export-all reload: new binding added, old one updated", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "ea-mod", c(
    "  (export-all)",
    "  (define x 1)"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  eng$eval_text(sprintf('(import "%s")', path_arl))
  expect_equal(eng$eval_text("x"), 1)

  write_module(path, "ea-mod", c(
    "  (export-all)",
    "  (define x 99)",
    "  (define y 42)"
  ))

  eng$eval_text(sprintf('(import "%s" :reload)', path_arl))
  expect_equal(eng$eval_text("x"), 99)
  expect_equal(eng$eval_text("y"), 42)
})

test_that("new exports visible after reload via proxy rebuild", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "new-exp-mod", c(
    "  (export x)",
    "  (define x 1)"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  eng$eval_text(sprintf('(import "%s")', path_arl))
  expect_equal(eng$eval_text("x"), 1)

  write_module(path, "new-exp-mod", c(
    "  (export x y)",
    "  (define x 10)",
    "  (define y 20)"
  ))

  eng$eval_text(sprintf('(import "%s" :reload)', path_arl))
  expect_equal(eng$eval_text("x"), 10)
  expect_equal(eng$eval_text("y"), 20)
})

test_that("removed exports cleaned up from proxy", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "rm-exp-mod", c(
    "  (export x y)",
    "  (define x 1)",
    "  (define y 2)"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  eng$eval_text(sprintf('(import "%s")', path_arl))
  expect_equal(eng$eval_text("y"), 2)

  write_module(path, "rm-exp-mod", c(
    "  (export x)",
    "  (define x 10)"
  ))

  eng$eval_text(sprintf('(import "%s" :reload)', path_arl))
  expect_equal(eng$eval_text("x"), 10)
  # y should no longer be accessible
  expect_error(eng$eval_text("y"))
})

test_that("error: reload unloaded module", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "never-loaded", c(
    "  (export x)",
    "  (define x 1)"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  expect_error(
    eng$eval_text(sprintf('(import "%s" :reload)', path_arl)),
    "cannot reload"
  )
})

test_that("transitive update: A imports B, reload B, A sees new values", {
  eng <- make_engine()
  dir <- tempdir()
  path_b <- file.path(dir, "mod-b.arl")
  path_a <- file.path(dir, "mod-a.arl")
  on.exit({
    unlink(path_a)
    unlink(path_b)
  }, add = TRUE)

  write_module(path_b, "mod-b", c(
    "  (export b-val)",
    "  (define b-val 100)"
  ))
  path_b_arl <- normalizePath(path_b, winslash = "/")

  write_module(path_a, "mod-a", c(
    "  (export get-b)",
    sprintf('  (import "%s")', path_b_arl),
    "  (define get-b (lambda () b-val))"
  ))
  path_a_arl <- normalizePath(path_a, winslash = "/")

  eng$eval_text(sprintf('(import "%s")', path_a_arl))
  expect_equal(eng$eval_text("(get-b)"), 100)

  # Reload B with new value — A should see it via proxy chain
  write_module(path_b, "mod-b", c(
    "  (export b-val)",
    "  (define b-val 200)"
  ))

  eng$eval_text(sprintf('(import "%s" :reload)', path_b_arl))
  expect_equal(eng$eval_text("(get-b)"), 200)
})

test_that("multiple consumers: both see updates after reload", {
  eng <- make_engine()
  dir <- tempdir()
  path_c <- file.path(dir, "shared-mod.arl")
  path_a <- file.path(dir, "consumer-a.arl")
  path_b <- file.path(dir, "consumer-b.arl")
  on.exit({
    unlink(path_a)
    unlink(path_b)
    unlink(path_c)
  }, add = TRUE)

  write_module(path_c, "shared-mod", c(
    "  (export shared-val)",
    "  (define shared-val 1)"
  ))
  path_c_arl <- normalizePath(path_c, winslash = "/")

  write_module(path_a, "consumer-a", c(
    "  (export get-a)",
    sprintf('  (import "%s")', path_c_arl),
    "  (define get-a (lambda () shared-val))"
  ))
  write_module(path_b, "consumer-b", c(
    "  (export get-b)",
    sprintf('  (import "%s")', path_c_arl),
    "  (define get-b (lambda () shared-val))"
  ))

  eng$eval_text(sprintf('(import "%s")', normalizePath(path_a, winslash = "/")))
  eng$eval_text(sprintf('(import "%s")', normalizePath(path_b, winslash = "/")))
  expect_equal(eng$eval_text("(get-a)"), 1)
  expect_equal(eng$eval_text("(get-b)"), 1)

  write_module(path_c, "shared-mod", c(
    "  (export shared-val)",
    "  (define shared-val 999)"
  ))

  eng$eval_text(sprintf('(import "%s" :reload)', path_c_arl))
  expect_equal(eng$eval_text("(get-a)"), 999)
  expect_equal(eng$eval_text("(get-b)"), 999)
})

test_that("cache bypass: modified file re-read on reload", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "cache-mod", c(
    "  (export x)",
    "  (define x 1)"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  # First load (may cache)
  eng$eval_text(sprintf('(import "%s")', path_arl))
  expect_equal(eng$eval_text("x"), 1)

  # Modify file
  write_module(path, "cache-mod", c(
    "  (export x)",
    "  (define x 42)"
  ))

  # Reload should bypass cache
  eng$eval_text(sprintf('(import "%s" :reload)', path_arl))
  expect_equal(eng$eval_text("x"), 42)
})

test_that(":reload with :only modifier", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "only-mod", c(
    "  (export x y)",
    "  (define x 1)",
    "  (define y 2)"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  eng$eval_text(sprintf('(import "%s" :refer (x))', path_arl))
  expect_equal(eng$eval_text("x"), 1)

  write_module(path, "only-mod", c(
    "  (export x y)",
    "  (define x 10)",
    "  (define y 20)"
  ))

  eng$eval_text(sprintf('(import "%s" :reload :refer (x))', path_arl))
  expect_equal(eng$eval_text("x"), 10)
})

test_that(":reload with :as modifier for qualified access", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "pfx-mod", c(
    "  (export x)",
    "  (define x 1)"
  ))
  path_arl <- normalizePath(path, winslash = "/")

  eng$eval_text(sprintf('(import "%s" :as pfx)', path_arl))
  expect_equal(eng$eval_text("pfx/x"), 1)

  write_module(path, "pfx-mod", c(
    "  (export x)",
    "  (define x 99)"
  ))

  eng$eval_text(sprintf('(import "%s" :reload :as pfx)', path_arl))
  expect_equal(eng$eval_text("pfx/x"), 99)
})

test_that("compile error: :reload inside nested scope", {
  eng <- make_engine()
  expect_error(
    eng$eval_text('(lambda () (import foo :reload))'),
    "top level"
  )
})

test_that("compile error: duplicate :reload", {
  eng <- make_engine()
  expect_error(
    eng$eval_text('(import foo :reload :reload)'),
    "duplicate :reload"
  )
})

test_that("macro reload: macro body changes visible through proxy", {
  eng <- make_engine()
  path <- tempfile(fileext = ".arl")
  on.exit(unlink(path), add = TRUE)

  write_module(path, "mac-mod", c(
    "  (export my-mac)",
    '  (defmacro my-mac (x) `(+ ,x 1))'
  ))
  path_arl <- normalizePath(path, winslash = "/")

  eng$eval_text(sprintf('(import "%s")', path_arl))
  expect_equal(eng$eval_text("(my-mac 5)"), 6)

  write_module(path, "mac-mod", c(
    "  (export my-mac)",
    '  (defmacro my-mac (x) `(+ ,x 10))'
  ))

  eng$eval_text(sprintf('(import "%s" :reload)', path_arl))
  expect_equal(eng$eval_text("(my-mac 5)"), 15)
})
