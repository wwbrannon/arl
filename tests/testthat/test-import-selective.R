# Tests for selective imports with :refer, :as, :rename modifiers

# Helper: create a temp module file with known exports
make_temp_module <- function(name = "testmod", exports = c("square", "cube", "helper-val"),
                             body = NULL) {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE)
  if (is.null(body)) {
    body <- c(
      sprintf("(module %s", name),
      sprintf("  (export %s)", paste(exports, collapse = " ")),
      "  (define square (lambda (x) (* x x)))",
      "  (define cube (lambda (x) (* x x x)))",
      "  (define helper-val 99))"
    )
  }
  module_file <- file.path(tmp_dir, paste0(name, ".arl"))
  writeLines(body, module_file)
  list(dir = tmp_dir, file = module_file, name = name)
}

# --- No modifiers (regression) ---

test_that("import with no modifiers imports all exports", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :refer :all)")
  expect_equal(engine$eval_text("(square 5)"), 25L)
  expect_equal(engine$eval_text("(cube 3)"), 27L)
  expect_equal(engine$eval_text("helper-val"), 99L)
})

# --- :refer ---

test_that("import :refer imports only specified symbols", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :refer (square))")
  expect_equal(engine$eval_text("(square 5)"), 25L)
  expect_error(engine$eval_text("(cube 3)"), "not found|could not find|object .* not found")
  expect_error(engine$eval_text("helper-val"), "not found|could not find|object .* not found")
})

test_that("import :refer errors on names not in module exports", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  expect_error(
    engine$eval_text("(import testmod :refer (nonexistent))"),
    "does not export 'nonexistent'"
  )
})

test_that("import :refer :all imports all exports", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :refer :all)")
  expect_equal(engine$eval_text("(square 5)"), 25L)
  expect_equal(engine$eval_text("(cube 3)"), 27L)
  expect_equal(engine$eval_text("helper-val"), 99L)
})

# --- :as ---

test_that("import :as aliases the module binding", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :as m)")
  # Qualified access works via alias
  expect_equal(engine$eval_text("(m/square 5)"), 25L)
  expect_equal(engine$eval_text("(m/cube 3)"), 27L)
  # Unqualified names not imported (no :refer)
  expect_error(engine$eval_text("(square 5)"), "not found|could not find|object .* not found")
})

test_that("import :as with :refer combines correctly", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :as m :refer (square))")
  expect_equal(engine$eval_text("(square 5)"), 25L)
  expect_equal(engine$eval_text("(m/cube 3)"), 27L)
  expect_error(engine$eval_text("(cube 3)"), "not found|could not find|object .* not found")
})

# --- :rename ---

test_that("import :rename renames specified symbols", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :rename ((square sq)))")
  expect_equal(engine$eval_text("(sq 5)"), 25L)
  expect_equal(engine$eval_text("(cube 3)"), 27L)
  expect_error(engine$eval_text("(square 5)"), "not found|could not find|object .* not found")
})

test_that("import :rename errors on names not in module exports", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  expect_error(
    engine$eval_text("(import testmod :rename ((nonexistent foo)))"),
    "does not export 'nonexistent'"
  )
})

# --- Composition ---

test_that("import :refer + :rename composes correctly", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :refer (square cube) :rename ((square sq)))")
  expect_equal(engine$eval_text("(sq 5)"), 25L)
  expect_equal(engine$eval_text("(cube 3)"), 27L)
  expect_error(engine$eval_text("(square 5)"), "not found|could not find|object .* not found")
})

# --- Macros ---

test_that("import :refer works with macros", {
  m <- make_temp_module(name = "macmod", exports = c("my-when", "my-unless"), body = c(
    "(module macmod",
    "  (export my-when my-unless)",
    "  (defmacro my-when (test . body) `(if ,test (begin ,@body) #nil))",
    "  (defmacro my-unless (test . body) `(if ,test #nil (begin ,@body))))"
  ))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import macmod :refer (my-when))")
  expect_equal(engine$eval_text("(my-when #t 42)"), 42L)
  # my-unless was not referred — not visible as a macro
  expect_error(engine$eval_text("(my-unless #f 42)"), "not found|could not find")
})

test_that("import :refer works with macros", {
  m <- make_temp_module(name = "macmod2", exports = c("mw"), body = c(
    "(module macmod2",
    "  (export mw)",
    "  (defmacro mw (test . body) `(if ,test (begin ,@body) #nil)))"
  ))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import macmod2 :refer (mw))")
  expect_equal(engine$eval_text("(mw #t 42)"), 42L)
})

test_that("import :rename works with macros", {
  m <- make_temp_module(name = "macmod3", exports = c("mw3"), body = c(
    "(module macmod3",
    "  (export mw3)",
    "  (defmacro mw3 (test . body) `(if ,test (begin ,@body) #nil)))"
  ))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import macmod3 :rename ((mw3 my-when3)))")
  expect_equal(engine$eval_text("(my-when3 #t 42)"), 42L)
  # original name is not visible
  expect_error(engine$eval_text("(mw3 #t 42)"), "not found|could not find")
})

# --- Edge cases ---

test_that("import with empty :refer imports nothing unqualified", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :refer ())")
  expect_error(engine$eval_text("(square 5)"), "not found|could not find|object .* not found")
  expect_error(engine$eval_text("(cube 3)"), "not found|could not find|object .* not found")
  # But qualified access should work
  expect_equal(engine$eval_text("(testmod/square 5)"), 25L)
})

test_that("import with path string supports modifiers", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text(sprintf('(import "%s" :refer (square))', arl_path(m$file)))
  expect_equal(engine$eval_text("(square 5)"), 25L)
  expect_error(engine$eval_text("(cube 3)"), "not found|could not find|object .* not found")
})

test_that("proxy-based imports are accessible via scoping but not in ls(env)", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :refer (square))")
  env <- engine$get_env()
  # Proxy-based imports live in parent chain, not in the env itself
  # (but the module binding "testmod" IS in env)
  expect_false("square" %in% ls(env, all.names = FALSE))
  # But they're accessible via get with inherits
  expect_equal(get("square", envir = env)(5L), 25L)
})

test_that("reference semantics: module bindings are locked", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :refer (square))")

  # Get the module env
  arl_env <- arl:::Env$new(engine$get_env())
  registry <- arl_env$module_registry
  entry <- registry$get("testmod")
  module_env <- entry$env

  # Module bindings should be locked
  expect_true(bindingIsLocked("square", module_env))
})

# --- Compile-time errors ---

test_that("import with unknown modifier errors", {
  engine <- make_engine()
  expect_error(
    engine$eval_text("(import testmod :foobar (x))"),
    "unknown modifier"
  )
})

test_that("import :as rejects non-symbol alias", {
  engine <- make_engine()
  # a/b is parsed as (module-ref a b), not a symbol
  expect_error(
    engine$eval_text("(import testmod :as a/b)"),
    ":as requires a symbol"
  )
})

# ============================================================================
# Qualified access via / syntax and module-ref
# ============================================================================

test_that("parser: math/inc produces (module-ref math inc)", {
  engine <- make_engine()
  result <- engine$read("math/inc")
  expr <- result[[1]]
  expect_true(is.call(expr))
  expect_equal(as.character(expr[[1]]), "module-ref")
  expect_equal(as.character(expr[[2]]), "math")
  expect_equal(as.character(expr[[3]]), "inc")
})

test_that("parser: a/b/c produces nested module-ref", {
  engine <- make_engine()
  result <- engine$read("a/b/c")
  expr <- result[[1]]
  # (module-ref (module-ref a b) c)
  expect_true(is.call(expr))
  expect_equal(as.character(expr[[1]]), "module-ref")
  expect_equal(as.character(expr[[3]]), "c")
  inner <- expr[[2]]
  expect_true(is.call(inner))
  expect_equal(as.character(inner[[1]]), "module-ref")
  expect_equal(as.character(inner[[2]]), "a")
  expect_equal(as.character(inner[[3]]), "b")
})

test_that("parser: bare / stays as division symbol", {
  engine <- make_engine()
  result <- engine$read("(/ 10 2)")
  expr <- result[[1]]
  expect_true(is.call(expr))
  expect_equal(as.character(expr[[1]]), "/")
})

test_that("parser: round-trip math/inc -> write -> math/inc", {
  engine <- make_engine()
  result <- engine$read("math/inc")
  written <- engine$write(result[[1]])
  expect_equal(written, "math/inc")
})

test_that("parser: round-trip a/b/c -> write -> a/b/c", {
  engine <- make_engine()
  result <- engine$read("a/b/c")
  written <- engine$write(result[[1]])
  expect_equal(written, "a/b/c")
})

test_that("parser: R infix operators %/% are not split", {
  engine <- make_engine()
  result <- engine$read("(%/% 10 3)")
  expr <- result[[1]]
  expect_true(is.call(expr))
  expect_equal(as.character(expr[[1]]), "%/%")
})

test_that("qualified access: import :refer then mod/sym resolves", {
  engine <- make_engine()
  engine$eval_text("(import math :refer (inc))")
  result <- engine$eval_text("math/inc")
  expect_true(is.function(result))
})

test_that("qualified access: import :as then alias/sym resolves", {
  engine <- make_engine()
  engine$eval_text("(import math :as m)")
  result <- engine$eval_text("m/inc")
  expect_true(is.function(result))
})

test_that("bare / is still division", {
  engine <- make_engine()
  result <- engine$eval_text("(/ 10 2)")
  expect_equal(result, 5)
})
