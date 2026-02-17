# Tests for selective/prefixed/renamed imports
# Covers :only, :except, :prefix, :rename modifiers and their composition

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

  engine$eval_text("(import testmod)")
  expect_equal(engine$eval_text("(square 5)"), 25L)
  expect_equal(engine$eval_text("(cube 3)"), 27L)
  expect_equal(engine$eval_text("helper-val"), 99L)
})

# --- :only ---

test_that("import :only imports only specified symbols", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :only (square))")
  expect_equal(engine$eval_text("(square 5)"), 25L)
  expect_error(engine$eval_text("(cube 3)"), "not found|could not find|object .* not found")
  expect_error(engine$eval_text("helper-val"), "not found|could not find|object .* not found")
})

test_that("import :only errors on names not in module exports", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  expect_error(
    engine$eval_text("(import testmod :only (nonexistent))"),
    "does not export 'nonexistent'"
  )
})

# --- :except ---

test_that("import :except excludes specified symbols", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :except (helper-val))")
  expect_equal(engine$eval_text("(square 5)"), 25L)
  expect_equal(engine$eval_text("(cube 3)"), 27L)
  expect_error(engine$eval_text("helper-val"), "not found|could not find|object .* not found")
})

test_that("import :except errors on names not in module exports", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  expect_error(
    engine$eval_text("(import testmod :except (nonexistent))"),
    "does not export 'nonexistent'"
  )
})

# --- :prefix ---

test_that("import :prefix adds prefix to all imported names", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :prefix m/)")
  expect_equal(engine$eval_text("(m/square 5)"), 25L)
  expect_equal(engine$eval_text("(m/cube 3)"), 27L)
  expect_error(engine$eval_text("(square 5)"), "not found|could not find|object .* not found")
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

test_that("import :only + :prefix composes correctly", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :only (square) :prefix m/)")
  expect_equal(engine$eval_text("(m/square 5)"), 25L)
  expect_error(engine$eval_text("(m/cube 3)"), "not found|could not find")
  expect_error(engine$eval_text("(square 5)"), "not found|could not find|object .* not found")
})

test_that("import :except + :rename composes correctly", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :except (cube) :rename ((square sq)))")
  expect_equal(engine$eval_text("(sq 5)"), 25L)
  expect_error(engine$eval_text("(square 5)"), "not found|could not find|object .* not found")
  expect_error(engine$eval_text("(cube 3)"), "not found|could not find|object .* not found")
})

test_that("import :only + :rename + :prefix composes correctly", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :only (square cube) :rename ((square sq)) :prefix m/)")
  expect_equal(engine$eval_text("(m/sq 5)"), 25L)
  expect_equal(engine$eval_text("(m/cube 3)"), 27L)
  expect_error(engine$eval_text("(square 5)"), "not found|could not find|object .* not found")
  expect_error(engine$eval_text("(m/square 5)"), "not found|could not find")
})

# --- Macros ---
# With properly scoped macro registries, selective import works for macros too.

test_that("import :only works with macros", {
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

  engine$eval_text("(import macmod :only (my-when))")
  expect_equal(engine$eval_text("(my-when #t 42)"), 42L)
  # my-unless was excluded — not visible as a macro
  expect_error(engine$eval_text("(my-unless #f 42)"), "not found|could not find")
})

test_that("import :prefix works with macros", {
  m <- make_temp_module(name = "macmod2", exports = c("mw"), body = c(
    "(module macmod2",
    "  (export mw)",
    "  (defmacro mw (test . body) `(if ,test (begin ,@body) #nil)))"
  ))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import macmod2 :prefix c/)")
  expect_equal(engine$eval_text("(c/mw #t 42)"), 42L)
  # unprefixed name is not visible
  expect_error(engine$eval_text("(mw #t 42)"), "not found|could not find")
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

test_that("import with empty :only imports nothing", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :only ())")
  expect_error(engine$eval_text("(square 5)"), "not found|could not find|object .* not found")
  expect_error(engine$eval_text("(cube 3)"), "not found|could not find|object .* not found")
})

test_that("import with path string supports modifiers", {
  m <- make_temp_module()
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text(sprintf('(import "%s" :only (square))', m$file))
  expect_equal(engine$eval_text("(square 5)"), 25L)
  expect_error(engine$eval_text("(cube 3)"), "not found|could not find|object .* not found")
})

test_that("proxy-based imports are accessible via scoping but not in ls(env)", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :only (square))")
  env <- engine$get_env()
  # Proxy-based imports live in parent chain, not in the env itself
  expect_false("square" %in% ls(env, all.names = FALSE))
  # But they're accessible via get with inherits
  expect_equal(get("square", envir = env)(5L), 25L)
})

test_that("reference semantics: changes in module visible through proxy", {
  m <- make_temp_module(exports = c("square", "cube"))
  on.exit(unlink(m$dir, recursive = TRUE), add = TRUE)

  engine <- make_engine()
  old_wd <- setwd(m$dir)
  on.exit(setwd(old_wd), add = TRUE)

  engine$eval_text("(import testmod :only (square))")

  # Get the module env and modify the binding
  arl_env <- arl:::Env$new(engine$get_env())
  registry <- arl_env$module_registry
  entry <- registry$get("testmod")
  module_env <- entry$env

  # Replace square with a doubling function
  assign("square", function(x) x * 2L, envir = module_env)

  # The proxy should reflect the new value
  expect_equal(engine$eval_text("(square 5)"), 10L)
})

# --- Compile-time errors ---

test_that("import :only and :except are mutually exclusive", {
  engine <- make_engine()
  expect_error(
    engine$eval_text("(import testmod :only (x) :except (y))"),
    "mutually exclusive"
  )
})

test_that("import with unknown modifier errors", {
  engine <- make_engine()
  expect_error(
    engine$eval_text("(import testmod :foobar (x))"),
    "unknown modifier"
  )
})
