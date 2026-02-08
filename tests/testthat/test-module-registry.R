# Tests for ModuleRegistry (R/module-registry.R): module registration, retrieval, aliasing, attachment

test_that("ModuleRegistry requires a RyeEnv", {
  expect_error(
    rye:::ModuleRegistry$new(NULL),
    "ModuleRegistry requires a RyeEnv"
  )
  expect_error(
    rye:::ModuleRegistry$new("not an env"),
    "ModuleRegistry requires a RyeEnv"
  )
})

test_that("ModuleRegistry initializes with RyeEnv", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_true(r6_isinstance(registry, "ModuleRegistry"))
  expect_identical(registry$rye_env, rye_env)

  # Should create the registry environment
  reg_env <- rye_env$module_registry_env(create = FALSE)
  expect_true(!is.null(reg_env))
  expect_true(is.environment(reg_env))
})

# Module registration tests
test_that("register() requires valid name", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  expect_error(
    registry$register(123, mod_env, c("foo")),
    "module name must be a single string"
  )
  expect_error(
    registry$register(c("a", "b"), mod_env, c("foo")),
    "module name must be a single string"
  )
  expect_error(
    registry$register(character(0), mod_env, c("foo")),
    "module name must be a single string"
  )
})

test_that("register() creates module entry", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()
  mod_env$foo <- 42

  entry <- registry$register("test-mod", mod_env, c("foo"), path = "/path/to/test.rye")

  expect_true(is.environment(entry))
  expect_identical(entry$env, mod_env)
  expect_equal(entry$exports, c("foo"))
  expect_equal(entry$path, "/path/to/test.rye")
})

test_that("register() locks the entry environment", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  entry <- registry$register("test-mod", mod_env, c("foo"))

  # Entry should be locked
  expect_true(environmentIsLocked(entry))
  expect_true(bindingIsLocked("env", entry))
  expect_true(bindingIsLocked("exports", entry))
  expect_true(bindingIsLocked("path", entry))
})

test_that("register() prevents duplicate registration", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))

  expect_error(
    registry$register("test-mod", new.env(), c("bar")),
    "module 'test-mod' is already defined"
  )
})

test_that("register() locks binding in registry", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))

  reg_env <- rye_env$module_registry_env(create = FALSE)
  expect_true(bindingIsLocked("test-mod", reg_env))
})

test_that("register() handles empty exports", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  entry <- registry$register("test-mod", mod_env, character())

  expect_equal(entry$exports, character())
})

test_that("register() handles NULL path", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  entry <- registry$register("test-mod", mod_env, c("foo"))

  expect_null(entry$path)
})

# Module retrieval tests
test_that("exists() returns FALSE for missing modules", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_false(registry$exists("nonexistent"))
})

test_that("exists() returns TRUE for registered modules", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))

  expect_true(registry$exists("test-mod"))
})

test_that("exists() handles invalid input gracefully", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_false(registry$exists(NULL))
  expect_false(registry$exists(123))
  expect_false(registry$exists(c("a", "b")))
})

test_that("get() returns NULL for missing modules", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_null(registry$get("nonexistent"))
})

test_that("get() returns entry for registered modules", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()
  mod_env$foo <- 42

  registry$register("test-mod", mod_env, c("foo"), path = "/test.rye")
  entry <- registry$get("test-mod")

  expect_true(is.environment(entry))
  expect_identical(entry$env, mod_env)
  expect_equal(entry$exports, c("foo"))
  expect_equal(entry$path, "/test.rye")
})

# Export management tests
test_that("update_exports() requires valid name", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_error(
    registry$update_exports(123, c("bar")),
    "module name must be a single string"
  )
})

test_that("update_exports() fails for non-existent module", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_error(
    registry$update_exports("nonexistent", c("bar")),
    "module 'nonexistent' is not loaded"
  )
})

test_that("update_exports() updates exports list", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))
  entry <- registry$update_exports("test-mod", c("foo", "bar", "baz"))

  expect_equal(entry$exports, c("foo", "bar", "baz"))
})

test_that("update_exports() preserves environment and path", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"), path = "/test.rye")
  entry <- registry$update_exports("test-mod", c("bar"))

  expect_identical(entry$env, mod_env)
  expect_equal(entry$path, "/test.rye")
})

test_that("update_exports() maintains locking", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))
  entry <- registry$update_exports("test-mod", c("bar"))

  expect_true(environmentIsLocked(entry))
  reg_env <- rye_env$module_registry_env(create = FALSE)
  expect_true(bindingIsLocked("test-mod", reg_env))
})

# Path aliasing tests
test_that("alias() requires valid path and name", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_error(
    registry$alias(123, "test"),
    "alias requires path and name as single strings"
  )
  expect_error(
    registry$alias("/path", 123),
    "alias requires path and name as single strings"
  )
  expect_error(
    registry$alias(c("a", "b"), "test"),
    "alias requires path and name as single strings"
  )
})

test_that("alias() fails for non-existent module", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_error(
    registry$alias("/path/to/test.rye", "nonexistent"),
    "module 'nonexistent' is not loaded"
  )
})

test_that("alias() creates path lookup", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))
  registry$alias("/path/to/test.rye", "test-mod")

  # Can retrieve by path
  entry_by_path <- registry$get("/path/to/test.rye")
  entry_by_name <- registry$get("test-mod")

  expect_identical(entry_by_path, entry_by_name)
})

test_that("alias() is idempotent for same module", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))
  registry$alias("/path/to/test.rye", "test-mod")

  # Second alias call should succeed
  expect_silent(registry$alias("/path/to/test.rye", "test-mod"))
})

test_that("alias() prevents conflicting paths", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env1 <- new.env()
  mod_env2 <- new.env()

  registry$register("mod1", mod_env1, c("foo"))
  registry$register("mod2", mod_env2, c("bar"))
  registry$alias("/path/test.rye", "mod1")

  expect_error(
    registry$alias("/path/test.rye", "mod2"),
    "path '/path/test.rye' is already bound to a different module"
  )
})

test_that("alias() locks the path binding", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))
  registry$alias("/path/to/test.rye", "test-mod")

  reg_env <- rye_env$module_registry_env(create = FALSE)
  expect_true(bindingIsLocked("/path/to/test.rye", reg_env))
})

test_that("exists() works with aliased paths", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))
  registry$alias("/path/to/test.rye", "test-mod")

  expect_true(registry$exists("/path/to/test.rye"))
})

# Module attachment tests
test_that("attach() fails for missing module", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_error(
    registry$attach("nonexistent"),
    "module 'nonexistent' is not loaded"
  )
})

test_that("attach() copies exports to RyeEnv", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()
  mod_env$foo <- 42
  mod_env$bar <- "test"

  registry$register("test-mod", mod_env, c("foo", "bar"))
  registry$attach("test-mod")

  expect_equal(rye_env$env$foo, 42)
  expect_equal(rye_env$env$bar, "test")
})

test_that("attach() handles macro exports", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  # Create a macro in the module's macro registry
  mod_macro_registry <- rye_env$macro_registry_env(mod_env, create = TRUE)
  mod_macro_registry$my_macro <- function(x) x

  registry$register("test-mod", mod_env, c("my_macro"))
  registry$attach("test-mod")

  # Macro should be in target macro registry
  target_macro_registry <- rye_env$macro_registry_env(create = FALSE)
  expect_true(exists("my_macro", envir = target_macro_registry, inherits = FALSE))
  expect_true(bindingIsLocked("my_macro", target_macro_registry))
})

test_that("attach() errors on missing export", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()
  mod_env$foo <- 42

  registry$register("test-mod", mod_env, c("foo", "missing"))

  expect_error(
    registry$attach("test-mod"),
    "module 'test-mod' does not export 'missing'"
  )
})

test_that("attach_into() fails for missing module", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  target <- new.env()

  expect_error(
    registry$attach_into("nonexistent", target),
    "module 'nonexistent' is not loaded"
  )
})

test_that("attach_into() copies exports to target environment", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()
  mod_env$foo <- 42
  mod_env$bar <- "test"
  target <- new.env()

  registry$register("test-mod", mod_env, c("foo", "bar"))
  registry$attach_into("test-mod", target)

  expect_equal(target$foo, 42)
  expect_equal(target$bar, "test")
})

test_that("attach_into() handles macros to target's macro registry", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()
  target <- new.env()

  # Create a macro in the module's macro registry
  mod_macro_registry <- rye_env$macro_registry_env(mod_env, create = TRUE)
  mod_macro_registry$my_macro <- function(x) x

  registry$register("test-mod", mod_env, c("my_macro"))
  registry$attach_into("test-mod", target)

  # Macro should be in target's macro registry
  target_rye <- rye:::RyeEnv$new(target)
  target_macro_registry <- target_rye$macro_registry_env(create = FALSE)
  expect_true(exists("my_macro", envir = target_macro_registry, inherits = FALSE))
})

test_that("attach_into() errors on missing export", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()
  mod_env$foo <- 42
  target <- new.env()

  registry$register("test-mod", mod_env, c("foo", "missing"))

  expect_error(
    registry$attach_into("test-mod", target),
    "module 'test-mod' does not export 'missing'"
  )
})

# Cleanup tests
test_that("unregister() requires valid name", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  expect_error(
    registry$unregister(123),
    "module name must be a single string"
  )
})

test_that("unregister() removes module from registry", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)
  mod_env <- new.env()

  registry$register("test-mod", mod_env, c("foo"))
  expect_true(registry$exists("test-mod"))

  registry$unregister("test-mod")
  expect_false(registry$exists("test-mod"))
})

test_that("unregister() is safe for non-existent modules", {
  test_env <- new.env()
  rye_env <- rye:::RyeEnv$new(test_env)
  registry <- rye:::ModuleRegistry$new(rye_env)

  # Should not error
  expect_silent(registry$unregister("nonexistent"))
})
