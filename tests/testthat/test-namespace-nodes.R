# Tests for namespace nodes (hierarchical module names)

test_that("namespace? predicate", {
  engine <- make_engine()
  ns <- arl:::make_namespace_node("collections")
  expect_true(inherits(ns, "arl_namespace"))
  expect_true(engine$eval_text("(namespace? ns)", env = list2env(list(ns = ns), parent = engine$get_env())))
  expect_false(engine$eval_text("(namespace? 42)"))
})

test_that("make_namespace_node creates locked env with prefix", {
  ns <- arl:::make_namespace_node("collections")
  expect_true(environmentIsLocked(ns))
  expect_equal(get(".__namespace_prefix", envir = ns), "collections")
  expect_true(inherits(ns, "arl_namespace"))
})

test_that("module-ref on namespace node returns module if registered", {
  engine <- make_engine()
  env <- engine$get_env()
  # Create a module with a hierarchical name using a string literal
  engine$eval_text('
    (module "collections/sorted-set"
      (export make-set)
      (define make-set (lambda () (list))))
  ', env = env)
  # Create namespace node and test module-ref
  ns <- arl:::make_namespace_node("collections")
  test_env <- new.env(parent = env)
  assign("ns", ns, envir = test_env)
  result <- engine$eval_text("(module-ref ns sorted-set)", env = test_env)
  expect_true(is.environment(result))
})
