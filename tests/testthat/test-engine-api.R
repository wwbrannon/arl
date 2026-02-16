# Tests for Engine public API methods.

# =============================================================================
# define()
# =============================================================================

test_that("define() binds a value visible to Arl code", {
  engine <- make_engine()
  engine$define("my_val", 42)
  expect_equal(engine$eval_text("my_val"), 42)
})

test_that("define() works with complex R objects", {
  engine <- make_engine()
  engine$define("df", mtcars)
  expect_equal(engine$eval_text("(nrow df)"), nrow(mtcars))
  expect_equal(engine$eval_text("(median ($ df \"mpg\"))"), median(mtcars$mpg))
})

test_that("define() returns engine invisibly for chaining", {
  engine <- make_engine()
  result <- engine$define("x", 10)$define("y", 20)$eval_text("(+ x y)")
  expect_equal(result, 30)
})

test_that("define() validates name argument", {
  engine <- make_engine()
  expect_error(engine$define(42, "val"), "non-empty character string")
  expect_error(engine$define("", "val"), "non-empty character string")
  expect_error(engine$define(c("a", "b"), "val"), "non-empty character string")
})

test_that("define() overwrites existing bindings", {
  engine <- make_engine()
  engine$define("x", 1)
  expect_equal(engine$eval_text("x"), 1)
  engine$define("x", 2)
  expect_equal(engine$eval_text("x"), 2)
})

# =============================================================================
# get_env()
# =============================================================================

test_that("get_env() provides access to engine bindings", {
  engine <- make_engine()
  engine$eval_text("(define test-binding 99)")
  env <- engine$get_env()
  expect_true(exists("test-binding", envir = env, inherits = FALSE))
  expect_equal(get("test-binding", envir = env), 99)
})

# =============================================================================
# eval_text() / eval_string()
# =============================================================================

test_that("eval_text returns last value of multiple expressions", {
  engine <- make_engine()
  result <- engine$eval_text("(define a 1) (define b 2) (+ a b)")
  expect_equal(result, 3)
})

test_that("eval_text with empty input returns NULL invisibly", {
  engine <- make_engine()
  result <- engine$eval_text("")
  expect_null(result)
})

test_that("eval_string is an alias for eval_text", {
  engine <- make_engine()
  expect_equal(engine$eval_string("(+ 1 2)"), 3)
})

# =============================================================================
# read() + eval()
# =============================================================================

test_that("read() parses source into expression list", {
  engine <- make_engine()
  exprs <- engine$read("(+ 1 2) (* 3 4)")
  expect_length(exprs, 2)
})

test_that("eval() evaluates multiple expressions sequentially", {
  engine <- make_engine()
  exprs <- engine$read("(define ea-x 10) (+ ea-x 5)")
  result <- engine$eval(exprs[[1]], exprs[[2]])
  expect_equal(result, 15)
})

# =============================================================================
# format_value()
# =============================================================================

test_that("format_value formats Arl values as strings", {
  engine <- make_engine()
  engine$eval_text("(define fv-lst (list 1 2 3))")
  result <- engine$format_value(engine$eval_text("fv-lst"))
  expect_true(is.character(result))
  expect_true(nzchar(result))
})

# =============================================================================
# inspect_compilation()
# =============================================================================

test_that("inspect_compilation returns parsed, expanded, compiled, and deparsed", {
  engine <- make_engine()
  result <- engine$inspect_compilation("(+ 1 2)")
  expect_true(is.list(result))
  expect_named(result, c("parsed", "expanded", "compiled", "compiled_deparsed"))
  expect_false(is.null(result$parsed))
  expect_false(is.null(result$compiled))
  expect_true(is.character(result$compiled_deparsed))
})

test_that("inspect_compilation with empty input returns NULLs", {
  engine <- make_engine()
  result <- engine$inspect_compilation("")
  expect_true(all(vapply(result, is.null, logical(1))))
})

# =============================================================================
# write()
# =============================================================================

test_that("write() converts expression to string", {
  engine <- make_engine()
  expr <- engine$read("(+ 1 2)")[[1]]
  result <- engine$write(expr)
  expect_equal(result, "(+ 1 2)")
})

# =============================================================================
# macroexpand()
# =============================================================================

test_that("macroexpand() expands macros in an expression", {
  engine <- make_engine()
  expr <- engine$read("(when #t 42)")[[1]]
  expanded <- engine$macroexpand(expr)
  # when expands to an if form
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "if")
})

# =============================================================================
# load_file_in_env()
# =============================================================================

test_that("load_file_in_env evaluates a file in the engine env", {
  engine <- make_engine()
  path <- tempfile(fileext = ".arl")
  writeLines("(define lf-test-val 77)", path)
  on.exit(unlink(path))

  engine$load_file_in_env(path)
  expect_equal(engine$eval_text("lf-test-val"), 77)
})

# =============================================================================
# import_stdlib()
# =============================================================================

test_that("import_stdlib loads stdlib into a bare engine", {
  engine <- Engine$new(load_stdlib = FALSE)
  # map should not be available yet
  expect_error(engine$eval_text("(map car (list (list 1 2)))"))
  engine$import_stdlib()
  expect_equal(engine$eval_text("(map car (list (list 1 2)))"), list(1))
})
