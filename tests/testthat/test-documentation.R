test_that("doc! macro attaches docstrings to functions", {
  engine <- make_engine()
  env <- engine$get_env()

  # Create a test function
  engine$eval(engine$read('(define test-fn (lambda (x) (* x 2)))')[[1]], env = env)

  # Attach docstring
  engine$eval(engine$read('(doc! test-fn "Doubles the input value.")')[[1]], env = env)

  # Retrieve and verify
  result <- engine$eval(engine$read('(doc test-fn)')[[1]], env = env)
  expect_equal(result, "Doubles the input value.")
})

test_that("doc returns NULL for undocumented functions", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define no-doc-fn (lambda (x) x))')[[1]], env = env)

  result <- engine$eval(engine$read('(doc no-doc-fn)')[[1]], env = env)
  expect_null(result)
})

test_that("doc! works with direct assignment functions", {
  engine <- make_engine()
  env <- engine$get_env()

  # Direct assignment from R function
  engine$eval(engine$read('(define my-abs abs)')[[1]], env = env)
  engine$eval(engine$read('(doc! my-abs "My absolute value function")')[[1]], env = env)

  result <- engine$eval(engine$read('(doc my-abs)')[[1]], env = env)
  expect_equal(result, "My absolute value function")
})

test_that("doc! updates existing docstrings", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define fn-with-doc (lambda (x) (+ x 1)))')[[1]], env = env)

  # First docstring
  engine$eval(engine$read('(doc! fn-with-doc "Original doc")')[[1]], env = env)
  result1 <- engine$eval(engine$read('(doc fn-with-doc)')[[1]], env = env)
  expect_equal(result1, "Original doc")

  # Update docstring
  engine$eval(engine$read('(doc! fn-with-doc "Updated doc")')[[1]], env = env)
  result2 <- engine$eval(engine$read('(doc fn-with-doc)')[[1]], env = env)
  expect_equal(result2, "Updated doc")
})

test_that("inline strings in lambda body are not stripped as docstrings", {
  engine <- make_engine()
  env <- engine$get_env()

  # Lambda with a string as first body expression — not treated as docstring
  engine$eval(engine$read('(define test-fn (lambda (x) "This is just a string" (* x 2)))')[[1]], env = env)

  fn <- engine$eval(engine$read('test-fn')[[1]], env = env)
  doc_attr <- attr(fn, "arl_doc", exact = TRUE)

  # No arl_doc attribute — strings are not treated as docstrings
  expect_null(doc_attr)
  # The function still works (string is evaluated and discarded)
  result <- engine$eval(engine$read('(test-fn 3)')[[1]], env = env)
  expect_equal(result, 6)
})

test_that("optimized math functions have docstrings", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(import math)')[[1]], env = env)

  # Test a few optimized functions
  abs_doc <- engine$eval(engine$read('(doc abs)')[[1]], env = env)
  expect_match(abs_doc, "absolute value", ignore.case = TRUE)

  sqrt_doc <- engine$eval(engine$read('(doc sqrt)')[[1]], env = env)
  expect_match(sqrt_doc, "square root", ignore.case = TRUE)

  sin_doc <- engine$eval(engine$read('(doc sin)')[[1]], env = env)
  expect_match(sin_doc, "sine", ignore.case = TRUE)
})

test_that("optimized predicate functions have docstrings", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(import types)')[[1]], env = env)

  symbol_doc <- engine$eval(engine$read('(doc symbol?)')[[1]], env = env)
  expect_match(symbol_doc, "symbol", ignore.case = TRUE)

  number_doc <- engine$eval(engine$read('(doc number?)')[[1]], env = env)
  expect_match(number_doc, "number", ignore.case = TRUE)
})

test_that("optimized string functions have docstrings", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(import strings)')[[1]], env = env)

  trim_doc <- engine$eval(engine$read('(doc trim)')[[1]], env = env)
  expect_match(trim_doc, "trim", ignore.case = TRUE)

  to_string_doc <- engine$eval(engine$read('(doc ->string)')[[1]], env = env)
  expect_match(to_string_doc, "string", ignore.case = TRUE)
})

# Regression tests: nested imports must not clobber the outer module's
# doc annotations. Modules that import other modules should still have
# their own ;;' doc comments attached to their exported functions.

test_that("docstrings survive nested imports (strings imports list, math, core)", {
  engine <- make_engine()
  env <- engine$get_env()
  engine$eval(engine$read('(import strings)')[[1]], env = env)

  # trim is defined after (import list), (import math), (import core)
  # so its ;;' annotation must survive those nested module loads
  trim_fn <- get("trim", envir = env, inherits = TRUE)
  expect_false(is.null(attr(trim_fn, "arl_doc", exact = TRUE)))
  expect_match(attr(trim_fn, "arl_doc")$description, "whitespace", ignore.case = TRUE)

  # format is defined later in the file — also after all imports
  fmt_fn <- get("format", envir = env, inherits = TRUE)
  expect_false(is.null(attr(fmt_fn, "arl_doc", exact = TRUE)))
  expect_match(attr(fmt_fn, "arl_doc")$description, "sprintf|format", ignore.case = TRUE)
})

test_that("docstrings survive nested imports across multiple modules", {
  engine <- make_engine()
  env <- engine$get_env()

  # Import several non-prelude modules that each have nested imports
  for (mod in c("strings", "sort", "dict")) {
    engine$eval(engine$read(sprintf('(import %s)', mod))[[1]], env = env)
  }

  # strings: trim (after 3 nested imports)
  expect_false(is.null(attr(get("trim", envir = env, inherits = TRUE), "arl_doc")))
  # sort: sort (imports list, types, core)
  sort_fn <- get("sort", envir = env, inherits = TRUE)
  expect_false(is.null(attr(sort_fn, "arl_doc", exact = TRUE)))
  # dict: dict (imports list, types, core, equality)
  dict_fn <- get("dict", envir = env, inherits = TRUE)
  expect_false(is.null(attr(dict_fn, "arl_doc", exact = TRUE)))
})

# --- New tests for expanded doc!/doc API ---

test_that("doc! sets specific fields with keyword args", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define my-fn (lambda (x) (* x 2)))')[[1]], env = env)
  engine$eval(engine$read('(doc! my-fn :examples "(my-fn 3) ; => 6")')[[1]], env = env)

  result <- engine$eval(engine$read('(doc my-fn "examples")')[[1]], env = env)
  expect_equal(result, "(my-fn 3) ; => 6")

  # description should be NULL since we only set examples
  desc <- engine$eval(engine$read('(doc my-fn)')[[1]], env = env)
  expect_null(desc)
})

test_that("doc! sets multiple fields with keyword args", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define my-fn (lambda (x) (* x 2)))')[[1]], env = env)
  engine$eval(engine$read('(doc! my-fn :description "Doubles x." :examples "(my-fn 3) ; => 6" :note "Fast.")')[[1]], env = env)

  expect_equal(engine$eval(engine$read('(doc my-fn)')[[1]], env = env), "Doubles x.")
  expect_equal(engine$eval(engine$read('(doc my-fn "examples")')[[1]], env = env), "(my-fn 3) ; => 6")
  expect_equal(engine$eval(engine$read('(doc my-fn "note")')[[1]], env = env), "Fast.")
})

test_that("doc! merges fields — setting description then examples preserves both", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define my-fn (lambda (x) (* x 2)))')[[1]], env = env)

  # Set description first
  engine$eval(engine$read('(doc! my-fn "Doubles x.")')[[1]], env = env)
  # Then add examples via keyword
  engine$eval(engine$read('(doc! my-fn :examples "(my-fn 3) ; => 6")')[[1]], env = env)

  # Both should be present
  expect_equal(engine$eval(engine$read('(doc my-fn)')[[1]], env = env), "Doubles x.")
  expect_equal(engine$eval(engine$read('(doc my-fn "examples")')[[1]], env = env), "(my-fn 3) ; => 6")
})

test_that("doc with 'all' returns full named list", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define my-fn (lambda (x) (* x 2)))')[[1]], env = env)
  engine$eval(engine$read('(doc! my-fn :description "Doubles x." :examples "(my-fn 3)")')[[1]], env = env)

  result <- engine$eval(engine$read('(doc my-fn "all")')[[1]], env = env)
  expect_type(result, "list")
  expect_equal(result$description, "Doubles x.")
  expect_equal(result$examples, "(my-fn 3)")
})

test_that("doc returns NULL for non-existent field", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define my-fn (lambda (x) x))')[[1]], env = env)
  engine$eval(engine$read('(doc! my-fn "Some desc.")')[[1]], env = env)

  result <- engine$eval(engine$read('(doc my-fn "examples")')[[1]], env = env)
  expect_null(result)
})

test_that("doc! works on primitives", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define my-sum sum)')[[1]], env = env)
  engine$eval(engine$read('(doc! my-sum :description "Sum values." :note "Wraps primitive.")')[[1]], env = env)

  expect_equal(engine$eval(engine$read('(doc my-sum)')[[1]], env = env), "Sum values.")
  expect_equal(engine$eval(engine$read('(doc my-sum "note")')[[1]], env = env), "Wraps primitive.")
  # Function should still work after wrapping
  result <- engine$eval(engine$read('(my-sum (c 1 2 3))')[[1]], env = env)
  expect_equal(result, 6)
})

test_that("@internal flag is present in arl_doc at runtime", {
  engine <- make_engine()
  env <- engine$get_env()

  code <- '(module int-test-mod
  (export __int-helper int-pub-fn)

  ;;\' @internal
  ;;\' @description Internal helper.
  (define __int-helper (lambda (x) x))

  ;;\' @description Public function.
  (define int-pub-fn (lambda (x) x))
)'

  child <- new.env(parent = env)
  engine$eval_text(code, env = child)
  engine$eval_text("(import int-test-mod)", env = child)

  helper <- get("__int-helper", envir = child)
  doc_h <- attr(helper, "arl_doc", exact = TRUE)
  expect_true(isTRUE(doc_h$internal))

  pub <- get("int-pub-fn", envir = child)
  doc_p <- attr(pub, "arl_doc", exact = TRUE)
  expect_null(doc_p$internal)
})

test_that("@noeval flag is present in arl_doc at runtime", {
  engine <- make_engine()
  env <- engine$get_env()

  code <- '(module noeval-test-mod
  (export io-fn pure-fn)

  ;;\' @noeval
  ;;\' @description Reads a file.
  ;;\' @examples
  ;;\' (io-fn "test.txt")
  (define io-fn (lambda (path) path))

  ;;\' @description Pure function.
  ;;\' @examples
  ;;\' (pure-fn 42)
  (define pure-fn (lambda (x) x))
)'

  engine$eval_text(code, env = env)
  engine$eval_text("(import noeval-test-mod)", env = env)

  io <- get("io-fn", envir = env)
  doc_io <- attr(io, "arl_doc", exact = TRUE)
  expect_true(isTRUE(doc_io$noeval))

  pure <- get("pure-fn", envir = env)
  doc_pure <- attr(pure, "arl_doc", exact = TRUE)
  expect_null(doc_pure$noeval)
})

test_that("@noeval flag round-trips through doc/doc!", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define my-fn (lambda (x) x))')[[1]], env = env)
  engine$eval(engine$read('(doc! my-fn :noeval #t)')[[1]], env = env)

  result <- engine$eval(engine$read('(doc my-fn "noeval")')[[1]], env = env)
  expect_true(result)
})

test_that("stdlib @noeval functions carry the flag at runtime", {
  engine <- make_engine()
  env <- toplevel_env(engine)

  rf <- get("read-file", envir = env)
  doc <- attr(rf, "arl_doc", exact = TRUE)
  expect_true(isTRUE(doc$noeval))
})

test_that("doc! keyword args evaluate variable values", {
  engine <- make_engine()
  env <- engine$get_env()

  engine$eval(engine$read('(define my-fn (lambda (x) x))')[[1]], env = env)
  engine$eval(engine$read('(define d "computed description")')[[1]], env = env)
  engine$eval(engine$read('(doc! my-fn :description d)')[[1]], env = env)

  result <- engine$eval(engine$read('(doc my-fn)')[[1]], env = env)
  expect_equal(result, "computed description")
})

test_that("annotation docs attach complete arl_doc for functions and macros", {
  engine <- make_engine()
  env <- engine$get_env()

  code <- '(module doc-complete-mod
  (export f m)

  ;;\' @description Function docs.
  ;;\' @signature (f x)
  ;;\' @examples
  ;;\' (f 2) ; => 4
  ;;\' @assert (assert-equal 4 (f 2))
  ;;\' @seealso m
  ;;\' @note Function note.
  ;;\' @internal
  ;;\' @noeval
  (define f (lambda (x) (* x 2)))

  ;;\' @description Macro docs.
  ;;\' @signature (m x)
  ;;\' @examples
  ;;\' (m 2) ; => 2
  ;;\' @assert (assert-equal 2 (m 2))
  ;;\' @seealso f
  ;;\' @note Macro note.
  ;;\' @internal
  ;;\' @noeval
  (defmacro m (x) x)
)'

  child <- new.env(parent = env)
  engine$eval_text(code, env = child)
  engine$eval_text("(import doc-complete-mod)", env = child)

  fn <- get("f", envir = child)
  fn_doc <- attr(fn, "arl_doc", exact = TRUE)
  expect_equal(fn_doc$description, "Function docs.")
  expect_equal(fn_doc$signature, "(f x)")
  expect_match(fn_doc$examples, "\\(f 2\\)")
  expect_match(fn_doc$assert, "\\(assert-equal 4")
  expect_equal(fn_doc$seealso, "m")
  expect_equal(fn_doc$note, "Function note.")
  expect_true(isTRUE(fn_doc$internal))
  expect_true(isTRUE(fn_doc$noeval))

  macro <- get("m", envir = child)
  macro_doc <- attr(macro, "arl_doc", exact = TRUE)
  expect_equal(macro_doc$description, "Macro docs.")
  expect_equal(macro_doc$signature, "(m x)")
  expect_match(macro_doc$examples, "\\(m 2\\)")
  expect_match(macro_doc$assert, "\\(assert-equal 2")
  expect_equal(macro_doc$seealso, "f")
  expect_equal(macro_doc$note, "Macro note.")
  expect_true(isTRUE(macro_doc$internal))
  expect_true(isTRUE(macro_doc$noeval))
})
