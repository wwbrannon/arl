# Remaining stdlib tests (tests for string/io helpers, numeric helpers, dict/set,
# defstruct, error/debug, conversions, equality, math, macro expansion, formatting, etc.)
# Core function tests moved to test-stdlib-core.R
# List operation tests moved to test-stdlib-list.R
# Functional tests moved to test-stdlib-functional.R
# Predicate tests moved to test-stdlib-predicates.R
# Sequence tests moved to test-stdlib-sequences.R
# Control flow tests moved to test-stdlib-control.R
# Looping tests moved to test-stdlib-looping.R
# Binding tests moved to test-stdlib-binding.R

engine <- RyeEngine$new()

test_that("stdlib loads successfully", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_true(exists("car", envir = env))
  expect_true(exists("cdr", envir = env))
  expect_true(exists("map", envir = env))
  expect_true(exists("filter", envir = env))
  expect_true(exists("reduce", envir = env))
})

test_that("numeric helpers inc/dec/clamp/within? work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$inc(5), 6)
  expect_equal(env$inc(5, 2), 7)
  expect_equal(env$dec(5), 4)
  expect_equal(env$dec(5, 2), 3)

  expect_equal(env$clamp(5, 1, 10), 5)
  expect_equal(env$clamp(-1, 0, 10), 0)
  expect_equal(env$clamp(11, 0, 10), 10)

  expect_true(env$`within?`(5, 1, 10))
  expect_false(env$`within?`(0, 1, 10))
  expect_false(env$`within?`(11, 1, 10))
})

test_that("string and io helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$str("a", 1, "b"), "a1b")
  expect_equal(env$`format-value`(list(1, 2, 3)), "1 2 3")
  expect_equal(env$`format-value`(quote(f(a, b))), "f a b")
  expect_equal(env$`string-join`(list("a", "b", "c"), "-"), "a-b-c")
  expect_equal(env$`string-split`("a-b-c", "-"), c("a", "b", "c"))
  expect_equal(env$trim("  hi "), "hi")
  expect_equal(env$format("x=%s", "y"), "x=y")

  con <- textConnection("hello")
  old_opts <- options(rye.stdin = con)
  on.exit({
    options(old_opts)
    close(con)
  }, add = TRUE)
  expect_equal(env$`read-line`(), "hello")
})

test_that("format-value handles environments correctly", {
  env <- new.env()
  stdlib_env(engine, env)

  # Plain environment should format as <environment>
  plain_env <- new.env(hash = TRUE)
  expect_equal(env$`format-value`(plain_env), "<environment>")

  # Environment with class should show class name
  classed_env <- new.env()
  class(classed_env) <- c("MyClass", "environment")
  expect_equal(env$`format-value`(classed_env), "<MyClass, environment>")

  # Dict should still format as values (regression test)
  dict <- env$dict(a = 1, b = 2)
  formatted_dict <- env$`format-value`(dict)
  expect_true(grepl("1", formatted_dict))
  expect_true(grepl("2", formatted_dict))

  # Set should still format as values (regression test)
  set_obj <- env$set(1, 2, 3)
  formatted_set <- env$`format-value`(set_obj)
  expect_true(grepl("[123]", formatted_set))

  # Promise should still format as <promise> (regression test)
  promise_obj <- engine$eval_text("(delay 42)")
  expect_equal(env$`format-value`(promise_obj), "<promise>")

  # R6 class if available
  if (requireNamespace("R6", quietly = TRUE)) {
    r6_class <- R6::R6Class("TestClass")
    expect_true(grepl("R6ClassGenerator", env$`format-value`(r6_class)))
  }
})

test_that("format-value for dotted pair (rye_cons) shows dotted form", {
  env <- new.env()
  stdlib_env(engine, env)
  pair <- engine$read("'(a . b)")[[1]][[2]]
  expect_true(r6_isinstance(pair, "RyeCons"))
  formatted <- env$`format-value`(pair)
  expect_true(grepl(" \\. ", formatted))
  expect_true(grepl("a", formatted))
  expect_true(grepl("b", formatted))
})

test_that("format-value for improper list shows dotted tail", {
  env <- new.env()
  stdlib_env(engine, env)
  improper <- engine$read("'(a b . c)")[[1]][[2]]
  expect_true(r6_isinstance(improper, "RyeCons"))
  formatted <- env$`format-value`(improper)
  expect_true(grepl(" \\. ", formatted))
  expect_true(grepl("a", formatted))
  expect_true(grepl("b", formatted))
  expect_true(grepl("c", formatted))
})

test_that("string match helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_true(env$`string-contains?`("hello", "ell"))
  expect_false(env$`string-contains?`("hello", "^ell"))
  expect_true(env$`string-contains?`("hello", "^he", fixed = FALSE))

  expect_true(env$`string-match?`("hello", "^he"))
  expect_false(env$`string-match?`("hello", "ELL"))
  expect_false(env$`string-match?`("hello", "ELL", fixed = TRUE))

  expect_equal(env$`string-find`("hello", "ll"), 2)
  expect_equal(env$`string-find`("hello", "nope"), NULL)
  expect_equal(env$`string-find`("hello", "^he", fixed = FALSE), 0)

  expect_equal(env$`string-replace`("hello", "l", "L"), "heLlo")
  expect_equal(env$`string-replace-all`("hello", "l", "L"), "heLLo")
})

test_that("file io helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  path <- tempfile(fileext = ".txt")
  env$`write-file`(path, "hello")
  expect_true(env$`file-exists?`(path))
  expect_equal(env$`read-file`(path), "hello")

  env$`append-file`(path, list("world", "there"))
  expect_equal(env$`read-file`(path), "hello\nworld\nthere")

  env$`write-lines`(path, list("a", "b", "c"))
  expect_equal(env$`read-lines`(path), list("a", "b", "c"))
})

test_that("dict and set helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  dict <- env$dict(a = 1, b = 2)
  expect_equal(env$`dict-get`(dict, "a"), 1)
  expect_equal(env$`dict-get`(dict, "missing", 99), 99)
  expect_true(env$`dict-has?`(dict, "b"))
  expect_false(env$`dict-has?`(dict, "c"))
  expect_equal(env$`dict-keys`(dict), list("a", "b"))
  expect_equal(env$`dict-values`(dict), list(1, 2))

  updated <- env$`dict-set`(dict, "c", 3)
  expect_equal(env$`dict-get`(updated, "c"), 3)
  removed <- env$`dict-remove`(updated, "a")
  expect_false(env$`dict-has?`(removed, "a"))

  merged <- env$`dict-merge`(env$dict(a = 1, b = 2), env$dict(b = 3, c = 4))
  expect_equal(env$`dict-get`(merged, "a"), 1)
  expect_equal(env$`dict-get`(merged, "b"), 3)
  expect_equal(env$`dict-get`(merged, "c"), 4)

  set_values <- function(set) {
    keys <- ls(envir = set, all.names = TRUE, sorted = FALSE)
    if (length(keys) == 0) {
      return(list())
    }
    as.list(mget(keys, envir = set, inherits = FALSE))
  }
  set <- env$set(1, 2, 2, 3)
  expect_true(env$`set?`(set))
  expect_true(env$`set-contains?`(set, 2))
  expect_false(env$`set-contains?`(set, 4))

  updated_set <- env$`set-add`(set, 4)
  expect_true(env$`set-contains?`(updated_set, 4))

  removed_set <- env$`set-remove`(set, 2)
  expect_false(env$`set-contains?`(removed_set, 2))

  union_set <- env$`set-union`(env$set(1, 2), env$set(2, 3))
  expect_true(env$`set-contains?`(union_set, 1))
  expect_true(env$`set-contains?`(union_set, 2))
  expect_true(env$`set-contains?`(union_set, 3))

  intersection_set <- env$`set-intersection`(env$set(1, 2), env$set(2, 3))
  expect_true(env$`set-contains?`(intersection_set, 2))
  expect_false(env$`set-contains?`(intersection_set, 1))

  difference_set <- env$`set-difference`(env$set(1, 2), env$set(2, 3))
  expect_true(env$`set-contains?`(difference_set, 1))
  expect_false(env$`set-contains?`(difference_set, 2))
  expect_equal(length(set_values(difference_set)), 1)
})

test_that("defstruct macro defines constructor and accessors", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("struct"), env)

  engine$eval_in_env(engine$read("(defstruct Point (x y))")[[1]], env)
  engine$eval_in_env(engine$read("(define p (make-Point 1 2))")[[1]], env)

  expect_true(engine$eval_in_env(engine$read("(Point? p)")[[1]], env))
  expect_equal(engine$eval_in_env(engine$read("(Point-x p)")[[1]], env), 1)
  expect_equal(engine$eval_in_env(engine$read("(Point-y p)")[[1]], env), 2)
})

test_that("error and debug helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_error(env$error("boom"), "boom")
  expect_warning(env$warn("warn"))
  expect_error(env$assert(FALSE, "nope"), "nope")
  expect_true(env$assert(TRUE, "nope"))

  output <- capture.output(env$trace("hi", "label"))
  expect_true(any(grepl("label", output)))
})

test_that("equal? dispatches on class of first argument", {
  env <- new.env()
  stdlib_env(engine, env)

  # Built-in methods: list and environment
  expect_true(env$`equal?`(list(1, 2, 3), list(1, 2, 3)))
  expect_false(env$`equal?`(list(1, 2), list(1, 3)))
  e1 <- new.env()
  e2 <- new.env()
  assign("x", 1, envir = e1)
  assign("x", 1, envir = e2)
  expect_true(env$`equal?`(e1, e2))

  # With a custom method (registered in set-method! test below), list/env still work
  my_a <- structure(list(42), class = "my_thing")
  my_b <- structure(list(42), class = "my_thing")
  env$`set-method!`(as.symbol("equal?"), as.symbol("my_thing"), function(a, b, strict) {
    identical(a[[1]], b[[1]])
  })
  expect_true(env$`equal?`(my_a, my_b))

  # Recursion: equal? on list of custom objects dispatches to custom method for elements
  expect_true(env$`equal?`(list(my_a), list(my_b)))

  # Fallback: class with no method uses equal?.default (no error, returns boolean)
  x <- structure(1, class = "no_method_yet")
  y <- structure(1, class = "no_method_yet")
  expect_true(is.logical(env$`equal?`(x, y)))

  # strict is passed through to the method (use two distinct objects to avoid identical? fast path)
  env$`set-method!`(as.symbol("equal?"), as.symbol("strict_thing"), function(a, b, strict) {
    strict
  })
  s1 <- structure(1, class = "strict_thing")
  s2 <- structure(2, class = "strict_thing")
  expect_true(env$`equal?`(s1, s2, strict = TRUE))
  expect_false(env$`equal?`(s1, s2, strict = FALSE))
})

test_that("set-method! registers and overwrites methods", {
  # Fresh engine so equal? and set-method! share one env (no prior test env / copy)
  eng <- RyeEngine$new()
  env <- stdlib_env(eng)

  # Register a method and use it
  my_a <- structure(list(42), class = "my_thing")
  my_b <- structure(list(42), class = "my_thing")
  my_c <- structure(list(99), class = "my_thing")
  env$`set-method!`(as.symbol("equal?"), as.symbol("my_thing"), function(a, b, strict) {
    identical(a[[1]], b[[1]])
  })
  expect_true(env$`equal?`(my_a, my_b))
  expect_false(env$`equal?`(my_a, my_c))

  # Overwrite: second registration for same generic.class wins.
  # Run entirely in Rye so equal? and set-method! use the same env (no R->Rye closure env subtlety).
  eng$eval_in_env(eng$read('(set-method! (quote equal?) (quote overwrite_test) (lambda (a b strict) #t))')[[1]], env)
  eng$eval_in_env(eng$read("(define o1 (r/call \"structure\" (list (list 1) :class \"overwrite_test\")))")[[1]], env)
  eng$eval_in_env(eng$read("(define o2 (r/call \"structure\" (list (list 1) :class \"overwrite_test\")))")[[1]], env)
  res_first <- eng$eval_in_env(eng$read("(equal? o1 o2)")[[1]], env)
  expect_true(identical(res_first, TRUE))

  eng$eval_in_env(eng$read('(set-method! (quote equal?) (quote overwrite_test) equal?.list)')[[1]], env)
  # Binding must exist in (stdlib-env) after set-method!
  exists_after <- eng$eval_in_env(eng$read('(r/call "exists" (list "equal?.overwrite_test" :envir (stdlib-env)))')[[1]], env)
  expect_true(identical(exists_after, TRUE))
  # set up some objects to use
  eng$eval_in_env(eng$read("(define o3 (r/call \"structure\" (list (list 3) :class \"overwrite_test\")))")[[1]], env)
  eng$eval_in_env(eng$read("(define o4 (r/call \"structure\" (list (list 4) :class \"overwrite_test\")))")[[1]], env)
  # Directly get method from (stdlib-env) and call it: should be the one able to return FALSE
  direct_call <- eng$eval_in_env(eng$read('(begin (define e (stdlib-env)) (define m (r/call "get0" (list "equal?.overwrite_test" :envir e :inherits #f))) (m o3 o4 #f))')[[1]], env)
  expect_identical(direct_call, FALSE)
  res_second <- eng$eval_in_env(eng$read("(equal? o3 o4)")[[1]], env)
  expect_identical(res_second, FALSE)
})

test_that("macroexpand-1 expands macros one level", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define a simple macro
  engine$eval_in_env(engine$read("(defmacro my-when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Expand once
  expr <- engine$read("(my-when #t 42)")[[1]]
  expanded <- env$`macroexpand-1`(expr)

  # Should be an if expression
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "if")
})

test_that("macroexpand fully expands nested macros", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define nested macros
  engine$eval_in_env(engine$read("(defmacro inner (x) `(* ,x 2))")[[1]], env)
  engine$eval_in_env(engine$read("(defmacro outer (y) `(inner (+ ,y 1)))")[[1]], env)

  # Fully expand
  expr <- engine$read("(outer 5)")[[1]]
  expanded <- env$macroexpand(expr)

  # Should be fully expanded to arithmetic
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "*")
})

test_that("macro? predicate identifies macros", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define a macro
  engine$eval_in_env(engine$read("(defmacro test-macro (x) x)")[[1]], env)

  # Test predicate
  expect_true(env$`macro?`(quote(`test-macro`)))
  expect_false(env$`macro?`(quote(`not-a-macro`)))
  expect_false(env$`macro?`(42))
})

# Type conversion tests
test_that("->symbol converts to symbols", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  result <- engine$eval_in_env(engine$read("(->symbol \"foo\")")[[1]], env)
  expect_true(is.symbol(result))
})

test_that("->number converts to numbers", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(->number \"42\")")[[1]], env), 42)
})

test_that("->integer converts to integers", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(->integer \"42\")")[[1]], env), 42L)
  expect_equal(engine$eval_in_env(engine$read("(->integer 3.14)")[[1]], env), 3L)
})

test_that("->double converts to doubles", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(->double 42)")[[1]], env), 42.0)
})

test_that("->complex converts to complex", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  z <- engine$eval_in_env(engine$read("(->complex 42)")[[1]], env)
  expect_equal(Re(z), 42.0)
})

test_that("symbol->string and string->symbol work", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(symbol->string 'foo)")[[1]], env), "foo")
  result <- engine$eval_in_env(engine$read("(string->symbol \"bar\")")[[1]], env)
  expect_true(is.symbol(result))
})

test_that("->list converts vectors to lists", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  result <- engine$eval_in_env(engine$read("(->list 1)")[[1]], env)
  expect_equal(length(result), 1)
})

test_that("->vector converts lists to vectors", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  result <- engine$eval_in_env(engine$read("(->vector '(1 2 3))")[[1]], env)
  expect_equal(length(result), 3)
})

test_that("exact->inexact converts integers to doubles", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(exact->inexact 5)")[[1]], env), 5.0)
})

test_that("inexact->exact converts doubles to integers", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(inexact->exact 5.0)")[[1]], env), 5L)
  expect_equal(engine$eval_in_env(engine$read("(inexact->exact 5.7)")[[1]], env), 5L)
})

test_that("conversion roundtrips work", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  result <- engine$eval_in_env(engine$read("(inexact->exact (exact->inexact 42))")[[1]], env)
  expect_equal(result, 42L)
})

test_that("->integer truncates towards zero", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(->integer 3.7)")[[1]], env), 3L)
  expect_equal(engine$eval_in_env(engine$read("(->integer -3.7)")[[1]], env), -3L)
})

# Equality tests
test_that("identical? tests object identity", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  # eq? and eqv? not implemented - use identical? instead
  engine$eval_in_env(engine$read("(define x '(1 2 3))")[[1]], env)
  expect_true(engine$eval_in_env(engine$read("(identical? x x)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(identical? 'foo 'foo)")[[1]], env))
})

test_that("identical? tests value identity", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  # identical? in R compares values, not storage types
  expect_true(engine$eval_in_env(engine$read("(identical? 42 42)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(identical? 42 42.0)")[[1]], env))  # R coerces these
  expect_false(engine$eval_in_env(engine$read("(identical? 42 43)")[[1]], env))
})

test_that("equal? tests structural equality", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(equal? '(1 2 3) '(1 2 3))")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(equal? '(1 2 3) '(1 2 4))")[[1]], env))
})

test_that("equal? handles nested structures", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(equal? '(1 (2 3)) '(1 (2 3)))")[[1]], env))
})

test_that("equality handles empty collections", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(equal? '() '())")[[1]], env))
})

test_that("equal? compares strings", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(equal? \"hello\" \"hello\")")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(equal? \"hello\" \"world\")")[[1]], env))
})

test_that("equality handles NULL", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  # eq? not implemented - use eqv? or identical? instead
  expect_true(engine$eval_in_env(engine$read("(identical? NULL NULL)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(equal? NULL NULL)")[[1]], env))
})

test_that("identical? and equal? both work for lists", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  # In R, identical? does structural comparison for lists
  expect_true(engine$eval_in_env(engine$read("(identical? '(1 2) '(1 2))")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(equal? '(1 2) '(1 2))")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(equal? '(1 2) '(1 3))")[[1]], env))
})

test_that("equality handles symbols", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  # eq? not implemented - use identical? instead
  expect_true(engine$eval_in_env(engine$read("(identical? 'foo 'foo)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(identical? 'foo 'bar)")[[1]], env))
})

# Complex number tests
test_that("type coercion functions work", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  expect_equal(engine$eval_in_env(engine$read("(exact->inexact 5)")[[1]], env), 5.0)
  expect_equal(engine$eval_in_env(engine$read("(inexact->exact 5.7)")[[1]], env), 5L)
  expect_equal(engine$eval_in_env(engine$read("(->integer \"42\")")[[1]], env), 42L)
  expect_equal(engine$eval_in_env(engine$read("(->double 5)")[[1]], env), 5.0)
})

test_that("complex number utilities work", {
  env <- new.env(parent = emptyenv())
  stdlib_env(engine, env)

  z <- engine$eval_in_env(engine$read("(make-rectangular 3 4)")[[1]], env)
  expect_equal(Re(z), 3.0)
  expect_equal(Im(z), 4.0)

  expect_equal(engine$eval_in_env(engine$read("(real-part (make-rectangular 3 4))")[[1]], env), 3.0)
  expect_equal(engine$eval_in_env(engine$read("(imag-part (make-rectangular 3 4))")[[1]], env), 4.0)
  expect_equal(engine$eval_in_env(engine$read("(magnitude (make-rectangular 3 4))")[[1]], env), 5.0)
})
