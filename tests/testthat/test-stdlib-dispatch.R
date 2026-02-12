# Comprehensive generic method dispatch tests

engine <- make_engine()

test_that("equal? dispatches on class of first argument", {
  env <- new.env()
  toplevel_env(engine, env)

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

test_that("use-method dispatches using generic-name parameter (not hardcoded to equal?)", {
  eng <- make_engine()
  env <- toplevel_env(eng)

  # Register a custom generic (not equal?) via set-method!
  # Use (begin ...) to avoid string being consumed as docstring
  eng$eval_in_env(eng$read('(set-method! (quote describe) (quote my_obj) (lambda (a) (begin "my_obj description")))')[[1]], env)

  # Register a default method for describe
  eng$eval_in_env(eng$read('(define describe.default (lambda (a) (begin "default description")))')[[1]], env)

  # Create an object of class my_obj
  eng$eval_in_env(eng$read('(define obj (r/call "structure" (list (list 1) :class "my_obj")))')[[1]], env)

  # use-method with "describe" should find describe.my_obj, not equal?.my_obj
  result <- eng$eval_in_env(eng$read('(use-method "describe" obj (list obj))')[[1]], env)
  expect_equal(result, "my_obj description")

  # use-method with unknown class should fall back to describe.default
  eng$eval_in_env(eng$read('(define other (r/call "structure" (list (list 2) :class "unknown_cls")))')[[1]], env)
  result <- eng$eval_in_env(eng$read('(use-method "describe" other (list other))')[[1]], env)
  expect_equal(result, "default description")
})

test_that("set-method! registers and overwrites methods", {
  # Fresh engine so equal? and set-method! share one env (no prior test env / copy)
  eng <- make_engine()
  env <- toplevel_env(eng)

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
  # Run entirely in Arl so equal? and set-method! use the same env (no R->Arl closure env subtlety).
  eng$eval_in_env(eng$read('(set-method! (quote equal?) (quote overwrite_test) (lambda (a b strict) #t))')[[1]], env)
  eng$eval_in_env(eng$read("(define o1 (r/call \"structure\" (list (list 1) :class \"overwrite_test\")))")[[1]], env)
  eng$eval_in_env(eng$read("(define o2 (r/call \"structure\" (list (list 1) :class \"overwrite_test\")))")[[1]], env)
  res_first <- eng$eval_in_env(eng$read("(equal? o1 o2)")[[1]], env)
  expect_true(identical(res_first, TRUE))

  eng$eval_in_env(eng$read('(set-method! (quote equal?) (quote overwrite_test) equal?.list)')[[1]], env)
  # Binding must exist in (toplevel-env) after set-method!
  exists_after <- eng$eval_in_env(eng$read('(r/call "exists" (list "equal?.overwrite_test" :envir (toplevel-env)))')[[1]], env)
  expect_true(identical(exists_after, TRUE))
  # set up some objects to use
  eng$eval_in_env(eng$read("(define o3 (r/call \"structure\" (list (list 3) :class \"overwrite_test\")))")[[1]], env)
  eng$eval_in_env(eng$read("(define o4 (r/call \"structure\" (list (list 4) :class \"overwrite_test\")))")[[1]], env)
  # Directly get method from (toplevel-env) and call it: should be the one able to return FALSE
  direct_call <- eng$eval_in_env(eng$read('(begin (define e (toplevel-env)) (define m (r/call "get0" (list "equal?.overwrite_test" :envir e :inherits #f))) (m o3 o4 #f))')[[1]], env)
  expect_identical(direct_call, FALSE)
  res_second <- eng$eval_in_env(eng$read("(equal? o3 o4)")[[1]], env)
  expect_identical(res_second, FALSE)
})
