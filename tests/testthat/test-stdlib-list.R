# List operation tests: car/cdr, cons, append, reverse, ordinal accessors, assoc

engine <- make_engine()

test_that("car returns first element", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Test with R list
  expect_equal(get("car", envir = env)(list(1, 2, 3)), 1)

  # Test with parsed expression
  expr <- engine$read("(+ 1 2)")[[1]]
  expect_equal(as.character(get("car", envir = env)(expr)), "+")
})

test_that("cdr returns rest of list", {
  env <- new.env()
  toplevel_env(engine, env = env)

  # Test with R list
  result <- get("cdr", envir = env)(list(1, 2, 3))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 3)
})

test_that("common composed list accessors work (cadr, caddr, caar, cdar, ...)", {
  env <- toplevel_env(engine, new.env())

  # From list values
  expect_equal(engine$eval(engine$read("(begin (import list) (cadr (list 1 2 3 4)))")[[1]], env = env), 2)
  expect_equal(engine$eval(engine$read("(begin (import list) (caddr (list 1 2 3 4)))")[[1]], env = env), 3)
  expect_equal(engine$eval(engine$read("(begin (import list) (cadddr (list 1 2 3 4)))")[[1]], env = env), 4)
  expect_equal(
    engine$eval(engine$read("(begin (import list) (caar (list (list 10 11) (list 20 21))))")[[1]], env = env),
    10
  )
  expect_equal(
    engine$eval(engine$read("(begin (import list) (cdar (list (list 10 11) (list 20 21))))")[[1]], env = env),
    list(11)
  )
  expect_equal(
    engine$eval(engine$read("(begin (import list) (cddr (list 1 2 3 4)))")[[1]], env = env),
    list(3, 4)
  )

  # From quoted calls (call objects)
  expect_equal(
    engine$eval(engine$read("(begin (import list) (cadr '(+ 1 2 3)))")[[1]], env = env),
    1
  )
  expect_equal(
    engine$eval(engine$read("(begin (import list) (caddr '(+ 1 2 3)))")[[1]], env = env),
    2
  )
  expect_equal(
    engine$eval(engine$read("(begin (import list) (cadddr '(+ 1 2 3)))")[[1]], env = env),
    3
  )
})

test_that("ordinal list accessors work (second, third, fourth)", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("second", envir = env)(list(1, 2, 3)), 2)
  expect_equal(get("third", envir = env)(list(1, 2, 3, 4)), 3)
  expect_equal(get("fourth", envir = env)(list(1, 2, 3, 4)), 4)

  expect_null(get("second", envir = env)(list(1)))
  expect_null(get("third", envir = env)(list(1, 2)))
  expect_null(get("fourth", envir = env)(list(1, 2, 3)))
})

test_that("first is an alias for car", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("first", envir = env)(list(1, 2, 3)), 1)
  expect_null(get("first", envir = env)(list()))
  expect_equal(get("first", envir = env)(list("a", "b")), "a")
})

test_that("rest is an alias for cdr", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("rest", envir = env)(list(1, 2, 3)), list(2, 3))
  expect_equal(get("rest", envir = env)(list(1)), list())
  expect_equal(get("rest", envir = env)(list()), list())
})

test_that("last returns last element", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("last", envir = env)(list(1, 2, 3)), 3)
  expect_equal(get("last", envir = env)(list(42)), 42)
  expect_null(get("last", envir = env)(list()))
  expect_equal(get("last", envir = env)(list("a", "b", "c")), "c")
})

test_that("nth returns element at index", {
  env <- new.env()
  toplevel_env(engine, env = env)

  lst <- list(10, 20, 30, 40)

  # 0-indexed
  expect_equal(get("nth", envir = env)(lst, 0), 10)
  expect_equal(get("nth", envir = env)(lst, 1), 20)
  expect_equal(get("nth", envir = env)(lst, 2), 30)
  expect_equal(get("nth", envir = env)(lst, 3), 40)

  # Out of bounds
  expect_error(get("nth", envir = env)(lst, -1), "out of bounds")
  expect_error(get("nth", envir = env)(lst, 4), "out of bounds")
})

test_that("assoc family: assoc, assoc-by-equal?, assoc-by-identical?, assoc-by-==, rassoc, rassoc-by-equal?", {
  env <- toplevel_env(engine, new.env())

  # assoc (equal?) and assoc-by-equal? (alias)
  alist <- list(list(quote(a), 1), list(quote(b), 2), list(quote(c), 3))
  expect_equal(get("assoc", envir = env)(quote(b), alist), list(quote(b), 2))
  expect_equal(get("assoc-by-equal?", envir = env)(quote(b), alist), list(quote(b), 2))

  # assoc-by-identical? uses R's identical()
  key <- quote(k)
  alist_id <- list(list(key, 1))
  expect_equal(get("assoc-by-identical?", envir = env)(key, alist_id), list(quote(k), 1))

  # assoc-by-== uses R's == (e.g. 1 and 1L match)
  alist_num <- list(list(1, "one"), list(2, "two"), list(3, "three"))
  expect_equal(get("assoc-by-==", envir = env)(1, alist_num), list(1, "one"))
  expect_equal(get("assoc-by-==", envir = env)(1L, alist_num), list(1, "one"))

  # rassoc and rassoc-by-equal? (alias)
  expect_equal(get("rassoc", envir = env)(2, alist), list(quote(b), 2))
  expect_equal(get("rassoc-by-equal?", envir = env)(2, alist), list(quote(b), 2))
})

test_that("assq and assv error (cannot implement eq?/eqv? in R)", {
  env <- toplevel_env(engine, new.env())
  expect_error(
    engine$eval(engine$read("(assq 'x (list (list 'x 1)))")[[1]], env = env),
    "assq cannot be properly implemented"
  )
  expect_error(
    engine$eval(engine$read("(assv 5 (list (list 5 \"five\")))")[[1]], env = env),
    "assv cannot be properly implemented"
  )
})

test_that("cons adds element to front", {
  env <- new.env()
  toplevel_env(engine, env = env)

  result <- get("cons", envir = env)(1, list(2, 3))
  expect_equal(result[[1]], 1)
  expect_equal(result[[2]], 2)
  expect_equal(result[[3]], 3)
})

test_that("cons with non-list cdr produces dotted pair (arl_cons)", {
  env <- toplevel_env(engine, new.env())
  result <- engine$eval(engine$read("(cons 'a 'b)")[[1]], env = env)
  expect_true(r6_isinstance(result, "Cons"))
  expect_equal(as.character(result$car), "a")
  expect_equal(as.character(result$cdr), "b")
})

test_that("car and cdr on dotted pair", {
  env <- toplevel_env(engine, new.env())
  pair <- engine$eval(engine$read("'(a . 42)")[[1]], env = env)
  expect_equal(as.character(get("car", envir = env)(pair)), "a")
  expect_equal(get("cdr", envir = env)(pair), 42)
})

test_that("list? is false but pair? is true for dotted pair (Cons)", {
  env <- toplevel_env(engine, new.env())
  pair <- engine$eval(engine$read("(cons 1 2)")[[1]], env = env)
  expect_false(get("list?", envir = env)(pair))
  expect_true(get("pair?", envir = env)(pair))  # pair? = dotted pair (Cons)
})

test_that("__as-list on improper list returns proper prefix only", {
  env <- toplevel_env(engine, new.env())
  pl <- engine$read("'(a b . c)")[[1]][[2]]
  expect_true(r6_isinstance(pl, "Cons"))
  prefix <- get("__as-list", envir = env)(pl)
  expect_equal(length(prefix), 2)
  expect_equal(as.character(prefix[[1]]), "a")
  expect_equal(as.character(prefix[[2]]), "b")
})

test_that("append combines lists", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("append", envir = env)(list(1, 2), list(3)), list(1, 2, 3))
})

test_that("reverse reverses list order", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("reverse", envir = env)(list(1, 2, 3)), list(3, 2, 1))
})

test_that("list* constructs list with final element as tail", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_equal(get("list*", envir = env)(1, list(2, 3)), list(1, 2, 3))
})

# ============================================================================
# NEW: High-priority list operation tests
# ============================================================================

test_that("range generates numeric sequences", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("list"), env = env)

  # Basic range
  expect_equal(
    engine$eval(engine$read("(range 0 5)")[[1]], env = env),
    list(0, 1, 2, 3, 4))

  # Range with step
  expect_equal(
    engine$eval(engine$read("(range 0 10 2)")[[1]], env = env),
    list(0, 2, 4, 6, 8))

  # Negative step (descending)
  expect_equal(
    engine$eval(engine$read("(range 10 0 -2)")[[1]], env = env),
    list(10, 8, 6, 4, 2))

  # Empty range (start >= end with positive step)
  expect_equal(
    engine$eval(engine$read("(range 5 5)")[[1]], env = env),
    list())

  # Single element range
  expect_equal(
    engine$eval(engine$read("(range 0 1)")[[1]], env = env),
    list(0))
})

test_that("iota generates sequences with count", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("list"), env = env)

  # Basic iota (count from 0)
  expect_equal(
    engine$eval(engine$read("(iota 5)")[[1]], env = env),
    list(0, 1, 2, 3, 4))

  # iota with custom start
  expect_equal(
    engine$eval(engine$read("(iota 5 10)")[[1]], env = env),
    list(10, 11, 12, 13, 14))

  # iota with custom start and step
  expect_equal(
    engine$eval(engine$read("(iota 5 0 2)")[[1]], env = env),
    list(0, 2, 4, 6, 8))

  # Empty iota (count <= 0)
  expect_equal(
    engine$eval(engine$read("(iota 0)")[[1]], env = env),
    list())

  # Single element
  expect_equal(
    engine$eval(engine$read("(iota 1)")[[1]], env = env),
    list(0))
})

test_that("make-list creates repeated values", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("list"), env = env)

  # Repeat number
  expect_equal(
    engine$eval(engine$read("(make-list 5 42)")[[1]], env = env),
    list(42, 42, 42, 42, 42))

  # Repeat symbol
  result <- engine$eval(engine$read("(make-list 3 'x)")[[1]], env = env)
  expect_equal(length(result), 3)
  expect_equal(as.character(result[[1]]), "x")

  # Zero length
  expect_equal(
    engine$eval(engine$read("(make-list 0 42)")[[1]], env = env),
    list())

  # Single element
  expect_equal(
    engine$eval(engine$read("(make-list 1 99)")[[1]], env = env),
    list(99))
})

test_that("list-ref accesses list by index", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("list"), env = env)

  # list-ref is an alias for nth (0-indexed)
  expect_equal(
    engine$eval(engine$read("(list-ref '(10 20 30 40) 0)")[[1]], env = env),
    10)

  expect_equal(
    engine$eval(engine$read("(list-ref '(10 20 30 40) 2)")[[1]], env = env),
    30)

  # Out of bounds should error
  expect_error(
    engine$eval(engine$read("(list-ref '(1 2 3) 5)")[[1]], env = env),
    "out of bounds")
})

test_that("list-tail returns list without first k elements", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("list"), env = env)

  # Drop first 2 elements
  expect_equal(
    engine$eval(engine$read("(list-tail '(1 2 3 4 5) 2)")[[1]], env = env),
    list(3, 4, 5))

  # Drop all elements
  expect_equal(
    engine$eval(engine$read("(list-tail '(1 2 3) 3)")[[1]], env = env),
    list())

  # Drop more than length
  expect_equal(
    engine$eval(engine$read("(list-tail '(1 2 3) 10)")[[1]], env = env),
    list())

  # Drop 0 elements
  expect_equal(
    engine$eval(engine$read("(list-tail '(1 2 3) 0)")[[1]], env = env),
    list(1, 2, 3))
})

# ============================================================================
# Coverage: range zero-step error, nth negative index error
# ============================================================================

test_that("range errors when step is zero", {
  env <- toplevel_env(engine, new.env())
  import_stdlib_modules(engine, c("list"), env = env)

  expect_error(
    engine$eval(engine$read("(range 1 10 0)")[[1]], env = env),
    "step cannot be zero")
})

test_that("nth errors on negative index", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_error(get("nth", envir = env)(list(1, 2, 3), -1), "out of bounds")
})
