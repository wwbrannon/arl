# Type predicates and boolean tests from types.arl, math.arl, logic.arl

engine <- make_engine()

thin <- make_cran_thinner()

test_that("predicates work correctly", {
  thin()
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(number? 42)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(number? \"hello\")")[[1]], env = env))

  expect_true(engine$eval(engine$read("(string? \"hello\")")[[1]], env = env))
  expect_false(engine$eval(engine$read("(string? 42)")[[1]], env = env))

  expect_true(engine$eval(engine$read("(null? #nil)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(null? 42)")[[1]], env = env))
})

test_that("extended predicates work correctly", {
  thin()
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(get("boolean?", envir = env)(TRUE))
  expect_true(get("boolean?", envir = env)(FALSE))
  expect_false(get("boolean?", envir = env)(c(TRUE, FALSE)))
  expect_false(get("boolean?", envir = env)(1))

  expect_true(engine$eval(engine$read("(xor #t #f)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(xor #t #t)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(xor #f #f)")[[1]], env = env))

  expect_true(get("even?", envir = env)(4))
  expect_true(get("odd?", envir = env)(5))
  expect_true(get("zero?", envir = env)(0))
  expect_true(get("positive?", envir = env)(3))
  expect_true(get("negative?", envir = env)(-1))
  expect_true(get("non-negative?", envir = env)(0))
  expect_true(get("non-positive?", envir = env)(0))

  expect_true(get("integer?", envir = env)(2))
  expect_false(get("integer?", envir = env)(2.5))
  expect_true(get("natural?", envir = env)(0))
  expect_false(get("natural?", envir = env)(-1))

  expect_true(get("finite?", envir = env)(1))
  expect_false(get("finite?", envir = env)(Inf))
  expect_true(get("infinite?", envir = env)(Inf))
  expect_false(get("infinite?", envir = env)(1))
  expect_true(get("nan?", envir = env)(NaN))
  expect_false(get("nan?", envir = env)(1))

  expect_true(get("empty?", envir = env)(list()))
  expect_true(get("empty?", envir = env)(NULL))
  expect_false(get("empty?", envir = env)(""))
  expect_true(get("empty?", envir = env)(character()))
  expect_true(get("empty?", envir = env)(c()))
  expect_false(get("empty?", envir = env)(list(1)))
  expect_false(get("empty?", envir = env)("x"))
  expect_false(get("empty?", envir = env)(c(1)))

  expect_true(get("length=", envir = env)(list(1, 2, 3), 3))
  expect_true(get("length>", envir = env)(list(1, 2, 3), 2))
  expect_true(get("length<", envir = env)(list(1, 2, 3), 4))
})

test_that("numeric tower predicates work correctly", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # number? includes complex
  expect_true(engine$eval(engine$read("(number? 42)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(number? (complex :real 3 :imaginary 4))")[[1]], env = env))

  # real? includes infinities but not complex
  expect_true(engine$eval(engine$read("(real? Inf)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(real? (complex :real 3 :imaginary 4))")[[1]], env = env))

  # rational? excludes infinities
  expect_false(engine$eval(engine$read("(rational? Inf)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(rational? 3.14)")[[1]], env = env))

  # exact? and inexact?
  expect_true(engine$eval(engine$read("(exact? 5L)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(exact? 5.0)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(inexact? 5.0)")[[1]], env = env))
})

test_that("list? identifies lists correctly", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(list? '(1 2 3))")[[1]], env = env))
  expect_true(engine$eval(engine$read("(list? '())")[[1]], env = env))
  expect_false(engine$eval(engine$read("(list? 42)")[[1]], env = env))
})

test_that("symbol? identifies symbols correctly", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(symbol? 'foo)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(symbol? \"foo\")")[[1]], env = env))
})

test_that("number? identifies all numeric types", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(number? 42)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(number? 3.14)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(number? \"42\")")[[1]], env = env))
})

test_that("string? identifies strings correctly", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(string? \"hello\")")[[1]], env = env))
  expect_false(engine$eval(engine$read("(string? 'hello)")[[1]], env = env))
})

test_that("vector? identifies R vectors", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(vector? 42)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(vector? '(1 2))")[[1]], env = env))
})

test_that("boolean? identifies logical values", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(boolean? #t)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(boolean? #f)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(boolean? 1)")[[1]], env = env))
})

test_that("atom? identifies non-compound values", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(atom? 42)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(atom? '(1 2))")[[1]], env = env))
})

test_that("empty? identifies zero-length collections", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(empty? '())")[[1]], env = env))
  expect_false(engine$eval(engine$read("(empty? '(1 2))")[[1]], env = env))
})

test_that("null? identifies NULL and empty lists", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(null? '())")[[1]], env = env))
  expect_true(engine$eval(engine$read("(null? NULL)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(null? 0)")[[1]], env = env))
})

test_that("procedure? identifies functions", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(procedure? +)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(procedure? 42)")[[1]], env = env))
})

test_that("environment? identifies environments", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(environment? (new.env))")[[1]], env = env))
  expect_false(engine$eval(engine$read("(environment? 42)")[[1]], env = env))
})

test_that("fn? and callable? are function predicates", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(fn? +)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(callable? (lambda (x) x))")[[1]], env = env))
})

test_that("not returns logical negation", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(not #f)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(not #t)")[[1]], env = env))
})

test_that("not treats #f and 0 as false", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(not 0)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(not #f)")[[1]], env = env))
})

test_that("xor implements exclusive or", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_false(engine$eval(engine$read("(xor #f #f)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(xor #f #t)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(xor #t #t)")[[1]], env = env))
})

test_that("xor works with truthy values", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(xor #f 1)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(xor 1 2)")[[1]], env = env))
})

test_that("predicates and interop helpers work", {
  thin()
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(get("list-or-pair?", envir = env)(list(1)))
  expect_false(get("list-or-pair?", envir = env)(list()))
  expect_true(get("keyword?", envir = env)(structure("from", class = "arl_keyword")))
  expect_true(get("vector?", envir = env)(c(1, 2, 3)))
  expect_true(get("true?", envir = env)(TRUE))
  expect_true(get("false?", envir = env)(FALSE))
  expect_true(get("fn?", envir = env)(function(x) x))
  expect_true(get("callable?", envir = env)(function(x) x))
})

test_that("real? excludes complex numbers", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(real? 42)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(real? Inf)")[[1]], env = env))
})

test_that("rational? excludes infinities", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(rational? 42)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(rational? Inf)")[[1]], env = env))
})

test_that("integer? checks integer-valued numbers", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(integer? 42L)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(integer? 42.0)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(integer? 3.14)")[[1]], env = env))
})

test_that("exact? identifies integer storage", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(exact? 5L)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(exact? 5.0)")[[1]], env = env))
})

test_that("inexact? identifies double storage", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(inexact? 5.0)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(inexact? 5L)")[[1]], env = env))
})

test_that("natural? requires non-negative integers", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(natural? 1)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(natural? 0)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(natural? -1)")[[1]], env = env))
})

test_that("zero? identifies zero", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(zero? 0)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(zero? 1)")[[1]], env = env))
})

test_that("positive? and negative? work", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(positive? 1)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(positive? -1)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(negative? -1)")[[1]], env = env))
})

test_that("even? and odd? work", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(even? 2)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(even? 1)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(odd? 1)")[[1]], env = env))
})

test_that("finite? and infinite? work", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(finite? 42)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(finite? Inf)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(infinite? Inf)")[[1]], env = env))
})

test_that("nan? identifies NaN", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(nan? NaN)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(nan? 42)")[[1]], env = env))
})

# ============================================================================
# Equality Tests
# ============================================================================

test_that("identical? tests object identity", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # eq? and eqv? not implemented - use identical? instead
  engine$eval(engine$read("(define x '(1 2 3))")[[1]], env = env)
  expect_true(engine$eval(engine$read("(identical? x x)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(identical? 'foo 'foo)")[[1]], env = env))
})

test_that("identical? tests value identity", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # identical? in R compares values, not storage types
  # note that in R, "42" and "42.0" are both double; if you want
  # an integer, you have to write "42L"
  expect_true(engine$eval(engine$read("(identical? 42 42)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(identical? 42 42.0)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(identical? 42 43)")[[1]], env = env))
})

test_that("equal? tests structural equality", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(equal? '(1 2 3) '(1 2 3))")[[1]], env = env))
  expect_false(engine$eval(engine$read("(equal? '(1 2 3) '(1 2 4))")[[1]], env = env))
})

test_that("equal? handles nested structures", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(equal? '(1 (2 3)) '(1 (2 3)))")[[1]], env = env))
})

test_that("equality handles empty collections", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(equal? '() '())")[[1]], env = env))
})

test_that("equal? compares strings", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(equal? \"hello\" \"hello\")")[[1]], env = env))
  expect_false(engine$eval(engine$read("(equal? \"hello\" \"world\")")[[1]], env = env))
})

test_that("equality handles NULL", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # eq? not implemented - use eqv? or identical? instead
  expect_true(engine$eval(engine$read("(identical? NULL NULL)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(equal? NULL NULL)")[[1]], env = env))
})

test_that("= handles NULL (Scheme semantics, not R logical(0))", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(= NULL NULL)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(= NULL 1)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(= 1 NULL)")[[1]], env = env))
  # variadic path
  expect_true(engine$eval(engine$read("(= NULL NULL NULL)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(= 1 NULL 1)")[[1]], env = env))
})

test_that("identical? and equal? both work for lists", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # In R, identical? does structural comparison for lists
  expect_true(engine$eval(engine$read("(identical? '(1 2) '(1 2))")[[1]], env = env))
  expect_true(engine$eval(engine$read("(equal? '(1 2) '(1 2))")[[1]], env = env))
  expect_false(engine$eval(engine$read("(equal? '(1 2) '(1 3))")[[1]], env = env))
})

test_that("equality handles symbols", {
  thin()
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # eq? not implemented - use identical? instead
  expect_true(engine$eval(engine$read("(identical? 'foo 'foo)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(identical? 'foo 'bar)")[[1]], env = env))
})

# ============================================================================
# Edge cases: type predicates
# ============================================================================

test_that("predicates handle edge cases", {
  thin()
  env <- new.env()
  toplevel_env(engine, env = env)

  # null? with various falsy values
  expect_true(get("null?", envir = env)(NULL))
  expect_false(get("null?", envir = env)(FALSE))
  expect_false(get("null?", envir = env)(0))
  expect_false(get("null?", envir = env)(""))
  expect_true(get("null?", envir = env)(list()))  # empty list is considered null

  # list? with various types
  expect_true(get("list?", envir = env)(list()))
  expect_true(get("list?", envir = env)(list(1, 2, 3)))
  expect_false(get("list?", envir = env)(c(1, 2, 3)))  # vector, not list
  expect_false(get("list?", envir = env)(NULL))

  # list-or-pair? edge cases (non-empty list or dotted pair)
  expect_false(get("list-or-pair?", envir = env)(list()))
  expect_true(get("list-or-pair?", envir = env)(list(1)))
  expect_true(get("list-or-pair?", envir = env)(list(1, 2)))

  # vector? with various types (only numeric vectors)
  expect_true(get("vector?", envir = env)(c(1, 2, 3)))
  expect_true(get("vector?", envir = env)(c("a", "b")))
  expect_true(get("vector?", envir = env)(1:10))
  expect_false(get("vector?", envir = env)(list(1, 2, 3)))

  # number? with various numeric types
  expect_true(get("number?", envir = env)(42))
  expect_true(get("number?", envir = env)(3.14))
  expect_true(get("number?", envir = env)(1L))  # integer
  expect_false(get("number?", envir = env)("42"))

  # string? edge cases
  expect_true(get("string?", envir = env)(""))
  expect_true(get("string?", envir = env)("hello"))
  expect_false(get("string?", envir = env)(NULL))

  # fn? and callable? with various types
  expect_true(get("fn?", envir = env)(function(x) x))
  expect_true(get("callable?", envir = env)(function(x) x))
  expect_true(get("callable?", envir = env)(`+`))
  expect_false(get("fn?", envir = env)(NULL))
  expect_false(get("callable?", envir = env)(42))
})
