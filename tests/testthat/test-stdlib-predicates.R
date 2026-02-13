# Type predicates and boolean tests from types.arl, math.arl, logic.arl

engine <- make_engine()

test_that("predicates work correctly", {
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
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(env$`boolean?`(TRUE))
  expect_true(env$`boolean?`(FALSE))
  expect_false(env$`boolean?`(c(TRUE, FALSE)))
  expect_false(env$`boolean?`(1))

  expect_true(engine$eval(engine$read("(xor #t #f)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(xor #t #t)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(xor #f #f)")[[1]], env = env))

  expect_true(env$`even?`(4))
  expect_true(env$`odd?`(5))
  expect_true(env$`zero?`(0))
  expect_true(env$`positive?`(3))
  expect_true(env$`negative?`(-1))
  expect_true(env$`non-negative?`(0))
  expect_true(env$`non-positive?`(0))

  expect_true(env$`integer?`(2))
  expect_false(env$`integer?`(2.5))
  expect_true(env$`natural?`(0))
  expect_false(env$`natural?`(-1))

  expect_true(env$`finite?`(1))
  expect_false(env$`finite?`(Inf))
  expect_true(env$`infinite?`(Inf))
  expect_false(env$`infinite?`(1))
  expect_true(env$`nan?`(NaN))
  expect_false(env$`nan?`(1))

  expect_true(env$`empty?`(list()))
  expect_true(env$`empty?`(NULL))
  expect_false(env$`empty?`(""))
  expect_true(env$`empty?`(character()))
  expect_true(env$`empty?`(c()))
  expect_false(env$`empty?`(list(1)))
  expect_false(env$`empty?`("x"))
  expect_false(env$`empty?`(c(1)))

  expect_true(env$`length=`(list(1, 2, 3), 3))
  expect_true(env$`length>`(list(1, 2, 3), 2))
  expect_true(env$`length<`(list(1, 2, 3), 4))
})

test_that("numeric tower predicates work correctly", {
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
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(list? '(1 2 3))")[[1]], env = env))
  expect_true(engine$eval(engine$read("(list? '())")[[1]], env = env))
  expect_false(engine$eval(engine$read("(list? 42)")[[1]], env = env))
})

test_that("symbol? identifies symbols correctly", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(symbol? 'foo)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(symbol? \"foo\")")[[1]], env = env))
})

test_that("number? identifies all numeric types", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(number? 42)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(number? 3.14)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(number? \"42\")")[[1]], env = env))
})

test_that("string? identifies strings correctly", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(string? \"hello\")")[[1]], env = env))
  expect_false(engine$eval(engine$read("(string? 'hello)")[[1]], env = env))
})

test_that("vector? identifies R vectors", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(vector? 42)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(vector? '(1 2))")[[1]], env = env))
})

test_that("boolean? identifies logical values", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(boolean? #t)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(boolean? #f)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(boolean? 1)")[[1]], env = env))
})

test_that("atom? identifies non-compound values", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(atom? 42)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(atom? '(1 2))")[[1]], env = env))
})

test_that("empty? identifies zero-length collections", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(empty? '())")[[1]], env = env))
  expect_false(engine$eval(engine$read("(empty? '(1 2))")[[1]], env = env))
})

test_that("null? identifies NULL and empty lists", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(null? '())")[[1]], env = env))
  expect_true(engine$eval(engine$read("(null? NULL)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(null? 0)")[[1]], env = env))
})

test_that("procedure? identifies functions", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(procedure? +)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(procedure? 42)")[[1]], env = env))
})

test_that("environment? identifies environments", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(environment? (new.env))")[[1]], env = env))
  expect_false(engine$eval(engine$read("(environment? 42)")[[1]], env = env))
})

test_that("fn? and callable? are function predicates", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(fn? +)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(callable? (lambda (x) x))")[[1]], env = env))
})

test_that("not returns logical negation", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(not #f)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(not #t)")[[1]], env = env))
})

test_that("not treats #f and 0 as false", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(not 0)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(not #f)")[[1]], env = env))
})

test_that("xor implements exclusive or", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_false(engine$eval(engine$read("(xor #f #f)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(xor #f #t)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(xor #t #t)")[[1]], env = env))
})

test_that("xor works with truthy values", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(xor #f 1)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(xor 1 2)")[[1]], env = env))
})

test_that("predicates and interop helpers work", {
  env <- new.env()
  toplevel_env(engine, env = env)

  expect_true(env$`list-or-pair?`(list(1)))
  expect_false(env$`list-or-pair?`(list()))
  expect_true(env$`keyword?`(structure("from", class = "arl_keyword")))
  expect_true(env$`vector?`(c(1, 2, 3)))
  expect_true(env$`true?`(TRUE))
  expect_true(env$`false?`(FALSE))
  expect_true(env$`fn?`(function(x) x))
  expect_true(env$`callable?`(function(x) x))
})

test_that("real? excludes complex numbers", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(real? 42)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(real? Inf)")[[1]], env = env))
})

test_that("rational? excludes infinities", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(rational? 42)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(rational? Inf)")[[1]], env = env))
})

test_that("integer? checks integer-valued numbers", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(integer? 42L)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(integer? 42.0)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(integer? 3.14)")[[1]], env = env))
})

test_that("exact? identifies integer storage", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(exact? 5L)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(exact? 5.0)")[[1]], env = env))
})

test_that("inexact? identifies double storage", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(inexact? 5.0)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(inexact? 5L)")[[1]], env = env))
})

test_that("natural? requires non-negative integers", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(natural? 1)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(natural? 0)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(natural? -1)")[[1]], env = env))
})

test_that("zero? identifies zero", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(zero? 0)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(zero? 1)")[[1]], env = env))
})

test_that("positive? and negative? work", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(positive? 1)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(positive? -1)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(negative? -1)")[[1]], env = env))
})

test_that("even? and odd? work", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(even? 2)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(even? 1)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(odd? 1)")[[1]], env = env))
})

test_that("finite? and infinite? work", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(finite? 42)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(finite? Inf)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(infinite? Inf)")[[1]], env = env))
})

test_that("nan? identifies NaN", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(nan? NaN)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(nan? 42)")[[1]], env = env))
})

# ============================================================================
# Equality Tests
# ============================================================================

test_that("identical? tests object identity", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # eq? and eqv? not implemented - use identical? instead
  engine$eval(engine$read("(define x '(1 2 3))")[[1]], env = env)
  expect_true(engine$eval(engine$read("(identical? x x)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(identical? 'foo 'foo)")[[1]], env = env))
})

test_that("identical? tests value identity", {
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
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(equal? '(1 2 3) '(1 2 3))")[[1]], env = env))
  expect_false(engine$eval(engine$read("(equal? '(1 2 3) '(1 2 4))")[[1]], env = env))
})

test_that("equal? handles nested structures", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(equal? '(1 (2 3)) '(1 (2 3)))")[[1]], env = env))
})

test_that("equality handles empty collections", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(equal? '() '())")[[1]], env = env))
})

test_that("equal? compares strings", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  expect_true(engine$eval(engine$read("(equal? \"hello\" \"hello\")")[[1]], env = env))
  expect_false(engine$eval(engine$read("(equal? \"hello\" \"world\")")[[1]], env = env))
})

test_that("equality handles NULL", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # eq? not implemented - use eqv? or identical? instead
  expect_true(engine$eval(engine$read("(identical? NULL NULL)")[[1]], env = env))
  expect_true(engine$eval(engine$read("(equal? NULL NULL)")[[1]], env = env))
})

test_that("= handles NULL (Scheme semantics, not R logical(0))", {
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
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # In R, identical? does structural comparison for lists
  expect_true(engine$eval(engine$read("(identical? '(1 2) '(1 2))")[[1]], env = env))
  expect_true(engine$eval(engine$read("(equal? '(1 2) '(1 2))")[[1]], env = env))
  expect_false(engine$eval(engine$read("(equal? '(1 2) '(1 3))")[[1]], env = env))
})

test_that("equality handles symbols", {
  env <- new.env(parent = emptyenv())
  toplevel_env(engine, env = env)

  # eq? not implemented - use identical? instead
  expect_true(engine$eval(engine$read("(identical? 'foo 'foo)")[[1]], env = env))
  expect_false(engine$eval(engine$read("(identical? 'foo 'bar)")[[1]], env = env))
})
