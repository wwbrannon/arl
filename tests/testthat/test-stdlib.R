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

test_that("force evaluates promises", {
  env <- stdlib_env(engine, new.env())
  forced <- engine$eval_in_env(engine$read("(force (delay (+ 1 2)))")[[1]], env)
  expect_equal(forced, 3)
})

test_that("force memoizes delayed expressions", {
  env <- stdlib_env(engine, new.env())
  engine$eval_in_env(
    engine$read("(begin (define counter 0)\n  (define p (delay (begin (set! counter (+ counter 1)) counter)))\n  (force p)\n  (force p)\n  counter)")[[1]],
    env
  )
  expect_equal(env$counter, 1)
})

test_that("force returns non-promises unchanged", {
  env <- stdlib_env(engine)
  result <- engine$eval_in_env(engine$read("(force 42)")[[1]], env)
  expect_equal(result, 42)
})

test_that("call/cc exits to current continuation", {
  env <- stdlib_env(engine)
  result <- engine$eval_in_env(
    engine$read("(call/cc (lambda (k) (+ 1 (k 42) 3)))")[[1]],
    env
  )
  expect_equal(result, 42)
})

test_that("call/cc is downward-only (R's callCC behavior)", {
  env <- stdlib_env(engine)
  # R's callCC is one-shot and downward-only
  result <- engine$eval_in_env(
    engine$read("(call/cc (lambda (k) (k 5)))")[[1]],
    env
  )
  expect_equal(result, 5)
  
  # Test that it works as a regular function
  result2 <- engine$eval_in_env(
    engine$read("(call/cc (lambda (exit) (if (> 2 1) (exit 10) 20)))")[[1]],
    env
  )
  expect_equal(result2, 10)
})

test_that("call/cc is first-class and has an alias", {
  env <- stdlib_env(engine)
  engine$eval_in_env(engine$read("(define cc call/cc)")[[1]], env)
  result <- engine$eval_in_env(engine$read("(cc (lambda (k) (k 7)))")[[1]], env)
  expect_equal(result, 7)
  alias_result <- engine$eval_in_env(
    engine$read("(call-with-current-continuation (lambda (k) (k 9)))")[[1]],
    env
  )
  expect_equal(alias_result, 9)
})

test_that("car returns first element", {
  env <- new.env()
  stdlib_env(engine, env)

  # Test with R list
  expect_equal(env$car(list(1, 2, 3)), 1)

  # Test with parsed expression
  expr <- engine$read("(+ 1 2)")[[1]]
  expect_equal(as.character(env$car(expr)), "+")
})

test_that("cdr returns rest of list", {
  env <- new.env()
  stdlib_env(engine, env)

  # Test with R list
  result <- env$cdr(list(1, 2, 3))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 3)
})

test_that("common composed list accessors work (cadr, caddr, caar, cdar, ...)", {
  env <- stdlib_env(engine, new.env())

  # From list values
  expect_equal(engine$eval_in_env(engine$read("(begin (import list) (cadr (list 1 2 3 4)))")[[1]], env), 2)
  expect_equal(engine$eval_in_env(engine$read("(begin (import list) (caddr (list 1 2 3 4)))")[[1]], env), 3)
  expect_equal(engine$eval_in_env(engine$read("(begin (import list) (cadddr (list 1 2 3 4)))")[[1]], env), 4)
  expect_equal(
    engine$eval_in_env(engine$read("(begin (import list) (caar (list (list 10 11) (list 20 21))))")[[1]], env),
    10
  )
  expect_equal(
    engine$eval_in_env(engine$read("(begin (import list) (cdar (list (list 10 11) (list 20 21))))")[[1]], env),
    list(11)
  )
  expect_equal(
    engine$eval_in_env(engine$read("(begin (import list) (cddr (list 1 2 3 4)))")[[1]], env),
    list(3, 4)
  )

  # From quoted calls (call objects)
  expect_equal(
    engine$eval_in_env(engine$read("(begin (import list) (cadr '(+ 1 2 3)))")[[1]], env),
    1
  )
  expect_equal(
    engine$eval_in_env(engine$read("(begin (import list) (caddr '(+ 1 2 3)))")[[1]], env),
    2
  )
  expect_equal(
    engine$eval_in_env(engine$read("(begin (import list) (cadddr '(+ 1 2 3)))")[[1]], env),
    3
  )
})

test_that("ordinal list accessors work (second, third, fourth)", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$second(list(1, 2, 3)), 2)
  expect_equal(env$third(list(1, 2, 3, 4)), 3)
  expect_equal(env$fourth(list(1, 2, 3, 4)), 4)

  expect_null(env$second(list(1)))
  expect_null(env$third(list(1, 2)))
  expect_null(env$fourth(list(1, 2, 3)))
})

test_that("assoc family: assoc, assoc-by-equal?, assoc-by-identical?, assoc-by-==, rassoc, rassoc-by-equal?", {
  env <- stdlib_env(engine, new.env())

  # assoc (equal?) and assoc-by-equal? (alias)
  alist <- list(list(quote(a), 1), list(quote(b), 2), list(quote(c), 3))
  expect_equal(env$assoc(quote(b), alist), list(quote(b), 2))
  expect_equal(env$`assoc-by-equal?`(quote(b), alist), list(quote(b), 2))

  # assoc-by-identical? uses R's identical()
  key <- quote(k)
  alist_id <- list(list(key, 1))
  expect_equal(env$`assoc-by-identical?`(key, alist_id), list(quote(k), 1))

  # assoc-by-== uses R's == (e.g. 1 and 1L match)
  alist_num <- list(list(1, "one"), list(2, "two"), list(3, "three"))
  expect_equal(env$`assoc-by-==`(1, alist_num), list(1, "one"))
  expect_equal(env$`assoc-by-==`(1L, alist_num), list(1, "one"))

  # rassoc and rassoc-by-equal? (alias)
  expect_equal(env$rassoc(2, alist), list(quote(b), 2))
  expect_equal(env$`rassoc-by-equal?`(2, alist), list(quote(b), 2))
})

test_that("assq and assv error (cannot implement eq?/eqv? in R)", {
  env <- stdlib_env(engine, new.env())
  expect_error(
    engine$eval_in_env(engine$read("(assq 'x (list (list 'x 1)))")[[1]], env),
    "assq cannot be properly implemented"
  )
  expect_error(
    engine$eval_in_env(engine$read("(assv 5 (list (list 5 \"five\")))")[[1]], env),
    "assv cannot be properly implemented"
  )
})

test_that("cons adds element to front", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- env$cons(1, list(2, 3))
  expect_equal(result[[1]], 1)
  expect_equal(result[[2]], 2)
  expect_equal(result[[3]], 3)
})

test_that("cons with non-list cdr produces dotted pair (rye_cons)", {
  env <- stdlib_env(engine, new.env())
  result <- engine$eval_in_env(engine$read("(cons 'a 'b)")[[1]], env)
  expect_true(r6_isinstance(result, "RyeCons"))
  expect_equal(as.character(result$car), "a")
  expect_equal(as.character(result$cdr), "b")
})

test_that("car and cdr on dotted pair", {
  env <- stdlib_env(engine, new.env())
  pair <- engine$eval_in_env(engine$read("'(a . 42)")[[1]], env)
  expect_equal(as.character(env$car(pair)), "a")
  expect_equal(env$cdr(pair), 42)
})

test_that("list? and pair? are true for rye_cons", {
  env <- stdlib_env(engine, new.env())
  pair <- engine$eval_in_env(engine$read("(cons 1 2)")[[1]], env)
  expect_true(env$`list?`(pair))
  expect_true(env$`pair?`(pair))
})

test_that("__as-list on improper list returns proper prefix only", {
  env <- stdlib_env(engine, new.env())
  pl <- engine$read("'(a b . c)")[[1]][[2]]
  expect_true(r6_isinstance(pl, "RyeCons"))
  prefix <- env$`__as-list`(pl)
  expect_equal(length(prefix), 2)
  expect_equal(as.character(prefix[[1]]), "a")
  expect_equal(as.character(prefix[[2]]), "b")
})

test_that("map applies function to list", {
  env <- new.env()
  stdlib_env(engine, env)

  double <- function(x) x * 2
  result <- env$map(double, list(1, 2, 3))

  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("filter selects matching elements", {
  env <- new.env()
  stdlib_env(engine, env)

  is_even <- function(x) x %% 2 == 0
  result <- env$filter(is_even, list(1, 2, 3, 4, 5, 6))

  expect_equal(length(result), 3)
  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("reduce combines list elements", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- env$reduce(`+`, list(1, 2, 3, 4))
  expect_equal(result, 10)

  result <- env$reduce(`*`, list(1, 2, 3, 4))
  expect_equal(result, 24)
})

test_that("map works from Rye code", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define a doubling function in Rye
  engine$eval_in_env(engine$read("(define double (lambda (x) (* x 2)))")[[1]], env)

  # Use map with the Rye function
  result <- engine$eval_in_env(engine$read("(map double (list 1 2 3))")[[1]], env)

  expect_equal(result[[1]], 2)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 6)
})

test_that("filter works from Rye code", {
  env <- new.env()
  stdlib_env(engine, env)

  # Define a predicate in Rye
  engine$eval_in_env(engine$read("(define even? (lambda (x) (= (% x 2) 0)))")[[1]], env)

  # Use filter
  result <- engine$eval_in_env(engine$read("(filter even? (list 1 2 3 4 5 6))")[[1]], env)

  expect_equal(length(result), 3)
})

test_that("predicates work correctly", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_true(engine$eval_in_env(engine$read("(number? 42)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(number? \"hello\")")[[1]], env))

  expect_true(engine$eval_in_env(engine$read("(string? \"hello\")")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(string? 42)")[[1]], env))

  expect_true(engine$eval_in_env(engine$read("(null? #nil)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(null? 42)")[[1]], env))
})

test_that("extended predicates work correctly", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_true(env$`boolean?`(TRUE))
  expect_true(env$`boolean?`(FALSE))
  expect_false(env$`boolean?`(c(TRUE, FALSE)))
  expect_false(env$`boolean?`(1))

  expect_true(engine$eval_in_env(engine$read("(xor #t #f)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(xor #t #t)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(xor #f #f)")[[1]], env))

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

test_that("and macro works", {
  env <- new.env()

  # Define and macro
  engine$eval_in_env(engine$read("(defmacro and2 (first second) `(if ,first ,second #f))")[[1]], env)

  result <- engine$eval_in_env(engine$read("(and2 #t #t)")[[1]], env)
  expect_true(result)

  result <- engine$eval_in_env(engine$read("(and2 #t #f)")[[1]], env)
  expect_false(result)

  result <- engine$eval_in_env(engine$read("(and2 #f #t)")[[1]], env)
  expect_false(result)
})

test_that("or macro works", {
  env <- new.env()

  # Define or macro
  engine$eval_in_env(engine$read("(defmacro or2 (first second) `(if ,first #t ,second))")[[1]], env)

  result <- engine$eval_in_env(engine$read("(or2 #t #f)")[[1]], env)
  expect_true(result)

  result <- engine$eval_in_env(engine$read("(or2 #f #t)")[[1]], env)
  expect_true(result)

  result <- engine$eval_in_env(engine$read("(or2 #f #f)")[[1]], env)
  expect_false(result)
})

test_that("variadic and/or short-circuit correctly", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("control"), env)

  result <- engine$eval_in_env(engine$read("(and #t 1 2 3)")[[1]], env)
  expect_equal(result, 3)

  result <- engine$eval_in_env(engine$read("(or #f 1 2)")[[1]], env)
  expect_equal(result, 1)

  engine$eval_in_env(engine$read("(define x 0)")[[1]], env)
  result <- engine$eval_in_env(engine$read("(and #f (begin (set! x 1) x))")[[1]], env)
  expect_false(result)
  expect_equal(env$x, 0)

  result <- engine$eval_in_env(engine$read("(or #t (begin (set! x 2) x))")[[1]], env)
  expect_true(result)
  expect_equal(env$x, 0)
})

test_that("binding macros when-let and if-let work", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("binding"), env)

  result <- engine$eval_in_env(engine$read("(when-let (x 10) (+ x 1))")[[1]], env)
  expect_equal(result, 11)

  result <- engine$eval_in_env(engine$read("(when-let (x #f) (+ x 1))")[[1]], env)
  expect_null(result)

  result <- engine$eval_in_env(engine$read("(when-let ((a b) (list 1 2)) (+ a b))")[[1]], env)
  expect_equal(result, 3)

  result <- engine$eval_in_env(engine$read("(if-let (x 5) (+ x 1) 0)")[[1]], env)
  expect_equal(result, 6)

  result <- engine$eval_in_env(engine$read("(if-let (x #nil) 1 2)")[[1]], env)
  expect_equal(result, 2)

  result <- engine$eval_in_env(engine$read("(if-let (x #f) 1 2)")[[1]], env)
  expect_equal(result, 2)

  result <- engine$eval_in_env(engine$read("(if-let (x #f) 1)")[[1]], env)
  expect_null(result)

  result <- engine$eval_in_env(engine$read("(if-let ((a b) (list 3 4)) (+ a b) 0)")[[1]], env)
  expect_equal(result, 7)
})

test_that("until macro repeats until test is truthy", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("looping"), env)

  result <- engine$eval_in_env(
    engine$read("(begin (define i 0) (until (= i 3) (set! i (+ i 1))) i)")[[1]],
    env
  )
  expect_equal(result, 3)
})

test_that("loop/recur iterates with rebinding", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("looping"), env)

  result <- engine$eval_in_env(
    engine$read("(loop ((i 0) (acc 0)) (if (< i 5) (recur (+ i 1) (+ acc i)) acc))")[[1]],
    env
  )
  expect_equal(result, 10)

  result <- engine$eval_in_env(
    engine$read("(loop ((x 1)) (+ x 2))")[[1]],
    env
  )
  expect_equal(result, 3)

  result <- engine$eval_in_env(
    engine$read("(loop ((i 0) (sum 0)) (if (< i 3) (recur (+ i 1) (+ sum (loop ((j 0) (acc 0)) (if (< j 2) (recur (+ j 1) (+ acc 1)) acc)))) sum))")[[1]],
    env
  )
  expect_equal(result, 6)

  result <- engine$eval_in_env(
    engine$read("(loop ((n 5) (acc 1)) (if (< n 2) acc (recur (- n 1) (* acc n))))")[[1]],
    env
  )
  expect_equal(result, 120)

  result <- engine$eval_in_env(
    engine$read("(loop ((xs (list 1 2 3)) (sum 0)) (if (null? xs) sum (recur (cdr xs) (+ sum (car xs)))))")[[1]],
    env
  )
  expect_equal(result, 6)
})

test_that("recur errors outside loop", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("looping"), env)

  expect_error(engine$eval_in_env(engine$read("(recur 1)")[[1]], env), "recur can only be used inside loop")
})

test_that("not function works", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_false(engine$eval_in_env(engine$read("(not #t)")[[1]], env))
  expect_true(engine$eval_in_env(engine$read("(not #f)")[[1]], env))
  expect_false(engine$eval_in_env(engine$read("(not 42)")[[1]], env))
})

test_that("list helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$`list*`(1, list(2, 3)), list(1, 2, 3))
  expect_equal(env$append(list(1, 2), list(3)), list(1, 2, 3))
  expect_equal(env$reverse(list(1, 2, 3)), list(3, 2, 1))
  expect_equal(env$funcall(sum, list(1, 2, 3)), 6)
})

test_that("values and call-with-values work", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- engine$eval_in_env(engine$read("(call-with-values (lambda () (values)) (lambda () 42))")[[1]], env)
  expect_equal(result, 42)

  result <- engine$eval_in_env(engine$read("(call-with-values (lambda () (values 1)) (lambda (x) (+ x 1)))")[[1]], env)
  expect_equal(result, 2)

  result <- engine$eval_in_env(engine$read("(call-with-values (lambda () (values 1 2)) (lambda (a b) (+ a b)))")[[1]], env)
  expect_equal(result, 3)

  result <- engine$eval_in_env(engine$read("(call-with-values (lambda () 5) (lambda (x) (* x 2)))")[[1]], env)
  expect_equal(result, 10)
})

test_that("sequence helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- env$mapcat(function(x) list(x, x + 10), list(1, 2))
  expect_equal(result, list(1, 11, 2, 12))

  result <- env$remove(function(x) x %% 2 == 0, list(1, 2, 3, 4))
  expect_equal(result, list(1, 3))

  expect_equal(env$foldl(`-`, list(1, 2, 3)), -4)
  expect_equal(env$foldr(`-`, list(1, 2, 3)), 2)

  expect_true(env$`every?`(function(x) x > 0, list(1, 2, 3)))
  expect_false(env$`every?`(function(x) x > 1, list(1, 2, 3)))
  expect_true(env$`any?`(function(x) x > 2, list(1, 2, 3)))
  expect_false(env$`any?`(function(x) x > 5, list(1, 2, 3)))

  expect_equal(env$take(2, list(1, 2, 3)), list(1, 2))
  expect_equal(env$drop(2, list(1, 2, 3)), list(3))
  expect_equal(env$`take-while`(function(x) x < 3, list(1, 2, 3, 1)), list(1, 2))
  expect_equal(env$`drop-while`(function(x) x < 3, list(1, 2, 3, 1)), list(3, 1))
  expect_equal(env$partition(2, list(1, 2, 3, 4)), list(list(1, 2), list(3, 4)))
  expect_equal(env$flatten(list(1, list(2, list(3)), 4)), list(1, 2, 3, 4))
})

test_that("member and contains? sequence helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$member(2, list(1, 2, 3)), list(2, 3))
  expect_false(env$member(5, list(1, 2, 3)))

  expect_true(env$`contains?`(2, list(1, 2, 3)))
  expect_false(env$`contains?`(5, list(1, 2, 3)))
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

test_that("predicates and interop helpers work", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_true(env$`pair?`(list(1)))
  expect_false(env$`pair?`(list()))
  expect_true(env$`keyword?`(structure("from", class = "rye_keyword")))
  expect_true(env$`vector?`(c(1, 2, 3)))
  expect_true(env$`true?`(TRUE))
  expect_true(env$`false?`(FALSE))
  expect_true(env$`fn?`(function(x) x))
  expect_true(env$`callable?`(function(x) x))

  dict <- env$dict(a = 1, b = 2)
  expect_true(env$`dict?`(dict))
  expect_true(env$`dict-has?`(dict, "a"))
  expect_equal(env$`dict-get`(dict, "a"), 1)
  expect_equal(env$`dict-get`(dict, "b"), 2)
  expect_equal(env$`r/call`("sum", list(1, 2, 3)), 6)
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

test_that("call function converts lists to calls", {
  env <- new.env()
  stdlib_env(engine, env)

  # Convert list to call
  lst <- list(quote(`+`), 1, 2)
  result <- env$call(lst)
  expect_true(is.call(result))
  expect_equal(result[[1]], quote(`+`))
  expect_equal(result[[2]], 1)
  expect_equal(result[[3]], 2)

  # Already a call should be returned as-is
  call_obj <- engine$read("(+ 1 2)")[[1]]
  expect_equal(
    engine$source_tracker$strip_src(env$call(call_obj)),
    engine$source_tracker$strip_src(call_obj)
  )
})

test_that("eval function evaluates Rye expressions", {
  env <- new.env()
  stdlib_env(engine, env)

  # Eval simple arithmetic
  result <- env$eval(engine$read("(+ 1 2)")[[1]], env)
  expect_equal(result, 3)

  # Eval with variables
  env$x <- 10
  result <- env$eval(engine$read("(* x 5)")[[1]], env)
  expect_equal(result, 50)

  # Eval function definition and call
  env$eval(engine$read("(define double (lambda (n) (* n 2)))")[[1]], env)
  result <- env$eval(engine$read("(double 21)")[[1]], env)
  expect_equal(result, 42)
})

test_that("gensym generates unique symbols", {
  env <- new.env()
  stdlib_env(engine, env)

  # Generate unique symbols
  sym1 <- env$gensym()
  sym2 <- env$gensym()

  expect_true(is.symbol(sym1))
  expect_true(is.symbol(sym2))
  expect_false(identical(sym1, sym2))

  # Custom prefix
  sym_custom <- env$gensym("foo")
  expect_true(is.symbol(sym_custom))
  expect_true(grepl("^foo", as.character(sym_custom)))
})

test_that("try* with only error handler works", {
  env <- new.env()
  stdlib_env(engine, env)

  # Success case
  result <- env$`try*`(
    function() 42,
    function(e) "error"
  )
  expect_equal(result, 42)

  # Error case
  result <- env$`try*`(
    function() stop("boom"),
    function(e) "caught"
  )
  expect_equal(result, "caught")
})

test_that("try* with only finally handler works", {
  env <- new.env()
  stdlib_env(engine, env)

  # Track whether finally ran
  finally_ran <- FALSE

  # Success case
  result <- env$`try*`(
    function() 42,
    NULL,
    function() finally_ran <<- TRUE
  )
  expect_equal(result, 42)
  expect_true(finally_ran)

  # Error case (finally should run but error should propagate)
  finally_ran <- FALSE
  expect_error({
    env$`try*`(
      function() stop("boom"),
      NULL,
      function() finally_ran <<- TRUE
    )
  })
  expect_true(finally_ran)
})

test_that("try* with both handlers works", {
  env <- new.env()
  stdlib_env(engine, env)

  # Track execution
  finally_ran <- FALSE

  # Error caught and finally runs
  result <- env$`try*`(
    function() stop("boom"),
    function(e) "caught",
    function() finally_ran <<- TRUE
  )
  expect_equal(result, "caught")
  expect_true(finally_ran)

  # Success and finally runs
  finally_ran <- FALSE
  result <- env$`try*`(
    function() 99,
    function(e) "error",
    function() finally_ran <<- TRUE
  )
  expect_equal(result, 99)
  expect_true(finally_ran)
})

test_that("r/call invokes R functions with arguments", {
  env <- new.env()
  stdlib_env(engine, env)

  # Call R function by name (string)
  result <- env$`r/call`("sum", list(1, 2, 3, 4))
  expect_equal(result, 10)

  # Call R function by symbol
  result <- env$`r/call`(quote(prod), list(2, 3, 4))
  expect_equal(result, 24)

  # Call with single argument
  result <- env$`r/call`("sqrt", list(16))
  expect_equal(result, 4)

  # Call with no arguments
  result <- env$`r/call`("ls", list())
  expect_true(is.character(result))
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

# ===========================================================================
# Convenience Functions Tests
# ===========================================================================

test_that("identity returns its argument", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$identity(42), 42)
  expect_equal(env$identity("hello"), "hello")
  expect_equal(env$identity(list(1, 2, 3)), list(1, 2, 3))
  expect_null(env$identity(NULL))
})

test_that("first is an alias for car", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$first(list(1, 2, 3)), 1)
  expect_null(env$first(list()))
  expect_equal(env$first(list("a", "b")), "a")
})

test_that("rest is an alias for cdr", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$rest(list(1, 2, 3)), list(2, 3))
  expect_equal(env$rest(list(1)), list())
  expect_equal(env$rest(list()), list())
})

test_that("last returns last element", {
  env <- new.env()
  stdlib_env(engine, env)

  expect_equal(env$last(list(1, 2, 3)), 3)
  expect_equal(env$last(list(42)), 42)
  expect_null(env$last(list()))
  expect_equal(env$last(list("a", "b", "c")), "c")
})

test_that("nth returns element at index", {
  env <- new.env()
  stdlib_env(engine, env)

  lst <- list(10, 20, 30, 40)

  # 0-indexed
  expect_equal(env$nth(lst, 0), 10)
  expect_equal(env$nth(lst, 1), 20)
  expect_equal(env$nth(lst, 2), 30)
  expect_equal(env$nth(lst, 3), 40)

  # Out of bounds
  expect_error(env$nth(lst, -1), "out of bounds")
  expect_error(env$nth(lst, 4), "out of bounds")
})

test_that("complement negates predicate", {
  env <- new.env()
  stdlib_env(engine, env)

  is_even <- function(x) x %% 2 == 0
  is_odd <- env$complement(is_even)

  expect_true(is_odd(1))
  expect_false(is_odd(2))
  expect_true(is_odd(3))
  expect_false(is_odd(4))

  # Use with filter
  result <- env$filter(is_odd, list(1, 2, 3, 4, 5, 6))
  expect_equal(result, list(1, 3, 5))
})

test_that("compose combines functions", {
  env <- new.env()
  stdlib_env(engine, env)

  double <- function(x) x * 2
  add_one <- function(x) x + 1

  # compose applies right-to-left: f(g(x))
  double_then_add_one <- env$compose(add_one, double)
  expect_equal(double_then_add_one(5), 11)  # (5 * 2) + 1

  # Multiple compositions
  add_ten <- function(x) x + 10
  complex_fn <- env$compose(double, env$compose(add_one, add_ten))
  expect_equal(complex_fn(5), 32)  # ((5 + 10) + 1) * 2
})

test_that("repeatedly calls function n times", {
  env <- new.env()
  stdlib_env(engine, env)

  counter <- 0
  increment <- function() {
    counter <<- counter + 1
    counter
  }

  result <- env$repeatedly(5, increment)
  expect_equal(length(result), 5)
  expect_equal(result[[1]], 1)
  expect_equal(result[[5]], 5)
})

test_that("repeat creates list with repeated value", {
  env <- new.env()
  stdlib_env(engine, env)

  result <- env$`repeat`(5, "x")
  expect_equal(length(result), 5)
  expect_equal(result[[1]], "x")
  expect_equal(result[[5]], "x")

  # With number
  result <- env$`repeat`(3, 42)
  expect_equal(result, list(42, 42, 42))

  # With NULL
  result <- env$`repeat`(2, NULL)
  expect_equal(length(result), 2)
  expect_null(result[[1]])
})

test_that("zip combines lists element-wise", {
  env <- new.env()
  stdlib_env(engine, env)

  # Two lists
  result <- env$zip(list(1, 2, 3), list("a", "b", "c"))
  expect_equal(length(result), 3)
  expect_equal(result[[1]], list(1, "a"))
  expect_equal(result[[2]], list(2, "b"))
  expect_equal(result[[3]], list(3, "c"))

  # Three lists
  result <- env$zip(list(1, 2), list("a", "b"), list(TRUE, FALSE))
  expect_equal(length(result), 2)
  expect_equal(result[[1]], list(1, "a", TRUE))
  expect_equal(result[[2]], list(2, "b", FALSE))

  # Different lengths (zip to shortest)
  result <- env$zip(list(1, 2, 3, 4), list("a", "b"))
  expect_equal(length(result), 2)

  # Empty list
  result <- env$zip(list(), list(1, 2))
  expect_equal(length(result), 0)
})

test_that("partial applies arguments partially", {
  env <- new.env()
  stdlib_env(engine, env)

  # Partial application
  add <- function(a, b) a + b
  add_five <- env$partial(add, 5)

  expect_equal(add_five(3), 8)
  expect_equal(add_five(10), 15)

  # Multiple arguments
  multiply <- function(a, b, c) a * b * c
  multiply_by_2_3 <- env$partial(multiply, 2, 3)

  expect_equal(multiply_by_2_3(4), 24)  # 2 * 3 * 4
  expect_equal(multiply_by_2_3(5), 30)  # 2 * 3 * 5
})
