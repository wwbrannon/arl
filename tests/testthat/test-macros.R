engine <- make_engine()

thin <- make_cran_thinner()

test_that("gensym generates unique symbols", {
  thin()
  s1 <- engine_field(engine, "macro_expander")$gensym()
  s2 <- engine_field(engine, "macro_expander")$gensym()
  expect_true(is.symbol(s1))
  expect_true(is.symbol(s2))
  expect_false(identical(s1, s2))
})

test_that("gensym accepts prefix", {
  thin()
  s <- engine_field(engine, "macro_expander")$gensym("temp")
  expect_true(grepl("^temp__", as.character(s)))
})

test_that("quasiquote returns template", {
  thin()
  env <- new.env()
  env$x <- 5

  # Basic quasiquote
  result <- engine$eval(engine$read("`(+ 1 2)")[[1]], env = env)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")
})

test_that("unquote evaluates within quasiquote", {
  thin()
  env <- new.env()
  env$x <- 5

  result <- engine$eval(engine$read("`(+ 1 ,x)")[[1]], env = env)
  expect_true(is.call(result))
  expect_equal(result[[3]], 5)
})

test_that("unquote-splicing splices lists", {
  thin()
  env <- new.env()
  env$nums <- quote(c(2, 3, 4))

  result <- engine$eval(engine$read("`(+ 1 ,@nums)")[[1]], env = env)
  expect_true(is.call(result))
  # Should be (+ 1 c 2 3 4)
  expect_equal(length(result), 6)
})

test_that("defmacro defines macros", {
  thin()
  env <- new.env()

  # Define a simple macro
  engine$eval(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env = env)

  # Check macro is defined
  expect_true(engine_field(engine, "macro_expander")$is_macro(as.symbol("when"), env = env))
})

test_that("get_macro returns registered macro function", {
  thin()
  env <- new.env()
  engine$eval(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env = env)

  macro_fn <- engine_field(engine, "macro_expander")$get_macro(as.symbol("when"), env = env)
  expect_true(is.function(macro_fn))
})

test_that("defmacro validates dotted parameter lists", {
  thin()
  env <- new.env()
  params_multi_dot <- list(as.symbol("a"), as.symbol("."), as.symbol("b"), as.symbol("."), as.symbol("c"))
  expect_error(
    engine_field(engine, "macro_expander")$defmacro(as.symbol("bad"), params_multi_dot, list(as.symbol("a")), env = env),
    "Dotted parameter list can only contain one '\\.'")

  params_bad_position <- list(as.symbol("a"), as.symbol("."), as.symbol("b"), as.symbol("c"))
  expect_error(
    engine_field(engine, "macro_expander")$defmacro(as.symbol("bad2"), params_bad_position, list(as.symbol("a")), env = env),
    "Dotted parameter list must have exactly one parameter after '\\.'")
})

test_that("macros expand correctly", {
  thin()
  env <- new.env()

  # Define when macro
  engine$eval(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env = env)

  # Expand macro
  expanded <- engine$macroexpand(engine$read("(when #t 42)")[[1]], env = env)

  # Should expand to (if #t 42 #nil)
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "if")
  expect_equal(expanded[[2]], TRUE)
  expect_equal(expanded[[3]], 42)
})

test_that("macros evaluate correctly", {
  thin()
  env <- new.env()

  # Define when macro
  engine$eval(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env = env)

  # Evaluate macro call
  result <- engine$eval(engine$read("(when #t 42)")[[1]], env = env)
  expect_equal(result, 42)

  result <- engine$eval(engine$read("(when #f 42)")[[1]], env = env)
  expect_null(result)
})

test_that("when macro with print works", {
  thin()
  env <- new.env()

  # Define when macro
  engine$eval(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env = env)

  # Test from plan: (when (> 5 3) (print "yes"))
  output <- capture.output({
    result <- engine$eval(engine$read('(when (> 5 3) (print "yes"))')[[1]], env = env)
  })

  expect_true(any(grepl("yes", output)))
})

test_that("unless macro works", {
  thin()
  env <- new.env()

  # Define unless macro - just swap the branches of if
  engine$eval(engine$read("(defmacro unless (test body) `(if ,test #nil ,body))")[[1]], env = env)

  result <- engine$eval(engine$read("(unless #f 42)")[[1]], env = env)
  expect_equal(result, 42)

  result <- engine$eval(engine$read("(unless #t 42)")[[1]], env = env)
  expect_null(result)
})

test_that("nested quasiquote works", {
  thin()
  env <- new.env()
  env$x <- 5

  # Nested quasiquote: outer unquote evaluates x, inner stays quoted
  result <- engine$eval(engine$read("``(+ 1 ,,x)")[[1]], env = env)

  # Should be (quasiquote (+ 1 (unquote 5)))
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "quasiquote")
  inner <- result[[2]]
  expect_true(is.call(inner))
  # The third element should be (unquote 5), not (unquote (unquote x))
  unq <- inner[[3]]
  expect_equal(as.character(unq[[1]]), "unquote")
  expect_equal(unq[[2]], 5)

  # Same pattern from the vignette: `(a `(b ,,x))
  env$x <- 10
  result2 <- engine$eval(engine$read("`(a `(b ,,x))")[[1]], env = env)
  expected <- quote(a(quasiquote(b(unquote(10)))))
  expect_identical(result2, expected)
})

test_that("macros can compose", {
  thin()
  env <- new.env()

  # Define a macro that uses begin
  engine$eval(engine$read("(defmacro let1 (var val body) `(begin (define ,var ,val) ,body))")[[1]], env = env)

  result <- engine$eval(engine$read("(let1 x 10 (+ x 5))")[[1]], env = env)
  expect_equal(result, 15)
})

test_that("macros with unquote-splicing work", {
  thin()
  env <- new.env()

  # Define a list for splicing
  env$items <- list(2, 3, 4)

  # Use unquote-splicing in a quasiquote
  result <- engine$eval(engine$read("`(+ 1 ,@items 5)")[[1]], env = env)

  # Should be (+ 1 2 3 4 5)
  expect_true(is.call(result))
  expect_equal(length(result), 6)
  expect_equal(result[[2]], 1)
  expect_equal(result[[3]], 2)
  expect_equal(result[[4]], 3)
  expect_equal(result[[5]], 4)
  expect_equal(result[[6]], 5)
})

test_that("macro-introduced bindings are hygienic", {
  thin()
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  engine$eval(engine$read("(define tmp 100)")[[1]], env = env)
  engine$eval(engine$read("(defmacro wrap (expr) `(begin (define tmp 1) ,expr))")[[1]], env = env)

  result <- engine$eval(engine$read("(wrap tmp)")[[1]], env = env)
  expect_equal(result, 100)
})

test_that("capture allows intentional binding capture", {
  thin()
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  engine$eval(
    engine$read(
      "(defmacro aif (test then alt)\n  `(let ((it ,test))\n     (if it ,(capture 'it then) ,(capture 'it alt))))"
    )[[1]],
    env = env
  )

  engine$eval(engine$read("(define it 99)")[[1]], env = env)
  result <- engine$eval(engine$read("(aif 10 it #nil)")[[1]], env = env)
  expect_equal(result, 10)
})

test_that("stdlib macros from files work", {
  thin()
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("control", "binding", "looping", "threading"), env = env)

  result <- engine$eval(engine$read("(cond ((> 1 2) 1) ((< 1 2) 2) (else 3))")[[1]], env = env)
  expect_equal(result, 2)

  result <- engine$eval(engine$read("(case 2 ((1) \"a\") ((2) \"b\") (else \"c\"))")[[1]], env = env)
  expect_equal(result, "b")

  result <- engine$eval(engine$read("(let ((x 1) (y 2)) (+ x y))")[[1]], env = env)
  expect_equal(result, 3)

  result <- engine$eval(engine$read("(let* ((x 1) (y (+ x 2))) y)")[[1]], env = env)
  expect_equal(result, 3)

  result <- engine$eval(engine$read("(letrec ((f (lambda (n) (if (< n 1) 0 (+ n (f (- n 1))))))) (f 3))")[[1]], env = env)
  expect_equal(result, 6)

  result <- engine$eval(
    engine$read(paste0(
      "(begin (define x 0) (define acc 0) ",
      "(while (< x 3) (set! acc (+ acc x)) (set! x (+ x 1))) ",
      "acc)"
    ))[[1]],
    env = env
  )
  expect_equal(result, 3)

  result <- engine$eval(engine$read("(-> 1 (+ 2) (* 3))")[[1]], env = env)
  expect_equal(result, 9)

  result <- engine$eval(engine$read("(->> (list 1 2 3) (map (lambda (x) (* x 2))) (reduce +))")[[1]], env = env)
  expect_equal(result, 12)

  result <- engine$eval(engine$read("(try-catch (error \"boom\") (catch e 42))")[[1]], env = env)
  expect_equal(result, 42)

  result <- engine$eval(
    engine$read(paste0(
      "(begin (define x 0) ",
      "(try-catch (begin (set! x 1) (error \"boom\")) ",
      "(catch e (set! x 2)) ",
      "(finally (set! x 3))) ",
      "x)"
    ))[[1]],
    env = env
  )
  expect_equal(result, 3)
})

test_that("letrec supports mutual recursion", {
  thin()
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  result <- engine$eval(
    engine$read(
      "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
         (even? 4))"
    )[[1]],
    env = env
  )

  expect_true(result)
})

test_that("destructuring bindings work for define and let forms", {
  thin()
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  engine$eval(engine$read("(define (a b . rest) (list 1 2 3 4))")[[1]], env = env)
  expect_equal(env$a, 1)
  expect_equal(env$b, 2)
  expect_equal(env$rest, list(3, 4))

  engine$eval(engine$read("(define ((x y) z) (list (list 9 8) 7))")[[1]], env = env)
  expect_equal(env$x, 9)
  expect_equal(env$y, 8)
  expect_equal(env$z, 7)

  result <- engine$eval(
    engine$read("(let (((a b) (list 1 2)) (c 3)) (+ a (+ b c)))")[[1]],
    env = env
  )
  expect_equal(result, 6)

  result <- engine$eval(
    engine$read("(let* (((a b) (list 1 2)) (c (+ a b))) c)")[[1]],
    env = env
  )
  expect_equal(result, 3)

  result <- engine$eval(
    engine$read(
      "(letrec (((even? odd?) (list (lambda (n) (if (= n 0) #t (odd? (- n 1))))\n        (lambda (n) (if (= n 0) #f (even? (- n 1))))))) (even? 4))"
    )[[1]],
    env = env
  )
  expect_true(result)
})

test_that("destructuring errors on arity mismatch", {
  thin()
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("binding"), env = env)

  expect_error(
    engine$eval(engine$read("(define (a b) (list 1))")[[1]], env = env),
    "expects 2 item"
  )
})

# ==============================================================================
# A. Simple Optional Parameters (7 tests)
# ==============================================================================

test_that("defmacro supports single optional parameter", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro opt-test ((x 42)) x)")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(opt-test)")[[1]], env = env)
  expect_equal(result, 42)

  result2 <- engine$eval(engine$read("(opt-test 100)")[[1]], env = env)
  expect_equal(result2, 100)
})

test_that("defmacro mixed required and optional", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro mix (a (b 10)) `(+ ,a ,b))")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(mix 5)")[[1]], env = env)
  expect_equal(result, 15)

  result2 <- engine$eval(engine$read("(mix 5 20)")[[1]], env = env)
  expect_equal(result2, 25)
})

test_that("defmacro all optional parameters", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro all-opt ((a 1) (b 2) (c 3)) `(list ,a ,b ,c))")[[1]],
    env = env
  )

  result1 <- engine$eval(engine$read("(all-opt)")[[1]], env = env)
  expect_equal(result1, list(1, 2, 3))

  result2 <- engine$eval(engine$read("(all-opt 10)")[[1]], env = env)
  expect_equal(result2, list(10, 2, 3))

  result3 <- engine$eval(engine$read("(all-opt 10 20)")[[1]], env = env)
  expect_equal(result3, list(10, 20, 3))

  result4 <- engine$eval(engine$read("(all-opt 10 20 30)")[[1]], env = env)
  expect_equal(result4, list(10, 20, 30))
})

test_that("defmacro optional at different positions", {
  thin()
  env <- new.env()
  # Optional in middle
  engine$eval(
    engine$read("(defmacro mid-opt (a (b 5) c) `(list ,a ,b ,c))")[[1]],
    env = env
  )

  result <- engine$eval(engine$read("(mid-opt 1 2 3)")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))

  # Can't call with just 2 args - c is required
  expect_error(
    engine$eval(engine$read("(mid-opt 1 2)")[[1]], env = env),
    "Missing required parameter"
  )
})

test_that("defmacro optional with rest", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro opt-rest ((x 1) . rest) `(list ,x ,@rest))")[[1]],
    env = env
  )

  result1 <- engine$eval(engine$read("(opt-rest)")[[1]], env = env)
  expect_equal(result1, list(1))

  result2 <- engine$eval(engine$read("(opt-rest 2)")[[1]], env = env)
  expect_equal(result2, list(2))

  result3 <- engine$eval(engine$read("(opt-rest 2 3 4)")[[1]], env = env)
  expect_equal(result3, list(2, 3, 4))
})

test_that("defmacro defaults see definition environment", {
  thin()
  env <- new.env()
  env$default_val <- 42
  engine$eval(
    engine$read("(defmacro env-test ((x default_val)) x)")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(env-test)")[[1]], env = env)
  expect_equal(result, 42)
})

test_that("defmacro defaults can be complex expressions", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro complex-default ((x (list 1 2 3))) x)")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(complex-default)")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))
})

# ==============================================================================
# B. Pattern Destructuring (10 tests)
# ==============================================================================

test_that("defmacro basic pattern destructuring", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro let-pair ((pattern (name value))) `(define ,name ,value))")[[1]],
    env = env
  )
  engine$eval(engine$read("(let-pair (x 10))")[[1]], env = env)
  result <- engine$eval(engine$read("x")[[1]], env = env)
  expect_equal(result, 10)
})

test_that("defmacro empty list pattern", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro empty-pat ((pattern ())) `'empty)")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(empty-pat ())")[[1]], env = env)
  expect_equal(result, as.symbol("empty"))
})

test_that("defmacro single element pattern", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro single ((pattern (x))) x)")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(single (42))")[[1]], env = env)
  expect_equal(result, 42)
})

test_that("defmacro nested patterns", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro nested ((pattern (a (b c)))) `(list ,a ,b ,c))")[[1]],
    env = env
  )
  result <- engine$eval(
    engine$read("(nested (1 (2 3)))")[[1]],
    env = env
  )
  expect_equal(result, list(1, 2, 3))
})

test_that("defmacro deeply nested patterns", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro deep ((pattern (a (b (c d))))) `(list ,a ,b ,c ,d))")[[1]],
    env = env
  )
  result <- engine$eval(
    engine$read("(deep (1 (2 (3 4))))")[[1]],
    env = env
  )
  expect_equal(result, list(1, 2, 3, 4))
})

test_that("defmacro pattern with internal rest", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro head-rest ((pattern (first . rest))) `(list ,first ,rest))")[[1]],
    env = env
  )
  result <- engine$eval(
    engine$read("(head-rest (1 2 3 4))")[[1]],
    env = env
  )
  expect_equal(result, list(1, list(2, 3, 4)))
})

test_that("defmacro multiple patterns", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro two-pairs ((pattern (a b)) (pattern (c d))) `(list ,a ,b ,c ,d))")[[1]],
    env = env
  )
  result <- engine$eval(
    engine$read("(two-pairs (1 2) (3 4))")[[1]],
    env = env
  )
  expect_equal(result, list(1, 2, 3, 4))
})

test_that("defmacro pattern with default", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro with-point ((pattern (x y) (list 0 0))) `(+ ,x ,y))")[[1]],
    env = env
  )

  result1 <- engine$eval(engine$read("(with-point)")[[1]], env = env)
  expect_equal(result1, 0)

  result2 <- engine$eval(engine$read("(with-point (3 4))")[[1]], env = env)
  expect_equal(result2, 7)
})

test_that("defmacro pattern with evaluated default", {
  thin()
  env <- new.env()
  env$make_pair <- function() list(5, 10)
  engine$eval(
    engine$read("(defmacro smart-default ((pattern (a b) (make_pair))) `(* ,a ,b))")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(smart-default)")[[1]], env = env)
  expect_equal(result, 50)
})

test_that("defmacro mixed patterns and simple params", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro complex (a (pattern (b c)) d) `(list ,a ,b ,c ,d))")[[1]],
    env = env
  )
  result <- engine$eval(
    engine$read("(complex 1 (2 3) 4)")[[1]],
    env = env
  )
  expect_equal(result, list(1, 2, 3, 4))
})

# ==============================================================================
# C. Pattern Rest Parameters (5 tests)
# ==============================================================================

test_that("defmacro pattern rest parameter", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro collect-pair (x . (pattern (a b))) `(list ,x ,a ,b))")[[1]],
    env = env
  )
  result <- engine$eval(
    engine$read("(collect-pair 1 2 3)")[[1]],
    env = env
  )
  expect_equal(result, list(1, 2, 3))
})

test_that("defmacro pattern rest with nesting", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro nest-rest (x . (pattern ((a b) c))) `(list ,x ,a ,b ,c))")[[1]],
    env = env
  )
  result <- engine$eval(
    engine$read("(nest-rest 1 (2 3) 4)")[[1]],
    env = env
  )
  expect_equal(result, list(1, 2, 3, 4))
})

test_that("defmacro optional with pattern rest", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro opt-pat-rest ((x 0) . (pattern (a b))) `(list ,x ,a ,b))")[[1]],
    env = env
  )

  result1 <- engine$eval(engine$read("(opt-pat-rest 1 2 3)")[[1]], env = env)
  expect_equal(result1, list(1, 2, 3))

  result2 <- engine$eval(engine$read("(opt-pat-rest 5 6 7)")[[1]], env = env)
  expect_equal(result2, list(5, 6, 7))
})

test_that("defmacro pattern rest with internal rest", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro double-rest (x . (pattern (a . rest))) `(list ,x ,a ,rest))")[[1]],
    env = env
  )
  result <- engine$eval(
    engine$read("(double-rest 1 2 3 4)")[[1]],
    env = env
  )
  expect_equal(result, list(1, 2, list(3, 4)))
})

test_that("defmacro empty pattern rest", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro empty-rest (x . (pattern ())) `'empty)")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(empty-rest 1)")[[1]], env = env)
  expect_equal(result, as.symbol("empty"))
})

# ==============================================================================
# D. Combined Features (5 tests)
# ==============================================================================

test_that("defmacro all patterns all defaults", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro all-default ((pattern (a b) (list 1 2)) (pattern (c d) (list 3 4))) `(list ,a ,b ,c ,d))")[[1]],
    env = env
  )

  result1 <- engine$eval(engine$read("(all-default)")[[1]], env = env)
  expect_equal(result1, list(1, 2, 3, 4))

  result2 <- engine$eval(engine$read("(all-default (10 20))")[[1]], env = env)
  expect_equal(result2, list(10, 20, 3, 4))

  result3 <- engine$eval(engine$read("(all-default (10 20) (30 40))")[[1]], env = env)
  expect_equal(result3, list(10, 20, 30, 40))
})

test_that("defmacro complex parameter mixing", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro mega-mix (req (opt 10) (pattern (a b)) (pattern (c d) (list 5 6))) `(list ,req ,opt ,a ,b ,c ,d))")[[1]],
    env = env
  )

  result <- engine$eval(
    engine$read("(mega-mix 1 2 (3 4))")[[1]],
    env = env
  )
  expect_equal(result, list(1, 2, 3, 4, 5, 6))
})

test_that("defmacro realistic let macro", {
  thin()
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("core", "list", "functional", "binding"), env = env)

  engine$eval(
    engine$read("(defmacro my-let (bindings . body) (begin (define expanded-bindings (map (lambda (b) `(define ,(car b) ,(cadr b))) bindings)) `(begin ,@expanded-bindings ,@body)))")[[1]],
    env = env
  )

  result <- engine$eval(
    engine$read("(my-let ((x 10) (y 20)) (+ x y))")[[1]],
    env = env
  )
  expect_equal(result, 30)
})

test_that("defmacro realistic cond with default", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro when-else ((pattern (test then) (list #t `'nothing))) `(if ,test ,then #nil))")[[1]],
    env = env
  )

  result1 <- engine$eval(engine$read("(when-else (#t 42))")[[1]], env = env)
  expect_equal(result1, 42)

  result2 <- engine$eval(engine$read("(when-else)")[[1]], env = env)
  expect_equal(result2, as.symbol("nothing"))
})

test_that("defmacro pattern optional and rest together", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro everything (req (pattern (a b) (list 1 2)) (opt 10) . rest) `(list ,req ,a ,b ,opt ,@rest))")[[1]],
    env = env
  )

  result <- engine$eval(
    engine$read("(everything 'x (3 4) 5 6 7)")[[1]],
    env = env
  )
  expect_equal(result, list(as.symbol("x"), 3, 4, 5, 6, 7))
})

# ==============================================================================
# E. Error Cases (8 tests)
# ==============================================================================

test_that("defmacro error on missing required", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro needs-req (a (b 5)) `(+ ,a ,b))")[[1]],
    env = env
  )
  expect_error(
    engine$eval(engine$read("(needs-req)")[[1]], env = env),
    "Missing required parameter"
  )
})

test_that("defmacro error on pattern too many elements", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro pair-only ((pattern (a b))) `(+ ,a ,b))")[[1]],
    env = env
  )
  expect_error(
    engine$eval(engine$read("(pair-only (1 2 3))")[[1]], env = env),
    "expects 2 item"
  )
})

test_that("defmacro error on pattern too few elements", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro triple ((pattern (a b c))) `(+ ,a ,b ,c))")[[1]],
    env = env
  )
  expect_error(
    engine$eval(engine$read("(triple (1 2))")[[1]], env = env),
    "expects 3 item"
  )
})

test_that("defmacro error on too many arguments", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro two-only (a b) `(+ ,a ,b))")[[1]],
    env = env
  )
  expect_error(
    engine$eval(engine$read("(two-only 1 2 3)")[[1]], env = env),
    "expects.*arguments"
  )
})

test_that("defmacro error on invalid parameter syntax", {
  thin()
  env <- new.env()
  expect_error(
    engine$eval(
      engine$read("(defmacro bad-param (123) 'x)")[[1]],
      env = env
    ),
    "must be symbols"
  )
})

test_that("defmacro error on invalid pattern wrapper", {
  thin()
  env <- new.env()
  expect_error(
    engine$eval(
      engine$read("(defmacro bad-pattern ((pattern)) 'x)")[[1]],
      env = env
    ),
    "pattern must be"
  )
})

test_that("defmacro error on empty pattern rest with args", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro empty-only (x . (pattern ())) x)")[[1]],
    env = env
  )
  expect_error(
    engine$eval(engine$read("(empty-only 1 2)")[[1]], env = env),
    "expects 0 item"
  )
})

test_that("defmacro error pattern rest arity", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro pair-rest (x . (pattern (a b))) `(list ,x ,a ,b))")[[1]],
    env = env
  )
  expect_error(
    engine$eval(engine$read("(pair-rest 1 2)")[[1]], env = env),
    "expects 2 item"
  )
})

# ==============================================================================
# F. Backward Compatibility (2 tests)
# ==============================================================================

test_that("defmacro with empty parameter list", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro always-42 () 42)")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(always-42)")[[1]], env = env)
  expect_equal(result, 42)
})

test_that("defmacro backward compatible simple symbols", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro old-style (a b) `(+ ,a ,b))")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(old-style 1 2)")[[1]], env = env)
  expect_equal(result, 3)
})

test_that("defmacro backward compatible simple rest", {
  thin()
  env <- new.env()
  engine$eval(
    engine$read("(defmacro old-rest (a . rest) `(list ,a ,@rest))")[[1]],
    env = env
  )
  result <- engine$eval(engine$read("(old-rest 1 2 3)")[[1]], env = env)
  expect_equal(result, list(1, 2, 3))
})
