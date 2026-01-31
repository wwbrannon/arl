engine <- RyeEngine$new()

test_that("gensym generates unique symbols", {
  s1 <- engine$macro_expander$gensym()
  s2 <- engine$macro_expander$gensym()
  expect_true(is.symbol(s1))
  expect_true(is.symbol(s2))
  expect_false(identical(s1, s2))
})

test_that("gensym accepts prefix", {
  s <- engine$macro_expander$gensym("temp")
  expect_true(grepl("^temp__", as.character(s)))
})

test_that("quasiquote returns template", {
  env <- new.env()
  env$x <- 5

  # Basic quasiquote
  result <- engine$eval_in_env(engine$read("`(+ 1 2)")[[1]], env)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")
})

test_that("unquote evaluates within quasiquote", {
  env <- new.env()
  env$x <- 5

  result <- engine$eval_in_env(engine$read("`(+ 1 ,x)")[[1]], env)
  expect_true(is.call(result))
  expect_equal(result[[3]], 5)
})

test_that("unquote-splicing splices lists", {
  env <- new.env()
  env$nums <- quote(c(2, 3, 4))

  result <- engine$eval_in_env(engine$read("`(+ 1 ,@nums)")[[1]], env)
  expect_true(is.call(result))
  # Should be (+ 1 c 2 3 4)
  expect_equal(length(result), 6)
})

test_that("defmacro defines macros", {
  env <- new.env()

  # Define a simple macro
  engine$eval_in_env(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Check macro is defined
  expect_true(engine$macro_expander$is_macro(as.symbol("when"), env = env))
})

test_that("get_macro returns registered macro function", {
  env <- new.env()
  engine$eval_in_env(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env)

  macro_fn <- engine$macro_expander$get_macro(as.symbol("when"), env = env)
  expect_true(is.function(macro_fn))
})

test_that("defmacro validates dotted parameter lists", {
  env <- new.env()
  params_multi_dot <- c("a", ".", "b", ".", "c")
  engine$macro_expander$defmacro(as.symbol("bad"), params_multi_dot, list(as.symbol("a")), env = env)
  macro_fn <- engine$macro_expander$get_macro(as.symbol("bad"), env = env)
  expect_error(macro_fn(), "Dotted parameter list can only contain one '\\.'")

  params_bad_position <- c("a", ".", "b", "c")
  engine$macro_expander$defmacro(as.symbol("bad2"), params_bad_position, list(as.symbol("a")), env = env)
  macro_fn <- engine$macro_expander$get_macro(as.symbol("bad2"), env = env)
  expect_error(macro_fn(), "Dotted parameter list must have exactly one parameter after '\\.'")
})

test_that("macros expand correctly", {
  env <- new.env()

  # Define when macro
  engine$eval_in_env(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Expand macro
  expanded <- engine$macroexpand_in_env(engine$read("(when #t 42)")[[1]], env)

  # Should expand to (if #t 42 #nil)
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "if")
  expect_equal(expanded[[2]], TRUE)
  expect_equal(expanded[[3]], 42)
})

test_that("macros evaluate correctly", {
  env <- new.env()

  # Define when macro
  engine$eval_in_env(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Evaluate macro call
  result <- engine$eval_in_env(engine$read("(when #t 42)")[[1]], env)
  expect_equal(result, 42)

  result <- engine$eval_in_env(engine$read("(when #f 42)")[[1]], env)
  expect_null(result)
})

test_that("when macro with print works", {
  env <- new.env()

  # Define when macro
  engine$eval_in_env(engine$read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Test from plan: (when (> 5 3) (print "yes"))
  output <- capture.output({
    result <- engine$eval_in_env(engine$read('(when (> 5 3) (print "yes"))')[[1]], env)
  })

  expect_true(any(grepl("yes", output)))
})

test_that("unless macro works", {
  env <- new.env()

  # Define unless macro - just swap the branches of if
  engine$eval_in_env(engine$read("(defmacro unless (test body) `(if ,test #nil ,body))")[[1]], env)

  result <- engine$eval_in_env(engine$read("(unless #f 42)")[[1]], env)
  expect_equal(result, 42)

  result <- engine$eval_in_env(engine$read("(unless #t 42)")[[1]], env)
  expect_null(result)
})

test_that("nested quasiquote works", {
  env <- new.env()
  env$x <- 5

  # Nested quasiquote
  result <- engine$eval_in_env(engine$read("``(+ 1 ,,x)")[[1]], env)

  # Should be `(+ 1 ,5)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "quasiquote")
})

test_that("macros can compose", {
  env <- new.env()

  # Define a macro that uses begin
  engine$eval_in_env(engine$read("(defmacro let1 (var val body) `(begin (define ,var ,val) ,body))")[[1]], env)

  result <- engine$eval_in_env(engine$read("(let1 x 10 (+ x 5))")[[1]], env)
  expect_equal(result, 15)
})

test_that("macros with unquote-splicing work", {
  env <- new.env()

  # Define a list for splicing
  env$items <- list(2, 3, 4)

  # Use unquote-splicing in a quasiquote
  result <- engine$eval_in_env(engine$read("`(+ 1 ,@items 5)")[[1]], env)

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
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("binding"), env)

  engine$eval_in_env(engine$read("(define tmp 100)")[[1]], env)
  engine$eval_in_env(engine$read("(defmacro wrap (expr) `(begin (define tmp 1) ,expr))")[[1]], env)

  result <- engine$eval_in_env(engine$read("(wrap tmp)")[[1]], env)
  expect_equal(result, 100)
})

test_that("capture allows intentional binding capture", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("binding"), env)

  engine$eval_in_env(
    engine$read(
      "(defmacro aif (test then alt)\n  `(let ((it ,test))\n     (if it ,(capture 'it then) ,(capture 'it alt))))"
    )[[1]],
    env
  )

  engine$eval_in_env(engine$read("(define it 99)")[[1]], env)
  result <- engine$eval_in_env(engine$read("(aif 10 it #nil)")[[1]], env)
  expect_equal(result, 10)
})

test_that("stdlib macros from files work", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("control", "binding", "looping", "threading", "error"), env)

  result <- engine$eval_in_env(engine$read("(cond ((> 1 2) 1) ((< 1 2) 2) (else 3))")[[1]], env)
  expect_equal(result, 2)

  result <- engine$eval_in_env(engine$read("(case 2 (1 \"a\") (2 \"b\") (else \"c\"))")[[1]], env)
  expect_equal(result, "b")

  result <- engine$eval_in_env(engine$read("(let ((x 1) (y 2)) (+ x y))")[[1]], env)
  expect_equal(result, 3)

  result <- engine$eval_in_env(engine$read("(let* ((x 1) (y (+ x 2))) y)")[[1]], env)
  expect_equal(result, 3)

  result <- engine$eval_in_env(engine$read("(letrec ((f (lambda (n) (if (< n 1) 0 (+ n (f (- n 1))))))) (f 3))")[[1]], env)
  expect_equal(result, 6)

  result <- engine$eval_in_env(
    engine$read(paste0(
      "(begin (define x 0) (define acc 0) ",
      "(while (< x 3) (set! acc (+ acc x)) (set! x (+ x 1))) ",
      "acc)"
    ))[[1]],
    env
  )
  expect_equal(result, 3)

  result <- engine$eval_in_env(engine$read("(for (x (list 1 2 3)) (* x 2))")[[1]], env)
  expect_equal(result, list(2, 4, 6))

  result <- engine$eval_in_env(
    engine$read(paste0(
      "(begin (define x 0) ",
      "(while-r (< x 3) (<- x (+ x 1))) ",
      "x)"
    ))[[1]],
    env
  )
  expect_equal(result, 3)

  result <- engine$eval_in_env(
    engine$read(paste0(
      "(begin (define acc (list)) ",
      "(for-r (i (seq 1 3)) (<- acc (append acc (list i)))) ",
      "acc)"
    ))[[1]],
    env
  )
  expect_equal(result, list(1, 2, 3))

  result <- engine$eval_in_env(engine$read("(-> 1 (+ 2) (* 3))")[[1]], env)
  expect_equal(result, 9)

  result <- engine$eval_in_env(engine$read("(->> (list 1 2 3) (map (lambda (x) (* x 2))) (reduce +))")[[1]], env)
  expect_equal(result, 12)

  result <- engine$eval_in_env(engine$read("(try (error \"boom\") (catch e 42))")[[1]], env)
  expect_equal(result, 42)

  result <- engine$eval_in_env(
    engine$read(paste0(
      "(begin (define x 0) ",
      "(try (begin (set! x 1) (error \"boom\")) ",
      "(catch e (set! x 2)) ",
      "(finally (set! x 3))) ",
      "x)"
    ))[[1]],
    env
  )
  expect_equal(result, 3)
})

test_that("letrec supports mutual recursion", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("binding"), env)

  result <- engine$eval_in_env(
    engine$read(
      "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
         (even? 4))"
    )[[1]],
    env
  )

  expect_true(result)
})

test_that("destructuring bindings work for define and let forms", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("binding"), env)

  engine$eval_in_env(engine$read("(define (a b . rest) (list 1 2 3 4))")[[1]], env)
  expect_equal(env$a, 1)
  expect_equal(env$b, 2)
  expect_equal(env$rest, list(3, 4))

  engine$eval_in_env(engine$read("(define ((x y) z) (list (list 9 8) 7))")[[1]], env)
  expect_equal(env$x, 9)
  expect_equal(env$y, 8)
  expect_equal(env$z, 7)

  result <- engine$eval_in_env(
    engine$read("(let (((a b) (list 1 2)) (c 3)) (+ a (+ b c)))")[[1]],
    env
  )
  expect_equal(result, 6)

  result <- engine$eval_in_env(
    engine$read("(let* (((a b) (list 1 2)) (c (+ a b))) c)")[[1]],
    env
  )
  expect_equal(result, 3)

  result <- engine$eval_in_env(
    engine$read(
      "(letrec (((even? odd?) (list (lambda (n) (if (= n 0) #t (odd? (- n 1))))\n        (lambda (n) (if (= n 0) #f (even? (- n 1))))))) (even? 4))"
    )[[1]],
    env
  )
  expect_true(result)
})

test_that("destructuring errors on arity mismatch", {
  env <- new.env(parent = baseenv())
  stdlib_env(engine, env)
  import_stdlib_modules(engine, c("binding"), env)

  expect_error(
    engine$eval_in_env(engine$read("(define (a b) (list 1))")[[1]], env),
    "expects 2 item"
  )
})
