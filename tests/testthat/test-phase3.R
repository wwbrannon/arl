test_that("gensym generates unique symbols", {
  s1 <- gensym()
  s2 <- gensym()
  expect_true(is.symbol(s1))
  expect_true(is.symbol(s2))
  expect_false(identical(s1, s2))
})

test_that("gensym accepts prefix", {
  s <- gensym("temp")
  expect_true(grepl("^temp__", as.character(s)))
})

test_that("quasiquote returns template", {
  env <- new.env()
  env$x <- 5

  # Basic quasiquote
  result <- rye_eval(rye_read("`(+ 1 2)")[[1]], env)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")
})

test_that("unquote evaluates within quasiquote", {
  env <- new.env()
  env$x <- 5

  result <- rye_eval(rye_read("`(+ 1 ,x)")[[1]], env)
  expect_true(is.call(result))
  expect_equal(result[[3]], 5)
})

test_that("unquote-splicing splices lists", {
  env <- new.env()
  env$nums <- quote(c(2, 3, 4))

  result <- rye_eval(rye_read("`(+ 1 ,@nums)")[[1]], env)
  expect_true(is.call(result))
  # Should be (+ 1 c 2 3 4)
  expect_equal(length(result), 6)
})

test_that("defmacro defines macros", {
  env <- new.env()

  # Define a simple macro
  rye_eval(rye_read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Check macro is defined
  expect_true(is_macro(as.symbol("when")))
})

test_that("macros expand correctly", {
  env <- new.env()

  # Define when macro
  rye_eval(rye_read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Expand macro
  expanded <- rye_macroexpand(rye_read("(when #t 42)")[[1]], env)

  # Should expand to (if #t 42 #nil)
  expect_true(is.call(expanded))
  expect_equal(as.character(expanded[[1]]), "if")
  expect_equal(expanded[[2]], TRUE)
  expect_equal(expanded[[3]], 42)
})

test_that("macros evaluate correctly", {
  env <- new.env()

  # Define when macro
  rye_eval(rye_read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Evaluate macro call
  result <- rye_eval(rye_read("(when #t 42)")[[1]], env)
  expect_equal(result, 42)

  result <- rye_eval(rye_read("(when #f 42)")[[1]], env)
  expect_null(result)
})

test_that("when macro with print works", {
  env <- new.env()

  # Define when macro
  rye_eval(rye_read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]], env)

  # Test from plan: (when (> 5 3) (print "yes"))
  output <- capture.output({
    result <- rye_eval(rye_read('(when (> 5 3) (print "yes"))')[[1]], env)
  })

  expect_true(any(grepl("yes", output)))
})

test_that("unless macro works", {
  env <- new.env()

  # Define unless macro - just swap the branches of if
  rye_eval(rye_read("(defmacro unless (test body) `(if ,test #nil ,body))")[[1]], env)

  result <- rye_eval(rye_read("(unless #f 42)")[[1]], env)
  expect_equal(result, 42)

  result <- rye_eval(rye_read("(unless #t 42)")[[1]], env)
  expect_null(result)
})

test_that("nested quasiquote works", {
  env <- new.env()
  env$x <- 5

  # Nested quasiquote
  result <- rye_eval(rye_read("``(+ 1 ,,x)")[[1]], env)

  # Should be `(+ 1 ,5)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "quasiquote")
})

test_that("macros can compose", {
  env <- new.env()

  # Define a macro that uses begin
  rye_eval(rye_read("(defmacro let1 (var val body) `(begin (define ,var ,val) ,body))")[[1]], env)

  result <- rye_eval(rye_read("(let1 x 10 (+ x 5))")[[1]], env)
  expect_equal(result, 15)
})

test_that("macros with unquote-splicing work", {
  env <- new.env()

  # Define a list for splicing
  env$items <- list(2, 3, 4)

  # Use unquote-splicing in a quasiquote
  result <- rye_eval(rye_read("`(+ 1 ,@items 5)")[[1]], env)

  # Should be (+ 1 2 3 4 5)
  expect_true(is.call(result))
  expect_equal(length(result), 6)
  expect_equal(result[[2]], 1)
  expect_equal(result[[3]], 2)
  expect_equal(result[[4]], 3)
  expect_equal(result[[5]], 4)
  expect_equal(result[[6]], 5)
})
