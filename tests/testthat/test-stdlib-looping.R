# Looping construct tests: until, loop/recur

engine <- make_engine()

test_that("until macro repeats until test is truthy", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("looping"), env = env)

  result <- engine$eval(
    engine$read("(begin (define i 0) (until (= i 3) (set! i (+ i 1))) i)")[[1]],
    env = env
  )
  expect_equal(result, 3)
})

test_that("loop/recur iterates with rebinding", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("looping"), env = env)

  result <- engine$eval(
    engine$read("(loop ((i 0) (acc 0)) (if (< i 5) (recur (+ i 1) (+ acc i)) acc))")[[1]],
    env = env
  )
  expect_equal(result, 10)

  result <- engine$eval(
    engine$read("(loop ((x 1)) (+ x 2))")[[1]],
    env = env
  )
  expect_equal(result, 3)

  result <- engine$eval(
    engine$read("(loop ((i 0) (sum 0)) (if (< i 3) (recur (+ i 1) (+ sum (loop ((j 0) (acc 0)) (if (< j 2) (recur (+ j 1) (+ acc 1)) acc)))) sum))")[[1]],
    env = env
  )
  expect_equal(result, 6)

  result <- engine$eval(
    engine$read("(loop ((n 5) (acc 1)) (if (< n 2) acc (recur (- n 1) (* acc n))))")[[1]],
    env = env
  )
  expect_equal(result, 120)

  result <- engine$eval(
    engine$read("(loop ((xs (list 1 2 3)) (sum 0)) (if (null? xs) sum (recur (cdr xs) (+ sum (car xs)))))")[[1]],
    env = env
  )
  expect_equal(result, 6)
})

test_that("recur errors outside loop", {
  env <- new.env(parent = baseenv())
  toplevel_env(engine, env = env)
  import_stdlib_modules(engine, c("looping"), env = env)

  expect_error(engine$eval(engine$read("(recur 1)")[[1]], env = env), "recur can only be used inside loop")
})
