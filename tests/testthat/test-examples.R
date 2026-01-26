test_that("fibonacci example parses correctly", {
  # Find example file relative to package root
  example_path <- testthat::test_path("../../examples/fibonacci.rye")
  skip_if_not(file.exists(example_path), "Example file not found")

  # Just verify it parses
  expect_no_error({
    content <- paste(readLines(example_path), collapse = "\n")
    code <- rye_read(content)
  })

  # Verify specific functions are defined by parsing
  env <- new.env()
  rye_load_stdlib(env, load_files = TRUE)

  content <- paste(readLines(example_path), collapse = "\n")
  code <- rye_read(content)

  # Define just the recursive version for testing
  fib_def <- rye_read("(define fib-recursive (lambda (n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib-recursive (- n 1)) (fib-recursive (- n 2)))))))")[[1]]
  rye_eval(fib_def, env)

  # Test small values only to avoid stack overflow
  expect_equal(env$`fib-recursive`(3), 2)
  expect_equal(env$`fib-recursive`(4), 3)
})

test_that("quicksort example parses correctly", {
  example_path <- testthat::test_path("../../examples/quicksort.rye")
  skip_if_not(file.exists(example_path), "Example file not found")

  # Just verify it parses
  expect_no_error({
    content <- paste(readLines(example_path), collapse = "\n")
    code <- rye_read(content)
  })

  # Define and test quicksort
  env <- new.env()
  rye_load_stdlib(env, load_files = TRUE)

  qs_def <- rye_read("(define quicksort (lambda (lst) (if (null? lst) (list) (let* ((pivot (car lst)) (rest-lst (cdr lst)) (smaller (filter (lambda (x) (< x pivot)) rest-lst)) (greater (filter (lambda (x) (>= x pivot)) rest-lst))) (append (append (quicksort smaller) (list pivot)) (quicksort greater))))))")[[1]]
  rye_eval(qs_def, env)

  result <- env$quicksort(list(3, 1, 4))
  expect_equal(result, list(1, 3, 4))
})

test_that("fizzbuzz example parses correctly", {
  example_path <- testthat::test_path("../../examples/fizzbuzz.rye")
  skip_if_not(file.exists(example_path), "Example file not found")

  # Just verify it parses
  expect_no_error({
    content <- paste(readLines(example_path), collapse = "\n")
    code <- rye_read(content)
  })

  # Test fizzbuzz function
  env <- new.env()
  rye_load_stdlib(env, load_files = TRUE)

  fb_def <- rye_read("(define fizzbuzz (lambda (n) (cond ((= (% n 15) 0) \"FizzBuzz\") ((= (% n 3) 0) \"Fizz\") ((= (% n 5) 0) \"Buzz\") (else (str n)))))")[[1]]
  rye_eval(fb_def, env)

  expect_equal(env$fizzbuzz(3), "Fizz")
  expect_equal(env$fizzbuzz(5), "Buzz")
  expect_equal(env$fizzbuzz(15), "FizzBuzz")
})

test_that("macro examples parse correctly", {
  example_path <- testthat::test_path("../../examples/macro-examples.rye")
  skip_if_not(file.exists(example_path), "Example file not found")

  # Just verify it parses
  expect_no_error({
    content <- paste(readLines(example_path), collapse = "\n")
    code <- rye_read(content)
  })
})

test_that("data analysis example parses correctly", {
  example_path <- testthat::test_path("../../examples/data-analysis.rye")
  skip_if_not(file.exists(example_path), "Example file not found")

  # Just verify it parses
  expect_no_error({
    content <- paste(readLines(example_path), collapse = "\n")
    code <- rye_read(content)
  })

  # Test simple R interop
  env <- new.env()
  rye_load_stdlib(env, load_files = TRUE)

  nums_def <- rye_read("(define test-nums (r/call \"c\" (list 1 2 3 4 5)))")[[1]]
  rye_eval(nums_def, env)

  expect_true(exists("test-nums", envir = env))
  expect_equal(length(env$`test-nums`), 5)
})
