test_that("evaluator handles simple arithmetic", {
  expect_equal(rye_eval(rye_read("(+ 1 2)")[[1]]), 3)
  expect_equal(rye_eval(rye_read("(- 5 3)")[[1]]), 2)
  expect_equal(rye_eval(rye_read("(* 4 5)")[[1]]), 20)
  expect_equal(rye_eval(rye_read("(/ 10 2)")[[1]]), 5)
})

test_that("evaluator handles R functions", {
  result <- rye_eval(rye_read("(mean (c 1 2 3 4 5))")[[1]])
  expect_equal(result, 3)
})

test_that("evaluator handles nested calls", {
  result <- rye_eval(rye_read("(+ (* 2 3) (* 4 5))")[[1]])
  expect_equal(result, 26)
})

test_that("evaluator handles :: sugar", {
  result <- rye_eval(rye_read("(base::mean (c 1 2 3))")[[1]])
  expect_equal(result, 2)
})

test_that("calculator with nested expressions", {
  result <- rye_eval(rye_read("(+ 1 (* 2 3))")[[1]])
  expect_equal(result, 7)
})
