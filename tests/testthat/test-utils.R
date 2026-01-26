test_that("rye_trimws_compat trims whitespace", {
  input <- c("  hello  ", "\tgoodbye", "see ya\n")
  expect_equal(rye:::rye_trimws_compat(input), c("hello", "goodbye", "see ya"))
  expect_equal(rye:::rye_trimws_compat(input, which = "left"), c("hello  ", "goodbye", "see ya\n"))
  expect_equal(rye:::rye_trimws_compat(input, which = "right"), c("  hello", "\tgoodbye", "see ya"))
})

test_that("rye_trimws_shim matches base trimws when available", {
  if (!exists("trimws", mode = "function", inherits = TRUE)) {
    skip("base trimws not available")
  }
  input <- c("  hello  ", "\tgoodbye", "see ya\n")
  expect_equal(rye:::rye_trimws_shim(input), trimws(input))
  expect_equal(rye:::rye_trimws_shim(input, which = "left"), trimws(input, which = "left"))
  expect_equal(rye:::rye_trimws_shim(input, which = "right"), trimws(input, which = "right"))
})

test_that("rye_quote_arg quotes symbols and calls by default", {
  result <- rye:::rye_quote_arg(as.symbol("x"))
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "quote")
  expect_equal(result[[2]], as.symbol("x"))

  call_result <- rye:::rye_quote_arg(quote(f(1)))
  expect_true(is.call(call_result))

  literal_result <- rye:::rye_quote_arg(42)
  expect_equal(literal_result, 42)
})

test_that("rye_quote_arg can skip symbol quoting", {
  result <- rye:::rye_quote_arg(as.symbol("x"), quote_symbols = FALSE)
  expect_true(is.symbol(result))
  expect_equal(as.character(result), "x")
})

test_that("rye_do_call quotes symbols except for accessors", {
  a <- 10
  result <- rye:::rye_do_call(base::list, list(as.symbol("a")))
  expect_true(is.symbol(result[[1]]))
  expect_equal(as.character(result[[1]]), "a")

  obj <- list(a = 1)
  expect_equal(rye:::rye_do_call(base::`$`, list(obj, as.symbol("a"))), 1)
  expect_equal(rye:::rye_do_call(base::`[`, list(c(10, 20), 2)), 20)
  expect_equal(rye:::rye_do_call(base::`[[`, list(list(1, 2), 2)), 2)
})

test_that("rye_assign targets nearest rye environment", {
  root <- new.env(parent = emptyenv())
  assign(".rye_env", TRUE, envir = root)
  root$x <- 1
  child <- new.env(parent = root)
  assign(".rye_env", TRUE, envir = child)

  rye:::rye_assign("x", 2, child)
  expect_equal(root$x, 2)
  expect_false(exists("x", envir = child, inherits = FALSE))
})

test_that("rye_assign falls back to current env", {
  parent_env <- new.env(parent = emptyenv())
  child <- new.env(parent = parent_env)
  assign(".rye_env", TRUE, envir = child)

  rye:::rye_assign("z", 3, child)
  expect_true(exists("z", envir = child, inherits = FALSE))
  expect_false(exists("z", envir = parent_env, inherits = FALSE))
})
