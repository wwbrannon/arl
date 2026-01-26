make_repl_input <- function(lines) {
  i <- 0
  function(prompt = "") {
    i <<- i + 1
    if (i > length(lines)) {
      return(NULL)
    }
    lines[[i]]
  }
}

test_that("repl_is_incomplete_error detects incomplete parse errors", {
  expect_true(rye:::repl_is_incomplete_error(simpleError("Unexpected end of input")))
  expect_true(rye:::repl_is_incomplete_error(simpleError("Unclosed parenthesis at line 1, column 1")))
  expect_false(rye:::repl_is_incomplete_error(simpleError("Unexpected ')' at line 1, column 1")))
})

test_that("repl_read_form collects multiline input", {
  input_fn <- make_repl_input(c("(+ 1", "2)"))
  form <- rye:::repl_read_form(input_fn = input_fn)

  expect_equal(form$text, "(+ 1\n2)")
  expect_length(form$exprs, 1)
})

test_that("repl_read_form skips leading blank lines", {
  input_fn <- make_repl_input(c("", "(+ 1 2)"))
  form <- rye:::repl_read_form(input_fn = input_fn)

  expect_equal(form$text, "(+ 1 2)")
  expect_length(form$exprs, 1)
})

test_that("repl_read_form surfaces non-incomplete parse errors", {
  input_fn <- make_repl_input(c(")"))
  expect_error(rye:::repl_read_form(input_fn = input_fn), "Unexpected")
})
