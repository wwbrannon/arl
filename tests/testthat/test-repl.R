engine <- RyeEngine$new()

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
  form <- rye:::repl_read_form(input_fn = input_fn, engine = engine)

  expect_equal(form$text, "(+ 1\n2)")
  expect_length(form$exprs, 1)
})

test_that("repl_read_form skips leading blank lines", {
  input_fn <- make_repl_input(c("", "(+ 1 2)"))
  form <- rye:::repl_read_form(input_fn = input_fn, engine = engine)

  expect_equal(form$text, "(+ 1 2)")
  expect_length(form$exprs, 1)
})

test_that("repl_read_form surfaces non-incomplete parse errors", {
  input_fn <- make_repl_input(c(")"))
  expect_error(rye:::repl_read_form(input_fn = input_fn, engine = engine), "Unexpected")
})

test_that("repl_read_form supports override option", {
  withr::local_options(list(
    rye.repl_read_form_override = function(...) list(text = "override", exprs = list(quote(1)))
  ))
  form <- rye:::repl_read_form(engine = engine)
  expect_equal(form$text, "override")
  expect_length(form$exprs, 1)

  withr::local_options(list(rye.repl_read_form_override = "static"))
  expect_equal(rye:::repl_read_form(engine = engine), "static")
})

test_that("repl_read_form returns NULL on EOF", {
  input_fn <- function(...) NULL
  expect_null(rye:::repl_read_form(input_fn = input_fn, engine = engine))
})

# Version and History Path Functions ----

test_that("repl_version returns version string", {
  version <- rye:::repl_version()
  expect_match(version, "^Rye REPL")
  expect_type(version, "character")
})

test_that("repl_history_path returns home directory path", {
  path <- rye:::repl_history_path()
  expect_match(path, "\\.rye_history$")
  expect_type(path, "character")
})

# History Management Functions ----

test_that("repl_can_use_history reflects interactive and readline", {
  expected <- isTRUE(interactive()) && isTRUE(capabilities("readline"))
  expect_equal(rye:::repl_can_use_history(), expected)
})

test_that("repl_can_use_history respects override option", {
  withr::local_options(list(rye.repl_can_use_history_override = FALSE))
  can_use <- testthat::with_mocked_bindings(
    rye:::repl_can_use_history(),
    interactive = function() TRUE,
    capabilities = function(...) c(readline = TRUE),
    .package = "base"
  )
  expect_false(can_use)

  withr::local_options(list(rye.repl_can_use_history_override = function() TRUE))
  can_use <- testthat::with_mocked_bindings(
    rye:::repl_can_use_history(),
    interactive = function() FALSE,
    capabilities = function(...) c(readline = FALSE),
    .package = "base"
  )
  expect_true(can_use)
})

test_that("repl_load_history handles non-interactive mode", {
  result <- testthat::with_mocked_bindings(
    rye:::repl_load_history("dummy.txt"),
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_false(result)
})

test_that("repl_load_history returns FALSE when savehistory fails", {
  state <- rye:::repl_history_state
  old_enabled <- state$enabled
  old_path <- state$path
  old_snapshot <- state$snapshot
  on.exit({
    state$enabled <- old_enabled
    state$path <- old_path
    state$snapshot <- old_snapshot
  }, add = TRUE)

  result <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      rye:::repl_load_history("dummy.txt"),
      repl_can_use_history = function() TRUE,
      .env = asNamespace("rye")
    ),
    savehistory = function(...) stop("fail"),
    .package = "utils"
  )
  expect_false(result)
  expect_false(isTRUE(state$enabled))
})

test_that("repl_save_history handles disabled state", {
  result <- testthat::with_mocked_bindings(
    rye:::repl_save_history(),
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_null(result)
})

test_that("repl_save_history resets state even on save errors", {
  state <- rye:::repl_history_state
  old_enabled <- state$enabled
  old_path <- state$path
  old_snapshot <- state$snapshot
  on.exit({
    state$enabled <- old_enabled
    state$path <- old_path
    state$snapshot <- old_snapshot
  }, add = TRUE)

  state$enabled <- TRUE
  state$path <- "dummy.txt"
  state$snapshot <- "dummy_snapshot"

  result <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      rye:::repl_save_history(),
      repl_can_use_history = function() TRUE,
      .env = asNamespace("rye")
    ),
    savehistory = function(...) stop("fail"),
    loadhistory = function(...) stop("fail"),
    .package = "utils"
  )
  expect_null(result)
  expect_false(isTRUE(state$enabled))
  expect_null(state$path)
  expect_null(state$snapshot)
})

test_that("repl_add_history handles non-interactive mode", {
  result <- testthat::with_mocked_bindings(
    rye:::repl_add_history("test input"),
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_null(result)
})

test_that("repl_add_history skips empty input", {
  # Test that empty/whitespace-only input is not added
  result <- testthat::with_mocked_bindings(
    rye:::repl_add_history("   "),
    repl_can_use_history = function() TRUE,
    .env = asNamespace("rye")
  )
  expect_null(result)
})

test_that("repl_add_history handles missing addHistory", {
  result <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      rye:::repl_add_history("(+ 1 2)"),
      repl_can_use_history = function() TRUE,
      .env = asNamespace("rye")
    ),
    getFromNamespace = function(...) NULL,
    .package = "utils"
  )
  expect_null(result)
})

# Print Value Function ----

test_that("repl_print_value handles NULL", {
  env <- RyeEngine$new()
  result <- capture.output(val <- rye:::repl_print_value(NULL, env))
  expect_length(result, 0)
  expect_null(val)
})

test_that("repl_print_value handles calls with str", {
  env <- RyeEngine$new()
  call_obj <- quote(f(a, b))
  output <- capture.output(val <- rye:::repl_print_value(call_obj, env))
  expect_true(length(output) > 0)
  expect_equal(val, call_obj)
})

test_that("repl_print_value handles lists with str", {
  engine <- RyeEngine$new()
  list_obj <- list(a = 1, b = 2)
  output <- capture.output(val <- rye:::repl_print_value(list_obj, engine))
  expect_true(length(output) > 0)
  expect_equal(val, list_obj)
})

test_that("repl_print_value handles vectors with print", {
  engine <- RyeEngine$new()
  output <- capture.output(val <- rye:::repl_print_value(c(1, 2, 3), engine))
  expect_true(any(grepl("1.*2.*3", output)))
  expect_equal(val, c(1, 2, 3))
})

# Eval Wrapper Function ----

test_that("repl_eval_exprs delegates to engine$eval_exprs", {
  exprs <- engine$read("(+ 1 2)")
  result <- rye:::repl_eval_exprs(exprs, engine)
  expect_equal(result, 3)
})

# Main REPL Loop (engine$repl) ----

test_that("engine$repl exits on (quit) command", {
  output <- testthat::with_mocked_bindings(
    capture.output(engine$repl()),
    repl_read_form = function(...) {
      list(text = "(quit)", exprs = engine$read("(quit)"))
    },
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl exits on (exit) command", {
  output <- testthat::with_mocked_bindings(
    capture.output(engine$repl()),
    repl_read_form = function(...) {
      list(text = "(exit)", exprs = engine$read("(exit)"))
    },
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl exits on quit command", {
  output <- testthat::with_mocked_bindings(
    capture.output(engine$repl()),
    repl_read_form = function(...) {
      list(text = "quit", exprs = engine$read("(list)"))
    },
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl exits on NULL from read_form", {
  output <- testthat::with_mocked_bindings(
    capture.output(engine$repl()),
    repl_read_form = function(...) NULL,
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl handles parse errors gracefully", {
  call_count <- 0
  output <- testthat::with_mocked_bindings(
    capture.output(engine$repl()),
    repl_read_form = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        list(error = TRUE)
      } else {
        NULL
      }
    },
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl evaluates expressions and prints results", {
  call_count <- 0
  output <- testthat::with_mocked_bindings(
    capture.output(engine$repl()),
    repl_read_form = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        list(text = "(+ 1 2)", exprs = engine$read("(+ 1 2)"))
      } else {
        NULL
      }
    },
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_true(any(grepl("3", output)))
})

test_that("engine$repl prints each expression result in input", {
  call_count <- 0
  output <- testthat::with_mocked_bindings(
    capture.output(engine$repl()),
    repl_read_form = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        list(text = "(+ 1 2)", exprs = engine$read("(+ 1 2)"))
      } else {
        NULL
      }
    },
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_true(any(grepl("3", output)))
})

test_that("engine$repl handles evaluation errors gracefully", {
  call_count <- 0
  output <- testthat::with_mocked_bindings(
    capture.output(engine$repl()),
    repl_read_form = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        list(text = "(undefined-fn)", exprs = engine$read("(undefined-fn)"))
      } else {
        NULL
      }
    },
    repl_can_use_history = function() FALSE,
    .env = asNamespace("rye")
  )
  expect_true(any(grepl("Error", output)))
})
