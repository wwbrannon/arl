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

test_that("RyeREPL detects incomplete parse errors", {
  repl <- rye:::RyeREPL$new()
  expect_true(repl$is_incomplete_error(simpleError("Unexpected end of input")))
  expect_true(repl$is_incomplete_error(simpleError("Unclosed parenthesis at line 1, column 1")))
  expect_true(repl$is_incomplete_error(simpleError("Unterminated string at line 1, column 5")))
  expect_false(repl$is_incomplete_error(simpleError("Unexpected ')' at line 1, column 1")))
})

test_that("RyeREPL read_form collects multiline input", {
  input_fn <- make_repl_input(c("(+ 1", "2)"))
  repl <- rye:::RyeREPL$new(engine = engine, input_fn = input_fn)
  form <- repl$read_form()

  expect_equal(form$text, "(+ 1\n2)")
  expect_length(form$exprs, 1)
})

test_that("RyeREPL read_form accepts multi-line first chunk (e.g. from paste)", {
  multi_line <- "(+ 1 2)\n(* 3 4)"
  input_fn <- make_repl_input(multi_line)
  repl <- rye:::RyeREPL$new(engine = engine, input_fn = input_fn)
  form <- repl$read_form()
  expect_equal(form$text, multi_line)
  expect_length(form$exprs, 2)
})

test_that("RyeREPL read_form skips leading blank lines", {
  input_fn <- make_repl_input(c("", "(+ 1 2)"))
  repl <- rye:::RyeREPL$new(engine = engine, input_fn = input_fn)
  form <- repl$read_form()

  expect_equal(form$text, "(+ 1 2)")
  expect_length(form$exprs, 1)
})

test_that("RyeREPL read_form surfaces non-incomplete parse errors", {
  input_fn <- make_repl_input(c(")"))
  repl <- rye:::RyeREPL$new(engine = engine, input_fn = input_fn)
  expect_error(repl$read_form(), "Unexpected")
})

test_that("RyeREPL read_form continues on Unterminated string (incomplete input)", {
  input_fn <- make_repl_input(c('(define x "', 'hello")'))
  repl <- rye:::RyeREPL$new(engine = engine, input_fn = input_fn)
  form <- repl$read_form()
  expect_equal(form$text, '(define x "\nhello")')
  expect_length(form$exprs, 1)
})

test_that("RyeREPL read_form supports override option", {
  withr::local_options(list(
    rye.repl_read_form_override = function(...) list(text = "override", exprs = list(quote(1)))
  ))
  repl <- rye:::RyeREPL$new(engine = engine)
  form <- repl$read_form()
  expect_equal(form$text, "override")
  expect_length(form$exprs, 1)

  withr::local_options(list(rye.repl_read_form_override = "static"))
  expect_equal(repl$read_form(), "static")
})

test_that("RyeREPL read_form returns NULL on EOF", {
  input_fn <- function(...) NULL
  repl <- rye:::RyeREPL$new(engine = engine, input_fn = input_fn)
  expect_null(repl$read_form())
})

# Version and History Path Functions ----

test_that("RyeREPL history_path_default returns home directory path", {
  repl <- rye:::RyeREPL$new()
  path <- repl$history_path_default()
  expect_match(path, "\\.rye_history$")
  expect_type(path, "character")
})

# History Management Functions ----

test_that("RyeREPL can_use_history reflects interactive and readline", {
  repl <- rye:::RyeREPL$new()
  expected <- isTRUE(interactive()) && isTRUE(capabilities("readline"))
  expect_equal(repl$can_use_history(), expected)
})

test_that("RyeREPL can_use_history respects override option", {
  repl <- rye:::RyeREPL$new()
  withr::local_options(list(rye.repl_can_use_history_override = FALSE))
  can_use <- testthat::with_mocked_bindings(
    repl$can_use_history(),
    interactive = function() TRUE,
    capabilities = function(...) c(readline = TRUE),
    .package = "base"
  )
  expect_false(can_use)

  withr::local_options(list(rye.repl_can_use_history_override = function() TRUE))
  can_use <- testthat::with_mocked_bindings(
    repl$can_use_history(),
    interactive = function() FALSE,
    capabilities = function(...) c(readline = FALSE),
    .package = "base"
  )
  expect_true(can_use)
})

test_that("RyeREPL can_use_history is FALSE when rye.repl_use_history is FALSE", {
  repl <- rye:::RyeREPL$new()
  withr::local_options(list(rye.repl_use_history = FALSE))
  can_use <- testthat::with_mocked_bindings(
    repl$can_use_history(),
    interactive = function() TRUE,
    capabilities = function(...) c(readline = TRUE),
    .package = "base"
  )
  expect_false(can_use)
})

test_that("RyeREPL load_history handles non-interactive mode", {
  state <- new.env(parent = emptyenv())
  state$enabled <- FALSE
  state$path <- NULL
  state$snapshot <- NULL
  repl <- rye:::RyeREPL$new(engine = NULL, history_state = state, history_path = "dummy.txt")
  withr::local_options(list(rye.repl_can_use_history_override = FALSE))
  result <- repl$load_history("dummy.txt")
  expect_false(result)
})

test_that("RyeREPL load_history returns FALSE when savehistory fails", {
  state <- new.env(parent = emptyenv())
  state$enabled <- FALSE
  state$path <- NULL
  state$snapshot <- NULL
  repl <- rye:::RyeREPL$new(engine = NULL, history_state = state, history_path = "dummy.txt")
  withr::local_options(list(rye.repl_can_use_history_override = TRUE))
  result <- testthat::with_mocked_bindings(
    repl$load_history("dummy.txt"),
    savehistory = function(...) stop("fail"),
    .package = "utils"
  )
  expect_false(result)
  expect_false(isTRUE(state$enabled))
})

test_that("RyeREPL save_history handles disabled state", {
  state <- new.env(parent = emptyenv())
  state$enabled <- FALSE
  state$path <- NULL
  state$snapshot <- NULL
  repl <- rye:::RyeREPL$new(engine = NULL, history_state = state)
  withr::local_options(list(rye.repl_can_use_history_override = FALSE))
  result <- repl$save_history()
  expect_null(result)
})

test_that("RyeREPL save_history resets state even on save errors", {
  state <- new.env(parent = emptyenv())
  state$enabled <- TRUE
  state$path <- "dummy.txt"
  state$snapshot <- "dummy_snapshot"
  repl <- rye:::RyeREPL$new(engine = NULL, history_state = state)

  withr::local_options(list(rye.repl_can_use_history_override = TRUE))
  result <- testthat::with_mocked_bindings(
    repl$save_history(),
    savehistory = function(...) stop("fail"),
    loadhistory = function(...) stop("fail"),
    .package = "utils"
  )
  expect_null(result)
  expect_false(isTRUE(state$enabled))
  expect_null(state$path)
  expect_null(state$snapshot)
})

test_that("RyeREPL add_history handles non-interactive mode", {
  state <- new.env(parent = emptyenv())
  state$enabled <- FALSE
  state$path <- NULL
  state$snapshot <- NULL
  repl <- rye:::RyeREPL$new(engine = NULL, history_state = state)
  withr::local_options(list(rye.repl_can_use_history_override = FALSE))
  result <- repl$add_history("test input")
  expect_null(result)
})

test_that("RyeREPL add_history skips empty input", {
  # Test that empty/whitespace-only input is not added
  state <- new.env(parent = emptyenv())
  state$enabled <- TRUE
  state$path <- "dummy.txt"
  state$snapshot <- "dummy_snapshot"
  repl <- rye:::RyeREPL$new(engine = NULL, history_state = state)
  withr::local_options(list(rye.repl_can_use_history_override = TRUE))
  result <- repl$add_history("   ")
  expect_null(result)
})

test_that("RyeREPL add_history handles missing addHistory", {
  state <- new.env(parent = emptyenv())
  state$enabled <- TRUE
  state$path <- "dummy.txt"
  state$snapshot <- "dummy_snapshot"
  repl <- rye:::RyeREPL$new(engine = NULL, history_state = state)
  withr::local_options(list(rye.repl_can_use_history_override = TRUE))
  result <- testthat::with_mocked_bindings(
    repl$add_history("(+ 1 2)"),
    getFromNamespace = function(...) NULL,
    .package = "utils"
  )
  expect_null(result)
})

# Print Value Function ----

test_that("RyeREPL print_value handles NULL", {
  env <- RyeEngine$new()
  repl <- rye:::RyeREPL$new(engine = env)
  result <- capture.output(val <- repl$print_value(NULL))
  expect_length(result, 0)
  expect_null(val)
})

test_that("RyeREPL print_value handles calls with str", {
  env <- RyeEngine$new()
  repl <- rye:::RyeREPL$new(engine = env)
  call_obj <- quote(f(a, b))
  output <- capture.output(val <- repl$print_value(call_obj))
  expect_true(length(output) > 0)
  expect_equal(val, call_obj)
})

test_that("RyeREPL print_value handles lists with str", {
  engine <- RyeEngine$new()
  repl <- rye:::RyeREPL$new(engine = engine)
  list_obj <- list(a = 1, b = 2)
  output <- capture.output(val <- repl$print_value(list_obj))
  expect_true(length(output) > 0)
  expect_equal(val, list_obj)
})

test_that("RyeREPL print_value handles vectors with print", {
  engine <- RyeEngine$new()
  repl <- rye:::RyeREPL$new(engine = engine)
  output <- capture.output(val <- repl$print_value(c(1, 2, 3)))
  expect_true(any(grepl("1.*2.*3", output)))
  expect_equal(val, c(1, 2, 3))
})

# Main REPL Loop (engine$repl) ----

test_that("engine$repl exits on (quit) command", {
  call_count <- 0
  withr::local_options(list(
    rye.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(quit)", exprs = engine$read("(quit)")))
      }
      NULL
    },
    rye.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl exits on (exit) command", {
  call_count <- 0
  withr::local_options(list(
    rye.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(exit)", exprs = engine$read("(exit)")))
      }
      NULL
    },
    rye.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl exits on quit command", {
  call_count <- 0
  withr::local_options(list(
    rye.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "quit", exprs = engine$read("(list)")))
      }
      NULL
    },
    rye.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl exits on NULL from read_form", {
  withr::local_options(list(
    rye.repl_read_form_override = function(...) NULL,
    rye.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl handles parse errors gracefully", {
  call_count <- 0
  withr::local_options(list(
    rye.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(error = TRUE))
      }
      NULL
    },
    rye.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("^Rye REPL", output)))
})

test_that("engine$repl evaluates expressions and prints results", {
  call_count <- 0
  withr::local_options(list(
    rye.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(+ 1 2)", exprs = engine$read("(+ 1 2)")))
      }
      NULL
    },
    rye.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("3", output)))
})

test_that("engine$repl prints each expression result in input", {
  call_count <- 0
  withr::local_options(list(
    rye.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(+ 1 2)", exprs = engine$read("(+ 1 2)")))
      }
      NULL
    },
    rye.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("3", output)))
})

test_that("engine$repl handles evaluation errors gracefully", {
  call_count <- 0
  withr::local_options(list(
    rye.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(undefined-fn)", exprs = engine$read("(undefined-fn)")))
      }
      NULL
    },
    rye.repl_can_use_history_override = FALSE
  ))
  tf <- tempfile()
  con <- file(tf, open = "w")
  sink(con)
  sink(con, type = "message")
  tryCatch(engine$repl(), error = function(e) NULL)
  sink(type = "message")
  sink()
  close(con)
  output <- readLines(tf, warn = FALSE)
  unlink(tf)
  expect_true(any(grepl("Error", output)))
})
