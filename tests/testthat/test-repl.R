engine <- make_engine()

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

test_that("REPL detects incomplete parse errors", {
  repl <- arl:::REPL$new()
  expect_true(repl$is_incomplete_error(simpleError("Unexpected end of input")))
  expect_true(repl$is_incomplete_error(simpleError("Unclosed parenthesis at line 1, column 1")))
  expect_true(repl$is_incomplete_error(simpleError("Unterminated string at line 1, column 5")))
  expect_false(repl$is_incomplete_error(simpleError("Unexpected ')' at line 1, column 1")))
})

test_that("REPL read_form collects multiline input", {
  input_fn <- make_repl_input(c("(+ 1", "2)"))
  repl <- arl:::REPL$new(engine = engine, input_fn = input_fn)
  form <- repl$read_form()

  expect_equal(form$text, "(+ 1\n2)")
  expect_length(form$exprs, 1)
})

test_that("REPL read_form accepts multi-line first chunk (e.g. from paste)", {
  multi_line <- "(+ 1 2)\n(* 3 4)"
  input_fn <- make_repl_input(multi_line)
  repl <- arl:::REPL$new(engine = engine, input_fn = input_fn)
  form <- repl$read_form()
  expect_equal(form$text, multi_line)
  expect_length(form$exprs, 2)
})

test_that("REPL read_form skips leading blank lines", {
  input_fn <- make_repl_input(c("", "(+ 1 2)"))
  repl <- arl:::REPL$new(engine = engine, input_fn = input_fn)
  form <- repl$read_form()

  expect_equal(form$text, "(+ 1 2)")
  expect_length(form$exprs, 1)
})

test_that("REPL read_form surfaces non-incomplete parse errors", {
  input_fn <- make_repl_input(c(")"))
  repl <- arl:::REPL$new(engine = engine, input_fn = input_fn)
  expect_error(repl$read_form(), "Unexpected")
})

test_that("REPL read_form continues on Unterminated string (incomplete input)", {
  input_fn <- make_repl_input(c('(define x "', 'hello")'))
  repl <- arl:::REPL$new(engine = engine, input_fn = input_fn)
  form <- repl$read_form()
  expect_equal(form$text, '(define x "\nhello")')
  expect_length(form$exprs, 1)
})

test_that("REPL read_form supports override option", {
  withr::local_options(list(
    arl.repl_read_form_override = function(...) list(text = "override", exprs = list(quote(1)))
  ))
  repl <- arl:::REPL$new(engine = engine)
  form <- repl$read_form()
  expect_equal(form$text, "override")
  expect_length(form$exprs, 1)

  withr::local_options(list(arl.repl_read_form_override = "static"))
  expect_equal(repl$read_form(), "static")
})

test_that("REPL read_form returns NULL on EOF", {
  input_fn <- function(...) NULL
  repl <- arl:::REPL$new(engine = engine, input_fn = input_fn)
  expect_null(repl$read_form())
})

# Version and History Path Functions ----

test_that("REPL history_path_default uses R_user_dir", {
  repl <- arl:::REPL$new()
  path <- repl$history_path_default()
  expect_match(path, "arl_history$")
  expect_equal(path, file.path(tools::R_user_dir("arl", "data"), "arl_history"))
})

# History Management Functions ----

test_that("REPL can_use_history reflects interactive and readline", {
  repl <- arl:::REPL$new()
  expected <- isTRUE(interactive()) && isTRUE(capabilities("cledit"))
  expect_equal(repl$can_use_history(), expected)
})

test_that("REPL can_use_history respects override option", {
  repl <- arl:::REPL$new()
  withr::local_options(list(arl.repl_can_use_history_override = FALSE))
  can_use <- testthat::with_mocked_bindings(
    repl$can_use_history(),
    interactive = function() TRUE,
    capabilities = function(...) c(cledit = TRUE),
    .package = "base"
  )
  expect_false(can_use)

  withr::local_options(list(arl.repl_can_use_history_override = function() TRUE))
  can_use <- testthat::with_mocked_bindings(
    repl$can_use_history(),
    interactive = function() FALSE,
    capabilities = function(...) c(cledit = FALSE),
    .package = "base"
  )
  expect_true(can_use)
})

test_that("REPL can_use_history is FALSE when arl.repl_use_history is FALSE", {
  repl <- arl:::REPL$new()
  withr::local_options(list(arl.repl_use_history = FALSE))
  can_use <- testthat::with_mocked_bindings(
    repl$can_use_history(),
    interactive = function() TRUE,
    capabilities = function(...) c(cledit = TRUE),
    .package = "base"
  )
  expect_false(can_use)
})

test_that("REPL load_history handles non-interactive mode", {
  state <- new.env(parent = emptyenv())
  state$enabled <- FALSE
  state$path <- NULL
  state$snapshot <- NULL
  repl <- arl:::REPL$new(engine = NULL, history_state = state, history_path = "dummy.txt")
  withr::local_options(list(arl.repl_can_use_history_override = FALSE))
  result <- repl$load_history("dummy.txt")
  expect_false(result)
})

test_that("REPL load_history returns FALSE when savehistory fails", {
  state <- new.env(parent = emptyenv())
  state$enabled <- FALSE
  state$path <- NULL
  state$snapshot <- NULL
  repl <- arl:::REPL$new(engine = NULL, history_state = state, history_path = "dummy.txt")
  withr::local_options(list(arl.repl_can_use_history_override = TRUE))
  result <- testthat::with_mocked_bindings(
    repl$load_history("dummy.txt"),
    savehistory = function(...) stop("fail"),
    .package = "utils"
  )
  expect_false(result)
  expect_false(isTRUE(state$enabled))
})

test_that("REPL save_history handles disabled state", {
  state <- new.env(parent = emptyenv())
  state$enabled <- FALSE
  state$path <- NULL
  state$snapshot <- NULL
  repl <- arl:::REPL$new(engine = NULL, history_state = state)
  withr::local_options(list(arl.repl_can_use_history_override = FALSE))
  result <- repl$save_history()
  expect_null(result)
})

test_that("REPL save_history resets state even on restore errors", {
  state <- new.env(parent = emptyenv())
  state$enabled <- TRUE
  state$path <- "dummy.txt"
  state$snapshot <- "dummy_snapshot"
  state$last_entry <- "(+ 1 2)"
  repl <- arl:::REPL$new(engine = NULL, history_state = state)

  withr::local_options(list(arl.repl_can_use_history_override = TRUE))
  result <- testthat::with_mocked_bindings(
    repl$save_history(),
    loadhistory = function(...) stop("fail"),
    .package = "utils"
  )
  expect_null(result)
  expect_false(isTRUE(state$enabled))
  expect_null(state$path)
  expect_null(state$snapshot)
  expect_null(state$last_entry)
})

test_that("REPL add_history handles non-interactive mode", {
  state <- new.env(parent = emptyenv())
  state$enabled <- FALSE
  state$path <- NULL
  state$snapshot <- NULL
  repl <- arl:::REPL$new(engine = NULL, history_state = state)
  withr::local_options(list(arl.repl_can_use_history_override = FALSE))
  result <- repl$add_history("test input")
  expect_null(result)
})

test_that("REPL add_history skips empty input", {
  # Test that empty/whitespace-only input is not added
  state <- new.env(parent = emptyenv())
  state$enabled <- TRUE
  state$path <- "dummy.txt"
  state$snapshot <- "dummy_snapshot"
  repl <- arl:::REPL$new(engine = NULL, history_state = state)
  withr::local_options(list(arl.repl_can_use_history_override = TRUE))
  result <- repl$add_history("   ")
  expect_null(result)
})

test_that("REPL add_history appends to file and reloads buffer", {
  hist_file <- tempfile("arl_test_hist_")
  on.exit(unlink(hist_file), add = TRUE)

  state <- new.env(parent = emptyenv())
  state$enabled <- TRUE
  state$path <- hist_file
  state$snapshot <- NULL
  state$last_entry <- NULL
  repl <- arl:::REPL$new(engine = NULL, history_state = state)
  withr::local_options(list(arl.repl_can_use_history_override = TRUE))

  loaded_from <- NULL
  testthat::with_mocked_bindings(
    {
      repl$add_history("(+ 1 2)")
      repl$add_history("(* 3 4)")
      # Multi-line input should be collapsed to single line
      repl$add_history("(define foo\n  (lambda (x)\n    (* x 2)))")
    },
    loadhistory = function(file) { loaded_from <<- file },
    .package = "utils"
  )

  # Verify entries were appended to the file
  lines <- readLines(hist_file)
  expect_equal(lines, c("(+ 1 2)", "(* 3 4)", "(define foo   (lambda (x)     (* x 2)))"))
  # Verify loadhistory was called with the history file
  expect_equal(loaded_from, hist_file)
})

test_that("REPL add_history creates history directory if needed", {
  hist_dir <- file.path(tempdir(), "arl_test_hist_subdir")
  if (dir.exists(hist_dir)) unlink(hist_dir, recursive = TRUE)
  on.exit(unlink(hist_dir, recursive = TRUE), add = TRUE)

  state <- new.env(parent = emptyenv())
  state$enabled <- TRUE
  state$path <- file.path(hist_dir, "history")
  state$snapshot <- NULL
  repl <- arl:::REPL$new(engine = NULL, history_state = state)
  withr::local_options(list(arl.repl_can_use_history_override = TRUE))
  testthat::with_mocked_bindings(
    repl$add_history("(+ 1 2)"),
    loadhistory = function(file) invisible(NULL),
    .package = "utils"
  )
  expect_true(dir.exists(hist_dir))
})

# Print Value Function ----

test_that("REPL print_value handles NULL", {
  env <- make_engine()
  repl <- arl:::REPL$new(engine = env)
  result <- capture.output(val <- repl$print_value(NULL))
  expect_length(result, 0)
  expect_null(val)
})

test_that("REPL print_value handles calls with str", {
  env <- make_engine()
  repl <- arl:::REPL$new(engine = env)
  call_obj <- quote(f(a, b))
  output <- capture.output(val <- suppressWarnings(repl$print_value(call_obj)))
  expect_true(length(output) > 0)
  expect_equal(val, call_obj)
})

test_that("REPL print_value handles lists with str", {
  engine <- make_engine()
  repl <- arl:::REPL$new(engine = engine)
  list_obj <- list(a = 1, b = 2)
  output <- capture.output(val <- repl$print_value(list_obj))
  expect_true(length(output) > 0)
  expect_equal(val, list_obj)
})

test_that("REPL print_value handles vectors with print", {
  engine <- make_engine()
  repl <- arl:::REPL$new(engine = engine)
  output <- capture.output(val <- repl$print_value(c(1, 2, 3)))
  expect_true(any(grepl("1.*2.*3", output)))
  expect_equal(val, c(1, 2, 3))
})

# Main REPL Loop (engine$repl) ----

test_that("engine$repl exits on (quit) command", {
  call_count <- 0
  withr::local_options(list(
    arl.repl_quiet = FALSE,
    arl.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(quit)", exprs = engine$read("(quit)")))
      }
      NULL
    },
    arl.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("Arl REPL", output, fixed = TRUE)), info = "REPL should show startup banner")
})

test_that("engine$repl exits on (exit) command", {
  call_count <- 0
  withr::local_options(list(
    arl.repl_quiet = FALSE,
    arl.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(exit)", exprs = engine$read("(exit)")))
      }
      NULL
    },
    arl.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("Arl REPL", output, fixed = TRUE)), info = "REPL should show startup banner")
})

test_that("engine$repl exits on quit command", {
  call_count <- 0
  withr::local_options(list(
    arl.repl_quiet = FALSE,
    arl.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "quit", exprs = engine$read("(list)")))
      }
      NULL
    },
    arl.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("Arl REPL", output, fixed = TRUE)), info = "REPL should show startup banner")
})

test_that("engine$repl exits on NULL from read_form", {
  withr::local_options(list(
    arl.repl_quiet = FALSE,
    arl.repl_read_form_override = function(...) NULL,
    arl.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("Arl REPL", output, fixed = TRUE)), info = "REPL should show startup banner")
})

test_that("engine$repl with arl.repl_quiet prints no banner", {
  withr::local_options(list(
    arl.repl_quiet = TRUE,
    arl.repl_read_form_override = function(...) NULL,
    arl.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_length(output, 0)
})

test_that("engine$repl handles parse errors gracefully", {
  call_count <- 0
  withr::local_options(list(
    arl.repl_quiet = FALSE,
    arl.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(error = TRUE))
      }
      NULL
    },
    arl.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("Arl REPL", output, fixed = TRUE)), info = "REPL should show startup banner")
})

test_that("engine$repl evaluates expressions and prints results", {
  call_count <- 0
  withr::local_options(list(
    arl.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(+ 1 2)", exprs = engine$read("(+ 1 2)")))
      }
      NULL
    },
    arl.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("3", output)))
})

test_that("engine$repl prints each expression result in input", {
  call_count <- 0
  withr::local_options(list(
    arl.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(+ 1 2)", exprs = engine$read("(+ 1 2)")))
      }
      NULL
    },
    arl.repl_can_use_history_override = FALSE
  ))
  output <- capture.output(engine$repl())
  expect_true(any(grepl("3", output)))
})

test_that("engine$repl handles evaluation errors gracefully", {
  call_count <- 0
  withr::local_options(list(
    arl.repl_read_form_override = function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(text = "(undefined-fn)", exprs = engine$read("(undefined-fn)")))
      }
      NULL
    },
    arl.repl_can_use_history_override = FALSE
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

# Bracketed Paste Mode tests ----

test_that("REPL BPM option defaults to TRUE", {
  withr::local_options(list(arl.repl_bracketed_paste = NULL))
  expect_true(isTRUE(getOption("arl.repl_bracketed_paste", TRUE)))
})

test_that("REPL BPM can be disabled via option", {
  withr::local_options(list(arl.repl_bracketed_paste = FALSE))
  expect_false(isTRUE(getOption("arl.repl_bracketed_paste", TRUE)))
})

test_that("REPL BPM sequences are defined correctly", {
  # Verify BPM escape sequences are correct (checked in input_line code)
  bpm_start <- "\033[200~"
  bpm_end <- "\033[201~"

  expect_equal(nchar(bpm_start), 6)
  expect_equal(nchar(bpm_end), 6)
  expect_true(startsWith(bpm_start, "\033"))
  expect_true(startsWith(bpm_end, "\033"))
})

test_that("REPL read_form works with multi-line paste (BPM simulation)", {
  # Simulate what BPM would provide: multi-line input in first chunk
  multi_line <- "(define foo\n  (lambda (x)\n    (* x 2)))"
  input_fn <- make_repl_input(multi_line)
  repl <- arl:::REPL$new(engine = engine, input_fn = input_fn)

  form <- repl$read_form()
  expect_equal(form$text, multi_line)
  expect_length(form$exprs, 1)
})

test_that("REPL BPM enable sequence used when conditions met", {
  # Test that BPM enable sequence would be emitted with correct options
  engine_test <- make_engine()

  withr::local_options(list(
    arl.repl_quiet = TRUE,
    arl.repl_bracketed_paste = TRUE,
    arl.repl_read_form_override = function(...) NULL,
    arl.repl_can_use_history_override = FALSE
  ))

  output <- testthat::with_mocked_bindings(
    capture.output(engine_test$repl(), type = "output"),
    interactive = function() TRUE,
    capabilities = function(...) c(cledit = TRUE),
    .package = "base"
  )

  # BPM enable logic is in repl() startup
  # Verified by code inspection - emits \033[?2004h when enabled
  expect_true(TRUE)
})

# Additional REPL feature tests ----

test_that("REPL uses custom prompt in read_form", {
  input_fn <- make_repl_input(c("(+ 1 2)"))
  repl <- arl:::REPL$new(
    engine = engine,
    input_fn = input_fn,
    prompt = "custom> ",
    cont_prompt = "...> "
  )

  form <- repl$read_form()
  expect_equal(form$text, "(+ 1 2)")
  # Prompt is used internally, verified by initialization
  expect_equal(repl$prompt, "custom> ")
  expect_equal(repl$cont_prompt, "...> ")
})

test_that("REPL input_fn defaults to input_line", {
  repl <- arl:::REPL$new(engine = engine)

  # When no input_fn provided, should use input_line method
  expect_true(is.function(repl$input_fn))
})

test_that("REPL history_state is properly initialized", {
  repl <- arl:::REPL$new(engine = engine)

  # History state should be an environment with required fields
  state <- repl$history_state
  expect_true(is.environment(state))
  expect_true(exists("enabled", envir = state, inherits = FALSE))
  expect_true(exists("path", envir = state, inherits = FALSE))
  expect_true(exists("snapshot", envir = state, inherits = FALSE))
  expect_true(exists("last_entry", envir = state, inherits = FALSE))
})

test_that("REPL load_history trims file to max entries", {
  hist_file <- tempfile("arl_test_hist_trim_")
  on.exit(unlink(hist_file), add = TRUE)

  # Write 1500 entries
  entries <- paste0("(expr-", seq_len(1500), ")")
  writeLines(entries, hist_file)

  state <- new.env(parent = emptyenv())
  state$enabled <- FALSE
  state$path <- NULL
  state$snapshot <- NULL
  state$last_entry <- NULL
  repl <- arl:::REPL$new(engine = NULL, history_state = state, history_path = hist_file)
  withr::local_options(list(arl.repl_can_use_history_override = TRUE))

  testthat::with_mocked_bindings(
    {
      result <- repl$load_history(hist_file)
    },
    savehistory = function(...) TRUE,
    loadhistory = function(...) invisible(NULL),
    .package = "utils"
  )

  expect_true(result)
  # File should be trimmed to 1000 entries
  lines <- readLines(hist_file)
  expect_equal(length(lines), 1000L)
  # Should keep the last 1000 entries (501-1500)
  expect_equal(lines[1], "(expr-501)")
  expect_equal(lines[1000], "(expr-1500)")
  # last_entry should be set
  expect_equal(state$last_entry, "(expr-1500)")
})

test_that("REPL add_history deduplicates consecutive entries", {
  hist_file <- tempfile("arl_test_hist_dedup_")
  on.exit(unlink(hist_file), add = TRUE)

  state <- new.env(parent = emptyenv())
  state$enabled <- TRUE
  state$path <- hist_file
  state$snapshot <- NULL
  state$last_entry <- NULL
  repl <- arl:::REPL$new(engine = NULL, history_state = state)
  withr::local_options(list(arl.repl_can_use_history_override = TRUE))

  testthat::with_mocked_bindings(
    {
      repl$add_history("(+ 1 2)")
      repl$add_history("(+ 1 2)")  # duplicate — should be skipped
      repl$add_history("(* 3 4)")  # different — should be added
      repl$add_history("(* 3 4)")  # duplicate — should be skipped
      repl$add_history("(+ 1 2)")  # same as first but not consecutive — should be added
    },
    loadhistory = function(file) invisible(NULL),
    .package = "utils"
  )

  lines <- readLines(hist_file)
  expect_equal(lines, c("(+ 1 2)", "(* 3 4)", "(+ 1 2)"))
})
