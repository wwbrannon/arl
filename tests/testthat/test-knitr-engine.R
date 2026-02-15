engine <- make_engine()

# ---------------------------------------------------------------------------
# evaluate_arl_code
# ---------------------------------------------------------------------------

test_that("evaluate_arl_code returns REPL transcript with prompt and output", {
  out <- arl:::evaluate_arl_code(engine, "(+ 1 2)")
  expect_match(out, "^arl> \\(\\+ 1 2\\)")
  expect_match(out, "#> 3")
})

test_that("evaluate_arl_code interleaves output per expression", {
  out <- arl:::evaluate_arl_code(engine, "(+ 1 2)\n(* 3 4)")
  lines <- strsplit(out, "\n")[[1]]
  # Source and output should alternate: source, output, source, output
  expect_equal(lines[1], "arl> (+ 1 2)")
  expect_equal(lines[2], "#> 3")
  expect_equal(lines[3], "arl> (* 3 4)")
  expect_equal(lines[4], "#> 12")
})

test_that("evaluate_arl_code suppresses invisible/NULL results", {
  # begin with only side-effect forms produces no visible output line
  out <- arl:::evaluate_arl_code(engine, '(begin (define knitr-test-x 42) #nil)')
  # Should have the prompted source but no #> output
  expect_match(out, "^arl> ")
  expect_false(grepl("#>", out))
})

test_that("evaluate_arl_code captures side-effect output", {
  out <- arl:::evaluate_arl_code(engine, '(display "hello")')
  expect_match(out, '#> "hello"')
})

test_that("evaluate_arl_code propagates errors by default", {
  expect_error(
    arl:::evaluate_arl_code(engine, "(error \"boom\")"),
    "boom"
  )
})

test_that("evaluate_arl_code returns empty string for empty input", {
  out <- arl:::evaluate_arl_code(engine, "")
  expect_equal(out, "")
})

test_that("evaluate_arl_code handles comments as gap lines", {
  out <- arl:::evaluate_arl_code(engine, ";; a comment\n(+ 1 2)")
  lines <- strsplit(out, "\n")[[1]]
  expect_equal(lines[1], "arl> ;; a comment")
  expect_equal(lines[2], "arl> (+ 1 2)")
  expect_equal(lines[3], "#> 3")
})

test_that("evaluate_arl_code preserves blank lines without prompt", {
  out <- arl:::evaluate_arl_code(engine, "(+ 1 2)\n\n(* 3 4)")
  lines <- strsplit(out, "\n")[[1]]
  expect_equal(lines[1], "arl> (+ 1 2)")
  expect_equal(lines[2], "#> 3")
  expect_equal(lines[3], "")
  expect_equal(lines[4], "arl> (* 3 4)")
  expect_equal(lines[5], "#> 12")
})

test_that("evaluate_arl_code handles multiline expressions", {
  out <- arl:::evaluate_arl_code(engine, "(define knitr-ml\n  42)")
  lines <- strsplit(out, "\n")[[1]]
  expect_equal(lines[1], "arl> (define knitr-ml")
  expect_equal(lines[2], "arl>   42)")
})

# ---------------------------------------------------------------------------
# get_arl_engine / reset_arl_engine
# ---------------------------------------------------------------------------

test_that("get_arl_engine returns an Engine instance", {
  # Reset first to ensure clean state
  arl:::reset_arl_engine()
  eng <- arl:::get_arl_engine()
  expect_true(inherits(eng, "Engine"))
})

test_that("get_arl_engine returns the same instance on repeated calls", {
  arl:::reset_arl_engine()
  eng1 <- arl:::get_arl_engine()
  eng2 <- arl:::get_arl_engine()
  expect_identical(eng1, eng2)
})

test_that("reset_arl_engine clears the cached engine", {
  arl:::reset_arl_engine()
  eng1 <- arl:::get_arl_engine()
  arl:::reset_arl_engine()
  eng2 <- arl:::get_arl_engine()
  expect_false(identical(eng1, eng2))
})

# ---------------------------------------------------------------------------
# State persistence across calls (simulating chunks)
# ---------------------------------------------------------------------------

test_that("state persists across evaluate_arl_code calls on same engine", {
  eng <- make_engine()
  arl:::evaluate_arl_code(eng, "(define knitr-persist-var 99)")
  out <- arl:::evaluate_arl_code(eng, "knitr-persist-var")
  expect_match(out, "#> 99")
})

# ---------------------------------------------------------------------------
# register_knitr_engine
# ---------------------------------------------------------------------------

test_that("register_knitr_engine registers the arl engine", {
  skip_if_not_installed("knitr")
  arl::register_knitr_engine()
  engines <- knitr::knit_engines$get()
  expect_true("arl" %in% names(engines))
  expect_true(is.function(engines$arl))
})

# ---------------------------------------------------------------------------
# eng_arl (the knitr engine function)
# ---------------------------------------------------------------------------

test_that("eng_arl evaluates code and produces output", {
  skip_if_not_installed("knitr")
  arl:::reset_arl_engine()

  options <- list(
    engine = "arl",
    code = c("(+ 1 2)"),
    eval = TRUE,
    echo = TRUE,
    error = FALSE,
    label = "test-chunk",
    fig.num = 0L,
    results = "markup"
  )
  result <- arl:::eng_arl(options)
  expect_true(is.character(result))
  expect_match(result, "3")
})

test_that("eng_arl respects eval=FALSE", {
  skip_if_not_installed("knitr")
  arl:::reset_arl_engine()

  options <- list(
    engine = "arl",
    code = c("(error \"should not run\")"),
    eval = FALSE,
    echo = TRUE,
    error = FALSE,
    label = "test-no-eval",
    fig.num = 0L,
    results = "markup"
  )
  # Should not error
  result <- arl:::eng_arl(options)
  expect_true(is.character(result))
})

test_that("eng_arl respects error=TRUE", {
  skip_if_not_installed("knitr")
  arl:::reset_arl_engine()

  options <- list(
    engine = "arl",
    code = c('(error "expected error")'),
    eval = TRUE,
    echo = TRUE,
    error = TRUE,
    label = "test-error-true",
    fig.num = 0L,
    results = "markup"
  )
  # Should not propagate the error
  result <- arl:::eng_arl(options)
  expect_true(is.character(result))
  expect_match(result, "expected error")
})

# ---------------------------------------------------------------------------
# arl_html_vignette
# ---------------------------------------------------------------------------

test_that("arl_html_vignette returns an rmarkdown output format", {
  skip_if_not_installed("rmarkdown")
  fmt <- arl::arl_html_vignette()
  expect_true(is.list(fmt))
  expect_true("knitr" %in% names(fmt))
})
