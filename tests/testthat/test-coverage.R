# Unit tests for RyeCoverageTracker (R/coverage.R)
#
# NOTE: To clean up root directory, manually remove:
#   cleanup_docs.sh, CLEANUP_NEEDED.md, validate_tests.R

# ============================================================================
# Test Harness Infrastructure
# ============================================================================

# Create .rye test files with controlled content
create_rye_file <- function(content, dir = NULL) {
  if (is.null(dir)) {
    file <- tempfile(fileext = ".rye")
  } else {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file <- file.path(dir, sprintf("test%d.rye", sample(1:10000, 1)))
  }
  writeLines(content, file)
  file
}

# Create mock rye_src for testing track()
make_rye_src <- function(file, start_line, end_line) {
  list(
    file = file,
    start_line = start_line,
    end_line = end_line
  )
}

# ============================================================================
# Phase 1: Foundation Tests (Initialize + Track)
# ============================================================================

test_that("initialize() creates default coverage tracker", {
  tracker <- RyeCoverageTracker$new()

  expect_type(tracker$coverage, "environment")
  expect_true(tracker$enabled)
  expect_equal(length(tracker$all_files), 0)
  expect_type(tracker$code_lines, "environment")
})

test_that("initialize() creates coverage environment with correct properties", {
  tracker <- RyeCoverageTracker$new()

  # Check environment properties
  expect_false(environmentIsLocked(tracker$coverage))
  expect_identical(parent.env(tracker$coverage), emptyenv())
})

test_that("initialize() creates code_lines environment with correct properties", {
  tracker <- RyeCoverageTracker$new()

  expect_false(environmentIsLocked(tracker$code_lines))
  expect_identical(parent.env(tracker$code_lines), emptyenv())
})

test_that("track() handles NULL rye_src", {
  tracker <- RyeCoverageTracker$new()

  # Should not error
  expect_silent(tracker$track(NULL))
  expect_equal(length(ls(tracker$coverage)), 0)
})

test_that("track() handles missing fields", {
  tracker <- RyeCoverageTracker$new()

  # Missing file
  expect_silent(tracker$track(list(start_line = 1, end_line = 1)))

  # Missing start_line
  expect_silent(tracker$track(list(file = "test.rye", end_line = 1)))

  # Missing end_line
  expect_silent(tracker$track(list(file = "test.rye", start_line = 1)))

  expect_equal(length(ls(tracker$coverage)), 0)
})

test_that("track() does nothing when enabled=FALSE", {
  tmp <- create_rye_file(c("(define x 1)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()
  tracker$set_enabled(FALSE)

  rye_src <- make_rye_src(tmp, start_line = 1, end_line = 1)
  tracker$track(rye_src)

  expect_equal(length(ls(tracker$coverage)), 0)
})

test_that("track() handles non-existent file gracefully", {
  tracker <- RyeCoverageTracker$new()

  rye_src <- make_rye_src("/nonexistent/file.rye", start_line = 1, end_line = 1)

  # Should not error, just skip tracking
  expect_silent(tracker$track(rye_src))
  expect_equal(length(ls(tracker$coverage)), 0)
})

test_that("track() marks single line as executed", {
  tmp <- create_rye_file(c(";; comment", "(define x 1)", "(define y 2)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()
  rye_src <- make_rye_src(tmp, start_line = 2, end_line = 2)

  tracker$track(rye_src)

  key <- paste0(tmp, ":2")
  expect_equal(tracker$coverage[[key]], 1L)
  expect_equal(length(ls(tracker$coverage)), 1)
})

test_that("track() marks multi-line range", {
  tmp <- create_rye_file(c("(define x 1)", "(define y 2)", "(define z 3)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()
  rye_src <- make_rye_src(tmp, start_line = 1, end_line = 3)

  tracker$track(rye_src)

  expect_equal(tracker$coverage[[paste0(tmp, ":1")]], 1L)
  expect_equal(tracker$coverage[[paste0(tmp, ":2")]], 1L)
  expect_equal(tracker$coverage[[paste0(tmp, ":3")]], 1L)
  expect_equal(length(ls(tracker$coverage)), 3)
})

test_that("track() increments count on multiple executions", {
  tmp <- create_rye_file(c("(define x 1)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()
  rye_src <- make_rye_src(tmp, start_line = 1, end_line = 1)

  tracker$track(rye_src)
  tracker$track(rye_src)
  tracker$track(rye_src)

  key <- paste0(tmp, ":1")
  expect_equal(tracker$coverage[[key]], 3L)
})

test_that("track() lazy-loads code_lines cache", {
  tmp <- create_rye_file(c(";; comment", "(define x 1)", "", "(define y 2)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()

  # code_lines should be empty initially
  expect_equal(length(ls(tracker$code_lines)), 0)

  rye_src <- make_rye_src(tmp, start_line = 2, end_line = 2)
  tracker$track(rye_src)

  # Now code_lines should be populated for this file
  expect_true(tmp %in% ls(tracker$code_lines))

  code_line_set <- tracker$code_lines[[tmp]]
  expect_true(2 %in% code_line_set)
  expect_true(4 %in% code_line_set)
  expect_false(1 %in% code_line_set)  # comment
  expect_false(3 %in% code_line_set)  # blank
})

test_that("track() filters comment and blank lines", {
  tmp <- create_rye_file(c(
    ";; This is a comment",
    "",
    "(define x 1)",
    "  ;; Another comment",
    "(define y 2)"
  ))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()

  # Track entire file
  rye_src <- make_rye_src(tmp, start_line = 1, end_line = 5)
  tracker$track(rye_src)

  # Only lines 3 and 5 should be tracked (code lines)
  expect_equal(length(ls(tracker$coverage)), 2)
  expect_true(!is.null(tracker$coverage[[paste0(tmp, ":3")]]))
  expect_true(!is.null(tracker$coverage[[paste0(tmp, ":5")]]))
  expect_null(tracker$coverage[[paste0(tmp, ":1")]])
  expect_null(tracker$coverage[[paste0(tmp, ":2")]])
  expect_null(tracker$coverage[[paste0(tmp, ":4")]])
})

test_that("track() respects custom code_line_pattern", {
  tmp <- create_rye_file(c(
    "# Python-style comment",
    "",
    "def foo():",
    "    pass"
  ))
  on.exit(unlink(tmp))

  # Use pattern that matches non-comment lines in Python-style
  tracker <- RyeCoverageTracker$new(code_line_pattern = "^\\s*[^#\\s]")

  rye_src <- make_rye_src(tmp, start_line = 1, end_line = 4)
  tracker$track(rye_src)

  # Lines 3 and 4 should match (not starting with # or blank)
  code_lines <- tracker$code_lines[[tmp]]
  expect_true(3 %in% code_lines)
  expect_true(4 %in% code_lines)
  expect_false(1 %in% code_lines)
  expect_false(2 %in% code_lines)
})

# ============================================================================
# Phase 2: Discovery & State Management
# ============================================================================

test_that("discover_files() with NULL search_paths uses stdlib", {
  tracker <- RyeCoverageTracker$new(search_paths = NULL)
  tracker$discover_files()

  # Should find stdlib files
  expect_true(length(tracker$all_files) > 0)

  # All files should be .rye files
  expect_true(all(grepl("\\.rye$", tracker$all_files)))
})

test_that("discover_files() with custom search_paths", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(define x 1)", file.path(tmp_dir, "test1.rye"))
  writeLines("(define y 2)", file.path(tmp_dir, "test2.rye"))

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  expect_equal(length(tracker$all_files), 2)
  expect_true(all(grepl("\\.rye$", tracker$all_files)))
})

test_that("discover_files() searches recursively", {
  tmp_dir <- tempfile()
  dir.create(file.path(tmp_dir, "subdir", "nested"), recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(define x 1)", file.path(tmp_dir, "test1.rye"))
  writeLines("(define y 2)", file.path(tmp_dir, "subdir", "test2.rye"))
  writeLines("(define z 3)", file.path(tmp_dir, "subdir", "nested", "test3.rye"))

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  expect_equal(length(tracker$all_files), 3)
})

test_that("discover_files() excludes test directories by default", {
  tmp_dir <- tempfile()
  dir.create(file.path(tmp_dir, "src"), recursive = TRUE)
  dir.create(file.path(tmp_dir, "tests"), recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(define x 1)", file.path(tmp_dir, "src", "code.rye"))
  writeLines("(define test 1)", file.path(tmp_dir, "tests", "test.rye"))

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir, include_tests = FALSE)
  tracker$discover_files()

  expect_equal(length(tracker$all_files), 1)
  expect_true(grepl("src/code.rye", tracker$all_files))
  expect_false(any(grepl("tests/", tracker$all_files)))
})

test_that("discover_files() includes test directories when include_tests=TRUE", {
  tmp_dir <- tempfile()
  dir.create(file.path(tmp_dir, "src"), recursive = TRUE)
  dir.create(file.path(tmp_dir, "tests"), recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(define x 1)", file.path(tmp_dir, "src", "code.rye"))
  writeLines("(define test 1)", file.path(tmp_dir, "tests", "test.rye"))

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir, include_tests = TRUE)
  tracker$discover_files()

  expect_equal(length(tracker$all_files), 2)
  expect_true(any(grepl("tests/", tracker$all_files)))
})

test_that("discover_files() handles non-existent directory", {
  tracker <- RyeCoverageTracker$new(search_paths = "/nonexistent/directory")

  # Should not error, just return empty
  expect_silent(tracker$discover_files())
  expect_equal(length(tracker$all_files), 0)
})

test_that("discover_files() handles multiple search_paths", {
  tmp_dir1 <- tempfile()
  tmp_dir2 <- tempfile()
  dir.create(tmp_dir1)
  dir.create(tmp_dir2)
  on.exit({
    unlink(tmp_dir1, recursive = TRUE)
    unlink(tmp_dir2, recursive = TRUE)
  })

  writeLines("(define x 1)", file.path(tmp_dir1, "test1.rye"))
  writeLines("(define y 2)", file.path(tmp_dir2, "test2.rye"))

  tracker <- RyeCoverageTracker$new(search_paths = c(tmp_dir1, tmp_dir2))
  tracker$discover_files()

  expect_equal(length(tracker$all_files), 2)
})

test_that("discover_files() deduplicates files", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("(define x 1)", file.path(tmp_dir, "test.rye"))

  # Pass same directory twice
  tracker <- RyeCoverageTracker$new(search_paths = c(tmp_dir, tmp_dir))
  tracker$discover_files()

  # Should only find the file once
  expect_equal(length(tracker$all_files), 1)
})

test_that("discover_files() populates code_lines cache", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  tmp_file <- file.path(tmp_dir, "test.rye")
  writeLines(c(";; comment", "(define x 1)", "", "(define y 2)"), tmp_file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # code_lines should be populated
  expect_true(tmp_file %in% ls(tracker$code_lines))
  code_lines <- tracker$code_lines[[tmp_file]]
  expect_true(2 %in% code_lines)
  expect_true(4 %in% code_lines)
  expect_false(1 %in% code_lines)
  expect_false(3 %in% code_lines)
})

test_that("discover_files() handles empty directory", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  expect_equal(length(tracker$all_files), 0)
})

test_that("get_summary() returns empty list for empty coverage", {
  tracker <- RyeCoverageTracker$new()

  summary <- tracker$get_summary()

  expect_type(summary, "list")
  expect_equal(length(summary), 0)
})

test_that("get_summary() returns single file/line structure", {
  tmp <- create_rye_file(c("(define x 1)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()
  rye_src <- make_rye_src(tmp, start_line = 1, end_line = 1)
  tracker$track(rye_src)

  summary <- tracker$get_summary()

  expect_equal(length(summary), 1)
  expect_true(tmp %in% names(summary))
  expect_equal(summary[[tmp]][["1"]], 1L)
})

test_that("get_summary() returns multiple files/lines structure", {
  tmp1 <- create_rye_file(c("(define x 1)", "(define y 2)"))
  tmp2 <- create_rye_file(c("(define z 3)"))
  on.exit({
    unlink(tmp1)
    unlink(tmp2)
  })

  tracker <- RyeCoverageTracker$new()
  tracker$track(make_rye_src(tmp1, 1, 2))
  tracker$track(make_rye_src(tmp2, 1, 1))

  summary <- tracker$get_summary()

  expect_equal(length(summary), 2)
  expect_true(tmp1 %in% names(summary))
  expect_true(tmp2 %in% names(summary))
  expect_equal(length(summary[[tmp1]]), 2)
  expect_equal(length(summary[[tmp2]]), 1)
})

test_that("get_summary() handles malformed keys gracefully", {
  tmp <- create_rye_file(c("(define x 1)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()

  # Manually add a malformed key (no colon)
  assign("badkey", 5L, envir = tracker$coverage)

  # Should not error
  summary <- tracker$get_summary()

  # Malformed key should be ignored
  expect_false("badkey" %in% names(summary))
})

test_that("get_summary() creates nested list structure correctly", {
  tmp <- create_rye_file(c("(define x 1)", "(define y 2)", "(define z 3)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()
  tracker$track(make_rye_src(tmp, 1, 1))
  tracker$track(make_rye_src(tmp, 1, 1))  # Execute line 1 twice
  tracker$track(make_rye_src(tmp, 3, 3))  # Execute line 3 once

  summary <- tracker$get_summary()

  # Check nested access: summary[[file]][[line]] = count
  expect_equal(summary[[tmp]][["1"]], 2L)
  expect_null(summary[[tmp]][["2"]])  # Line 2 not executed
  expect_equal(summary[[tmp]][["3"]], 1L)
})

test_that("reset() clears empty coverage", {
  tracker <- RyeCoverageTracker$new()

  tracker$reset()

  expect_equal(length(ls(tracker$coverage)), 0)
})

test_that("reset() clears populated coverage", {
  tmp <- create_rye_file(c("(define x 1)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()
  tracker$track(make_rye_src(tmp, 1, 1))

  expect_equal(length(ls(tracker$coverage)), 1)

  tracker$reset()

  expect_equal(length(ls(tracker$coverage)), 0)
})

test_that("reset() preserves all_files and code_lines", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  tmp_file <- file.path(tmp_dir, "test.rye")
  writeLines(c("(define x 1)"), tmp_file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  tracker$track(make_rye_src(tmp_file, 1, 1))

  expect_equal(length(tracker$all_files), 1)
  expect_equal(length(ls(tracker$code_lines)), 1)
  expect_equal(length(ls(tracker$coverage)), 1)

  tracker$reset()

  # all_files and code_lines preserved
  expect_equal(length(tracker$all_files), 1)
  expect_equal(length(ls(tracker$code_lines)), 1)
  # But coverage cleared
  expect_equal(length(ls(tracker$coverage)), 0)
})

test_that("set_enabled() toggles tracking", {
  tmp <- create_rye_file(c("(define x 1)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()

  expect_true(tracker$enabled)

  tracker$set_enabled(FALSE)
  expect_false(tracker$enabled)

  tracker$set_enabled(TRUE)
  expect_true(tracker$enabled)
})

test_that("disabled tracker ignores track() calls", {
  tmp <- create_rye_file(c("(define x 1)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()
  tracker$set_enabled(FALSE)

  tracker$track(make_rye_src(tmp, 1, 1))

  expect_equal(length(ls(tracker$coverage)), 0)
})

test_that("re-enabling resumes tracking", {
  tmp <- create_rye_file(c("(define x 1)", "(define y 2)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()

  # Track line 1
  tracker$track(make_rye_src(tmp, 1, 1))
  expect_equal(length(ls(tracker$coverage)), 1)

  # Disable and try to track line 2
  tracker$set_enabled(FALSE)
  tracker$track(make_rye_src(tmp, 2, 2))
  expect_equal(length(ls(tracker$coverage)), 1)  # Still only line 1

  # Re-enable and track line 2
  tracker$set_enabled(TRUE)
  tracker$track(make_rye_src(tmp, 2, 2))
  expect_equal(length(ls(tracker$coverage)), 2)  # Now both lines
})

# ============================================================================
# Phase 3: Console Reporting
# ============================================================================

test_that("report_console() shows message for empty coverage", {
  tracker <- RyeCoverageTracker$new()

  output <- capture.output(tracker$report_console())

  expect_true(any(grepl("No coverage data|0\\.0+%|0/0", output)))
})

test_that("report_console() discovers files if all_files empty", {
  # Use custom path so we know what to expect
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  tmp_file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", tmp_file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)

  expect_equal(length(tracker$all_files), 0)

  # Track something so coverage isn't empty (otherwise it returns early)
  tracker$track(make_rye_src(tmp_file, 1, 1))

  # report_console should trigger discover_files
  output <- capture.output(tracker$report_console())

  expect_true(length(tracker$all_files) > 0)
})

test_that("report_console() shows single file with partial coverage", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  tmp_file <- file.path(tmp_dir, "test.rye")
  writeLines(c("(define x 1)", "(define y 2)", "(define z 3)"), tmp_file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Only track line 1
  tracker$track(make_rye_src(tmp_file, 1, 1))

  output <- capture.output(tracker$report_console())
  output_text <- paste(output, collapse = "\n")

  # Should show filename
  expect_true(grepl("test\\.rye", output_text))

  # Should show coverage percentage (33% = 1/3 lines)
  expect_true(grepl("33\\.[0-9]+%", output_text) || grepl("1/3", output_text))
})

test_that("report_console() shows multiple files sorted alphabetically", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file_z <- file.path(tmp_dir, "z.rye")
  file_a <- file.path(tmp_dir, "a.rye")
  file_m <- file.path(tmp_dir, "m.rye")

  writeLines("(define x 1)", file_z)
  writeLines("(define y 2)", file_a)
  writeLines("(define z 3)", file_m)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Track at least one line in each file so they appear in output
  tracker$track(make_rye_src(file_z, 1, 1))
  tracker$track(make_rye_src(file_a, 1, 1))
  tracker$track(make_rye_src(file_m, 1, 1))

  output <- capture.output(tracker$report_console())
  output_text <- paste(output, collapse = "\n")

  # Find positions of filenames
  pos_a <- regexpr("a\\.rye", output_text)
  pos_m <- regexpr("m\\.rye", output_text)
  pos_z <- regexpr("z\\.rye", output_text)

  # Should be in alphabetical order
  expect_true(pos_a > 0, info = "a.rye not found in output")
  expect_true(pos_m > 0, info = "m.rye not found in output")
  expect_true(pos_z > 0, info = "z.rye not found in output")
  expect_true(pos_a < pos_m, info = "Files not in alphabetical order")
  expect_true(pos_m < pos_z, info = "Files not in alphabetical order")
})

test_that("report_console() calculates total lines correctly", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file1 <- file.path(tmp_dir, "file1.rye")
  file2 <- file.path(tmp_dir, "file2.rye")

  writeLines(c("(define x 1)", "(define y 2)"), file1)  # 2 lines
  writeLines(c("(define z 3)", "(define w 4)", "(define v 5)"), file2)  # 3 lines

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Track 1 line from file1, 2 lines from file2 = 3/5 total
  tracker$track(make_rye_src(file1, 1, 1))
  tracker$track(make_rye_src(file2, 1, 2))

  output <- capture.output(tracker$report_console())
  output_text <- paste(output, collapse = "\n")

  # Should show total: 3/5 = 60%
  expect_true(grepl("60\\.0+%", output_text) || grepl("3/5", output_text))
})

test_that("report_console() writes to file when output_file specified", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  out_file <- tempfile(fileext = ".txt")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(out_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  tracker$track(make_rye_src(file, 1, 1))

  # Capture console to ensure nothing printed
  output <- capture.output(tracker$report_console(output_file = out_file))

  # File should exist and contain report
  expect_true(file.exists(out_file))
  file_content <- readLines(out_file)
  expect_true(length(file_content) > 0)
  expect_true(any(grepl("test\\.rye", file_content)))
})

test_that("report_console() outputs to console when no output_file", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  tracker$track(make_rye_src(file, 1, 1))

  output <- capture.output(tracker$report_console())

  expect_true(length(output) > 0)
  expect_true(any(grepl("test\\.rye", output)))
})

# ============================================================================
# Phase 4: Advanced Reporting (HTML & JSON)
# ============================================================================

test_that("report_html() uses default output path", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(
    search_paths = tmp_dir,
    output_prefix = "test_prefix"
  )
  tracker$discover_files()

  # Change working directory temporarily
  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  suppressMessages(tracker$report_html())

  # Default path should be coverage/{prefix}/index.html
  expect_true(file.exists(file.path("coverage", "test_prefix", "index.html")))
})

test_that("report_html() uses custom output path", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  suppressMessages(tracker$report_html(output_file = html_file))

  expect_true(file.exists(html_file))
})

test_that("report_html() auto-creates output directory", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  output_dir <- file.path(tmp_dir, "deep", "nested", "dir")
  html_file <- file.path(output_dir, "report.html")
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  expect_false(dir.exists(output_dir))

  suppressMessages(tracker$report_html(output_file = html_file))

  expect_true(dir.exists(output_dir))
  expect_true(file.exists(html_file))
})

test_that("report_html() auto-discovers files if needed", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)

  # Don't call discover_files()
  expect_equal(length(tracker$all_files), 0)

  suppressMessages(tracker$report_html(output_file = html_file))

  # Should have auto-discovered
  expect_true(length(tracker$all_files) > 0)
  expect_true(file.exists(html_file))
})

test_that("report_html() generates valid HTML structure", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  suppressMessages(tracker$report_html(output_file = html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")

  expect_true(grepl("<!DOCTYPE html>", html_content))
  expect_true(grepl("<title>", html_content))
  expect_true(grepl("<table>", html_content))
})

test_that("report_html() uses custom report_title", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(
    search_paths = tmp_dir,
    report_title = "Custom Coverage Report"
  )
  tracker$discover_files()
  suppressMessages(tracker$report_html(output_file = html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")

  expect_true(grepl("<title>Custom Coverage Report</title>", html_content))
  expect_true(grepl("<h1>Custom Coverage Report</h1>", html_content))
})

test_that("report_html() includes summary table", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file1 <- file.path(tmp_dir, "test1.rye")
  file2 <- file.path(tmp_dir, "test2.rye")
  writeLines(c("(define x 1)", "(define y 2)"), file1)
  writeLines("(define z 3)", file2)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  tracker$track(make_rye_src(file1, 1, 1))
  suppressMessages(tracker$report_html(output_file = html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")

  expect_true(grepl("<table>", html_content))
  expect_true(grepl("<th>File</th>", html_content))
  expect_true(grepl("<th>Coverage %</th>", html_content))
})

test_that("report_html() uses coverage percentage CSS classes", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  # Create file with multiple lines for varying coverage
  file <- file.path(tmp_dir, "test.rye")
  writeLines(c("(define a 1)", "(define b 2)", "(define c 3)", "(define d 4)"), file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Track 3 out of 4 lines = 75%
  tracker$track(make_rye_src(file, 1, 3))

  suppressMessages(tracker$report_html(output_file = html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")

  # Should have CSS classes for coverage percentage
  expect_true(grepl("class=.(pct-high|pct-medium|pct-low).", html_content))
})

test_that("report_html() includes detailed file view sections", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines(c("(define x 1)", "(define y 2)"), file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  tracker$track(make_rye_src(file, 1, 1))
  suppressMessages(tracker$report_html(output_file = html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")

  # Should have h2 for file sections (not h3)
  expect_true(grepl("<h2>", html_content))

  # Should have file-content div
  expect_true(grepl("class=.file-content.", html_content))
})

test_that("report_html() shows line-by-line coverage with classes", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines(c("(define x 1)", "(define y 2)", "(define z 3)"), file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Track only line 1
  tracker$track(make_rye_src(file, 1, 1))

  suppressMessages(tracker$report_html(output_file = html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")

  # Should have covered and uncovered classes
  expect_true(grepl("covered", html_content))
  expect_true(grepl("uncovered", html_content))
})

test_that("report_html() displays hit counts", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Track line 3 times
  tracker$track(make_rye_src(file, 1, 1))
  tracker$track(make_rye_src(file, 1, 1))
  tracker$track(make_rye_src(file, 1, 1))

  suppressMessages(tracker$report_html(output_file = html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")

  # Should show hit count in format like "3x"
  expect_true(grepl("3x", html_content))
})

test_that("report_html() properly escapes HTML special characters", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  content <- c(
    "(define x \"<tag>\")",
    "(define y \"a & b\")",
    "(define z \"x > y\")"
  )
  writeLines(content, file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  suppressMessages(tracker$report_html(output_file = html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")

  # Critical: HTML special characters must be escaped
  expect_true(grepl("&lt;tag&gt;", html_content))
  expect_true(grepl("a &amp; b", html_content))
  expect_true(grepl("x &gt; y", html_content))
})

test_that("report_html() generates valid HTML for empty coverage", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Don't track anything
  suppressMessages(tracker$report_html(output_file = html_file))

  expect_true(file.exists(html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")
  expect_true(grepl("<!DOCTYPE html>", html_content))
  expect_true(grepl("<title>", html_content))
})

test_that("report_json() uses default output path", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(
    search_paths = tmp_dir,
    output_prefix = "test_json"
  )
  tracker$discover_files()

  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  suppressMessages(tracker$report_json())

  # Default path should be coverage/{prefix}/coverage.json
  expect_true(file.exists(file.path("coverage", "test_json", "coverage.json")))
})

test_that("report_json() uses custom output path", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  json_file <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(json_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  suppressMessages(tracker$report_json(output_file = json_file))

  expect_true(file.exists(json_file))
})

test_that("report_json() auto-creates output directory", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  output_dir <- file.path(tmp_dir, "deep", "nested", "dir")
  json_file <- file.path(output_dir, "coverage.json")
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  expect_false(dir.exists(output_dir))

  suppressMessages(tracker$report_json(output_file = json_file))

  expect_true(dir.exists(output_dir))
  expect_true(file.exists(json_file))
})

test_that("report_json() auto-discovers files if needed", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  json_file <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(json_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)

  expect_equal(length(tracker$all_files), 0)

  suppressMessages(tracker$report_json(output_file = json_file))

  expect_true(length(tracker$all_files) > 0)
  expect_true(file.exists(json_file))
})

test_that("report_json() generates codecov structure", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  json_file <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(json_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines("(define x 1)", file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  suppressMessages(tracker$report_json(output_file = json_file))

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    data <- jsonlite::fromJSON(json_file)
    expect_true("coverage" %in% names(data))
    expect_equal(length(data$coverage), 1)
  }
})

test_that("report_json() uses line coverage list format", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  json_file <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(json_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines(c("(define x 1)", "(define y 2)"), file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Track line 1 twice
  tracker$track(make_rye_src(file, 1, 1))
  tracker$track(make_rye_src(file, 1, 1))

  suppressMessages(tracker$report_json(output_file = json_file))

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    data <- jsonlite::fromJSON(json_file)

    # Get the coverage data for this file
    coverage_list <- data$coverage[[1]]

    # Line 1 should have count 2
    expect_equal(coverage_list[[1]], 2)

    # Line 2 should have count 0 (uncovered)
    expect_equal(coverage_list[[2]], 0)
  }
})

test_that("report_json() uses null for non-code lines", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  json_file <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(json_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines(c(";; comment", "(define x 1)", "", "(define y 2)"), file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  tracker$track(make_rye_src(file, 2, 2))

  suppressMessages(tracker$report_json(output_file = json_file))

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    data <- jsonlite::fromJSON(json_file)
    coverage_list <- data$coverage[[1]]

    # Line 1 (comment) should be null/NA (jsonlite converts null to NA)
    expect_true(is.na(coverage_list[[1]]))

    # Line 2 (code) should have count
    expect_equal(coverage_list[[2]], 1)

    # Line 3 (blank) should be null/NA
    expect_true(is.na(coverage_list[[3]]))

    # Line 4 (code) should have count 0 (uncovered)
    expect_equal(coverage_list[[4]], 0)
  }
})

test_that("report_json() shows 0 for uncovered code lines", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  json_file <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(json_file)
  })

  file <- file.path(tmp_dir, "test.rye")
  writeLines(c("(define x 1)", "(define y 2)"), file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Track only line 1
  tracker$track(make_rye_src(file, 1, 1))

  suppressMessages(tracker$report_json(output_file = json_file))

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    data <- jsonlite::fromJSON(json_file)
    coverage_list <- data$coverage[[1]]

    # Line 1 covered
    expect_equal(coverage_list[[1]], 1)

    # Line 2 uncovered (should be 0, not null)
    expect_equal(coverage_list[[2]], 0)
  }
})

# ============================================================================
# Phase 5: Integration Tests
# ============================================================================

test_that("RyeEngine accepts coverage_tracker parameter", {
  tracker <- RyeCoverageTracker$new()
  engine <- RyeEngine$new(use_env_cache = FALSE, coverage_tracker = tracker)

  expect_s3_class(engine, "RyeEngine")
})

test_that("RyeEngine tracks coverage for executed code", {
  tmp <- tempfile(fileext = ".rye")
  writeLines(c(
    "(define add (lambda (x y) (+ x y)))",
    "(define result (add 1 2))"
  ), tmp)
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new(search_paths = dirname(tmp))
  engine <- RyeEngine$new(use_env_cache = FALSE, coverage_tracker = tracker)

  tracker$discover_files()
  engine$load_file(tmp)

  summary <- tracker$get_summary()

  # Both lines should be covered
  expect_equal(length(summary[[tmp]]), 2)
  expect_true("1" %in% names(summary[[tmp]]))
  expect_true("2" %in% names(summary[[tmp]]))
})

test_that("disabled coverage tracker doesn't track", {
  tmp <- tempfile(fileext = ".rye")
  writeLines(c("(define x 1)"), tmp)
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new(search_paths = dirname(tmp))
  tracker$set_enabled(FALSE)

  engine <- RyeEngine$new(use_env_cache = FALSE, coverage_tracker = tracker)

  tracker$discover_files()
  engine$load_file(tmp)

  summary <- tracker$get_summary()

  # Should have no coverage data
  expect_equal(length(summary), 0)
})

test_that("coverage tracking persists across multiple evaluations", {
  tmp <- tempfile(fileext = ".rye")
  writeLines(c(
    "(define counter 0)",
    "(define inc (lambda () (set! counter (+ counter 1))))"
  ), tmp)
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new(search_paths = dirname(tmp))
  engine <- RyeEngine$new(use_env_cache = FALSE, coverage_tracker = tracker)

  tracker$discover_files()

  # Load file multiple times
  engine$load_file(tmp)
  engine$load_file(tmp)
  engine$load_file(tmp)

  summary <- tracker$get_summary()

  # Line 1 should be executed 3 times
  expect_equal(summary[[tmp]][["1"]], 3)

  # Line 2 should be executed 3 times
  expect_equal(summary[[tmp]][["2"]], 3)
})

# ============================================================================
# Edge Cases
# ============================================================================

test_that("handles empty files", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file <- file.path(tmp_dir, "empty.rye")
  writeLines(character(0), file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Should not error
  expect_equal(length(tracker$all_files), 1)

  # code_lines should show 0 code lines
  expect_true(file %in% ls(tracker$code_lines))
  expect_equal(length(tracker$code_lines[[file]]), 0)
})

test_that("handles files with only comments and blanks", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file <- file.path(tmp_dir, "comments.rye")
  writeLines(c(";; Comment 1", "", ";; Comment 2", ""), file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Should have 0 code lines
  expect_equal(length(tracker$code_lines[[file]]), 0)

  # Report should handle gracefully
  output <- capture.output(tracker$report_console())
  expect_true(length(output) > 0)
})

test_that("handles 100% coverage", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file <- file.path(tmp_dir, "full.rye")
  writeLines(c("(define x 1)", "(define y 2)"), file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()

  # Track all lines
  tracker$track(make_rye_src(file, 1, 2))

  output <- capture.output(tracker$report_console())
  output_text <- paste(output, collapse = "\n")

  # Should show 100%
  expect_true(grepl("100\\.0+%", output_text))
})

test_that("handles very large execution counts", {
  tmp <- create_rye_file(c("(define x 1)"))
  on.exit(unlink(tmp))

  tracker <- RyeCoverageTracker$new()
  rye_src <- make_rye_src(tmp, 1, 1)

  # Execute 1000 times (reduced from 10000 for speed)
  for (i in 1:1000) {
    tracker$track(rye_src)
  }

  key <- paste0(tmp, ":1")
  expect_equal(tracker$coverage[[key]], 1000L)

  # Reports should handle large counts
  output <- capture.output(tracker$report_console())
  expect_true(length(output) > 0)
})

test_that("HTML escaping prevents XSS", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  html_file <- tempfile(fileext = ".html")
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    unlink(html_file)
  })

  file <- file.path(tmp_dir, "xss.rye")
  # Potential XSS vectors
  content <- c(
    "(define x \"<script>alert('xss')</script>\")",
    "(define y \"<img src=x onerror=alert(1)>\")",
    "(define z \"<iframe src=evil.com></iframe>\")"
  )
  writeLines(content, file)

  tracker <- RyeCoverageTracker$new(search_paths = tmp_dir)
  tracker$discover_files()
  suppressMessages(tracker$report_html(output_file = html_file))

  html_content <- paste(readLines(html_file), collapse = "\n")

  # Check that dangerous strings are properly escaped
  expect_true(grepl("&lt;script&gt;", html_content))
  expect_true(grepl("&lt;img", html_content))
  expect_true(grepl("&lt;iframe", html_content))

  # Should not have literal unescaped versions
  expect_false(grepl("<script>alert", html_content))
  expect_false(grepl("<img src=x onerror", html_content))
})
