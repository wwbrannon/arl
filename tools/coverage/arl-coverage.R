# Arl Code Coverage Tool - CI Orchestration Script
#
# Runs native tests with execution coverage tracking and generates reports.
# Uses the CoverageTracker class from R/coverage.R.
#
# Usage:
#   source("tools/coverage/arl-coverage.R")
#   arl_coverage_report()

# Load package (use devtools in development mode)
if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(arl)
}

#' Run tests with execution coverage using testthat infrastructure
#'
#' @param engine Engine instance with coverage enabled
#' @return Invisible NULL
run_tests_with_coverage <- function(engine) {
  message("Running tests with coverage...")
  message("Note: Using testthat infrastructure to properly execute all tests")

  # Use testthat to run tests, which will call native test infrastructure
  # This properly executes all tests and calls stdlib functions
  suppressMessages({
    testthat::test_dir(
      "tests/testthat",
      reporter = testthat::ProgressReporter$new(max_failures = Inf),
      stop_on_failure = FALSE
    )
  })

  invisible(NULL)
}

#' Main entry point for Arl coverage reporting
#'
#' @param output Output format(s) - vector containing "console", "html", "json"
#' @param html_file Path to HTML output file
#' @param json_file Path to JSON output file (codecov format)
#' @param summary_file Path to text summary file (for CI)
#' @return CoverageTracker instance (invisibly)
#' @export
arl_coverage_report <- function(
  output = c("console", "html", "json"),
  html_file = "coverage/arl/index.html",
  json_file = "coverage/arl/coverage.json",
  summary_file = "coverage/arl/summary.txt"
) {
  # Create coverage tracker FIRST
  message("Creating coverage tracker...")
  tracker <- arl::CoverageTracker$new()

  # Create engine with coverage tracker
  # This ensures stdlib loading is tracked from the start
  message("Initializing Arl engine with coverage tracking...")
  engine <- arl::Engine$new(coverage_tracker = tracker)

  # Discover files to track
  message("Discovering .arl files (stdlib only, not tests)...")
  tracker$discover_files()

  if (length(tracker$all_files) == 0) {
    stop("No .arl files found")
  }

  message(sprintf("Found %d .arl files", length(tracker$all_files)))

  # Set global option so test engines pick up the tracker
  options(arl.coverage_tracker = tracker)
  on.exit(options(arl.coverage_tracker = NULL), add = TRUE)

  # Run tests (stdlib loads automatically during engine creation)
  run_tests_with_coverage(engine)

  # Generate reports using methods from R/coverage.R
  message("\nGenerating coverage reports...")

  if ("console" %in% output) {
    tracker$report_console()
  }

  if ("html" %in% output) {
    tracker$report_html(html_file)
  }

  if ("json" %in% output) {
    tracker$report_json(json_file)
  }

  # Also write console output to summary file for CI
  if (!is.null(summary_file)) {
    summary_dir <- dirname(summary_file)
    if (!dir.exists(summary_dir)) {
      dir.create(summary_dir, recursive = TRUE)
    }
    tracker$report_console(output_file = summary_file)
  }

  message("\nCoverage collection complete!")
  invisible(tracker)
}

# If run as a script (not sourced), execute the report
# Check if we're being executed directly vs. sourced
if (!interactive()) {
  args <- commandArgs(trailingOnly = FALSE)
  is_file_arg <- grep("^--file=", args, value = TRUE)
  if (length(is_file_arg) > 0 && grepl("arl-coverage\\.R$", is_file_arg[1])) {
    # Being run as Rscript tools/coverage/arl-coverage.R
    arl_coverage_report()
  }
}
