# Run a single test file with Arl coverage instrumentation.
# Usage: Rscript tools/coverage/run-test-file.R tests/testthat/test-foo.R

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript tools/coverage/run-test-file.R <test-file-path>")
}
test_file <- args[1]
if (!file.exists(test_file)) {
  stop(sprintf("Test file not found: %s", test_file))
}

devtools::load_all(".", quiet = TRUE)

tracker <- arl::CoverageTracker$new()
engine <- arl::Engine$new(coverage_tracker = tracker)
tracker$discover_files()
options(arl.coverage_tracker = tracker)

testthat::set_max_fails(Inf)
testthat::test_file(test_file)
