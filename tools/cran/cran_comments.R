desc <- read.dcf("DESCRIPTION")
pkg <- desc[1, "Package"]
version <- desc[1, "Version"]

today <- format(Sys.Date(), "%Y-%m-%d")
platform <- paste(Sys.info()[c("sysname", "release", "machine")], collapse = " ")
local_r <- R.version$version.string

# Read status from check log (expects `make check` to have run)
log_path <- file.path("arl.Rcheck", "00check.log")
if (!file.exists(log_path)) {
  stop("Check log not found: ", log_path, "\nRun 'make check' first.")
}
log_lines <- readLines(log_path, warn = FALSE)
status_hits <- grep("^Status:", log_lines, value = TRUE)
status_line <- if (length(status_hits) > 0) {
  status_hits[length(status_hits)]
} else {
  "Status: UNKNOWN (no Status line in log)"
}

counts <- list(errors = "0", warnings = "0", notes = "0")
status_counts <- regmatches(
  status_line,
  regexec("([0-9]+) ERROR.*([0-9]+) WARNING.*([0-9]+) NOTE", status_line)
)[[1]]
if (length(status_counts) == 4) {
  counts$errors <- status_counts[2]
  counts$warnings <- status_counts[3]
  counts$notes <- status_counts[4]
}

if (grepl("OK", status_line)) {
  counts <- list(errors = "0", warnings = "0", notes = "0")
}

cran_comments <- c(
  paste("Package:", pkg),
  paste("Version:", version),
  paste("Date:", today),
  "",
  "## Test environments",
  paste("- local:", local_r, sprintf("(%s)", platform)),
  "- GitHub Actions: ubuntu-latest (devel, release, oldrel-1, oldrel-2, oldrel-3), windows-latest (release), macos-latest (release)",
  "",
  "## R CMD check results",
  sprintf("%s errors | %s warnings | %s notes", counts$errors, counts$warnings, counts$notes),
  "",
  "## Notes",
  if (version == "0.0.1") "- Initial submission." else "- Resubmission.",
  "- If any notes remain, explain them here."
)

writeLines(cran_comments, "cran-comments.md")

sha <- "UNKNOWN"
sha_out <- tryCatch(
  system("git rev-parse --short HEAD", intern = TRUE),
  error = function(e) character(0)
)
if (length(sha_out) > 0) {
  sha <- sha_out[1]
}

cran_submission <- c(
  paste("Version:", version),
  paste("Date:", today),
  paste("SHA:", sha)
)
writeLines(cran_submission, "CRAN-SUBMISSION")

cat("Wrote cran-comments.md and CRAN-SUBMISSION.\n")
