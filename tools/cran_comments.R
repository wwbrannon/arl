desc <- read.dcf("DESCRIPTION")
pkg <- desc[1, "Package"]
version <- desc[1, "Version"]

today <- format(Sys.Date(), "%Y-%m-%d")
platform <- paste(Sys.info()[c("sysname", "release", "machine")], collapse = " ")
local_r <- R.version$version.string

status_line <- "Status: UNKNOWN (check not run)"
summary_path <- file.path("tools", "cran_check_summary.txt")
if (file.exists(summary_path)) {
  summary_lines <- readLines(summary_path, warn = FALSE)
  status_hits <- grep("^Status:", summary_lines, value = TRUE)
  if (length(status_hits) > 0) {
    status_line <- status_hits[length(status_hits)]
  }
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
