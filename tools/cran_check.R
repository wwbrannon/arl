args <- commandArgs(trailingOnly = TRUE)

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required. Install with install.packages('devtools').")
}

check_args <- "--as-cran"
if (length(args) > 0) {
  check_args <- args
}

check_dir_default <- file.path(normalizePath("."), "rye.Rcheck")
if (dir.exists(check_dir_default)) {
  unlink(check_dir_default, recursive = TRUE)
}

result <- devtools::check(args = check_args)

check_dir <- check_dir_default
if (!is.null(result$checkdir) && nzchar(result$checkdir)) {
  check_dir <- result$checkdir
}

log_path <- file.path(check_dir, "00check.log")
status_line <- "Status: UNKNOWN (00check.log not found)"
if (file.exists(log_path)) {
  lines <- readLines(log_path, warn = FALSE)
  status_hits <- grep("^Status:", lines, value = TRUE)
  if (length(status_hits) > 0) {
    status_line <- status_hits[length(status_hits)]
  } else {
    status_line <- "Status: UNKNOWN (no Status line)"
  }
}

summary_path <- file.path("tools", "cran_check_summary.txt")
writeLines(c(paste("CheckDir:", check_dir), status_line), summary_path)

cat("CRAN check completed.\n")
cat("Summary written to", summary_path, "\n")
cat(status_line, "\n")
