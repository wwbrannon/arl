desc <- read.dcf("DESCRIPTION")
pkg <- desc[1, "Package"]
version <- desc[1, "Version"]

today <- format(Sys.Date(), "%Y-%m-%d")
platform <- paste(Sys.info()[c("sysname", "release", "machine")], collapse = " ")
local_r <- R.version$version.string

# Parse check results (expects `make check` to have run)
check_dir <- "arl.Rcheck"
if (!dir.exists(check_dir)) {
  stop("Check directory not found: ", check_dir, "\nRun 'make check' first.")
}
if (!requireNamespace("rcmdcheck", quietly = TRUE)) {
  stop("rcmdcheck is required but not installed. Install it with: install.packages('rcmdcheck')")
}
parsed <- rcmdcheck::parse_check(check_dir)
counts <- list(
  errors   = as.character(length(parsed$errors)),
  warnings = as.character(length(parsed$warnings)),
  notes    = as.character(length(parsed$notes))
)

# Build GA test environments line from check.yaml matrix
yaml_path <- ".github/workflows/check.yaml"
if (!file.exists(yaml_path)) {
  stop("CI workflow not found: ", yaml_path)
}
wf <- yaml::read_yaml(yaml_path)
configs <- wf$jobs$check$strategy$matrix$config
if (is.null(configs) || length(configs) == 0) {
  stop("No matrix configs found in ", yaml_path)
}

# Group R versions by OS
by_os <- list()
for (cfg in configs) {
  by_os[[cfg$os]] <- c(by_os[[cfg$os]], cfg$r)
}

ga_envs <- paste("- GitHub Actions:", paste(
  vapply(names(by_os), function(os) {
    sprintf("%s (%s)", os, paste(by_os[[os]], collapse = ", "))
  }, character(1)),
  collapse = ", "
))

# Check whether the package is already on CRAN
is_on_cran <- tryCatch({
  ap <- available.packages(repos = "https://cloud.r-project.org")
  pkg %in% rownames(ap)
}, error = function(e) FALSE)

cran_comments <- c(
  paste("Package:", pkg),
  paste("Version:", version),
  paste("Date:", today),
  "",
  "## Test environments",
  paste("- local:", local_r, sprintf("(%s)", platform)),
  ga_envs,
  "",
  "## R CMD check results",
  sprintf("%s errors | %s warnings | %s notes", counts$errors, counts$warnings, counts$notes)
)

if (length(parsed$notes) > 0) {
  cran_comments <- c(
    cran_comments,
    "",
    "## Notes",
    if (!is_on_cran) "- Initial CRAN submission." else character(0),
    paste("-", parsed$notes)
  )
} else if (!is_on_cran) {
  cran_comments <- c(
    cran_comments,
    "",
    "## Notes",
    "- Initial CRAN submission."
  )
}

writeLines(cran_comments, "cran-comments.md")

# URL check — run against an extracted copy of the built tarball so that
# files outside the package (tools/, benchmarks/, etc.) don't trigger
# spurious warnings.
tarball <- sprintf("%s_%s.tar.gz", pkg, version)
if (!file.exists(tarball)) {
  stop("Built tarball not found: ", tarball, "\nRun 'make build' first.")
}

cat("Running urlchecker::url_check()...\n")
url_issues <- tryCatch({
  if (!requireNamespace("urlchecker", quietly = TRUE)) {
    message("urlchecker not installed, skipping URL check.")
    NULL
  } else {
    tmp <- tempfile()
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::untar(tarball, exdir = tmp)
    urlchecker::url_check(file.path(tmp, pkg))
  }
}, error = function(e) {
  message("urlchecker failed: ", conditionMessage(e))
  NULL
})
if (!is.null(url_issues) && nrow(url_issues) > 0) {
  cat("\nURL issues found:\n")
  print(url_issues)
  cat("\nFix these before submitting to CRAN.\n")
} else if (!is.null(url_issues)) {
  cat("All URLs OK.\n")
}

sha <- "UNKNOWN"
sha_out <- tryCatch(
  system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
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
