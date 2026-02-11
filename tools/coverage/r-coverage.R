# Run R code coverage and generate reports
# Output: coverage/r/{summary.txt, coverage.xml, index.html}

dir.create("coverage/r", showWarnings = FALSE, recursive = TRUE)

cov <- covr::package_coverage(type = "all", quiet = FALSE)
pct <- covr::percent_coverage(cov)

writeLines(c(
  paste0("rye Coverage: ", sprintf("%.2f%%", pct)),
  "",
  capture.output(print(cov), type = "message")
), "coverage/r/summary.txt")

covr::to_cobertura(cov, filename = "coverage/r/coverage.xml")
message(sprintf("R coverage: %.1f%%", pct))

if (requireNamespace("DT", quietly = TRUE) &&
    requireNamespace("htmltools", quietly = TRUE)) {
  covr::report(cov, file = "coverage/r/index.html")
  message("R coverage report: coverage/r/index.html")
} else {
  message("Skipping HTML report (install DT and htmltools packages for HTML output)")
}
