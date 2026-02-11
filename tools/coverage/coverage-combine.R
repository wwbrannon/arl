# Combined Coverage Report Generator
# Merges R and Rye coverage data into unified reports

#' Parse R coverage summary
parse_r_coverage <- function(summary_file = "coverage/r/summary.txt") {
  if (!file.exists(summary_file)) {
    warning("R coverage summary not found")
    return(NULL)
  }

  # Read summary and extract percentage
  lines <- readLines(summary_file, warn = FALSE)

  # Look for "Coverage: XX.XX%" pattern  (from print.coverage)
  pct_line <- grep("Coverage:", lines, value = TRUE)
  if (length(pct_line) > 0) {
    pct <- as.numeric(gsub(".*Coverage:\\s*([0-9.]+)%.*", "\\1", pct_line[1]))
  } else {
    pct <- NA
  }

  # Count R files
  r_files <- list.files("R", pattern = "\\.R$", full.names = FALSE)

  list(
    type = "R",
    percentage = pct,
    file_count = length(r_files),
    summary_file = summary_file
  )
}

#' Parse Rye coverage summary
parse_rye_coverage <- function(summary_file = "coverage/rye/summary.txt") {
  if (!file.exists(summary_file)) {
    warning("Rye coverage summary not found")
    return(NULL)
  }

  # Read summary and extract total percentage
  lines <- readLines(summary_file, warn = FALSE)

  # Look for "Total: XX/YY lines (ZZ.Z%)" pattern
  total_line <- grep("^Total:", lines, value = TRUE)
  if (length(total_line) > 0) {
    pct <- as.numeric(gsub(".*\\(([0-9.]+)%\\).*", "\\1", total_line[1]))
  } else {
    pct <- NA
  }

  # Count Rye files
  rye_files <- c(
    list.files("inst/rye", pattern = "\\.rye$", full.names = FALSE),
    list.files("tests/native", pattern = "\\.rye$", full.names = FALSE, recursive = TRUE)
  )

  list(
    type = "Rye",
    percentage = pct,
    file_count = length(rye_files),
    summary_file = summary_file
  )
}

#' Generate combined HTML report
generate_combined_html <- function(r_cov, rye_cov, output_file = "coverage/combined/index.html") {
  html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset='utf-8'>",
    "<title>Rye Combined Coverage Report</title>",
    "<style>",
    "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; max-width: 1200px; margin: 40px auto; padding: 20px; background: #f8f9fa; }",
    "h1 { color: #212529; border-bottom: 3px solid #007bff; padding-bottom: 15px; }",
    "h2 { color: #495057; margin-top: 40px; }",
    ".timestamp { color: #6c757d; font-size: 14px; margin: 10px 0; }",
    ".summary { display: flex; gap: 30px; margin: 40px 0; }",
    ".card { flex: 1; border: 2px solid #dee2e6; border-radius: 12px; padding: 30px; background: white; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }",
    ".card h3 { margin-top: 0; color: #007bff; font-size: 20px; }",
    ".percentage { font-size: 56px; font-weight: bold; margin: 25px 0; }",
    ".good { color: #28a745; }",
    ".warning { color: #ffc107; }",
    ".bad { color: #dc3545; }",
    ".meta { color: #6c757d; font-size: 16px; }",
    ".links { margin: 20px 0; }",
    ".links a { display: inline-block; padding: 8px 16px; background: #007bff; color: white; text-decoration: none; border-radius: 6px; font-size: 14px; }",
    ".links a:hover { background: #0056b3; }",
    "table { width: 100%; border-collapse: collapse; margin: 30px 0; background: white; border-radius: 8px; overflow: hidden; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }",
    "th, td { border: 1px solid #dee2e6; padding: 16px; text-align: left; }",
    "th { background: #f8f9fa; font-weight: 600; color: #495057; }",
    "tr:hover { background: #f8f9fa; }",
    ".note { background: #e7f3ff; border-left: 4px solid #007bff; padding: 15px; margin: 20px 0; border-radius: 4px; }",
    "</style>",
    "</head>",
    "<body>",
    "<h1>Rye Combined Coverage Report</h1>",
    sprintf("<p class='timestamp'>Generated: %s</p>", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "<div class='note'>",
    "<strong>Note:</strong> This report combines coverage for both R implementation code and Rye language code. ",
    "R coverage tracks the interpreter implementation, while Rye coverage tracks the standard library and tests.",
    "</div>",
    ""
  )

  # Summary cards
  html <- c(html, "<div class='summary'>")

  # R coverage card
  if (!is.null(r_cov) && !is.na(r_cov$percentage)) {
    pct_class <- if (r_cov$percentage >= 80) "good"
                 else if (r_cov$percentage >= 60) "warning"
                 else "bad"
    html <- c(html,
      "<div class='card'>",
      "<h3>R Code Coverage</h3>",
      sprintf("<div class='percentage %s'>%.1f%%</div>", pct_class, r_cov$percentage),
      sprintf("<p class='meta'>%d source files in R/</p>", r_cov$file_count),
      "<div class='links'><a href='../r/index.html'>View R Coverage Report →</a></div>",
      "</div>"
    )
  } else {
    html <- c(html,
      "<div class='card'>",
      "<h3>R Code Coverage</h3>",
      "<div class='percentage bad'>N/A</div>",
      "<p class='meta'>Coverage data not available</p>",
      "</div>"
    )
  }

  # Rye coverage card
  if (!is.null(rye_cov) && !is.na(rye_cov$percentage)) {
    pct_class <- if (rye_cov$percentage >= 85) "good"
                 else if (rye_cov$percentage >= 70) "warning"
                 else "bad"
    html <- c(html,
      "<div class='card'>",
      "<h3>Rye Code Coverage</h3>",
      sprintf("<div class='percentage %s'>%.1f%%</div>", pct_class, rye_cov$percentage),
      sprintf("<p class='meta'>%d .rye files</p>", rye_cov$file_count),
      "<div class='links'><a href='../rye/index.html'>View Rye Coverage Report →</a></div>",
      "</div>"
    )
  } else {
    html <- c(html,
      "<div class='card'>",
      "<h3>Rye Code Coverage</h3>",
      "<div class='percentage bad'>N/A</div>",
      "<p class='meta'>Coverage data not available</p>",
      "</div>"
    )
  }

  html <- c(html, "</div>")

  # Overall summary table
  if ((!is.null(r_cov) && !is.na(r_cov$percentage)) ||
      (!is.null(rye_cov) && !is.na(rye_cov$percentage))) {
    html <- c(html,
      "<h2>Coverage Details</h2>",
      "<table>",
      "<tr><th>Component</th><th>Coverage</th><th>Files</th><th>Target</th><th>Status</th></tr>"
    )

    if (!is.null(r_cov) && !is.na(r_cov$percentage)) {
      status_icon <- ifelse(r_cov$percentage >= 80, "✅", "⚠️")
      status_text <- ifelse(r_cov$percentage >= 80, "Meets target", "Below target")
      html <- c(html,
        sprintf("<tr><td><strong>R Implementation</strong></td><td>%.1f%%</td><td>%d</td><td>80%%</td><td>%s %s</td></tr>",
                r_cov$percentage, r_cov$file_count, status_icon, status_text)
      )
    }

    if (!is.null(rye_cov) && !is.na(rye_cov$percentage)) {
      status_icon <- ifelse(rye_cov$percentage >= 85, "✅", "⚠️")
      status_text <- ifelse(rye_cov$percentage >= 85, "Meets target", "Below target")
      html <- c(html,
        sprintf("<tr><td><strong>Rye Language</strong></td><td>%.1f%%</td><td>%d</td><td>85%%</td><td>%s %s</td></tr>",
                rye_cov$percentage, rye_cov$file_count, status_icon, status_text)
      )
    }

    # Calculate average if both available
    if (!is.null(r_cov) && !is.na(r_cov$percentage) &&
        !is.null(rye_cov) && !is.na(rye_cov$percentage)) {
      overall_pct <- mean(c(r_cov$percentage, rye_cov$percentage))
      total_files <- r_cov$file_count + rye_cov$file_count
      html <- c(html,
        sprintf("<tr style='background: #f8f9fa; font-weight: bold;'><td>Overall Average</td><td>%.1f%%</td><td>%d</td><td>—</td><td></td></tr>",
                overall_pct, total_files)
      )
    }

    html <- c(html, "</table>")
  }

  html <- c(html,
    "</body>",
    "</html>"
  )

  writeLines(html, output_file)
  message(sprintf("Combined HTML report: %s", output_file))
}

#' Generate combined markdown summary
generate_combined_markdown <- function(r_cov, rye_cov, output_file = "coverage/combined/COVERAGE.md") {
  md <- c(
    "# Coverage Summary",
    "",
    sprintf("*Generated: %s*", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    ""
  )

  has_data <- FALSE

  if (!is.null(r_cov) && !is.na(r_cov$percentage)) {
    has_data <- TRUE
    status <- ifelse(r_cov$percentage >= 80, "✅", "⚠️")
    md <- c(md,
      sprintf("## R Code Coverage: %s %.1f%%", status, r_cov$percentage),
      "",
      sprintf("- **Files:** %d", r_cov$file_count),
      sprintf("- **Target:** 80%%"),
      sprintf("- **Status:** %s", ifelse(r_cov$percentage >= 80, "Meets target", "Below target")),
      ""
    )
  }

  if (!is.null(rye_cov) && !is.na(rye_cov$percentage)) {
    has_data <- TRUE
    status <- ifelse(rye_cov$percentage >= 85, "✅", "⚠️")
    md <- c(md,
      sprintf("## Rye Code Coverage: %s %.1f%%", status, rye_cov$percentage),
      "",
      sprintf("- **Files:** %d", rye_cov$file_count),
      sprintf("- **Target:** 85%%"),
      sprintf("- **Status:** %s", ifelse(rye_cov$percentage >= 85, "Meets target", "Below target")),
      ""
    )
  }

  if (has_data &&
      !is.null(r_cov) && !is.na(r_cov$percentage) &&
      !is.null(rye_cov) && !is.na(rye_cov$percentage)) {
    overall <- mean(c(r_cov$percentage, rye_cov$percentage))
    md <- c(md,
      "## Overall",
      "",
      sprintf("**Average Coverage:** %.1f%%", overall),
      sprintf("**Total Files:** %d", r_cov$file_count + rye_cov$file_count),
      ""
    )
  }

  if (!has_data) {
    md <- c(md,
      "No coverage data available. Run `make coverage` to generate reports.",
      ""
    )
  }

  md <- c(md,
    "---",
    "",
    "📊 [View detailed HTML report](index.html)",
    ""
  )

  writeLines(md, output_file)
  message(sprintf("Combined markdown summary: %s", output_file))
}

#' Main entry point
#' @export
generate_combined_report <- function() {
  message("Generating combined coverage report...")

  # Ensure output directory exists
  dir.create("coverage/combined", recursive = TRUE, showWarnings = FALSE)

  # Parse coverage data
  r_cov <- parse_r_coverage()
  rye_cov <- parse_rye_coverage()

  if ((is.null(r_cov) || is.na(r_cov$percentage)) &&
      (is.null(rye_cov) || is.na(rye_cov$percentage))) {
    stop("No coverage data found. Run 'make coverage-r' and 'make coverage-rye' first.")
  }

  # Generate reports
  generate_combined_html(r_cov, rye_cov)
  generate_combined_markdown(r_cov, rye_cov)

  message("\n✅ Combined coverage report generated successfully!")
  message("   HTML: coverage/combined/index.html")
  message("   Markdown: coverage/combined/COVERAGE.md")

  invisible(list(r = r_cov, rye = rye_cov))
}

# Self-execute when run as a script
if (!interactive()) {
  generate_combined_report()
}
