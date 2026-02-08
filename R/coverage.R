#' R6 class for tracking and reporting Rye code execution coverage
#'
#' Tracks which lines of .rye source files actually execute during runtime.
#' Maintains execution counts per file/line and generates reports.
#'
#' @examples
#' \dontrun{
#' # In R session:
#' engine <- rye::RyeEngine$new()
#' tracker <- engine$enable_coverage()
#'
#' # Run some code
#' engine$eval(engine$read("(+ 1 2)"))
#'
#' # Generate reports
#' tracker$report_console()
#' tracker$report_html("coverage.html")
#' }
#' @export
RyeCoverageTracker <- R6::R6Class(
  "RyeCoverageTracker",
  public = list(
    coverage = NULL,    # environment: "file:line" -> count
    enabled = TRUE,     # flag to enable/disable tracking
    all_files = NULL,   # character vector of all .rye files to track

    #' @description Initialize the coverage tracker
    initialize = function() {
      self$coverage <- new.env(hash = TRUE, parent = emptyenv())
      self$enabled <- TRUE
      self$all_files <- character(0)
    },

    #' @description Track execution of an expression with source info
    #' @param rye_src Source information object with file, start_line, end_line
    track = function(rye_src) {
      if (!self$enabled || is.null(rye_src)) {
        return(invisible(NULL))
      }

      file <- rye_src$file
      start_line <- rye_src$start_line
      end_line <- rye_src$end_line

      if (is.null(file) || is.null(start_line) || is.null(end_line)) {
        return(invisible(NULL))
      }

      # Mark all lines in range as executed
      for (line in start_line:end_line) {
        key <- paste0(file, ":", line)
        current <- self$coverage[[key]]
        self$coverage[[key]] <- if (is.null(current)) 1L else current + 1L
      }

      invisible(NULL)
    },

    #' @description Get coverage summary as list: file -> line -> count
    get_summary = function() {
      result <- list()

      for (key in ls(self$coverage, all.names = TRUE)) {
        parts <- strsplit(key, ":", fixed = TRUE)[[1]]
        if (length(parts) != 2) next

        file <- parts[1]
        line <- parts[2]
        count <- self$coverage[[key]]

        if (is.null(result[[file]])) {
          result[[file]] <- list()
        }
        result[[file]][[line]] <- count
      }

      result
    },

    #' @description Discover all .rye files to track
    #'
    #' Only tracks stdlib files in inst/rye/, not test files.
    #' Tests are in tests/ and should not be tracked for coverage.
    discover_files = function() {
      rye_files <- c()

      # Add stdlib only - these are the files we want to track coverage for
      stdlib_dir <- system.file("rye", package = "rye")
      if (stdlib_dir == "") {
        stdlib_dir <- "inst/rye"  # Development mode
      }
      if (dir.exists(stdlib_dir)) {
        stdlib_files <- list.files(stdlib_dir, pattern = "\\.rye$",
                                   full.names = TRUE, recursive = TRUE)
        rye_files <- c(rye_files, stdlib_files)
      }

      # Do NOT add test files - they are tests, not code to instrument

      self$all_files <- rye_files
      invisible(self)
    },

    #' @description Reset coverage data
    reset = function() {
      rm(list = ls(self$coverage, all.names = TRUE), envir = self$coverage)
      invisible(self)
    },

    #' @description Enable/disable tracking
    set_enabled = function(enabled) {
      self$enabled <- enabled
      invisible(self)
    },

    #' @description Generate console coverage report
    #' @param output_file Optional file to write report to (default: console only)
    report_console = function(output_file = NULL) {
      lines <- c()

      lines <- c(lines, "")
      lines <- c(lines, "Rye Code Coverage Report (Execution Coverage)")
      lines <- c(lines, "==============================================")
      lines <- c(lines, "")

      if (length(self$coverage) == 0) {
        lines <- c(lines, "No coverage data available.")
        if (!is.null(output_file)) {
          writeLines(lines, output_file)
        } else {
          cat(paste0(lines, collapse = "\n"), "\n")
        }
        return(invisible(self))
      }

      # Discover files if not done yet
      if (length(self$all_files) == 0) {
        self$discover_files()
      }

      # Calculate coverage for each file
      file_stats <- list()
      total_lines <- 0
      total_covered <- 0

      coverage_summary <- self$get_summary()

      for (file in self$all_files) {
        if (!file.exists(file)) next

        # Count total lines (non-empty, non-comment)
        file_lines <- readLines(file, warn = FALSE)
        code_lines <- grep("^\\s*[^;\\s]", file_lines)
        non_empty <- length(code_lines)

        # Count covered lines
        covered <- if (!is.null(coverage_summary[[file]])) {
          length(coverage_summary[[file]])
        } else {
          0
        }

        # Store stats
        file_stats[[file]] <- list(
          total = non_empty,
          covered = covered,
          pct = if (non_empty > 0) covered / non_empty * 100 else 0
        )

        total_lines <- total_lines + non_empty
        total_covered <- total_covered + covered
      }

      # Sort by file path
      files <- names(file_stats)
      files <- files[order(files)]

      # Print per-file stats
      for (file in files) {
        stats <- file_stats[[file]]
        display_path <- sub("^\\./", "", file)
        display_path <- sub(".*/inst/rye/", "", display_path)
        display_path <- sub(".*/tests/native/", "tests/", display_path)

        lines <- c(lines, sprintf("%-40s %4d/%4d lines (%5.1f%%)",
                    display_path, stats$covered, stats$total, stats$pct))
      }

      # Print total
      lines <- c(lines, "")
      total_pct <- if (total_lines > 0) total_covered / total_lines * 100 else 0
      lines <- c(lines, sprintf("Total: %d/%d lines (%.1f%%)",
                  total_covered, total_lines, total_pct))
      lines <- c(lines, "")

      if (!is.null(output_file)) {
        writeLines(lines, output_file)
      } else {
        cat(paste0(lines, collapse = "\n"), "\n")
      }

      invisible(self)
    },

    #' @description Generate HTML coverage report
    #' @param output_file Path to output HTML file
    report_html = function(output_file = "coverage/rye/index.html") {
      # Create output directory
      output_dir <- dirname(output_file)
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      # Discover files if not done yet
      if (length(self$all_files) == 0) {
        self$discover_files()
      }

      coverage_summary <- self$get_summary()

      # Build HTML
      html_parts <- c(
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "<meta charset='utf-8'>",
        "<title>Rye Code Coverage</title>",
        "<style>",
        "body { font-family: monospace; margin: 20px; background: #f5f5f5; }",
        "h1 { font-size: 24px; color: #333; }",
        "h2 { font-size: 18px; color: #666; margin-top: 30px; }",
        "table { border-collapse: collapse; width: 100%; background: white; margin: 20px 0; }",
        "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
        "th { background-color: #f2f2f2; font-weight: bold; }",
        ".file-content { background: white; padding: 15px; margin: 20px 0; border: 1px solid #ddd; }",
        ".line { white-space: pre; }",
        ".covered { background-color: #d4edda; }",
        ".uncovered { background-color: #f8d7da; }",
        ".empty { background-color: #f8f9fa; color: #999; }",
        ".pct-high { color: green; font-weight: bold; }",
        ".pct-medium { color: orange; }",
        ".pct-low { color: red; font-weight: bold; }",
        ".line-num { display: inline-block; width: 50px; text-align: right; padding-right: 10px; color: #999; user-select: none; }",
        ".hit-count { display: inline-block; width: 50px; text-align: right; padding-right: 10px; color: #666; user-select: none; }",
        "</style>",
        "</head>",
        "<body>",
        "<h1>Rye Code Coverage Report</h1>",
        "<p><em>Note: This report shows execution coverage - lines that actually executed during tests.</em></p>"
      )

      # Calculate file stats and generate summary table
      file_stats <- list()
      total_lines <- 0
      total_covered <- 0

      for (file in self$all_files) {
        if (!file.exists(file)) next

        file_lines <- readLines(file, warn = FALSE)
        code_lines <- grep("^\\s*[^;\\s]", file_lines)
        non_empty <- length(code_lines)

        covered <- if (!is.null(coverage_summary[[file]])) {
          length(coverage_summary[[file]])
        } else {
          0
        }

        file_stats[[file]] <- list(
          total = non_empty,
          covered = covered,
          pct = if (non_empty > 0) covered / non_empty * 100 else 0
        )

        total_lines <- total_lines + non_empty
        total_covered <- total_covered + covered
      }

      # Summary table
      html_parts <- c(html_parts, "<h2>Coverage Summary</h2>", "<table>")
      html_parts <- c(html_parts, "<tr><th>File</th><th>Lines</th><th>Covered</th><th>Coverage %</th></tr>")

      files <- names(file_stats)
      files <- files[order(files)]

      for (file in files) {
        stats <- file_stats[[file]]
        display_path <- sub("^\\./", "", file)
        display_path <- sub(".*/inst/rye/", "", display_path)
        display_path <- sub(".*/tests/native/", "tests/", display_path)

        pct_class <- if (stats$pct >= 80) {
          "pct-high"
        } else if (stats$pct >= 50) {
          "pct-medium"
        } else {
          "pct-low"
        }

        html_parts <- c(html_parts, sprintf(
          "<tr><td><a href='#%s'>%s</a></td><td>%d</td><td>%d</td><td class='%s'>%.1f%%</td></tr>",
          gsub("[^a-zA-Z0-9]", "_", file),
          display_path,
          stats$total,
          stats$covered,
          pct_class,
          stats$pct
        ))
      }

      # Total row
      total_pct <- if (total_lines > 0) total_covered / total_lines * 100 else 0
      total_pct_class <- if (total_pct >= 80) {
        "pct-high"
      } else if (total_pct >= 50) {
        "pct-medium"
      } else {
        "pct-low"
      }

      html_parts <- c(html_parts, sprintf(
        "<tr style='font-weight: bold;'><td>Total</td><td>%d</td><td>%d</td><td class='%s'>%.1f%%</td></tr>",
        total_lines,
        total_covered,
        total_pct_class,
        total_pct
      ))
      html_parts <- c(html_parts, "</table>")

      # Per-file line-by-line view
      html_parts <- c(html_parts, "<h2>Detailed Coverage</h2>")

      for (file in files) {
        if (!file.exists(file)) next

        display_path <- sub("^\\./", "", file)
        display_path <- sub(".*/inst/rye/", "", display_path)
        display_path <- sub(".*/tests/native/", "tests/", display_path)

        file_id <- gsub("[^a-zA-Z0-9]", "_", file)
        html_parts <- c(html_parts, sprintf("<h3 id='%s'>%s</h3>", file_id, display_path))
        html_parts <- c(html_parts, "<div class='file-content'>")

        file_lines <- readLines(file, warn = FALSE)
        file_cov <- coverage_summary[[file]]

        for (i in seq_along(file_lines)) {
          line <- file_lines[i]
          line_str <- as.character(i)

          # Determine coverage status
          if (!is.null(file_cov) && !is.null(file_cov[[line_str]])) {
            # Covered
            hit_count <- file_cov[[line_str]]
            css_class <- "covered"
            hit_display <- sprintf("%dx", hit_count)
          } else if (grepl("^\\s*[^;\\s]", line)) {
            # Not covered but is code
            css_class <- "uncovered"
            hit_display <- "0x"
          } else {
            # Not code (comment/blank)
            css_class <- "empty"
            hit_display <- ""
          }

          # HTML escape
          line_html <- gsub("&", "&amp;", line)
          line_html <- gsub("<", "&lt;", line_html)
          line_html <- gsub(">", "&gt;", line_html)

          html_parts <- c(html_parts, sprintf(
            "<div class='line %s'><span class='line-num'>%d</span><span class='hit-count'>%s</span> %s</div>",
            css_class,
            i,
            hit_display,
            line_html
          ))
        }

        html_parts <- c(html_parts, "</div>")
      }

      html_parts <- c(html_parts, "</body>", "</html>")

      # Write HTML file
      writeLines(html_parts, output_file)
      message(sprintf("HTML report written to: %s", output_file))

      invisible(self)
    },

    #' @description Generate codecov-compatible JSON format
    #' @param output_file Path to output JSON file
    report_json = function(output_file = "coverage/rye/coverage.json") {
      # Create output directory
      output_dir <- dirname(output_file)
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      # Discover files if not done yet
      if (length(self$all_files) == 0) {
        self$discover_files()
      }

      coverage_summary <- self$get_summary()

      # Build codecov-compatible JSON structure
      coverage_data <- list()

      for (file in self$all_files) {
        if (!file.exists(file)) next

        lines <- readLines(file, warn = FALSE)

        # Build line coverage array (1-indexed, NULL for non-code lines)
        line_coverage <- lapply(seq_along(lines), function(i) {
          line_str <- as.character(i)

          # Check if line is covered
          if (!is.null(coverage_summary[[file]][[line_str]])) {
            coverage_summary[[file]][[line_str]]  # Execution count
          } else if (grepl("^\\s*[^;\\s]", lines[i])) {
            0  # Not covered but is code
          } else {
            NULL  # Not code (comment/blank)
          }
        })

        # Relative path for codecov
        rel_path <- sub("^\\./", "", file)
        coverage_data[[rel_path]] <- line_coverage
      }

      # Write JSON in codecov format
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        warning("jsonlite package not available, skipping JSON output")
        return(invisible(self))
      }

      json_str <- jsonlite::toJSON(
        list(coverage = coverage_data),
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
      )

      writeLines(json_str, output_file)
      message(sprintf("Codecov JSON written to: %s", output_file))

      invisible(self)
    }
  )
)
