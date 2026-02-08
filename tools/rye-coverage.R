# Rye Code Coverage Tool
#
# Tracks which lines of .rye source files can be compiled (compilation coverage).
# This is a simpler metric than execution coverage but still valuable.
#
# Usage:
#   source("tools/rye-coverage.R")
#   rye_coverage_report()

library(R6)

#' Extract all rye_src attributes from a compiled R expression
#'
#' Recursively walks an R expression tree and collects all rye_src attributes.
#'
#' @param expr Compiled R expression
#' @param source_tracker SourceTracker instance for src_get
#' @return List of rye_src objects
extract_src_recursive <- function(expr, source_tracker) {
  results <- list()

  # Helper to collect from a single expression
  collect_one <- function(e) {
    if (is.null(e) || is.symbol(e)) {
      return()
    }

    # Get src from this expression
    src <- source_tracker$src_get(e)
    if (!is.null(src)) {
      results <<- c(results, list(src))
    }

    # Recurse into call/list elements
    if (is.call(e)) {
      for (i in seq_along(e)) {
        collect_one(e[[i]])
      }
    } else if (is.list(e) && is.null(attr(e, "class", exact = TRUE))) {
      for (elem in e) {
        collect_one(elem)
      }
    }
  }

  collect_one(expr)
  results
}

#' R6 class for tracking and reporting Rye code coverage
#'
#' Tracks compilation coverage - which lines of .rye code can be successfully compiled.
RyeCoverageTracker <- R6Class(
  "RyeCoverageTracker",
  public = list(
    rye_coverage = NULL,     # List: rye_file -> list(line = 1)
    all_files = NULL,        # All .rye files being tracked

    #' @description Initialize the coverage tracker
    initialize = function() {
      self$rye_coverage <- list()
      self$all_files <- character(0)
    },

    #' @description Discover all .rye files to track
    discover_files = function() {
      rye_files <- c()

      # Add stdlib
      stdlib_dir <- system.file("rye", package = "rye")
      if (stdlib_dir == "") {
        # Development mode
        stdlib_dir <- "inst/rye"
      }
      if (dir.exists(stdlib_dir)) {
        stdlib_files <- list.files(stdlib_dir, pattern = "\\.rye$",
                                   full.names = TRUE, recursive = TRUE)
        rye_files <- c(rye_files, stdlib_files)
      }

      # Add native tests
      if (dir.exists("tests/native")) {
        test_files <- list.files("tests/native", pattern = "\\.rye$",
                                full.names = TRUE, recursive = TRUE)
        rye_files <- c(rye_files, test_files)
      }

      self$all_files <- rye_files

      # Initialize coverage tracking for all files
      for (file in rye_files) {
        self$rye_coverage[[file]] <- list()
      }

      invisible(self)
    },

    #' @description Compile all .rye files and extract source mappings
    build_coverage_from_compilation = function() {
      # Create engine for compilation
      engine <- rye::RyeEngine$new(use_env_cache = FALSE)

      for (file in self$all_files) {
        if (!file.exists(file)) {
          warning(sprintf("File not found: %s", file))
          next
        }

        message(sprintf("Compiling %s...", basename(file)))

        tryCatch({
          # Read and compile
          source_text <- paste(readLines(file, warn = FALSE), collapse = "\n")
          exprs <- engine$read(source_text, source_name = file)

          # Macroexpand
          expanded <- lapply(exprs, function(e) {
            engine$macroexpand(e, preserve_src = TRUE)
          })

          # Compile
          compiled <- lapply(expanded, function(e) {
            engine$compiler$compile(e)
          })

          # Extract all rye_src attributes
          all_srcs <- list()
          for (expr in compiled) {
            srcs <- extract_src_recursive(expr, engine$source_tracker)
            all_srcs <- c(all_srcs, srcs)
          }

          # Mark all lines that appear in any src as compiled (coverage = 1)
          for (src in all_srcs) {
            if (is.null(src)) next

            file_name <- src$file
            start_line <- src$start_line
            end_line <- src$end_line

            if (is.null(file_name) || !nzchar(file_name)) next
            if (is.null(start_line) || is.null(end_line)) next

            # Mark all lines in range
            for (line in start_line:end_line) {
              line_str <- as.character(line)
              self$rye_coverage[[file_name]][[line_str]] <- 1
            }
          }

        }, error = function(e) {
          warning(sprintf("Failed to compile %s: %s", file, e$message))
        })
      }

      invisible(self)
    },

    #' @description Generate console coverage report
    report_console = function() {
      cat("\n")
      cat("Rye Code Coverage Report (Compilation Coverage)\n")
      cat("===============================================\n\n")

      if (length(self$rye_coverage) == 0) {
        cat("No coverage data available.\n")
        return(invisible(self))
      }

      # Calculate coverage for each file
      file_stats <- list()
      total_lines <- 0
      total_covered <- 0

      for (file in self$all_files) {
        if (!file.exists(file)) {
          next
        }

        # Count total lines (non-empty, non-comment)
        lines <- readLines(file, warn = FALSE)
        # Simple heuristic: lines with non-whitespace that aren't just comments
        code_lines <- grep("^\\s*[^;\\s]", lines)
        non_empty <- length(code_lines)

        # Count covered lines
        covered_lines <- if (!is.null(self$rye_coverage[[file]])) {
          names(self$rye_coverage[[file]])
        } else {
          character(0)
        }
        covered <- length(covered_lines)

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
        # Shorten path for display
        display_path <- sub("^\\./", "", file)
        display_path <- sub(".*/rye/", "", display_path)

        cat(sprintf("%-40s %4d/%4d lines (%5.1f%%)\n",
                    display_path,
                    stats$covered,
                    stats$total,
                    stats$pct))
      }

      # Print total
      cat("\n")
      total_pct <- if (total_lines > 0) total_covered / total_lines * 100 else 0
      cat(sprintf("Total: %d/%d lines (%.1f%%)\n",
                  total_covered, total_lines, total_pct))
      cat("\n")

      invisible(self)
    },

    #' @description Generate HTML coverage report
    #' @param output_file Path to output HTML file
    report_html = function(output_file = "coverage-rye/index.html") {
      # Create output directory
      output_dir <- dirname(output_file)
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

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
        "<p><em>Note: This report shows compilation coverage - lines that can be successfully compiled.</em></p>"
      )

      # Calculate file stats
      file_stats <- list()
      for (file in self$all_files) {
        if (!file.exists(file)) next

        lines <- readLines(file, warn = FALSE)
        code_lines <- grep("^\\s*[^;\\s]", lines)
        non_empty <- length(code_lines)

        covered_lines <- if (!is.null(self$rye_coverage[[file]])) {
          names(self$rye_coverage[[file]])
        } else {
          character(0)
        }
        covered <- length(covered_lines)

        file_stats[[file]] <- list(
          total = non_empty,
          covered = covered,
          pct = if (non_empty > 0) covered / non_empty * 100 else 0
        )
      }

      # File summary table
      html_parts <- c(html_parts,
        "<h2>Summary</h2>",
        "<table>",
        "<tr><th>File</th><th>Coverage</th><th>Lines</th></tr>"
      )

      files <- names(file_stats)
      files <- files[order(files)]

      for (file in files) {
        stats <- file_stats[[file]]
        display_path <- sub("^\\./", "", file)
        display_path <- sub(".*/rye/", "", display_path)

        pct_class <- if (stats$pct >= 80) "pct-high"
                     else if (stats$pct >= 50) "pct-medium"
                     else "pct-low"

        html_parts <- c(html_parts,
          sprintf("<tr><td><a href='#%s'>%s</a></td><td class='%s'>%.1f%%</td><td>%d/%d</td></tr>",
                  gsub("[^a-zA-Z0-9]", "_", display_path),
                  display_path, pct_class, stats$pct, stats$covered, stats$total)
        )
      }

      html_parts <- c(html_parts, "</table>")

      # Per-file line-by-line view
      for (file in files) {
        display_path <- sub("^\\./", "", file)
        display_path <- sub(".*/rye/", "", display_path)
        anchor <- gsub("[^a-zA-Z0-9]", "_", display_path)

        html_parts <- c(html_parts,
          sprintf("<h2 id='%s'>%s</h2>", anchor, display_path),
          "<div class='file-content'>"
        )

        lines <- readLines(file, warn = FALSE)
        coverage <- self$rye_coverage[[file]]

        for (i in seq_along(lines)) {
          line_str <- as.character(i)
          hit_count <- coverage[[line_str]]

          # Determine line class and formatting
          if (!is.null(hit_count)) {
            line_class <- "covered"
            hit_display <- "   +"
          } else if (grepl("^\\s*[^;\\s]", lines[i])) {
            line_class <- "uncovered"
            hit_display <- "   -"
          } else {
            line_class <- "empty"
            hit_display <- "    "
          }

          # Escape HTML
          line_text <- gsub("&", "&amp;", lines[i])
          line_text <- gsub("<", "&lt;", line_text)
          line_text <- gsub(">", "&gt;", line_text)
          line_text <- gsub(" ", "&nbsp;", line_text)

          html_parts <- c(html_parts,
            sprintf("<div class='line %s'><span class='line-num'>%4d</span><span class='hit-count'>%s</span>%s</div>",
                    line_class, i, hit_display, line_text)
          )
        }

        html_parts <- c(html_parts, "</div>")
      }

      html_parts <- c(html_parts,
        "</body>",
        "</html>"
      )

      # Write HTML file
      writeLines(html_parts, output_file)
      message(sprintf("HTML report written to: %s", output_file))

      invisible(self)
    }
  )
)

#' Main entry point for Rye coverage reporting
#'
#' @param output Output format(s) - vector containing "console", "html"
#' @param html_file Path to HTML output file
#' @return RyeCoverageTracker instance (invisibly)
#' @export
rye_coverage_report <- function(
  output = c("console", "html"),
  html_file = "coverage-rye/index.html"
) {
  # Create tracker
  tracker <- RyeCoverageTracker$new()

  # Discover files
  message("Discovering .rye files...")
  tracker$discover_files()

  if (length(tracker$all_files) == 0) {
    stop("No .rye files found")
  }

  message(sprintf("Found %d .rye files", length(tracker$all_files)))

  # Build coverage from compilation
  message("Building compilation coverage...")
  tracker$build_coverage_from_compilation()

  # Generate reports
  if ("console" %in% output) {
    tracker$report_console()
  }
  if ("html" %in% output) {
    tracker$report_html(html_file)
  }

  invisible(tracker)
}
