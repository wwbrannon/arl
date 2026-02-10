#' R6 class for tracking and reporting Rye code execution coverage
#'
#' Tracks which lines of .rye source files actually execute during runtime.
#' Maintains execution counts per file/line and generates reports.
#' Supports flexible configuration for tracking custom directories,
#' test files, and custom comment syntax.
#'
#' @field coverage Environment mapping "file:line" keys to execution counts
#' @field enabled Logical flag to enable/disable tracking
#' @field all_files Character vector of all .rye files being tracked
#' @field code_lines Environment mapping file paths to integer vectors of code line numbers
#' @field coverable_lines Environment mapping file paths to integer vectors of AST-derived coverable line numbers
#'
#' @examples
#' \dontrun{
#' # Default usage: Track Rye stdlib
#' engine <- rye::RyeEngine$new()
#' tracker <- engine$enable_coverage()
#' engine$eval(engine$read("(+ 1 2)"))
#' tracker$report_console()
#' tracker$report_html()  # Outputs to coverage/rye/index.html
#'
#' # Track custom directories
#' tracker <- RyeCoverageTracker$new(
#'   search_paths = c("src/rye", "lib/rye"),
#'   output_prefix = "myproject",
#'   report_title = "My Project Coverage"
#' )
#' tracker$discover_files()
#' tracker$report_html()  # Outputs to coverage/myproject/index.html
#'
#' # Include test files in coverage
#' tracker <- RyeCoverageTracker$new(
#'   search_paths = "src",
#'   include_tests = TRUE
#' )
#'
#' # Custom path display in reports
#' tracker <- RyeCoverageTracker$new(
#'   search_paths = "/home/user/myproject/rye",
#'   path_strip_patterns = c(".*/myproject/rye/", ".*/myproject/tests/")
#' )
#'
#' # Custom comment syntax (e.g., # instead of ;)
#' tracker <- RyeCoverageTracker$new(
#'   search_paths = "src",
#'   code_line_pattern = "^\\s*[^[:space:]#]"
#' )
#' }
#' @export
RyeCoverageTracker <- R6::R6Class(
  "RyeCoverageTracker",
  public = list(
    coverage = NULL,    # environment: "file:line" -> count
    enabled = TRUE,     # flag to enable/disable tracking
    all_files = NULL,   # character vector of all .rye files to track
    code_lines = NULL,  # environment: "file" -> integer vector of code line numbers
    coverable_lines = NULL, # environment: "file" -> integer vector of AST-derived coverable line numbers

    #' @description Initialize the coverage tracker
    #' @param search_paths Character vector of directories to search for .rye files (NULL = use stdlib)
    #' @param include_tests Whether to include test files in coverage tracking (default: FALSE)
    #' @param path_strip_patterns Custom regex patterns for stripping paths in reports (NULL = use defaults)
    #' @param output_prefix Subdirectory name for report outputs (default: "rye")
    #' @param report_title Title to use in coverage reports (default: "Rye Code Coverage")
    #' @param code_line_pattern Regex pattern to identify code lines vs comments/blanks
    initialize = function(
      search_paths = NULL,
      include_tests = FALSE,
      path_strip_patterns = NULL,
      output_prefix = "rye",
      report_title = "Rye Code Coverage",
      code_line_pattern = "^\\s*[^[:space:];]"
    ) {
      self$coverage <- new.env(hash = TRUE, parent = emptyenv())
      self$enabled <- TRUE
      self$all_files <- character(0)
      self$code_lines <- new.env(hash = TRUE, parent = emptyenv())
      self$coverable_lines <- new.env(hash = TRUE, parent = emptyenv())

      # Store configuration
      private$search_paths <- search_paths
      private$include_tests <- isTRUE(include_tests)
      private$path_strip_patterns <- path_strip_patterns
      private$output_prefix <- output_prefix
      private$report_title <- report_title
      private$code_line_pattern <- code_line_pattern
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

      # Get cached code lines for this file (lazy load if not cached yet)
      code_line_nums <- self$code_lines[[file]]
      if (is.null(code_line_nums) && file.exists(file)) {
        # First time tracking this file - cache which lines are code
        file_lines <- readLines(file, warn = FALSE)
        code_line_nums <- grep(private$code_line_pattern, file_lines)
        self$code_lines[[file]] <- code_line_nums
      }

      # Mark only code lines in range as executed (skip comments and blanks)
      for (line in start_line:end_line) {
        # Only track if this line is a code line
        if (!is.null(code_line_nums) && line %in% code_line_nums) {
          key <- paste0(file, ":", line)
          current <- self$coverage[[key]]
          self$coverage[[key]] <- if (is.null(current)) 1L else current + 1L
        }
      }

      invisible(NULL)
    },

    #' @description Register coverable lines from an instrumented source range
    #' @param file Source file path
    #' @param start_line Start line of the instrumented form
    #' @param end_line End line of the instrumented form
    register_coverable = function(file, start_line, end_line) {
      if (is.null(file) || is.null(start_line) || is.null(end_line)) return(invisible(NULL))
      existing <- self$coverable_lines[[file]]
      new_lines <- start_line:end_line
      # Filter to actual code lines (skip blanks/comments within range)
      code <- self$code_lines[[file]]
      if (is.null(code) && file.exists(file)) {
        file_text <- readLines(file, warn = FALSE)
        code <- grep(private$code_line_pattern, file_text)
        self$code_lines[[file]] <- code
      }
      if (!is.null(code)) new_lines <- intersect(new_lines, code)
      self$coverable_lines[[file]] <- unique(c(existing, new_lines))
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
    #' Searches for .rye files in configured search paths or stdlib by default.
    #' By default excludes test files unless include_tests = TRUE.
    discover_files = function() {
      rye_files <- c()

      # Use custom search paths if provided, otherwise use stdlib helper
      if (!is.null(private$search_paths)) {
        # Custom: user-provided directories
        for (search_path in private$search_paths) {
          if (dir.exists(search_path)) {
            found_files <- list.files(
              search_path,
              pattern = "\\.rye$",
              full.names = TRUE,
              recursive = TRUE
            )

            # Filter out tests unless explicitly included
            if (!private$include_tests) {
              # Exclude files in directories named "test" or "tests"
              found_files <- found_files[!grepl("/tests?/", found_files)]
            }

            rye_files <- c(rye_files, found_files)
          }
        }
      } else {
        # Default: use stdlib helper
        rye_files <- private$find_stdlib_files()
      }

      # Remove duplicates if overlapping paths were provided
      rye_files <- unique(rye_files)
      self$all_files <- rye_files

      # Build cache of which lines are code (non-blank, non-comment) for each file
      for (file in rye_files) {
        if (file.exists(file)) {
          file_lines <- readLines(file, warn = FALSE)
          code_line_nums <- grep(private$code_line_pattern, file_lines)
          self$code_lines[[file]] <- code_line_nums
        }
      }

      invisible(self)
    },

    #' @description Reset coverage data
    reset = function() {
      rm(list = ls(self$coverage, all.names = TRUE), envir = self$coverage)
      invisible(self)
    },

    #' @description Enable/disable tracking
    #' @param enabled Logical value to enable (TRUE) or disable (FALSE) coverage tracking
    set_enabled = function(enabled) {
      self$enabled <- enabled
      invisible(self)
    },

    #' @description Generate console coverage report
    #' @param output_file Optional file to write report to (default: console only)
    report_console = function(output_file = NULL) {
      lines <- c()

      title <- sprintf("%s (Execution Coverage)", private$report_title)
      lines <- c(lines, "")
      lines <- c(lines, title)
      lines <- c(lines, strrep("=", nchar(title)))
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

        # Use AST-derived coverable lines as denominator when available,
        # fall back to regex-based code_lines for direct track() usage
        coverable <- self$coverable_lines[[file]]
        if (is.null(coverable)) coverable <- self$code_lines[[file]]
        non_empty <- if (!is.null(coverable)) length(coverable) else 0L

        # Count covered lines (intersect with coverable to avoid >100%)
        file_cov <- coverage_summary[[file]]
        covered <- if (!is.null(file_cov) && !is.null(coverable)) {
          covered_lines <- as.integer(names(file_cov))
          length(intersect(covered_lines, coverable))
        } else if (!is.null(file_cov)) {
          length(file_cov)
        } else {
          0L
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
        display_path <- private$strip_display_path(file)

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
    #' @param output_file Path to output HTML file (NULL = use default based on output_prefix)
    report_html = function(output_file = NULL) {
      # Set default output file if not provided
      if (is.null(output_file)) {
        output_file <- sprintf("coverage/%s/index.html", private$output_prefix)
      }

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
        sprintf("<title>%s</title>", private$report_title),
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
        sprintf("<h1>%s</h1>", private$report_title),
        "<p><em>Note: This report shows execution coverage - lines that actually executed during tests.</em></p>"
      )

      # Calculate file stats and generate summary table
      file_stats <- list()
      total_lines <- 0
      total_covered <- 0

      for (file in self$all_files) {
        if (!file.exists(file)) next

        # Use AST-derived coverable lines as denominator when available,
        # fall back to regex-based code_lines for direct track() usage
        coverable <- self$coverable_lines[[file]]
        if (is.null(coverable)) coverable <- self$code_lines[[file]]
        non_empty <- if (!is.null(coverable)) length(coverable) else 0L

        # Count covered lines (intersect with coverable to avoid >100%)
        file_cov <- coverage_summary[[file]]
        covered <- if (!is.null(file_cov) && !is.null(coverable)) {
          covered_lines <- as.integer(names(file_cov))
          length(intersect(covered_lines, coverable))
        } else if (!is.null(file_cov)) {
          length(file_cov)
        } else {
          0L
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
        display_path <- private$strip_display_path(file)

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

        display_path <- private$strip_display_path(file)

        file_id <- gsub("[^a-zA-Z0-9]", "_", file)
        html_parts <- c(html_parts, sprintf("<h3 id='%s'>%s</h3>", file_id, display_path))
        html_parts <- c(html_parts, "<div class='file-content'>")

        file_lines <- readLines(file, warn = FALSE)
        file_cov <- coverage_summary[[file]]
        file_coverable <- self$coverable_lines[[file]]
        if (is.null(file_coverable)) file_coverable <- self$code_lines[[file]]

        for (i in seq_along(file_lines)) {
          line <- file_lines[i]
          line_str <- as.character(i)

          # Determine coverage status
          if (!is.null(file_cov) && !is.null(file_cov[[line_str]])) {
            # Covered
            hit_count <- file_cov[[line_str]]
            css_class <- "covered"
            hit_display <- sprintf("%dx", hit_count)
          } else if (!is.null(file_coverable) && i %in% file_coverable) {
            # Not covered but is coverable code
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
    #' @param output_file Path to output JSON file (NULL = use default based on output_prefix)
    report_json = function(output_file = NULL) {
      # Set default output file if not provided
      if (is.null(output_file)) {
        output_file <- sprintf("coverage/%s/coverage.json", private$output_prefix)
      }

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
        file_coverable <- self$coverable_lines[[file]]
        if (is.null(file_coverable)) file_coverable <- self$code_lines[[file]]

        # Build line coverage array (1-indexed, NULL for non-code lines)
        line_coverage <- lapply(seq_along(lines), function(i) {
          line_str <- as.character(i)

          # Check if line is covered
          if (!is.null(coverage_summary[[file]][[line_str]])) {
            coverage_summary[[file]][[line_str]]  # Execution count
          } else if (!is.null(file_coverable) && i %in% file_coverable) {
            0  # Not covered but is coverable code
          } else {
            NULL  # Not code (comment/blank/unreachable)
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
  ),

  private = list(
    # Configuration fields
    search_paths = NULL,
    include_tests = FALSE,
    path_strip_patterns = NULL,
    output_prefix = "rye",
    report_title = "Rye Code Coverage",
    code_line_pattern = "^\\s*[^[:space:];]",

    # Helper: Find stdlib files (default behavior)
    find_stdlib_files = function() {
      rye_files <- c()

      stdlib_dir <- system.file("rye", package = "rye")
      if (stdlib_dir == "") {
        stdlib_dir <- "inst/rye"  # Development mode
      }

      if (dir.exists(stdlib_dir)) {
        stdlib_files <- list.files(
          stdlib_dir,
          pattern = "\\.rye$",
          full.names = TRUE,
          recursive = TRUE
        )
        rye_files <- c(rye_files, stdlib_files)
      }

      rye_files
    },

    # Helper: Strip paths for display in reports
    strip_display_path = function(file) {
      display_path <- sub("^\\./", "", file)

      if (!is.null(private$path_strip_patterns)) {
        for (pattern in private$path_strip_patterns) {
          display_path <- sub(pattern, "", display_path)
        }
      } else {
        # Default patterns for stdlib
        display_path <- sub(".*/inst/rye/", "", display_path)
        display_path <- sub(".*/tests/native/", "tests/", display_path)
      }

      display_path
    }
  )
)
