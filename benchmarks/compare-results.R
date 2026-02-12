# Compare Benchmark Results
# Compare two benchmark runs to identify improvements and regressions

# Source benchmark helpers (works from different working directories)
if (file.exists("benchmarks/benchmark-helpers.R")) {
  source("benchmarks/benchmark-helpers.R")
} else if (file.exists("benchmark-helpers.R")) {
  source("benchmark-helpers.R")
} else {
  # Try installed package
  helpers_path <- system.file("benchmarks/benchmark-helpers.R", package = "rye")
  if (helpers_path != "") {
    source(helpers_path)
  }
}

#' Compare two benchmark results
#'
#' @param old_file Path to baseline/old results CSV file
#' @param new_file Path to comparison/new results CSV file
#' @param regression_threshold Percentage slowdown to flag as regression (default: 5)
#' @return Comparison data frame
compare_benchmarks <- function(old_file, new_file, regression_threshold = 5) {
  quiet <- nzchar(Sys.getenv("TESTTHAT"))
  if (quiet) {
    sink(nullfile())
    on.exit(sink(), add = TRUE)
  }

  # Load results
  cat("Loading benchmark results...\n")
  old_df <- load_benchmark_results(old_file)
  new_df <- load_benchmark_results(new_file)

  cat(sprintf("  Old: %s\n", basename(old_file)))
  cat(sprintf("  New: %s\n", basename(new_file)))
  cat("\n")

  cat("========================================\n")
  cat("Benchmark Comparison\n")
  cat("========================================\n\n")

  # Build merge key from available columns
  key_cols <- intersect(c("component", "benchmark", "expression"), names(old_df))
  key_cols <- intersect(key_cols, names(new_df))

  comparison <- merge(old_df[, c(key_cols, "median_ms")],
                      new_df[, c(key_cols, "median_ms")],
                      by = key_cols, suffixes = c("_old", "_new"))

  names(comparison)[names(comparison) == "median_ms_old"] <- "old_ms"
  names(comparison)[names(comparison) == "median_ms_new"] <- "new_ms"

  comparison$speedup <- comparison$old_ms / comparison$new_ms
  comparison$change_pct <- ((comparison$new_ms - comparison$old_ms) / comparison$old_ms) * 100

  comparison$status <- ifelse(
    comparison$change_pct > regression_threshold, "REGRESSION",
    ifelse(comparison$change_pct < -5, "IMPROVEMENT", "SIMILAR")
  )

  # Sort by change percentage (worst regressions first)
  comparison <- comparison[order(-comparison$change_pct), ]
  rownames(comparison) <- NULL

  # Summary statistics
  cat("========================================\n")
  cat("Summary\n")
  cat("========================================\n\n")

  n_regressions <- sum(comparison$status == "REGRESSION")
  n_improvements <- sum(comparison$status == "IMPROVEMENT")
  n_similar <- sum(comparison$status == "SIMILAR")

  cat(sprintf("Total comparisons: %d\n", nrow(comparison)))
  cat(sprintf("Regressions:       %d (>%.0f%% slower)\n", n_regressions, regression_threshold))
  cat(sprintf("Improvements:      %d (>5%% faster)\n", n_improvements))
  cat(sprintf("Similar:           %d\n", n_similar))
  cat("\n")

  # Overall speedup
  old_total <- sum(comparison$old_ms)
  new_total <- sum(comparison$new_ms)
  overall_speedup <- old_total / new_total
  overall_change <- ((new_total - old_total) / old_total) * 100

  cat(sprintf("Overall performance:\n"))
  cat(sprintf("  Old total: %.2f ms\n", old_total))
  cat(sprintf("  New total: %.2f ms\n", new_total))
  cat(sprintf("  Speedup:   %.2fx\n", overall_speedup))
  cat(sprintf("  Change:    %+.1f%%\n\n", overall_change))

  # Regressions
  if (n_regressions > 0) {
    cat("========================================\n")
    cat("REGRESSIONS (slowdowns)\n")
    cat("========================================\n\n")

    regressions <- comparison[comparison$status == "REGRESSION", ]

    for (i in seq_len(min(10, nrow(regressions)))) {
      row <- regressions[i, ]
      label <- if ("component" %in% names(row)) {
        sprintf("%s (%s:%s)", row$expression, row$component, row$benchmark)
      } else {
        sprintf("%s (%s)", row$expression, row$benchmark)
      }
      cat(sprintf("%d. %s\n", i, label))
      cat(sprintf("   Old: %.2f ms -> New: %.2f ms\n", row$old_ms, row$new_ms))
      cat(sprintf("   Change: %+.1f%% (%.2fx slower)\n\n", row$change_pct, 1/row$speedup))
    }
  }

  # Improvements
  if (n_improvements > 0) {
    cat("========================================\n")
    cat("IMPROVEMENTS (speedups)\n")
    cat("========================================\n\n")

    improvements <- comparison[comparison$status == "IMPROVEMENT", ]
    improvements <- improvements[order(improvements$change_pct), ]

    for (i in seq_len(min(10, nrow(improvements)))) {
      row <- improvements[i, ]
      label <- if ("component" %in% names(row)) {
        sprintf("%s (%s:%s)", row$expression, row$component, row$benchmark)
      } else {
        sprintf("%s (%s)", row$expression, row$benchmark)
      }
      cat(sprintf("%d. %s\n", i, label))
      cat(sprintf("   Old: %.2f ms -> New: %.2f ms\n", row$old_ms, row$new_ms))
      cat(sprintf("   Change: %+.1f%% (%.2fx faster)\n\n", row$change_pct, row$speedup))
    }
  }

  # Component breakdown (only if component column exists)
  if ("component" %in% names(comparison)) {
    cat("========================================\n")
    cat("Component Breakdown\n")
    cat("========================================\n\n")

    comp_summary <- aggregate(
      cbind(old_ms, new_ms) ~ component,
      data = comparison,
      FUN = sum
    )

    comp_summary$speedup <- comp_summary$old_ms / comp_summary$new_ms
    comp_summary$change_pct <- ((comp_summary$new_ms - comp_summary$old_ms) / comp_summary$old_ms) * 100

    for (i in seq_len(nrow(comp_summary))) {
      row <- comp_summary[i, ]
      cat(sprintf("%-12s: %.2f ms -> %.2f ms (%+.1f%%, %.2fx)\n",
                  row$component, row$old_ms, row$new_ms, row$change_pct, row$speedup))
    }

    cat("\n========================================\n")
  }

  invisible(comparison)
}

#' Generate comparison report and save to file
#'
#' @param old_file Path to baseline/old results CSV file
#' @param new_file Path to comparison/new results CSV file
#' @param output_file Path to save report (default: auto-generated)
#' @return Path to saved report
generate_comparison_report <- function(old_file, new_file, output_file = NULL) {
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
    output_file <- file.path("benchmarks/results", paste0("comparison-", timestamp, ".txt"))
  }

  # Capture output
  output <- capture.output({
    compare_benchmarks(old_file, new_file)
  })

  # Write to file
  writeLines(output, output_file)

  cat(sprintf("\nComparison report saved to:\n  %s\n", output_file))

  invisible(output_file)
}

#' Quick comparison with automatic file selection
#'
#' @param old_pattern Pattern to match old results (default: "baseline")
#' @param new_pattern Pattern to match new results (default: most recent non-baseline)
#' @return Comparison data frame
quick_compare <- function(old_pattern = "baseline", new_pattern = NULL) {
  results_dir <- "benchmarks/results"
  all_files <- list.files(results_dir, pattern = "\\.csv$", full.names = TRUE)

  # Find baseline
  baseline_files <- grep(old_pattern, all_files, value = TRUE)

  if (length(baseline_files) == 0) {
    stop("No baseline file found matching pattern: ", old_pattern)
  }

  # Use most recent baseline
  baseline_times <- file.info(baseline_files)$mtime
  old_file <- baseline_files[which.max(baseline_times)]

  # Find comparison
  if (is.null(new_pattern)) {
    # Use most recent non-baseline file
    non_baseline <- setdiff(all_files, baseline_files)

    if (length(non_baseline) == 0) {
      stop("No comparison file found")
    }

    comp_times <- file.info(non_baseline)$mtime
    new_file <- non_baseline[which.max(comp_times)]
  } else {
    comp_files <- grep(new_pattern, all_files, value = TRUE)

    if (length(comp_files) == 0) {
      stop("No comparison file found matching pattern: ", new_pattern)
    }

    comp_times <- file.info(comp_files)$mtime
    new_file <- comp_files[which.max(comp_times)]
  }

  compare_benchmarks(old_file, new_file)
}

# Interactive usage if run directly (skip banner when running under testthat)
if (!interactive() && !exists(".comparison_loaded") && !nzchar(Sys.getenv("TESTTHAT"))) {
  cat("Benchmark Comparison Functions Loaded\n\n")
  cat("Available functions:\n")
  cat("  - compare_benchmarks(old_file, new_file, regression_threshold = 5)\n")
  cat("  - generate_comparison_report(old_file, new_file, output_file = NULL)\n")
  cat("  - quick_compare(old_pattern = 'baseline', new_pattern = NULL)\n\n")
  cat("Example usage:\n")
  cat("  # Compare two specific files\n")
  cat("  comparison <- compare_benchmarks(\n")
  cat("    'benchmarks/results/baseline-20260127-120000.csv',\n")
  cat("    'benchmarks/results/optimized-20260127-130000.csv'\n")
  cat("  )\n\n")
  cat("  # Quick comparison with latest\n")
  cat("  comparison <- quick_compare()\n\n")

  .comparison_loaded <- TRUE
}
