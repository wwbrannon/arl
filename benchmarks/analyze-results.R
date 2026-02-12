# Analyze Benchmark Results
# Functions for analyzing and visualizing benchmark results

# Source benchmark helpers (works from different working directories)
if (file.exists("benchmarks/benchmark-helpers.R")) {
  source("benchmarks/benchmark-helpers.R")
} else if (file.exists("benchmark-helpers.R")) {
  source("benchmark-helpers.R")
} else {
  # Try installed package
  helpers_path <- system.file("benchmarks/benchmark-helpers.R", package = "arl")
  if (helpers_path != "") {
    source(helpers_path)
  }
}

#' Normalize results to a flat data frame
#'
#' Accepts either a flat data frame (CSV format) or the legacy consolidated
#' list format (with $results), and returns a flat data frame.
#'
#' @param results Benchmark results
#' @return Data frame with columns: component, benchmark, expression, median_ms, mem_alloc_bytes
normalize_results <- function(results) {
  if (is.data.frame(results)) return(results)
  if ("results" %in% names(results) && is.data.frame(results$results)) {
    return(results$results)
  }
  stop("Unrecognized results format")
}

#' Identify bottlenecks from benchmark results
#'
#' @param results Benchmark results (data frame or list with $results)
#' @param threshold Minimum percentage of total time to be considered a bottleneck (default: 0.05)
#' @return Data frame of bottlenecks
identify_bottlenecks <- function(results, threshold = 0.05) {
  df <- normalize_results(results)
  total_time <- sum(df$median_ms)
  df$pct_total <- (df$median_ms / total_time) * 100

  bottlenecks <- df[df$median_ms / total_time >= threshold, ]
  bottlenecks <- bottlenecks[order(-bottlenecks$pct_total), ]
  rownames(bottlenecks) <- NULL
  bottlenecks
}

#' Generate component breakdown plot
#'
#' @param results Benchmark results
#' @return Data frame of component times
plot_breakdown <- function(results) {
  df <- normalize_results(results)

  breakdown <- aggregate(median_ms ~ component, data = df, FUN = sum)
  names(breakdown) <- c("component", "total_time_ms")
  breakdown <- breakdown[order(-breakdown$total_time_ms), ]
  rownames(breakdown) <- NULL

  cat("\nComponent Time Breakdown:\n\n")

  max_width <- 50
  max_time <- max(breakdown$total_time_ms)
  grand_total <- sum(breakdown$total_time_ms)

  for (i in seq_len(nrow(breakdown))) {
    comp <- breakdown$component[i]
    time <- breakdown$total_time_ms[i]
    pct <- (time / grand_total) * 100
    bar_width <- round((time / max_time) * max_width)

    cat(sprintf("%-12s %s %.2f ms (%.1f%%)\n",
                comp,
                strrep("\u2588", bar_width),
                time,
                pct))
  }

  cat("\n")

  invisible(breakdown)
}

#' Print detailed summary of all results
#'
#' @param results Benchmark results
#' @return Invisible NULL
print_detailed_summary <- function(results) {
  df <- normalize_results(results)

  cat("========================================\n")
  cat("Detailed Benchmark Summary\n")
  cat("========================================\n\n")

  for (comp_name in unique(df$component)) {
    cat(sprintf("=== %s ===\n\n", toupper(comp_name)))
    comp_df <- df[df$component == comp_name, ]

    for (bench_name in unique(comp_df$benchmark)) {
      cat(sprintf("%s:\n", bench_name))
      bench_df <- comp_df[comp_df$benchmark == bench_name, ]
      print(bench_df[, c("expression", "median_ms", "mem_alloc_bytes", "n_itr")])
      cat("\n")
    }
  }

  invisible(NULL)
}

#' Calculate memory allocation summary
#'
#' @param results Benchmark results
#' @return Data frame of memory stats
memory_summary <- function(results) {
  df <- normalize_results(results)

  mem_stats <- aggregate(
    mem_alloc_bytes ~ component,
    data = df,
    FUN = function(x) c(total = sum(x), max = max(x))
  )
  # aggregate with multi-value FUN returns a matrix column
  mem_stats <- data.frame(
    component = mem_stats$component,
    total_alloc_mb = mem_stats$mem_alloc_bytes[, "total"] / (1024^2),
    max_alloc_mb = mem_stats$mem_alloc_bytes[, "max"] / (1024^2),
    stringsAsFactors = FALSE
  )
  mem_stats <- mem_stats[order(-mem_stats$total_alloc_mb), ]
  rownames(mem_stats) <- NULL

  cat("\nMemory Allocation Summary:\n\n")
  print(mem_stats)
  cat("\n")

  invisible(mem_stats)
}

#' Find fastest and slowest operations
#'
#' @param results Benchmark results
#' @param n Number of results to show (default: 5)
#' @return List with fastest and slowest data frames
extremes <- function(results, n = 5) {
  df <- normalize_results(results)
  sorted <- df[order(df$median_ms), ]

  fastest <- head(sorted, n)
  slowest <- tail(sorted, n)
  slowest <- slowest[order(-slowest$median_ms), ]

  cat(sprintf("\n=== Top %d Fastest Operations ===\n\n", n))
  for (i in seq_len(nrow(fastest))) {
    row <- fastest[i, ]
    cat(sprintf("%d. %.4f ms - %s (%s:%s)\n",
                i, row$median_ms, row$expression, row$component, row$benchmark))
  }

  cat(sprintf("\n=== Top %d Slowest Operations ===\n\n", n))
  for (i in seq_len(nrow(slowest))) {
    row <- slowest[i, ]
    cat(sprintf("%d. %.2f ms - %s (%s:%s)\n",
                i, row$median_ms, row$expression, row$component, row$benchmark))
  }

  cat("\n")

  invisible(list(fastest = fastest, slowest = slowest))
}

# Interactive analysis if run directly (skip banner when running under testthat)
if (!interactive() && !exists(".benchmark_analysis_loaded") && !nzchar(Sys.getenv("TESTTHAT"))) {
  cat("Benchmark Analysis Functions Loaded\n\n")
  cat("Available functions:\n")
  cat("  - identify_bottlenecks(results, threshold = 0.05)\n")
  cat("  - plot_breakdown(results)\n")
  cat("  - print_detailed_summary(results)\n")
  cat("  - memory_summary(results)\n")
  cat("  - extremes(results, n = 5)\n\n")
  cat("Example usage:\n")
  cat("  results <- load_benchmark_results('benchmarks/results/baseline-YYYYMMDD-HHMMSS.csv')\n")
  cat("  plot_breakdown(results)\n")
  cat("  bottlenecks <- identify_bottlenecks(results)\n")
  cat("  print(bottlenecks)\n\n")

  .benchmark_analysis_loaded <- TRUE
}
