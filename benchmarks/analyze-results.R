# Analyze Benchmark Results
# Functions for analyzing and visualizing benchmark results

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

#' Identify bottlenecks from benchmark results
#'
#' @param results Benchmark results (from load_benchmark_results)
#' @param threshold Minimum percentage of total time to be considered a bottleneck (default: 0.05)
#' @return Data frame of bottlenecks
identify_bottlenecks <- function(results, threshold = 0.05) {
  bottlenecks <- data.frame(
    component = character(),
    benchmark = character(),
    expression = character(),
    time_ms = numeric(),
    pct_total = numeric(),
    stringsAsFactors = FALSE
  )

  if ("results" %in% names(results)) {
    # Consolidated results format
    comp_results <- results$results
  } else {
    # Direct results format
    comp_results <- results
  }

  # Calculate total time across all benchmarks
  total_time <- 0

  for (comp_name in names(comp_results)) {
    comp <- comp_results[[comp_name]]

    if (is.list(comp)) {
      for (bench_name in names(comp)) {
        bench_data <- comp[[bench_name]]

        if (inherits(bench_data, "bench_mark")) {
          for (i in seq_len(nrow(bench_data))) {
            time_ms <- as.numeric(bench_data$median[i])
            total_time <- total_time + time_ms
          }
        }
      }
    }
  }

  # Identify bottlenecks
  for (comp_name in names(comp_results)) {
    comp <- comp_results[[comp_name]]

    if (is.list(comp)) {
      for (bench_name in names(comp)) {
        bench_data <- comp[[bench_name]]

        if (inherits(bench_data, "bench_mark")) {
          for (i in seq_len(nrow(bench_data))) {
            expr_name <- as.character(bench_data$expression[i])
            time_ms <- as.numeric(bench_data$median[i])
            pct <- time_ms / total_time

            if (pct >= threshold) {
              bottlenecks <- rbind(bottlenecks, data.frame(
                component = comp_name,
                benchmark = bench_name,
                expression = expr_name,
                time_ms = time_ms,
                pct_total = pct * 100,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }
  }

  # Sort by percentage
  bottlenecks <- bottlenecks[order(-bottlenecks$pct_total), ]
  rownames(bottlenecks) <- NULL

  bottlenecks
}

#' Generate component breakdown plot
#'
#' @param results Benchmark results
#' @return Data frame of component times
plot_breakdown <- function(results) {
  if ("results" %in% names(results)) {
    comp_results <- results$results
  } else {
    comp_results <- results
  }

  breakdown <- data.frame(
    component = character(),
    total_time_ms = numeric(),
    stringsAsFactors = FALSE
  )

  for (comp_name in names(comp_results)) {
    comp <- comp_results[[comp_name]]
    comp_total <- 0

    if (is.list(comp)) {
      for (bench_name in names(comp)) {
        bench_data <- comp[[bench_name]]

        if (inherits(bench_data, "bench_mark")) {
          comp_total <- comp_total + sum(as.numeric(bench_data$median))
        }
      }
    }

    breakdown <- rbind(breakdown, data.frame(
      component = comp_name,
      total_time_ms = comp_total,
      stringsAsFactors = FALSE
    ))
  }

  # Sort by time
  breakdown <- breakdown[order(-breakdown$total_time_ms), ]
  rownames(breakdown) <- NULL

  # Print simple bar chart
  cat("\nComponent Time Breakdown:\n\n")

  max_width <- 50
  max_time <- max(breakdown$total_time_ms)

  for (i in seq_len(nrow(breakdown))) {
    comp <- breakdown$component[i]
    time <- breakdown$total_time_ms[i]
    pct <- (time / sum(breakdown$total_time_ms)) * 100
    bar_width <- round((time / max_time) * max_width)

    cat(sprintf("%-12s %s %.2f ms (%.1f%%)\n",
                comp,
                strrep("█", bar_width),
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
  if ("results" %in% names(results)) {
    comp_results <- results$results
    timestamp <- results$timestamp
  } else {
    comp_results <- results
    timestamp <- "unknown"
  }

  cat("========================================\n")
  cat("Detailed Benchmark Summary\n")
  cat(sprintf("Timestamp: %s\n", timestamp))
  cat("========================================\n\n")

  for (comp_name in names(comp_results)) {
    cat(sprintf("=== %s ===\n\n", toupper(comp_name)))
    comp <- comp_results[[comp_name]]

    if (is.list(comp)) {
      for (bench_name in names(comp)) {
        cat(sprintf("%s:\n", bench_name))
        bench_data <- comp[[bench_name]]

        if (inherits(bench_data, "bench_mark")) {
          print(bench_data[, c("expression", "median", "mem_alloc", "n_itr", "n_gc")])
          cat("\n")
        } else {
          cat("  (No benchmark data)\n\n")
        }
      }
    }
  }

  invisible(NULL)
}

#' Calculate memory allocation summary
#'
#' @param results Benchmark results
#' @return Data frame of memory stats
memory_summary <- function(results) {
  if ("results" %in% names(results)) {
    comp_results <- results$results
  } else {
    comp_results <- results
  }

  mem_stats <- data.frame(
    component = character(),
    total_alloc_mb = numeric(),
    max_alloc_mb = numeric(),
    stringsAsFactors = FALSE
  )

  for (comp_name in names(comp_results)) {
    comp <- comp_results[[comp_name]]
    comp_total <- 0
    comp_max <- 0

    if (is.list(comp)) {
      for (bench_name in names(comp)) {
        bench_data <- comp[[bench_name]]

        if (inherits(bench_data, "bench_mark")) {
          allocs <- as.numeric(bench_data$mem_alloc)
          comp_total <- comp_total + sum(allocs)
          comp_max <- max(comp_max, max(allocs))
        }
      }
    }

    mem_stats <- rbind(mem_stats, data.frame(
      component = comp_name,
      total_alloc_mb = comp_total / (1024^2),
      max_alloc_mb = comp_max / (1024^2),
      stringsAsFactors = FALSE
    ))
  }

  # Sort by total allocation
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
#' @return List with fastest and slowest
extremes <- function(results, n = 5) {
  if ("results" %in% names(results)) {
    comp_results <- results$results
  } else {
    comp_results <- results
  }

  all_ops <- list()

  for (comp_name in names(comp_results)) {
    comp <- comp_results[[comp_name]]

    if (is.list(comp)) {
      for (bench_name in names(comp)) {
        bench_data <- comp[[bench_name]]

        if (inherits(bench_data, "bench_mark")) {
          for (i in seq_len(nrow(bench_data))) {
            all_ops[[length(all_ops) + 1]] <- list(
              component = comp_name,
              benchmark = bench_name,
              expression = as.character(bench_data$expression[i]),
              time_ms = as.numeric(bench_data$median[i])
            )
          }
        }
      }
    }
  }

  # Sort by time
  sorted <- all_ops[order(sapply(all_ops, function(x) x$time_ms))]

  fastest <- head(sorted, n)
  slowest <- tail(sorted, n)
  slowest <- rev(slowest)

  cat(sprintf("\n=== Top %d Fastest Operations ===\n\n", n))
  for (i in seq_along(fastest)) {
    op <- fastest[[i]]
    cat(sprintf("%d. %.4f ms - %s (%s:%s)\n",
                i, op$time_ms, op$expression, op$component, op$benchmark))
  }

  cat(sprintf("\n=== Top %d Slowest Operations ===\n\n", n))
  for (i in seq_along(slowest)) {
    op <- slowest[[i]]
    cat(sprintf("%d. %.2f ms - %s (%s:%s)\n",
                i, op$time_ms, op$expression, op$component, op$benchmark))
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
  cat("  results <- load_benchmark_results('benchmarks/results/baseline-YYYYMMDD-HHMMSS.rds')\n")
  cat("  plot_breakdown(results)\n")
  cat("  bottlenecks <- identify_bottlenecks(results)\n")
  cat("  print(bottlenecks)\n\n")

  .benchmark_analysis_loaded <- TRUE
}
