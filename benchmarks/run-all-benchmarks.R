# Run All Benchmarks
# Execute all component benchmarks and generate summary report

cat("========================================\n")
cat("Rye Performance Benchmark Suite\n")
cat("========================================\n\n")

# Check for bench
if (!requireNamespace("bench", quietly = TRUE)) {
  stop("Package 'bench' is required. Install with: install.packages('bench')")
}

# Create output directory
output_dir <- "benchmarks/results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created directory:", output_dir, "\n\n")
}

# Track start time
start_time <- Sys.time()

# Run all benchmark scripts
scripts <- c(
  "bench-tokenizer.R",
  "bench-parser.R",
  "bench-macro.R",
  "bench-eval.R",
  "bench-stdlib.R",
  "bench-interop.R",
  "bench-modules.R",
  # "bench-e2e.R"  # FIXME disabled for now because too slow
)

all_results <- list()
script_timings <- list()

for (script in scripts) {
  script_path <- file.path("benchmarks", script)

  if (!file.exists(script_path)) {
    cat(sprintf("WARNING: Script not found: %s\n\n", script))
    next
  }

  cat(sprintf("Running %s...\n", script))
  script_start <- Sys.time()

  tryCatch({
    # Capture output
    output <- capture.output({
      source(script_path, local = TRUE)
    })

    script_end <- Sys.time()
    elapsed <- difftime(script_end, script_start, units = "secs")
    script_timings[[script]] <- list(status = "success", time = elapsed)

    cat(sprintf("✓ Completed in %.1f seconds\n\n", elapsed))
  }, error = function(e) {
    script_end <- Sys.time()
    elapsed <- difftime(script_end, script_start, units = "secs")
    script_timings[[script]] <<- list(status = "error", time = elapsed, message = e$message)
    cat(sprintf("✗ Failed after %.1f seconds: %s\n\n", elapsed, e$message))
  })
}

# Load all saved results
cat("========================================\n")
cat("Loading Benchmark Results\n")
cat("========================================\n\n")

result_files <- list.files(output_dir, pattern = "\\.rds$", full.names = TRUE)

# Get the most recent result for each component
components <- c("tokenizer", "parser", "macro", "eval", "stdlib", "interop", "modules", "e2e")

for (component in components) {
  component_files <- grep(paste0("^", component, "-"), basename(result_files), value = TRUE)

  if (length(component_files) > 0) {
    # Get most recent
    full_paths <- file.path(output_dir, component_files)
    file_times <- file.info(full_paths)$mtime
    latest_file <- full_paths[which.max(file_times)]

    tryCatch({
      all_results[[component]] <- readRDS(latest_file)
      cat(sprintf("✓ Loaded %s from %s\n", component, basename(latest_file)))
    }, error = function(e) {
      cat(sprintf("✗ Failed to load %s: %s\n", component, e$message))
    })
  } else {
    cat(sprintf("- No results found for %s\n", component))
  }
}

# Generate summary report
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "secs")

cat("\n========================================\n")
cat("Benchmark Summary\n")
cat("========================================\n\n")

successful <- sum(sapply(script_timings, function(x) x$status == "success"))
failed <- sum(sapply(script_timings, function(x) x$status == "error"))

cat(sprintf("Total scripts: %d\n", length(script_timings)))
cat(sprintf("Successful:    %d\n", successful))
cat(sprintf("Failed:        %d\n", failed))
cat(sprintf("Total time:    %.1f seconds\n\n", total_time))

# Component breakdown
if (length(all_results) > 0) {
  cat("Component Results:\n")

  for (comp_name in names(all_results)) {
    cat(sprintf("\n%s:\n", toupper(comp_name)))
    comp_results <- all_results[[comp_name]]

    # Extract median times from each benchmark
    if (is.list(comp_results)) {
      for (bench_name in names(comp_results)) {
        bench_data <- comp_results[[bench_name]]
        if (inherits(bench_data, "bench_mark")) {
          # Get summary stats
          median_time <- median(as.numeric(bench_data$median))
          max_time <- max(as.numeric(bench_data$median))
          total_alloc <- sum(as.numeric(bench_data$mem_alloc))

          cat(sprintf("  %s: median=%.2f ms, max=%.2f ms, mem=%s\n",
                      bench_name, median_time, max_time,
                      format(structure(total_alloc, class = "Bench_bytes"), digits = 2)))
        }
      }
    }
  }
}

# Save consolidated results
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
consolidated_file <- file.path(output_dir, paste0("baseline-", timestamp, ".rds"))

saveRDS(list(
  results = all_results,
  timings = script_timings,
  timestamp = timestamp,
  total_time = total_time
), consolidated_file)

cat(sprintf("\n\nConsolidated results saved to:\n  %s\n", consolidated_file))

# Hot path summary
cat("\n========================================\n")
cat("Performance Hotspots\n")
cat("========================================\n\n")

# Extract slowest operations from each component
slowest <- list()

for (comp_name in names(all_results)) {
  comp_results <- all_results[[comp_name]]

  if (is.list(comp_results)) {
    for (bench_name in names(comp_results)) {
      bench_data <- comp_results[[bench_name]]

      if (inherits(bench_data, "bench_mark")) {
        for (i in seq_len(nrow(bench_data))) {
          expr_name <- as.character(bench_data$expression[i])
          median_time <- as.numeric(bench_data$median[i])

          slowest[[length(slowest) + 1]] <- list(
            component = comp_name,
            benchmark = bench_name,
            expression = expr_name,
            time_ms = median_time
          )
        }
      }
    }
  }
}

# Sort by time and show top 10
if (length(slowest) > 0) {
  slowest_sorted <- slowest[order(sapply(slowest, function(x) -x$time_ms))]
  top_10 <- head(slowest_sorted, 10)

  cat("Top 10 slowest operations:\n\n")
  for (i in seq_along(top_10)) {
    item <- top_10[[i]]
    cat(sprintf("%2d. %.2f ms - %s (%s:%s)\n",
                i, item$time_ms, item$expression, item$component, item$benchmark))
  }
}

cat("\n========================================\n")
cat("Next Steps\n")
cat("========================================\n\n")
cat("1. Review HTML profiling reports:\n")
cat("   source('benchmarks/run-all-profiles.R')\n\n")
cat("2. Analyze results:\n")
cat("   source('benchmarks/analyze-results.R')\n\n")
cat("3. Compare with previous baseline:\n")
cat("   source('benchmarks/compare-results.R')\n\n")

cat("========================================\n")

# Return results invisibly
invisible(list(
  results = all_results,
  timings = script_timings,
  consolidated_file = consolidated_file
))
