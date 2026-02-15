# Run All Benchmarks
# Execute all component benchmarks and generate summary report

cat("========================================\n")
cat("Arl Performance Benchmark Suite\n")
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
  "bench-compile.R",
  "bench-r-eval.R",
  "bench-stdlib.R",
  "bench-interop.R",
  "bench-modules.R",
  "bench-e2e.R"
)

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

# Load all saved CSV results
cat("========================================\n")
cat("Loading Benchmark Results\n")
cat("========================================\n\n")

result_files <- list.files(output_dir, pattern = "\\.csv$", full.names = TRUE)

components <- c("tokenizer", "parser", "macro", "compile", "r-eval", "stdlib", "interop", "modules", "e2e")
all_dfs <- list()

for (component in components) {
  component_files <- grep(paste0("^", component, "-"), basename(result_files), value = TRUE)

  if (length(component_files) > 0) {
    full_paths <- file.path(output_dir, component_files)
    file_times <- file.info(full_paths)$mtime
    latest_file <- full_paths[which.max(file_times)]

    tryCatch({
      df <- read.csv(latest_file, stringsAsFactors = FALSE)
      df$component <- component
      all_dfs[[component]] <- df
      cat(sprintf("✓ Loaded %s from %s\n", component, basename(latest_file)))
    }, error = function(e) {
      cat(sprintf("✗ Failed to load %s: %s\n", component, e$message))
    })
  } else {
    cat(sprintf("- No results found for %s\n", component))
  }
}

# Consolidate into one data frame
consolidated <- if (length(all_dfs) > 0) do.call(rbind, all_dfs) else {
  data.frame(
    benchmark = character(), expression = character(),
    median_ms = numeric(), mem_alloc_bytes = numeric(),
    n_itr = integer(), component = character(),
    stringsAsFactors = FALSE
  )
}
rownames(consolidated) <- NULL

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
if (nrow(consolidated) > 0) {
  cat("Component Results:\n")

  for (comp_name in unique(consolidated$component)) {
    cat(sprintf("\n%s:\n", toupper(comp_name)))
    comp_df <- consolidated[consolidated$component == comp_name, ]

    for (bench_name in unique(comp_df$benchmark)) {
      bench_df <- comp_df[comp_df$benchmark == bench_name, ]
      median_time <- median(bench_df$median_ms)
      max_time <- max(bench_df$median_ms)
      total_alloc <- sum(bench_df$mem_alloc_bytes)

      cat(sprintf("  %s: median=%.2f ms, max=%.2f ms, mem=%.0f bytes\n",
                  bench_name, median_time, max_time, total_alloc))
    }
  }
}

# Save consolidated CSV
source("benchmarks/benchmark-helpers.R")
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
consolidated_file <- file.path(output_dir, paste0("baseline-", timestamp, ".csv"))
write.csv(consolidated, consolidated_file, row.names = FALSE)

cat(sprintf("\n\nConsolidated results saved to:\n  %s\n", consolidated_file))

# Write CI JSON
write_ci_json(consolidated, "benchmarks/results/benchmark-results.json")

# Hot path summary
cat("\n========================================\n")
cat("Performance Hotspots\n")
cat("========================================\n\n")

if (nrow(consolidated) > 0) {
  sorted <- consolidated[order(-consolidated$median_ms), ]
  top_10 <- head(sorted, 10)

  cat("Top 10 slowest operations:\n\n")
  for (i in seq_len(nrow(top_10))) {
    row <- top_10[i, ]
    cat(sprintf("%2d. %.2f ms - %s (%s:%s)\n",
                i, row$median_ms, row$expression, row$component, row$benchmark))
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
  results = consolidated,
  timings = script_timings,
  consolidated_file = consolidated_file
))
