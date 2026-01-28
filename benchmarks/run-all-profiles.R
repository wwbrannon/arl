# Run All Profiling Scripts
# Execute all component profiling and generate HTML reports

cat("========================================\n")
cat("Rye Performance Profiling Suite\n")
cat("========================================\n\n")

# Check for profvis
if (!requireNamespace("profvis", quietly = TRUE)) {
  stop("Package 'profvis' is required. Install with: install.packages('profvis')")
}

# Create output directory
output_dir <- "benchmarks/profiles"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created directory:", output_dir, "\n\n")
}

# Track start time
start_time <- Sys.time()

# Run all profiling scripts
scripts <- c(
  "profile-tokenizer.R",
  "profile-parser.R",
  "profile-macro.R",
  "profile-eval.R"
)

results <- list()

for (script in scripts) {
  script_path <- file.path("benchmarks", script)

  if (!file.exists(script_path)) {
    cat(sprintf("WARNING: Script not found: %s\n\n", script))
    next
  }

  cat(sprintf("Running %s...\n", script))
  script_start <- Sys.time()

  tryCatch({
    source(script_path)
    script_end <- Sys.time()
    elapsed <- difftime(script_end, script_start, units = "secs")
    results[[script]] <- list(status = "success", time = elapsed)
    cat(sprintf("✓ Completed in %.1f seconds\n\n", elapsed))
  }, error = function(e) {
    script_end <- Sys.time()
    elapsed <- difftime(script_end, script_start, units = "secs")
    results[[script]] <- list(status = "error", time = elapsed, message = e$message)
    cat(sprintf("✗ Failed after %.1f seconds: %s\n\n", elapsed, e$message))
  })
}

# Generate summary
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "secs")

cat("========================================\n")
cat("Profiling Summary\n")
cat("========================================\n\n")

# Count successful and failed
successful <- 0
failed <- 0
for (res in results) {
  if (!is.null(res$status)) {
    if (res$status == "success") {
      successful <- successful + 1
    } else if (res$status == "error") {
      failed <- failed + 1
    }
  }
}

cat(sprintf("Total scripts: %d\n", length(results)))
cat(sprintf("Successful:    %d\n", successful))
cat(sprintf("Failed:        %d\n", failed))
cat(sprintf("Total time:    %.1f seconds\n\n", total_time))

# List generated files
cat("Generated HTML reports:\n")
html_files <- list.files(output_dir, pattern = "\\.html$", full.names = TRUE)

if (length(html_files) > 0) {
  for (file in html_files) {
    file_info <- file.info(file)
    size_kb <- file_info$size / 1024
    cat(sprintf("  - %s (%.1f KB)\n", basename(file), size_kb))
  }
  cat(sprintf("\nTotal: %d HTML reports\n", length(html_files)))
} else {
  cat("  (No HTML files generated)\n\n")
  cat("NOTE: profvis requires an interactive R session to work properly.\n")
  cat("To generate profiles, run individual profile scripts interactively:\n\n")
  cat("  # In R console:\n")
  cat("  library(rye)\n")
  cat("  source('benchmarks/profile-tokenizer.R')\n")
  cat("  source('benchmarks/profile-parser.R')\n")
  cat("  source('benchmarks/profile-macro.R')\n")
  cat("  source('benchmarks/profile-eval.R')\n\n")
  cat("Or use make bench for automated performance measurement.\n")
}

if (length(html_files) > 0) {
  cat("\n========================================\n")
  cat("View reports with:\n")
  cat(sprintf("  browseURL('%s')\n", normalizePath(output_dir, mustWork = FALSE)))
  cat("========================================\n")
}

# Return results invisibly
invisible(results)
