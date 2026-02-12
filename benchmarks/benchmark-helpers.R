# Benchmark Infrastructure Helpers
# Helper functions for running benchmarks and profiling Rye components

#' Evaluate Rye code from text using an engine
#'
#' @param text Rye code as string
#' @param engine Engine instance
#' @param env Environment to evaluate in (defaults to engine env)
#' @return Result of last expression
eval_text <- function(text, engine, env = NULL) {
  if (is.null(env)) {
    env <- engine$env$env
  }
  engine$eval_text(text, env)
}

#' Benchmark a component with standard settings
#'
#' @param ... Expressions to benchmark (use named arguments)
#' @param iterations Number of iterations (default: NULL for auto)
#' @param check Check that results are equal (default: TRUE)
#' @return bench::mark result
benchmark_component <- function(..., iterations = NULL, check = TRUE) {
  if (!requireNamespace("bench", quietly = TRUE)) {
    stop("Package 'bench' is required for benchmarking. Install with: install.packages('bench')")
  }

  bench::mark(
    ...,
    iterations = iterations,
    check = check,
    memory = TRUE,
    filter_gc = FALSE,
    time_unit = "ms"
  )
}

#' Profile a component and save profiling data
#'
#' @param expr Expression to profile
#' @param name Name for the output file
#' @param output_dir Directory for output (default: benchmarks/profiles/)
#' @param save_html Whether to also save HTML (default: TRUE)
#' @return Path to generated .rds file
profile_component <- function(expr, name, output_dir = "benchmarks/profiles", save_html = TRUE) {
  if (!requireNamespace("profvis", quietly = TRUE)) {
    stop("Package 'profvis' is required for profiling. Install with: install.packages('profvis')")
  }

  # profvis wants this if run noninteractively
  old <- options(keep.source = TRUE)
  on.exit(options(old), add = TRUE)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Add timestamp to .rds for historical tracking (like benchmark results)
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  output_rds <- file.path(output_dir, paste0(name, "-", timestamp, ".rds"))
  output_html <- file.path(output_dir, paste0(name, ".html"))

  # Note: profvis has limitations when running from sourced scripts
  # It works best in interactive sessions
  result <- {
    expr <- substitute(expr)

    prof <- profvis::profvis({
        eval(expr, envir = parent.frame())
    }, rerun=TRUE)  # rerun helps if expr finishes too fast to sample

    # Save compact .rds format (much smaller, machine-readable)
    saveRDS(prof, output_rds, compress = "xz")
    if (!nzchar(Sys.getenv("TESTTHAT"))) {
      cat(sprintf("  ✓ Generated: %s\n", output_rds))
    }

    # Optionally save HTML for immediate viewing
    if (save_html) {
      htmlwidgets::saveWidget(prof, output_html, selfcontained = TRUE)
      if (!nzchar(Sys.getenv("TESTTHAT"))) {
        cat(sprintf("  ✓ Generated: %s\n", output_html))
      }
    }

    output_rds
  }

  invisible(result)
}

#' Create synthetic Rye code workload
#'
#' @param type Type of workload: "micro", "small", "medium", "large", "xl"
#' @param kind Kind of code: "arithmetic", "nested_lists", "strings", "mixed"
#' @return Character string of Rye code
create_workload <- function(type = "small", kind = "arithmetic") {
  size <- switch(type,
    micro = 1,
    small = 10,
    medium = 100,
    large = 500,
    xl = 2000,
    stop("Unknown workload type: ", type)
  )

  if (kind == "arithmetic") {
    # Simple arithmetic expressions
    paste0("(+ ", paste(rep("1", size), collapse = " "), ")")
  } else if (kind == "nested_lists") {
    # Deeply nested lists
    code <- "1"
    for (i in seq_len(size)) {
      code <- paste0("(list ", code, ")")
    }
    code
  } else if (kind == "strings") {
    # String operations
    str_size <- size * 10
    paste0('(str "', paste(rep("x", str_size), collapse = ""), '")')
  } else if (kind == "mixed") {
    # Mixed content
    elements <- replicate(size, {
      sample(c(
        paste0(sample(1:100, 1)),
        paste0('"', paste(sample(letters, 5), collapse = ""), '"'),
        sample(c("#t", "#f", "#nil"), 1)
      ), 1)
    })
    paste0("(list ", paste(elements, collapse = " "), ")")
  } else {
    stop("Unknown workload kind: ", kind)
  }
}

#' Load an example file as a workload
#'
#' @param filename Name of example file (e.g., "fibonacci.rye")
#' @return Character string of Rye code
load_example_workload <- function(filename) {
  example_path <- system.file("examples", filename, package = "rye")
  if (example_path == "") {
    stop("Example file not found: ", filename)
  }
  readLines(example_path, warn = FALSE) |> paste(collapse = "\n")
}

#' Flatten bench_mark results to a data frame
#'
#' @param results Named list of bench_mark objects (e.g. list(strings = bench_strings))
#' @return Data frame with columns: benchmark, expression, median_ms, mem_alloc_bytes, n_itr
flatten_bench_results <- function(results) {
  rows <- list()
  for (bench_name in names(results)) {
    bench_data <- results[[bench_name]]
    if (inherits(bench_data, "bench_mark")) {
      # bench_mark stores labels in names(expression), deparsed code in as.character()
      expr_names <- names(bench_data$expression)
      for (i in seq_len(nrow(bench_data))) {
        label <- if (!is.null(expr_names) && nzchar(expr_names[i])) {
          expr_names[i]
        } else {
          as.character(bench_data$expression[i])
        }
        rows[[length(rows) + 1L]] <- data.frame(
          benchmark = bench_name,
          expression = label,
          median_ms = as.numeric(bench_data$median[i]),
          mem_alloc_bytes = as.numeric(bench_data$mem_alloc[i]),
          n_itr = as.integer(bench_data$n_itr[i]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      benchmark = character(), expression = character(),
      median_ms = numeric(), mem_alloc_bytes = numeric(),
      n_itr = integer(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' Save benchmark results to CSV file
#'
#' @param results Named list of bench_mark objects, or a pre-flattened data frame
#' @param name Name for the results (e.g., "tokenizer", "baseline")
#' @param output_dir Directory for results (default: benchmarks/results/)
#' @return Path to saved CSV file (invisibly)
save_benchmark_results <- function(results, name = "benchmark",
                                    output_dir = "benchmarks/results") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Flatten if given a list of bench_mark objects
  if (is.data.frame(results)) {
    df <- results
  } else {
    df <- flatten_bench_results(results)
  }

  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  filename <- paste0(name, "-", timestamp, ".csv")
  output_file <- file.path(output_dir, filename)

  write.csv(df, output_file, row.names = FALSE)
  if (!nzchar(Sys.getenv("TESTTHAT"))) {
    message("Results saved to: ", output_file)
  }
  invisible(output_file)
}

#' Load benchmark results from CSV file
#'
#' @param file Path to CSV file
#' @return Data frame of benchmark results
load_benchmark_results <- function(file) {
  if (!file.exists(file)) {
    stop("Results file not found: ", file)
  }
  read.csv(file, stringsAsFactors = FALSE)
}

#' Write benchmark results as JSON for github-action-benchmark
#'
#' Produces a JSON array in customSmallerIsBetter format.
#' No jsonlite dependency — hand-rolled with sprintf/paste.
#'
#' @param df Data frame with columns: component (optional), benchmark, expression, median_ms
#' @param file Output file path (default: "benchmark-results.json")
#' @return file path (invisibly)
write_ci_json <- function(df, file = "benchmark-results.json") {
  entries <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    # Build name: "component/benchmark/expression" or "benchmark/expression"
    if ("component" %in% names(row) && nzchar(row$component)) {
      name <- paste(row$component, row$benchmark, row$expression, sep = "/")
    } else {
      name <- paste(row$benchmark, row$expression, sep = "/")
    }
    # Escape for JSON
    name <- gsub("\\\\", "\\\\\\\\", name)
    name <- gsub('"', '\\\\"', name)
    entries[i] <- sprintf('  {"name": "%s", "unit": "ms", "value": %s}',
                          name, format(row$median_ms, scientific = FALSE))
  }
  json <- paste0("[\n", paste(entries, collapse = ",\n"), "\n]\n")
  writeLines(json, file, useBytes = TRUE)
  if (!nzchar(Sys.getenv("TESTTHAT"))) {
    message("CI JSON written to: ", file)
  }
  invisible(file)
}

#' Print a summary table of benchmark results
#'
#' @param results bench::mark output or list of results
#' @return Invisible results
summarize_benchmark <- function(results) {
  if (inherits(results, "bench_mark")) {
    print(results)
  } else if (is.list(results)) {
    for (name in names(results)) {
      cat("\n=== ", name, " ===\n", sep = "")
      print(results[[name]])
    }
  }
  invisible(results)
}

#' Create a simple timing function for quick measurements
#'
#' @param expr Expression to time
#' @param n Number of repetitions (default: 100)
#' @return Median time in milliseconds
quick_time <- function(expr, n = 100) {
  times <- numeric(n)
  for (i in seq_len(n)) {
    times[i] <- system.time(expr)[["elapsed"]]
  }
  median(times) * 1000  # Convert to ms
}
