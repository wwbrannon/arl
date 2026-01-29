# Benchmark Infrastructure Helpers
# Helper functions for running benchmarks and profiling Rye components

#' Evaluate Rye code from text using an engine
#'
#' @param text Rye code as string
#' @param engine RyeEngine instance
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

#' Profile a component and save HTML output
#'
#' @param expr Expression to profile
#' @param name Name for the output file
#' @param output_dir Directory for HTML output (default: benchmarks/profiles/)
#' @return Path to generated HTML file
profile_component <- function(expr, name, output_dir = "benchmarks/profiles") {
  if (!requireNamespace("profvis", quietly = TRUE)) {
    stop("Package 'profvis' is required for profiling. Install with: install.packages('profvis')")
  }

  # profvis wants this if run noninteractively
  old <- options(keep.source = TRUE)
  on.exit(options(old), add = TRUE)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_file <- file.path(output_dir, paste0(name, ".html"))

  # Note: profvis has limitations when running from sourced scripts
  # It works best in interactive sessions
  result <- {
    expr <- substitute(expr)

    prof <- profvis::profvis({
        eval(expr, envir = parent.frame())
    }, rerun=TRUE)  # rerun helps if expr finishes too fast to sample
    htmlwidgets::saveWidget(prof, output_file, selfcontained = TRUE)
    if (!nzchar(Sys.getenv("TESTTHAT"))) {
      cat(sprintf("  ✓ Generated: %s\n", output_file))
    }

    output_file
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

#' Save benchmark results to RDS file
#'
#' @param results Benchmark results (list or bench::mark output)
#' @param name Name for the results (e.g., "baseline", "optimized")
#' @param output_dir Directory for results (default: benchmarks/results/)
#' @return Path to saved RDS file
save_benchmark_results <- function(results, name = "benchmark",
                                    output_dir = "benchmarks/results") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  filename <- paste0(name, "-", timestamp, ".rds")
  output_file <- file.path(output_dir, filename)

  saveRDS(results, output_file)
  if (!nzchar(Sys.getenv("TESTTHAT"))) {
    message("Results saved to: ", output_file)
  }
  invisible(output_file)
}

#' Load benchmark results from RDS file
#'
#' @param file Path to RDS file
#' @return Benchmark results
load_benchmark_results <- function(file) {
  if (!file.exists(file)) {
    stop("Results file not found: ", file)
  }
  readRDS(file)
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
