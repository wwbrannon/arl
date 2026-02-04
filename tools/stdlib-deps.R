#!/usr/bin/env Rscript
# Thin wrapper: load rye and print stdlib load order (and optionally check undeclared).
# Usage: Rscript tools/stdlib-deps.R [--check-undeclared] [inst/rye]
# The real implementation lives in R/file-deps.R (FileDeps class).

run_as_script <- length(commandArgs(trailingOnly = FALSE)) > 1 &&
  any(grepl("stdlib-deps\\.R", commandArgs(trailingOnly = FALSE), fixed = TRUE))

if (run_as_script) {
  if (!requireNamespace("rye", quietly = TRUE)) {
    stop("Package 'rye' must be installed. Run from package root with devtools::load_all() or install first.")
  }
  args <- commandArgs(trailingOnly = TRUE)
  check_undeclared <- "--check-undeclared" %in% args
  args <- args[args != "--check-undeclared"]
  stdlib_dir <- if (length(args) > 0) args[1] else NULL
  if (check_undeclared) {
    rye:::rye_stdlib_print_order_and_check_undeclared(stdlib_dir)
  } else {
    rye:::rye_stdlib_print_order(stdlib_dir)
  }
}
