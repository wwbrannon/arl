# Generate inst/arl/load-order.txt from inst/arl/*.arl (run from package root; make build/install depend on this).
if (!requireNamespace("R6", quietly = TRUE)) {
  stop("R6 is required to build stdlib load order")
}
source("R/topological-sort.R")
source("R/file-deps.R")
d <- FileDeps$new(dir = "inst/arl")
dir.create("inst/arl", showWarnings = FALSE, recursive = TRUE)
writeLines(d$get_load_order(), "inst/arl/load-order.txt")
