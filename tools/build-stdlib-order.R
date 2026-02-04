# Generate inst/rye/load-order.rds from inst/rye/*.rye (run from package root; make build/install depend on this).
if (!requireNamespace("R6", quietly = TRUE)) {
  stop("R6 is required to build stdlib load order")
}
source("R/topological-sort.R")
source("R/file-deps.R")
d <- FileDeps$new(dir = "inst/rye")
dir.create("inst/rye", showWarnings = FALSE, recursive = TRUE)
saveRDS(d$get_load_order(), "inst/rye/load-order.rds")
