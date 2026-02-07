# Parser Profiling
# Generate profvis flame graphs for parser performance

library(rye)

# Source helpers (works from different working directories)
if (file.exists("benchmarks/benchmark-helpers.R")) {
  source("benchmarks/benchmark-helpers.R")
} else if (file.exists("benchmark-helpers.R")) {
  source("benchmark-helpers.R")
} else {
  helpers_path <- system.file("benchmarks/benchmark-helpers.R", package = "rye")
  if (helpers_path != "") source(helpers_path)
}

if (file.exists("benchmarks/workloads.R")) {
  source("benchmarks/workloads.R")
} else if (file.exists("workloads.R")) {
  source("workloads.R")
} else {
  workloads_path <- system.file("benchmarks/workloads.R", package = "rye")
  if (workloads_path != "") source(workloads_path)
}

cat("=== Profiling Parser ===\n\n")

# Profile 1: Large flat list (tests list growing)
cat("Profile 1: Large flat list (1000 elements)\n")

engine1 <- RyeEngine$new()
flat_1000 <- paste0("(list ", paste(seq_len(1000), collapse = " "), ")")
tokens_flat_1000 <- engine1$tokenize(flat_1000)

profile_component({
  for (i in 1:100) {
    engine1$parse(tokens_flat_1000)
  }
}, "parser-flat-list")


# Profile 2: Deep nesting (tests recursion depth)
cat("Profile 2: Deep nesting (50 levels)\n")

engine2 <- RyeEngine$new()
nested_50 <- paste(rep("(list ", 50), collapse = "")
nested_50 <- paste0(nested_50, "1", paste(rep(")", 50), collapse = ""))
tokens_nested_50 <- engine2$tokenize(nested_50)

profile_component({
  for (i in 1:100) {
    engine2$parse(tokens_nested_50)
  }
}, "parser-deep-nesting")


# Profile 3: Real example file (quicksort.rye)
cat("Profile 3: Real example file (quicksort.rye)\n")

engine3 <- RyeEngine$new()
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "quicksort" %in% names(real_workloads)) {
  tokens_qs <- engine3$tokenize(real_workloads$quicksort)

  profile_component({
    for (i in 1:100) {
      engine3$parse(tokens_qs)
    }
  }, "parser-quicksort")

} else {
  cat("(Skipped - quicksort.rye not available)\n\n")
}

# Profile 4: Quote sugar expansion
cat("Profile 4: Quote sugar expansion\n")

engine4 <- RyeEngine$new()
quote_heavy <- "(list 'x 'y 'z `(a ,b ,@c) :key1 val1 :key2 val2)"
quote_heavy <- paste(rep(quote_heavy, 100), collapse = " ")
quote_heavy <- paste0("(begin ", quote_heavy, ")")
tokens_quote <- engine4$tokenize(quote_heavy)

profile_component({
  for (i in 1:50) {
    engine4$parse(tokens_quote)
  }
}, "parser-quote-sugar")


# Profile 5: Many NULL values
cat("Profile 5: Many NULL values\n")

engine5 <- RyeEngine$new()
many_nulls <- paste0("(list ", paste(rep("#nil", 500), collapse = " "), ")")
tokens_nulls <- engine5$tokenize(many_nulls)

profile_component({
  for (i in 1:100) {
    engine5$parse(tokens_nulls)
  }
}, "parser-nulls")


cat("=== Parser Profiling Complete ===\n")
cat("Profiling data saved to benchmarks/profiles/*.rds\n")
cat("Load with: prof <- readRDS('benchmarks/profiles/parser-*.rds'); print(prof)\n")
