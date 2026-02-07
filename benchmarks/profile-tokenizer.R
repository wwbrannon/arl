# Tokenizer Profiling
# Generate profvis flame graphs for tokenizer performance

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

cat("=== Profiling Tokenizer ===\n\n")

# Profile 1: Large string literal (tests string accumulation)
cat("Profile 1: Large string literal (10K chars)\n")

engine1 <- RyeEngine$new()
large_string <- paste0('"', paste(rep("x", 10000), collapse = ""), '"')

result <- profile_component({
  for (i in 1:100) {
    engine1$tokenize(large_string)
  }
}, "tokenizer-large-string")

if (!is.null(result)) {
}

# Profile 2: Deep nesting (tests parenthesis handling)
cat("Profile 2: Deep nesting (100 levels)\n")

engine2 <- RyeEngine$new()
nested_100 <- paste(rep("(", 100), collapse = "")
nested_100 <- paste0(nested_100, "x", paste(rep(")", 100), collapse = ""))

profile_component({
  for (i in 1:100) {
    engine2$tokenize(nested_100)
  }
}, "tokenizer-deep-nesting")


# Profile 3: Real example file (fibonacci.rye)
cat("Profile 3: Real example file (fibonacci.rye)\n")

engine3 <- RyeEngine$new()
real_workloads <- get_real_workloads()

if (length(real_workloads) > 0 && "fibonacci" %in% names(real_workloads)) {
  profile_component({
    for (i in 1:100) {
      engine3$tokenize(real_workloads$fibonacci)
    }
  }, "tokenizer-fibonacci")

} else {
  cat("(Skipped - fibonacci.rye not available)\n\n")
}

# Profile 4: Many escape sequences
cat("Profile 4: Many escape sequences\n")

engine4 <- RyeEngine$new()
many_escapes <- paste0('"', paste(rep('\\n\\t\\r\\"\\\\', 200), collapse = ""), '"')

profile_component({
  for (i in 1:100) {
    engine4$tokenize(many_escapes)
  }
}, "tokenizer-escapes")


# Profile 5: Mixed content (comprehensive)
cat("Profile 5: Mixed content\n")

engine5 <- RyeEngine$new()
mixed <- '(define x 42) (+ 1 2 3) (str "hello" "world") (lambda (x) (* x x))'
mixed_large <- paste(rep(mixed, 100), collapse = " ")

profile_component({
  for (i in 1:50) {
    engine5$tokenize(mixed_large)
  }
}, "tokenizer-mixed")


cat("=== Tokenizer Profiling Complete ===\n")
cat("Profiling data saved to benchmarks/profiles/*.rds\n")
cat("Load with: prof <- readRDS('benchmarks/profiles/tokenizer-*.rds'); print(prof)\n")
