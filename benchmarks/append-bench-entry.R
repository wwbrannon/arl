# Append a new benchmark entry to data.js
# Usage: Rscript benchmarks/append-bench-entry.R <data.js> <new-entry-file> <output.js>

args <- commandArgs(TRUE)
lines <- readLines(args[1], warn = FALSE)
text <- paste(lines, collapse = "\n")

# Strip JS prefix
json_text <- sub("^window\\.BENCHMARK_DATA = ", "", text)
new_entry <- paste(readLines(args[2], warn = FALSE), collapse = "\n")

# Find the last occurrence of the closing bracket for the Benchmark array.
# The structure is: ... "Benchmark": [ {entry}, {entry} ] } }
# We insert before the final "]\n  }\n}" sequence.
pattern <- "\\]\\s*\\}\\s*\\}\\s*$"
m <- regexpr(pattern, json_text)
if (m == -1) stop("Could not find end of Benchmark array in data.js")

before <- substr(json_text, 1, m - 1)
after <- substr(json_text, m, nchar(json_text))

updated <- paste0(before, ",\n", new_entry, "\n", after)

# Update lastUpdate timestamp
updated <- sub(
  '"lastUpdate": *[0-9.]+',
  sprintf('"lastUpdate": %.2f', as.numeric(Sys.time()) * 1000),
  updated
)

writeLines(paste0("window.BENCHMARK_DATA = ", updated), args[3])
