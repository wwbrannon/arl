# Tests that all {arl} code chunks in generated vignettes evaluate without error.
#
# This catches stale references, broken examples, and bad asserts in all
# vignettes (both auto-generated lang-*.Rmd and hand-written ones).  The
# Makefile ensures `make test` regenerates lang docs before running tests.

# Find package root (DESCRIPTION lives there).
pkg_root <- function() {
  wd <- getwd()
  for (root in c(wd, file.path(wd, ".."), file.path(wd, "..", ".."))) {
    if (file.exists(file.path(root, "DESCRIPTION")))
      return(normalizePath(root, winslash = "/"))
  }
  wd
}

# Extract {arl} chunks from an Rmd file via knitr::purl(), then parse
# chunk options from the #| comments that purl preserves.
extract_arl_chunks <- function(path) {
  lines <- readLines(path, warn = FALSE)
  chunks <- list()
  i <- 1L
  while (i <= length(lines)) {
    m <- regmatches(lines[i], regexec("^```\\{arl([^}]*)\\}", lines[i]))[[1]]
    if (length(m) == 2L) {
      opts_str <- m[2]
      eval <- !grepl("eval\\s*=\\s*FALSE", opts_str)
      error_ok <- grepl("error\\s*=\\s*TRUE", opts_str)
      label <- sub(".*label\\s*=\\s*[\"']([^\"']+)[\"'].*", "\\1",
                    opts_str, perl = TRUE)
      if (label == opts_str) label <- sprintf("chunk-%d", length(chunks) + 1L)
      code_lines <- character(0)
      i <- i + 1L
      while (i <= length(lines) && lines[i] != "```") {
        code_lines <- c(code_lines, lines[i])
        i <- i + 1L
      }
      chunks <- c(chunks, list(list(
        code = paste(code_lines, collapse = "\n"),
        eval = eval,
        error_ok = error_ok,
        label = label
      )))
    }
    i <- i + 1L
  }
  chunks
}

# Test all vignettes that may contain {arl} chunks
vignette_dir <- file.path(pkg_root(), "vignettes")
all_rmds <- list.files(vignette_dir, pattern = "\\.Rmd$", full.names = TRUE)

skip_if(length(all_rmds) == 0, "No .Rmd vignettes found")

# Use a single engine with full stdlib loaded (mirrors knitr setup)
arl:::reset_arl_engine()
engine <- arl:::get_arl_engine()

for (rmd_path in all_rmds) {
  rmd_name <- basename(rmd_path)
  chunks <- extract_arl_chunks(rmd_path)

  for (chunk in chunks) {
    if (!chunk$eval || !nzchar(trimws(chunk$code))) next
    # Chunks with error=TRUE intentionally demonstrate errors; skip them
    if (chunk$error_ok) next

    test_that(sprintf("%s: %s evaluates without error", rmd_name, chunk$label), {
      expect_no_error(
        arl:::evaluate_arl_code(engine, chunk$code)
      )
    })
  }
}
