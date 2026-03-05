#!/usr/bin/env Rscript

## Render vignettes and set up .Rmd.orig + stub layout in a given
## package directory.  Called by both build-pkg.R and install-pkg.R.
##
## Usage:
##   source("tools/prebuild-vignettes.R")
##   prebuild_vignettes(pkg_dir)

prebuild_vignettes <- function(pkg_dir) {
  pkg_dir <- normalizePath(pkg_dir, mustWork = TRUE)
  vig_dir <- file.path(pkg_dir, "vignettes")

  if (!dir.exists(vig_dir)) {
    stop("No vignettes/ directory found in ", pkg_dir)
  }

  ## Load the package so arl::arl_html_vignette and the knitr engine
  ## are available during rendering.

  devtools::load_all(pkg_dir, quiet = TRUE)

  ## Find all top-level .Rmd files (skip articles/ -- they are
  ## excluded from the tarball via .Rbuildignore).
  rmds <- list.files(vig_dir, pattern = "\\.Rmd$", full.names = TRUE)

  ## benchmarks.Rmd needs benchmarks/results/data.js -- make sure it
  ## is findable from the vignettes/ working directory.
  ## Use a directory junction on Windows (symlinks need elevated privileges)
  ## and a symlink elsewhere.
  bench_src <- file.path(pkg_dir, "benchmarks", "results", "data.js")
  bench_link <- file.path(vig_dir, "benchmarks")
  if (file.exists(bench_src) && !file.exists(bench_link)) {
    if (.Platform$OS.type == "windows") {
      shell(sprintf('mklink /J "%s" "%s"',
                    normalizePath(bench_link, mustWork = FALSE),
                    normalizePath(file.path(pkg_dir, "benchmarks"))),
            intern = TRUE)
    } else {
      file.symlink(
        file.path(pkg_dir, "benchmarks"),
        bench_link
      )
    }
  }

  message("Rendering ", length(rmds), " vignettes...")

  for (rmd in rmds) {
    name <- tools::file_path_sans_ext(basename(rmd))
    message("  ", name, " ...", appendLF = FALSE)
    rmarkdown::render(
      rmd,
      output_dir = vig_dir,
      quiet = TRUE,
      envir = new.env(parent = globalenv())
    )
    message(" done")
  }

  ## Clean up symlink/junction if we created it
  if (file.exists(bench_link)) {
    if (.Platform$OS.type == "windows") {
      shell(sprintf('rmdir "%s"', normalizePath(bench_link)), intern = TRUE)
    } else if (is.symlink(bench_link)) {
      unlink(bench_link)
    }
  }

  ## Now set up the .Rmd.orig + stub pattern
  for (rmd in rmds) {
    name <- tools::file_path_sans_ext(basename(rmd))
    orig <- paste0(rmd, ".orig")

    ## Read the original YAML header to extract title and VignetteIndexEntry
    lines <- readLines(rmd, warn = FALSE)
    yaml_end <- which(lines == "---")[2]
    yaml_block <- lines[1:yaml_end]

    title <- sub("^title:\\s*[\"']?(.*?)[\"']?\\s*$", "\\1",
                 grep("^title:", yaml_block, value = TRUE)[1])

    ## Extract VignetteIndexEntry
    vie_line <- grep("VignetteIndexEntry", yaml_block, value = TRUE)[1]
    vie <- sub(".*VignetteIndexEntry\\{(.*)\\}.*", "\\1", vie_line)

    ## Rename original -> .Rmd.orig
    file.rename(rmd, orig)

    ## Write stub .Rmd
    stub <- c(
      "---",
      sprintf('title: "%s"', title),
      "output: rmarkdown::html_vignette",
      "vignette: >",
      sprintf("  %%\\VignetteIndexEntry{%s}", vie),
      "  %\\VignetteEngine{knitr::rmarkdown}",
      "  %\\VignetteEncoding{UTF-8}",
      "---",
      "",
      sprintf("Source and arl chunks are in `%s.Rmd.orig`.", name)
    )
    writeLines(stub, rmd)
  }

  ## Create inst/doc/ with the rendered HTML, stubs, and .Rmd.orig files
  doc_dir <- file.path(pkg_dir, "inst", "doc")
  dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

  htmls <- list.files(vig_dir, pattern = "\\.html$", full.names = TRUE)
  stubs <- list.files(vig_dir, pattern = "\\.Rmd$", full.names = TRUE)
  origs <- list.files(vig_dir, pattern = "\\.Rmd\\.orig$", full.names = TRUE)

  file.copy(c(htmls, stubs, origs), doc_dir, overwrite = TRUE)

  message("Pre-built ", length(htmls), " vignettes into inst/doc/")
  invisible(doc_dir)
}

## Helper: base R doesn't export is.symlink
is.symlink <- function(path) {
  info <- file.info(path)
  !is.na(info$isdir) && nzchar(Sys.readlink(path))
}
