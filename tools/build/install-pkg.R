#!/usr/bin/env Rscript

## Install the package with pre-built vignettes.
##
## This replaces the simple `devtools::install()` call.
## It works on a temp copy so the source tree is never modified.
##
## Usage:  Rscript tools/install-pkg.R

src_dir <- normalizePath(".", mustWork = TRUE)

## 1. Copy package source to a temp directory
tmp <- tempfile("arl-install-")
dir.create(tmp)
pkg_dir <- file.path(tmp, basename(src_dir))

message("Copying source to ", pkg_dir, " ...")
file.copy(src_dir, tmp, recursive = TRUE)

## Remove .git to save time/space
unlink(file.path(pkg_dir, ".git"), recursive = TRUE)

## 2. Pre-build vignettes in the temp copy
source(file.path(src_dir, "tools", "build", "prebuild-vignettes.R"))
prebuild_vignettes(pkg_dir)

## 3. Install from the temp copy (inst/doc/ is now populated)
message("Installing ...")
devtools::install(pkg_dir, upgrade = "never")

## Cleanup
unlink(tmp, recursive = TRUE)
