#!/usr/bin/env Rscript

## Build a package tarball with pre-built vignettes.
##
## This replaces the simple `devtools::build(path='.')` call.
## It works on a temp copy so the source tree is never modified.
##
## Usage:  Rscript tools/build-pkg.R

src_dir <- normalizePath(".", mustWork = TRUE)

## 1. Copy package source to a temp directory
tmp <- tempfile("arl-build-")
dir.create(tmp)
pkg_dir <- file.path(tmp, basename(src_dir))

message("Copying source to ", pkg_dir, " ...")
file.copy(src_dir, tmp, recursive = TRUE)

## Remove .git and other large dirs that aren't needed
unlink(file.path(pkg_dir, ".git"), recursive = TRUE)

## 2. Pre-build vignettes in the temp copy
source(file.path(src_dir, "tools", "build", "prebuild-vignettes.R"))
prebuild_vignettes(pkg_dir)

## 3. Build the tarball from the temp copy
message("Running R CMD build ...")
tarball <- devtools::build(pkg_dir, path = tmp, vignettes = TRUE)

## 4. Copy tarball back to original working directory
dest <- file.path(src_dir, basename(tarball))
file.copy(tarball, dest, overwrite = TRUE)
message("Tarball: ", dest)

## Cleanup
unlink(tmp, recursive = TRUE)
