#!/usr/bin/env Rscript

## Submit the package to remote check services (win-builder, mac-builder)
## using a temp copy with pre-built vignettes, so the exact same tarball
## layout that CRAN receives is what gets tested.
##
## Usage:  Rscript tools/build/check-remote.R win_devel win_release
##         Rscript tools/build/check-remote.R mac_release

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Specify one or more targets: win_devel, win_release, mac_release")
}

src_dir <- normalizePath(".", mustWork = TRUE)

## Prepare a temp copy with pre-built vignettes
tmp <- tempfile("arl-remote-check-")
dir.create(tmp)
pkg_dir <- file.path(tmp, basename(src_dir))

message("Copying source to ", pkg_dir, " ...")
file.copy(src_dir, tmp, recursive = TRUE)
unlink(file.path(pkg_dir, ".git"), recursive = TRUE)

source(file.path(src_dir, "tools", "build", "prebuild-vignettes.R"))
prebuild_vignettes(pkg_dir)

## Submit to each requested service
dispatch <- list(
  win_devel   = function(pkg) devtools::check_win_devel(pkg = pkg),
  win_release = function(pkg) devtools::check_win_release(pkg = pkg),
  mac_release = function(pkg) devtools::check_mac_release(pkg = pkg)
)

for (target in args) {
  fn <- dispatch[[target]]
  if (is.null(fn)) stop("Unknown target: ", target)
  message("Submitting to ", target, " ...")
  fn(pkg_dir)
}

unlink(tmp, recursive = TRUE)
