# Clean up module cache files written during tests.
# R CMD check monitors ~/.cache recursively (_R_CHECK_THINGS_IN_OTHER_DIRS_),
# and our module cache writes .code.rds files under R_user_dir("arl", "cache").
# Without cleanup, these would be flagged as detritus on CRAN.
withr::defer(
  {
    cache_dir <- file.path(tools::R_user_dir("arl", "cache"), "modules")
    if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
  },
  teardown_env()
)
