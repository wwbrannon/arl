# Extracted from test-examples.R:89

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rye", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
if (!nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
  withr::local_envvar(NOT_CRAN = "true")
}
run_example <- function(example_name) {
  example_path <- system.file("examples", example_name, package = "rye")
  if (!nzchar(example_path)) {
    example_path <- testthat::test_path("../../inst/examples", example_name)
  }
  testthat::skip_if_not(file.exists(example_path), "Example file not found")

  env <- new.env()
  engine <- make_engine()
  stdlib_env(engine, env)

  out_dir <- tempfile("rye-example-")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  old_env <- Sys.getenv("RYE_OUTPUT_DIR", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("RYE_OUTPUT_DIR")
    } else {
      Sys.setenv(RYE_OUTPUT_DIR = old_env)
    }
  }, add = TRUE)
  Sys.setenv(RYE_OUTPUT_DIR = out_dir)

  output <- capture.output(engine$load_file_in_env(example_path, env))
  list(env = env, output = output, out_dir = out_dir, path = example_path)
}

# test -------------------------------------------------------------------------
result <- run_example("log-parser.rye")
