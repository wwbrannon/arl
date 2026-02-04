# Native test infrastructure for running .rye test files
# Tests are functions named `test-*` in .rye files

#' Run a single native test file
#'
#' Loads a .rye file and executes all functions named test-*
#'
#' @param path Path to the .rye test file
#' @param engine A Rye engine instance
#' @param env Environment to run tests in
#' @return List of test results
run_native_test_file <- function(path, engine, env) {
  # Load the test file
  tryCatch({
    exprs <- engine$read(sprintf('(load "%s")', path))
    engine$eval_in_env(exprs[[1]], env)
  }, error = function(e) {
    stop(sprintf("Failed to load test file %s: %s", path, e$message))
  })

  # Get all symbols in the environment
  all_names <- ls(env, all.names = TRUE)

  # Find all test-* functions
  test_names <- grep("^test-", all_names, value = TRUE)

  if (length(test_names) == 0) {
    warning(sprintf("No test functions found in %s", path))
    return(list())
  }

  results <- list()

  # Run each test function
  for (test_name in test_names) {
    # Wrap test_that in tryCatch to prevent abort on failure
    tryCatch({
      test_that(sprintf("%s: %s", basename(path), test_name), {
        # Call the test function
        tryCatch({
          exprs <- engine$read(sprintf('(%s)', test_name))
          engine$eval_in_env(exprs[[1]], env)
          # If we get here, the test passed
          expect_true(TRUE)
        }, error = function(e) {
          # Test failed with an error
          expect_true(FALSE, info = sprintf("Test %s failed: %s", test_name, e$message))
        })
      })
    }, error = function(e) {
      # Silently catch testthat abort errors and continue
      NULL
    })
  }

  invisible(results)
}

#' Discover and run all native tests in a directory
#'
#' Finds all .rye files in the given directory and runs their tests
#'
#' @param dir Directory to search for .rye test files (default: tests/native)
#' @return NULL (invisibly)
run_native_tests <- function(dir = "tests/native") {
  # Find all .rye files
  if (!dir.exists(dir)) {
    warning(sprintf("Native test directory %s does not exist", dir))
    return(invisible(NULL))
  }

  test_files <- list.files(dir, pattern = "\\.rye$", full.names = TRUE, recursive = TRUE)

  if (length(test_files) == 0) {
    message(sprintf("No .rye test files found in %s", dir))
    return(invisible(NULL))
  }

  # Create a fresh engine for running tests
  engine <- RyeEngine$new()
  env <- engine$env$env

  # Load stdlib modules needed for tests (assert needs core, predicates, strings, dict)
  for (module in c("control", "binding", "strings", "math", "predicates", "list", "higher-order", "sequences", "looping", "dict", "core", "assert")) {
    exprs <- engine$read(sprintf('(load "%s")', module))
    engine$eval_in_env(exprs[[1]], env)
  }

  # Run each test file
  for (test_file in test_files) {
    run_native_test_file(test_file, engine, env)
  }

  invisible(NULL)
}
