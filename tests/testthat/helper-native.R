# Native test infrastructure for running .arl test files
# Tests are functions named `test-*` in .arl files

#' Run a single native test file
#'
#' Loads a .arl file and executes all functions named test-*
#'
#' @param path Path to the .arl test file
#' @param engine A Arl engine instance
#' @param env Environment to run tests in
#' @return List of test results
run_native_test_file <- function(path, engine, env) {
  # Load the test file
  tryCatch({
    exprs <- engine$read(sprintf('(load "%s")', path))
    engine$eval(exprs[[1]], env = env)
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
      test_that(sprintf("%s: %s", basename(path), test_name), { # nolint: object_usage_linter.
        # Call the test function
        tryCatch({
          exprs <- engine$read(sprintf('(%s)', test_name))
          engine$eval(exprs[[1]], env = env)
          # If we get here, the test passed
          expect_true(TRUE) # nolint: object_usage_linter.
        }, skip = function(e) {
          # Test was skipped - re-signal the skip condition
          testthat::skip(e$message)
        }, error = function(e) {
          # Test failed with an error
          expect_true(FALSE, info = sprintf("Test %s failed: %s", test_name, e$message)) # nolint: object_usage_linter.
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
#' Finds all .arl files in the given directory and runs their tests
#'
#' @param dir Directory to search for .arl test files (default: tests/native)
#' @return NULL (invisibly)
run_native_tests <- function(dir = "tests/native") {
  # Find all .arl files
  if (!dir.exists(dir)) {
    warning(sprintf("Native test directory %s does not exist", dir))
    return(invisible(NULL))
  }

  test_files <- list.files(dir, pattern = "\\.arl$", full.names = TRUE, recursive = TRUE)

  if (length(test_files) == 0) {
    message(sprintf("No .arl test files found in %s", dir))
    return(invisible(NULL))
  }

  # Run each test file with a fresh engine for complete isolation
  for (test_file in test_files) {
    # Create a fresh engine for this test file
    # This ensures complete test isolation - no state leaks between files
    engine <- make_engine() # nolint: object_usage_linter.
    env <- engine$get_env()

    # Load test-specific helpers (like skip function)
    native_helper <- system.file("tests", "testthat", "helper-native.arl", package = "arl")
    if (native_helper == "") {
      # During development, file is not installed - use relative path
      native_helper <- file.path("tests", "testthat", "helper-native.arl")
    }
    if (file.exists(native_helper)) {
      exprs <- engine$read(sprintf('(load "%s")', native_helper))
      engine$eval(exprs[[1]], env = env)
    }

    # Run this test file in its isolated environment
    run_native_test_file(test_file, engine, env = env)
  }

  invisible(NULL)
}
