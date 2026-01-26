#' Start the Rye REPL (Read-Eval-Print Loop)
#'
#' @export
rye_repl <- function() {
  cat("Rye REPL v0.0.1\n")
  cat("Type (quit) or press Ctrl+C to exit\n\n")

  # Create a fresh environment for REPL session
  repl_env <- new.env(parent = .GlobalEnv)

  # Load standard library
  rye_load_stdlib(repl_env)

  repeat {
    # Read
    cat("rye> ")
    input <- readline()

    if (input == "" || is.null(input)) {
      next
    }

    # Check for quit command
    if (trimws_shim(input) %in% c("(quit)", "(exit)", "quit", "exit")) { # nolint: object_usage_linter
      cat("Goodbye!\n")
      break
    }

    # Eval and Print
    tryCatch({
      exprs <- rye_read(input)
      result <- NULL
      for (expr in exprs) {
        result <- rye_eval(expr, repl_env)
      }
      # Print result
      if (!is.null(result)) {
        print(result)
      }
    }, error = function(e) {
      cat("Error: ", conditionMessage(e), "\n", sep = "")
    })
  }
}
