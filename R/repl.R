# REPL helpers (non-exported)
repl_version <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("rye")),
    error = function(...) "unknown"
  )
  paste0("Rye REPL ", version)
}

repl_history_path <- function() {
  path.expand("~/.rye_history")
}

repl_can_use_history <- function() {
  interactive() && capabilities("readline")
}

repl_load_history <- function(path) {
  if (!repl_can_use_history()) {
    return(invisible(NULL))
  }
  if (file.exists(path)) {
    try(utils::loadhistory(path), silent = TRUE)
  }
  invisible(NULL)
}

repl_save_history <- function(path) {
  if (!repl_can_use_history()) {
    return(invisible(NULL))
  }
  try(utils::savehistory(path), silent = TRUE)
  invisible(NULL)
}

repl_add_history <- function(text) {
  if (!repl_can_use_history()) {
    return(invisible(NULL))
  }
  if (rye_trimws_shim(text) == "") { # nolint: object_usage_linter
    return(invisible(NULL))
  }
  add_history <- tryCatch(
    utils::getFromNamespace("addHistory", "utils"),
    error = function(...) NULL
  )
  if (is.null(add_history)) {
    return(invisible(NULL))
  }
  try(add_history(text), silent = TRUE)
  invisible(NULL)
}

repl_is_incomplete_error <- function(e) {
  msg <- conditionMessage(e)
  grepl("Unexpected end of input|Unclosed parenthesis", msg)
}

repl_read_form <- function(input_fn = readline, prompt = "rye> ", cont_prompt = "...> ") {
  buffer <- character(0)

  repeat {
    line <- input_fn(if (length(buffer) == 0) prompt else cont_prompt)
    if (is.null(line)) {
      return(NULL)
    }
    if (length(buffer) == 0 && rye_trimws_shim(line) == "") { # nolint: object_usage_linter
      next
    }

    buffer <- c(buffer, line)
    text <- paste(buffer, collapse = "\n")

    parsed <- tryCatch(
      rye_read(text),
      error = function(e) e
    )

    if (inherits(parsed, "error")) {
      if (repl_is_incomplete_error(parsed)) {
        next
      }
      stop(parsed)
    }

    return(list(text = text, exprs = parsed))
  }
}

repl_eval_exprs <- function(exprs, env) {
  result <- NULL
  for (expr in exprs) {
    result <- rye_eval(expr, env)
  }
  result
}

repl_print_value <- function(value) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (is.call(value) || is.list(value)) {
    utils::str(value)
    return(invisible(value))
  }
  print(value)
  invisible(value)
}

#' Start the Rye REPL (Read-Eval-Print Loop)
#'
#' @export
rye_repl <- function() {
  cat(repl_version(), "\n", sep = "")
  cat("Type (quit) or press Ctrl+C to exit\n\n")

  repl_env <- cli_load_env()

  history_path <- repl_history_path()
  repl_load_history(history_path)
  on.exit(repl_save_history(history_path), add = TRUE)

  repeat {
    form <- tryCatch(
      repl_read_form(),
      error = function(e) {
        cat("Error: ", conditionMessage(e), "\n", sep = "")
        return(list(error = TRUE))
      }
    )

    if (is.null(form)) {
      cat("Goodbye!\n")
      break
    }

    if (isTRUE(form$error)) {
      next
    }

    input_text <- form$text
    if (rye_trimws_shim(input_text) %in% c("(quit)", "(exit)", "quit", "exit")) { # nolint: object_usage_linter
      cat("Goodbye!\n")
      break
    }

    repl_add_history(input_text)

    tryCatch({
      result <- repl_eval_exprs(form$exprs, repl_env)
      repl_print_value(result)
    }, error = function(e) {
      cat("Error: ", conditionMessage(e), "\n", sep = "")
    })
  }
}
