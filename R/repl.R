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

repl_history_state <- new.env(parent = emptyenv())
repl_history_state$enabled <- FALSE
repl_history_state$path <- NULL
repl_history_state$snapshot <- NULL

repl_can_use_history <- function() {
  override <- getOption("rye.repl_can_use_history_override", NULL)
  if (!is.null(override)) {
    if (is.function(override)) {
      return(isTRUE(override()))
    }
    return(isTRUE(override))
  }
  isTRUE(interactive()) && isTRUE(capabilities("readline"))
}

repl_load_history <- function(path) {
  if (!repl_can_use_history()) {
    return(invisible(FALSE))
  }
  snapshot <- tempfile("rye_rhistory_")
  saved <- tryCatch({
    utils::savehistory(snapshot)
    TRUE
  }, error = function(...) FALSE)
  if (!saved) {
    return(invisible(FALSE))
  }
  if (file.exists(path)) {
    try(utils::loadhistory(path), silent = TRUE)
  }
  repl_history_state$enabled <- TRUE
  repl_history_state$path <- path
  repl_history_state$snapshot <- snapshot
  invisible(TRUE)
}

repl_save_history <- function() {
  if (!repl_can_use_history()) {
    return(invisible(NULL))
  }
  if (!isTRUE(repl_history_state$enabled)) {
    return(invisible(NULL))
  }
  try(utils::savehistory(repl_history_state$path), silent = TRUE)
  if (!is.null(repl_history_state$snapshot)) {
    try(utils::loadhistory(repl_history_state$snapshot), silent = TRUE)
  }
  repl_history_state$enabled <- FALSE
  repl_history_state$path <- NULL
  repl_history_state$snapshot <- NULL
  invisible(NULL)
}

repl_add_history <- function(text) {
  if (!repl_can_use_history()) {
    return(invisible(NULL))
  }
  if (!isTRUE(repl_history_state$enabled)) {
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

repl_input_line <- function(prompt) {
  if (isTRUE(interactive()) && isTRUE(capabilities("readline"))) {
    return(readline(prompt))
  }
  cat(prompt)
  utils::flush.console()
  line <- readLines("stdin", n = 1, warn = FALSE)
  if (length(line) == 0) {
    return(NULL)
  }
  line
}

repl_read_form <- function(input_fn = repl_input_line, prompt = "rye> ", cont_prompt = "...> ") {
  override <- getOption("rye.repl_read_form_override", NULL)
  if (!is.null(override)) {
    if (is.function(override)) {
      return(override(input_fn = input_fn, prompt = prompt, cont_prompt = cont_prompt))
    }
    return(override)
  }
  # NOTE: readline provides line editing/history, but EOF (Ctrl-D) is not
  # distinguishable from an empty line, so we cannot reliably exit on Ctrl-D.
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
  rye_eval_exprs(exprs, env)
}

repl_eval_and_print_exprs <- function(exprs, env) {
  result <- NULL
  for (expr in exprs) {
    result <- rye_eval(expr, env)
    repl_print_value(result)
  }
  invisible(result)
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
#' Launches an interactive Rye session with command history.
#'
#' @examples
#' if (interactive()) {
#'   rye_repl()
#' }
#' @export
rye_repl <- function() {
  cat(repl_version(), "\n", sep = "")
  cat("Type (quit) or press Ctrl+C to exit\n")
  cat("Builtin readline support:", ifelse(isTRUE(capabilities("readline")), "yes", "no"), "\n\n")

  repl_env <- cli_load_env()

  history_path <- repl_history_path()
  repl_load_history(history_path)
  on.exit(repl_save_history(), add = TRUE)

  repeat {
    form <- tryCatch(
      repl_read_form(),
      error = function(e) {
        cat("Error: ", conditionMessage(e), "\n", sep = "")
        list(error = TRUE)
      }
    )

    if (is.null(form)) {
      break
    }

    if (isTRUE(form$error)) {
      next
    }

    input_text <- form$text
    if (rye_trimws_shim(input_text) %in% c("(quit)", "(exit)", "quit", "exit")) { # nolint: object_usage_linter
      break
    }

    repl_add_history(input_text)

    tryCatch({
      repl_eval_and_print_exprs(form$exprs, repl_env)
    }, error = function(e) {
      cat("Error: ", conditionMessage(e), "\n", sep = "")
    })
  }
}
