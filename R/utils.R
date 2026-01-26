rye_trimws_compat <- function(x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") {
  which <- match.arg(which)
  left_pattern <- paste0("^", whitespace, "+")
  right_pattern <- paste0(whitespace, "+$")
  if (which == "left") {
    return(sub(left_pattern, "", x))
  }
  if (which == "right") {
    return(sub(right_pattern, "", x))
  }
  sub(left_pattern, "", sub(right_pattern, "", x))
}
rye_trimws_shim <- function(x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") {
  if (exists("trimws", mode = "function", inherits = TRUE)) {
    return(trimws(x, which = which, whitespace = whitespace))
  }
  rye_trimws_compat(x, which = which, whitespace = whitespace)
}

rye_eval_exprs <- function(exprs, env) {
  result <- NULL
  for (expr in exprs) {
    result <- rye_eval(expr, env)
  }
  result
}

rye_eval_text <- function(text, env) {
  exprs <- rye_read(text)
  rye_eval_exprs(exprs, env)
}

rye_quote_arg <- function(value, quote_symbols = TRUE) {
  if (is.call(value) || (quote_symbols && is.symbol(value))) {
    return(as.call(list(as.symbol("quote"), value)))
  }
  value
}

rye_do_call <- function(fn, args) {
  quote_symbols <- !identical(fn, base::`$`) &&
    !identical(fn, base::`[`) &&
    !identical(fn, base::`[[`)
  args <- lapply(args, rye_quote_arg, quote_symbols = quote_symbols)
  do.call(fn, args)
}

rye_assign <- function(name, value, env) {
  target <- env
  repeat {
    if (exists(name, envir = target, inherits = FALSE)) {
      assign(name, value, envir = target)
      return(invisible(NULL))
    }
    parent_env <- parent.env(target)
    if (!exists(".rye_env", envir = parent_env, inherits = FALSE)) {
      break
    }
    target <- parent_env
  }
  assign(name, value, envir = env)
  invisible(NULL)
}

rye_resolve_stdlib_path <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    return(NULL)
  }
  dir_path <- system.file("rye", package = "rye")
  if (identical(dir_path, "")) {
    return(NULL)
  }
  candidates <- c(
    file.path(dir_path, name),
    file.path(dir_path, paste0(name, ".rye"))
  )
  for (path in candidates) {
    if (file.exists(path)) {
      return(path)
    }
  }
  NULL
}

#' Load and evaluate a Rye source file
#'
#' @param path Path to a Rye source file
#' @param env Environment in which to evaluate the file
#' @return The result of the final expression in the file
#' @export
rye_load_file <- function(path, env = parent.frame()) {
  if (!is.character(path) || length(path) != 1) {
    stop("load requires a single file path string")
  }
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path))
  }
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  rye_eval_text(text, env)
}
