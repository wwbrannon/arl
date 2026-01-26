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

#' Load the Rye prelude into an environment
#'
#' @param env Environment in which to evaluate the prelude
#' @return The result of the final expression in the prelude
#' @export
rye_load_prelude <- function(env = parent.frame()) {
  path <- system.file("rye/prelude.rye", package = "rye")
  if (identical(path, "")) {
    stop("Rye prelude not found in installed package")
  }
  rye_load_file(path, env)
}
