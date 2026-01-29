rye_stdlib_current_env <- function() {
  for (i in 0:20) {
    frame_env <- parent.frame(i + 1)
    if (exists("env", envir = frame_env, inherits = FALSE)) {
      candidate <- get("env", envir = frame_env, inherits = FALSE)
      if (is.environment(candidate) &&
          exists(".rye_env", envir = candidate, inherits = FALSE)) {
        return(candidate)
      }
    }
    frame_names <- ls(envir = frame_env, all.names = TRUE)
    if (length(frame_names) > 0) {
      frame_values <- mget(frame_names, envir = frame_env, inherits = FALSE)
      for (value in frame_values) {
        if (is.environment(value) &&
            exists(".rye_env", envir = value, inherits = FALSE)) {
          return(value)
        }
      }
    }
  }
  fallback <- parent.frame()
  if (is.environment(fallback) &&
      exists(".rye_env", envir = fallback, inherits = FALSE)) {
    return(fallback)
  }
  globalenv()
}

#' @export
print.rye_promise <- function(x, ...) {
  cat("<promise>\n")
  invisible(x)
}
