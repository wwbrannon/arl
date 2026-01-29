rye_stdlib_try <- function(thunk, error_handler = NULL, finally_handler = NULL) {
  if (!is.function(thunk)) {
    stop("try* expects a function as first argument")
  }
  if (!is.null(error_handler) && !is.function(error_handler)) {
    stop("try* error handler must be a function")
  }
  if (!is.null(finally_handler) && !is.function(finally_handler)) {
    stop("try* finally handler must be a function")
  }
  if (!is.null(finally_handler)) {
    on.exit(finally_handler(), add = TRUE)
  }
  if (is.null(error_handler)) {
    return(thunk())
  }
  tryCatch(
    thunk(),
    error = function(e) {
      error_handler(e)
    }
  )
}

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

rye_stdlib_promise_p <- function(x) {
  is.environment(x) && inherits(x, "rye_promise")
}

rye_stdlib_force <- function(x) {
  if (!rye_stdlib_promise_p(x)) {
    return(x)
  }
  get(rye_promise_value_key, envir = x, inherits = FALSE)
}

#' @export
print.rye_promise <- function(x, ...) {
  cat("<promise>\n")
  invisible(x)
}

attr(rye_stdlib_try, "rye_doc") <- list(
  description = "Evaluate thunk with error/finally handlers."
)
attr(rye_stdlib_promise_p, "rye_doc") <- list(
  description = "Return TRUE if x is a promise."
)
attr(rye_stdlib_force, "rye_doc") <- list(
  description = "Force a promise or return x unchanged."
)
