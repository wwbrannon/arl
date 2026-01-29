rye_as_list <- function(x) {
  if (is.call(x)) {
    return(as.list(x))
  }
  if (is.list(x)) {
    return(x)
  }
  if (length(x) == 0) {
    return(list())
  }
  as.list(x)
}

rye_stdlib_apply <- function(fn, args) {
  args <- rye_as_list(args)
  if (length(args) > 2 &&
        (identical(fn, base::`+`) || identical(fn, base::`*`) ||
           identical(fn, base::`-`) || identical(fn, base::`/`))) {
    return(Reduce(fn, args))
  }
  do.call(fn, args)
}

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

rye_stdlib_eval <- function(expr, env = parent.frame()) {
  engine <- get0(".rye_engine", envir = env, inherits = TRUE)
  if (inherits(engine, "RyeEngine")) {
    return(engine$eval(expr, env))
  }
  stop("rye_stdlib_eval requires a RyeEngine-backed environment")
}

rye_resolve_r_callable <- function(fn_name, stdlib_env = NULL, max_frames = 10) {
  if (!is.character(fn_name) || length(fn_name) != 1 || !nzchar(fn_name)) {
    stop("r/call requires a symbol or string function name")
  }
  if (!is.null(stdlib_env)) {
    fn_obj <- get0(fn_name, envir = stdlib_env, inherits = FALSE, ifnotfound = NULL)
    if (!is.null(fn_obj) && is.function(fn_obj)) {
      return(fn_obj)
    }
  }
  for (i in 0:max_frames) {
    tryCatch({
      frame_env <- parent.frame(i + 1)
      fn_obj <- get0(fn_name, envir = frame_env, inherits = TRUE, ifnotfound = NULL)
      if (!is.null(fn_obj) && is.function(fn_obj)) {
        return(fn_obj)
      }
    }, error = function(e) NULL)
  }
  get(fn_name, envir = baseenv())
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

rye_stdlib_r_eval <- function(expr, env = NULL) {
  if (is.null(env)) {
    env <- rye_stdlib_current_env()
  }
  engine <- get0(".rye_engine", envir = env, inherits = TRUE)
  if (inherits(engine, "RyeEngine")) {
    expr <- engine$macro_expander$hygiene_unwrap(expr)
  }
  saved <- list()
  if (is.call(expr) && length(expr) > 0) {
    op <- expr[[1]]
    if (is.symbol(op)) {
      op_name <- as.character(op)
      if (op_name == "while" && exists("while", envir = env, inherits = FALSE)) {
        saved[["while"]] <- get("while", envir = env, inherits = FALSE)
        rm("while", envir = env, inherits = FALSE)
      }
      if (op_name == "for" && exists("for", envir = env, inherits = FALSE)) {
        saved[["for"]] <- get("for", envir = env, inherits = FALSE)
        rm("for", envir = env, inherits = FALSE)
      }
    }
  }
  on.exit({
    if (!is.null(saved[["while"]])) {
      assign("while", saved[["while"]], envir = env)
    }
    if (!is.null(saved[["for"]])) {
      assign("for", saved[["for"]], envir = env)
    }
  }, add = TRUE)
  eval(expr, env)
}
attr(rye_stdlib_r_eval, "rye_no_quote") <- TRUE

rye_stdlib_r_call <- function(fn, args = list()) {
  fn_name <- if (is.symbol(fn)) {
    as.character(fn)
  } else if (is.character(fn)) {
    fn
  } else {
    stop("r/call requires a symbol or string function name")
  }
  fn <- rye_resolve_r_callable(fn_name, stdlib_env = NULL, max_frames = 10)
  do.call(fn, rye_as_list(args))
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

attr(rye_stdlib_apply, "rye_doc") <- list(
  description = "Apply fn to the elements of lst as arguments."
)
attr(rye_stdlib_try, "rye_doc") <- list(
  description = "Evaluate thunk with error/finally handlers."
)
attr(rye_stdlib_eval, "rye_doc") <- list(
  description = "Evaluate expr in the current environment."
)
attr(rye_stdlib_promise_p, "rye_doc") <- list(
  description = "Return TRUE if x is a promise."
)
attr(rye_stdlib_force, "rye_doc") <- list(
  description = "Force a promise or return x unchanged."
)
attr(rye_stdlib_r_call, "rye_doc") <- list(
  description = "Call an R function with list arguments."
)
