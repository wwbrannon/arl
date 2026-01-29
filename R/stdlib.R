#' Load the Rye standard library
#'
#' Loads the base Rye stdlib functions and core Rye stdlib modules from
#' the package's `inst/rye` directory.
#'
#' @details
#' The returned environment uses `baseenv()` as its parent so that core R
#' functions remain available alongside Rye helpers. Additional Rye stdlib
#' modules can be loaded via `(import ...)`.
#'
#' @param env An environment to populate. If NULL, creates a new one.
#' @return An environment containing the Rye standard library
#' @examples
#' env <- rye_load_stdlib()
#' rye_eval(rye_read('(+ 1 2)'), env)
#' @importFrom stats setNames
#' @export
rye_load_stdlib <- function(env = NULL) {
  # Create environment with baseenv() as parent if not provided
  # This gives automatic access to all R base functions
  if (is.null(env)) {
    env <- new.env(parent = baseenv())
  }

  rye_env_module_registry(env, create = TRUE)
  rye_env_macro_registry(env, create = TRUE)

  # Core helpers implemented in R
  env$apply <- rye_stdlib_apply

  # Errors and debugging
  # Error helpers provided by stdlib files
  env$`try*` <- rye_stdlib_try

  # Macro and eval helpers
  env$gensym <- gensym
  env$capture <- rye_capture
  env$`macro?` <- function(x) {
    if (is.symbol(x)) {
      is_macro(x, env = env)
    } else {
      FALSE
    }
  }
  env$macroexpand <- rye_stdlib_macroexpand
  env$`macroexpand-1` <- rye_stdlib_macroexpand_1
  env$`macroexpand-all` <- rye_stdlib_macroexpand
  env$eval <- rye_stdlib_eval
  env$`current-env` <- rye_stdlib_current_env
  env$`promise?` <- rye_stdlib_promise_p
  env$force <- rye_stdlib_force
  env$rye_read <- rye_read
  env$rye_parse <- rye_parse
  env$rye_tokenize <- rye_tokenize

  # Create r/call with closure that captures the stdlib environment
  stdlib_env <- env
  env$`r/call` <- function(fn, args = list()) {
    fn_name <- if (is.symbol(fn)) {
      as.character(fn)
    } else if (is.character(fn)) {
      fn
    } else {
      stop("r/call requires a symbol or string function name")
    }
    fn_obj <- rye_resolve_r_callable(fn_name, stdlib_env = stdlib_env, max_frames = 5)
    rye_do_call(fn_obj, rye_as_list(args))
  }
  env$`r/eval` <- rye_stdlib_r_eval

  # load the rest of the stdlib: rye code in files
  loader_path <- rye_resolve_module_path('_stdlib_loader')
  rye_eval(rye_read(sprintf('(load "%s")', loader_path))[[1]], env)

  # Return the environment
  # All R base functions (+, -, *, /, <, >, print, etc.) are automatically
  # available via the parent environment chain
  env
}

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
  rye_do_call(fn, args)
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

rye_stdlib_macroexpand <- function(expr, env = parent.frame()) {
  rye_macroexpand(expr, env)
}

rye_stdlib_macroexpand_1 <- function(expr, env = parent.frame()) {
  rye_macroexpand_1(expr, env)
}

rye_stdlib_eval <- function(expr, env = parent.frame()) {
  rye_eval(expr, env)
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
  expr <- rye_hygiene_unwrap(expr)
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
  rye_do_call(fn, rye_as_list(args))
}

rye_stdlib_promise_p <- function(x) {
  rye_promise_p(x)
}

rye_stdlib_force <- function(x) {
  rye_promise_force(x)
}

attr(rye_stdlib_apply, "rye_doc") <- list(
  description = "Apply fn to the elements of lst as arguments."
)
attr(rye_stdlib_try, "rye_doc") <- list(
  description = "Evaluate thunk with error/finally handlers."
)
attr(rye_stdlib_macroexpand, "rye_doc") <- list(
  description = "Recursively expand macros in expr."
)
attr(rye_stdlib_macroexpand_1, "rye_doc") <- list(
  description = "Expand a single macro layer in expr."
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
