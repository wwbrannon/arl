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
#' env$`+`(1, 2)
#' @importFrom stats setNames
#' @export
rye_load_stdlib <- function(env = NULL) {
  env <- rye_load_stdlib_base(env)
  rye_load_stdlib_files(env)
  env
}

rye_load_stdlib_base <- function(env = NULL) {
  # Create environment with baseenv() as parent if not provided
  # This gives automatic access to all R base functions
  if (is.null(env)) {
    env <- new.env(parent = baseenv())
  }
  rye_env_module_registry(env, create = TRUE)
  rye_env_macro_registry(env, create = TRUE)
  # Core helpers implemented in R
  env$call <- rye_stdlib_call
  env$apply <- rye_stdlib_apply
  env$values <- rye_stdlib_values
  env$`values?` <- rye_values_p
  env$`call-with-values` <- rye_stdlib_call_with_values
  env$`call/cc` <- rye_make_builtin_callcc()
  env$`call-with-current-continuation` <- env$`call/cc`

  # Output
  env$display <- rye_stdlib_display
  env$println <- rye_stdlib_display
  env$str <- rye_stdlib_str

  # Errors and debugging
  # Error helpers provided by stdlib files
  env$trace <- rye_stdlib_trace
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
    # First try the stdlib environment (where wrappers are)
    fn_obj <- get0(fn_name, envir = stdlib_env, inherits = FALSE, ifnotfound = NULL)
    if (!is.null(fn_obj) && !is.function(fn_obj)) fn_obj <- NULL
    # Then try parent frames
    if (is.null(fn_obj)) {
      for (i in 0:5) {
        tryCatch({
          frame_env <- parent.frame(i + 1)
          fn_obj <- get0(fn_name, envir = frame_env, inherits = TRUE, ifnotfound = NULL)
          if (!is.null(fn_obj) && is.function(fn_obj)) break
          fn_obj <- NULL
        }, error = function(e) NULL)
      }
    }
    # Fall back to baseenv()
    if (is.null(fn_obj)) {
      fn_obj <- get(fn_name, envir = baseenv())
    }
    rye_do_call(fn_obj, rye_as_list(args))
  }

  # Return the environment
  # All R base functions (+, -, *, /, <, >, print, etc.) are automatically
  # available via the parent environment chain
  env
}

rye_load_stdlib_files <- function(env = parent.frame()) {
  base_modules <- c(
    "stdlib-core",
    "predicates",
    "list-core",
    "higher-order",
    "sequences",
    "strings",
    "display",
    "io",
    "dict",
    "set"
  )

  for (module_name in base_modules) {
    rye_module_unregister(module_name, registry_env = env)
  }

  result <- NULL
  for (module_name in base_modules) {
    exprs <- rye_read(sprintf("(import %s)", module_name))
    result <- rye_eval(exprs[[1]], env)
  }
  result
}


rye_stdlib_display <- function(x) {
  env <- parent.frame()
  cat(rye_env_format_value(env, x), "\n", sep = "")
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

rye_stdlib_call <- function(lst) {
  if (is.call(lst)) {
    lst
  } else {
    as.call(rye_as_list(lst))
  }
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

rye_stdlib_values <- function(...) {
  rye_values_new(list(...))
}

rye_stdlib_call_with_values <- function(producer, consumer) {
  if (!is.function(producer)) {
    stop("call-with-values expects a function as the producer")
  }
  if (!is.function(consumer)) {
    stop("call-with-values expects a function as the consumer")
  }
  produced <- producer()
  args <- rye_values_list(produced)
  rye_do_call(consumer, args)
}


rye_stdlib_str <- function(...) {
  args <- list(...)
  env <- parent.frame()
  formatted <- lapply(args, function(arg) rye_env_format_value(env, arg))
  do.call(paste0, formatted)
}


rye_stdlib_trace <- function(x, label = NULL) {
  if (!is.null(label)) {
    cat(rye_env_format_value(parent.frame(), label), ": ", sep = "")
  }
  env <- parent.frame()
  cat(rye_env_format_value(env, x), "\n", sep = "")
  x
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

rye_stdlib_macro_p <- function(x) {
  if (is.symbol(x)) {
    is_macro(x, env = parent.frame())
  } else {
    FALSE
  }
}

rye_stdlib_macroexpand <- function(expr, env = parent.frame()) {
  rye_macroexpand(expr, env)
}

rye_stdlib_macroexpand_1 <- function(expr, env = parent.frame()) {
  if (!is.call(expr) || length(expr) == 0) {
    return(expr)
  }
  op <- expr[[1]]
  if (is.symbol(op) && is_macro(op, env = env)) {
    macro_fn <- get_macro(op, env = env)
    args <- as.list(expr[-1])
    expanded <- do.call(macro_fn, args)
    expanded <- rye_hygienize(expanded)
    expanded <- rye_hygiene_unwrap(expanded)
    expanded <- rye_src_inherit(expanded, expr)
    return(expanded)
  }
  expr
}

rye_stdlib_eval <- function(expr, env = parent.frame()) {
  rye_eval(expr, env)
}


rye_stdlib_r_call <- function(fn, args = list()) {
  fn_name <- if (is.symbol(fn)) {
    as.character(fn)
  } else if (is.character(fn)) {
    fn
  } else {
    stop("r/call requires a symbol or string function name")
  }
  # Search for function in parent frames and their parent chains
  fn <- NULL
  # Try multiple parent frames
  for (i in 0:10) {
    tryCatch({
      frame_env <- parent.frame(i + 1)
      # Search this frame and its parent chain
      fn <- get0(fn_name, envir = frame_env, inherits = TRUE, ifnotfound = NULL)
      if (!is.null(fn)) break
      # Also check if this frame has a Rye environment in its parent chain
      current <- frame_env
      while (!is.null(current) && !identical(current, baseenv()) && !identical(current, emptyenv())) {
        if (exists(fn_name, envir = current, inherits = FALSE)) {
          fn <- get(fn_name, envir = current, inherits = FALSE)
          break
        }
        current <- parent.env(current)
      }
      if (!is.null(fn)) break
    }, error = function(e) NULL)
  }
  # Fall back to baseenv() if not found
  if (is.null(fn)) {
    fn <- get(fn_name, envir = baseenv())
  }
  rye_do_call(fn, rye_as_list(args))
}

rye_stdlib_promise_p <- function(x) {
  rye_promise_p(x)
}

rye_stdlib_force <- function(x) {
  rye_promise_force(x)
}


rye_stdlib_deparse_single <- function(x) {
  paste(deparse(x, width.cutoff = 500), collapse = " ")
}

rye_stdlib_format_value <- function(x) {
  rye_env_format_value(parent.frame(), x)
}

attr(rye_stdlib_apply, "rye_doc") <- list(
  description = "Apply fn to the elements of lst as arguments."
)
attr(rye_stdlib_values, "rye_doc") <- list(
  description = "Return multiple values to a call-with-values consumer."
)
attr(rye_stdlib_call_with_values, "rye_doc") <- list(
  description = "Call producer and pass its values to consumer."
)
attr(rye_stdlib_call, "rye_doc") <- list(
  description = "Convert a list to a callable form."
)
attr(rye_stdlib_display, "rye_doc") <- list(
  description = "Print x without formatting."
)
attr(rye_stdlib_str, "rye_doc") <- list(
  description = "Display structure of x."
)
attr(rye_stdlib_trace, "rye_doc") <- list(
  description = "Print x and return it."
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

