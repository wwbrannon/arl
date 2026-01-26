#' Global macro registry
.rye_macros <- new.env(parent = emptyenv())

#' Global gensym counter (stored in environment to avoid locking issues)
.rye_gensym_env <- new.env(parent = emptyenv())
.rye_gensym_env$counter <- 0

#' Generate a unique symbol
#'
#' @param prefix Optional prefix for the symbol (default: "G")
#' @return A unique symbol
#' @export
gensym <- function(prefix = "G") {
  .rye_gensym_env$counter <- .rye_gensym_env$counter + 1
  as.symbol(paste0(prefix, "__", .rye_gensym_env$counter))
}

#' Process quasiquote expressions
#'
#' @param expr The expression to process
#' @param env Environment for evaluation
#' @param depth Nesting depth of quasiquote (for nested quasiquotes)
#' @return The processed expression
rye_quasiquote <- function(expr, env, depth = 1) {
  # Handle atoms - return as-is
  if (!is.call(expr)) {
    return(expr)
  }

  # Check if it's an unquote
  if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "unquote") {
    if (depth == 1) {
      # Evaluate the unquoted expression
      if (length(expr) != 2) {
        stop("unquote requires exactly 1 argument")
      }
      return(rye_eval(expr[[2]], env))
    } else {
      # Nested quasiquote - decrease depth
      return(as.call(list(as.symbol("unquote"), rye_quasiquote(expr[[2]], env, depth - 1))))
    }
  }

  # Check if it's an unquote-splicing
  if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "unquote-splicing") {
    stop("unquote-splicing can only appear in list context")
  }

  # Check if it's a nested quasiquote
  if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "quasiquote") {
    # Increase depth
    return(as.call(list(as.symbol("quasiquote"), rye_quasiquote(expr[[2]], env, depth + 1))))
  }

  # Process list - handle unquote-splicing
  # Use a different approach to handle NULL values
  result <- list()
  i <- 1
  while (i <= length(expr)) {
    elem <- expr[[i]]

    # Check for unquote-splicing
    if (is.call(elem) && length(elem) > 0 && is.symbol(elem[[1]]) && as.character(elem[[1]]) == "unquote-splicing") {
      if (depth == 1) {
        # Evaluate and splice the result
        if (length(elem) != 2) {
          stop("unquote-splicing requires exactly 1 argument")
        }
        spliced <- rye_eval(elem[[2]], env)

        # If it's a call, convert to list for splicing
        if (is.call(spliced)) {
          spliced <- as.list(spliced)
        }

        # Splice elements (use c() to preserve NULLs in sublists)
        if (is.list(spliced)) {
          for (item in spliced) {
            result <- c(result, list(item))
          }
        } else {
          stop("unquote-splicing requires a list")
        }
      } else {
        # Nested quasiquote
        result <- c(result, list(as.call(list(as.symbol("unquote-splicing"), rye_quasiquote(elem[[2]], env, depth - 1)))))
      }
    } else {
      # Regular element - process recursively
      processed <- rye_quasiquote(elem, env, depth)
      # Use c() with list() to preserve NULL values
      result <- c(result, list(processed))
    }

    i <- i + 1
  }

  # Convert back to call if it was a call
  as.call(result)
}

#' Define a macro
#'
#' @param name Symbol naming the macro
#' @param params Parameter list
#' @param body Macro body expressions
#' @param env Environment in which to define the macro
rye_defmacro <- function(name, params, body, env) {
  # Create a function that will expand the macro
  macro_fn <- function(...) {
    # Create environment for macro expansion
    macro_env <- new.env(parent = env)

    # Bind parameters to arguments (unevaluated!)
    args <- match.call(expand.dots = FALSE)$...
    param_names <- as.character(params)

    # Handle rest parameters (param . rest)
    if (length(param_names) > 0 && param_names[length(param_names)] == ".") {
      # Last param is a dot - bind remaining args to previous param
      if (length(param_names) < 2) {
        stop("Dotted parameter list must have at least one parameter before .")
      }
      rest_param <- param_names[length(param_names) - 1]
      regular_params <- param_names[1:(length(param_names) - 2)]

      # Bind regular params
      for (i in seq_along(regular_params)) {
        if (i <= length(args)) {
          assign(regular_params[i], args[[i]], envir = macro_env)
        } else {
          stop(sprintf("Missing required parameter: %s", regular_params[i]))
        }
      }

      # Bind rest params as a list
      if (length(args) > length(regular_params)) {
        rest_args <- args[(length(regular_params) + 1):length(args)]
        # Store as a list for splicing
        assign(rest_param, rest_args, envir = macro_env)
      } else {
        assign(rest_param, list(), envir = macro_env)
      }
    } else {
      # Regular parameter binding
      if (length(args) != length(param_names)) {
        stop(sprintf("Macro %s expects %d arguments, got %d", as.character(name), length(param_names), length(args)))
      }

      for (i in seq_along(param_names)) {
        assign(param_names[i], args[[i]], envir = macro_env)
      }
    }

    # Evaluate macro body
    result <- NULL
    for (expr in body) {
      result <- rye_eval(expr, macro_env)
    }
    result
  }

  # Store in macro registry
  .rye_macros[[as.character(name)]] <- macro_fn
}

#' Check if a symbol names a macro
#'
#' @param name Symbol to check
#' @return TRUE if it's a macro, FALSE otherwise
is_macro <- function(name) {
  if (!is.symbol(name)) {
    return(FALSE)
  }
  exists(as.character(name), envir = .rye_macros)
}

#' Get a macro expander function
#'
#' @param name Symbol naming the macro
#' @return The macro expander function
get_macro <- function(name) {
  .rye_macros[[as.character(name)]]
}

#' Expand macros in an expression
#'
#' @param expr Expression to expand
#' @param env Environment for evaluation
#' @return Expanded expression
#' @export
rye_macroexpand <- function(expr, env = parent.frame()) {
  # Handle non-calls
  if (!is.call(expr) || length(expr) == 0) {
    return(expr)
  }

  op <- expr[[1]]

  # Check if it's a macro call
  if (is_macro(op)) {
    # Get the macro
    macro_fn <- get_macro(op)

    # Expand the macro with unevaluated arguments
    args <- as.list(expr[-1])
    expanded <- do.call(macro_fn, args)

    # Recursively expand the result
    return(rye_macroexpand(expanded, env))
  }

  # Not a macro - recursively expand subexpressions
  # But don't expand inside quote or quasiquote
  if (is.symbol(op)) {
    op_name <- as.character(op)
    if (op_name %in% c("quote", "defmacro")) {
      # Don't expand inside quote or defmacro
      return(expr)
    }
    if (op_name == "quasiquote") {
      # Only expand unquoted parts
      return(expr)
    }
  }

  # Recursively expand subexpressions
  result <- list(expr[[1]])
  if (length(expr) > 1) {
    for (i in 2:length(expr)) {
      result[[i]] <- rye_macroexpand(expr[[i]], env)
    }
  }

  as.call(result)
}
