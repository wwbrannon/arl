#' Global gensym counter (stored in environment to avoid locking issues)
#' @keywords internal
.rye_gensym_env <- new.env(parent = emptyenv())
.rye_gensym_env$counter <- 0

#' Global hygiene gensym counter
#' @keywords internal
.rye_hygiene_gensym_env <- new.env(parent = emptyenv())
.rye_hygiene_gensym_env$counter <- 0

#' Generate a unique symbol
#'
#' @param prefix Optional prefix for the symbol (default: "G")
#' @return A unique symbol
#' @examples
#' gensym()
#' gensym("tmp")
#' @export
gensym <- function(prefix = "G") {
  .rye_gensym_env$counter <- .rye_gensym_env$counter + 1
  as.symbol(paste0(prefix, "__", .rye_gensym_env$counter))
}

#' Generate a unique symbol for hygiene
#' @keywords internal
rye_hygiene_gensym <- function(prefix = "H") {
  .rye_hygiene_gensym_env$counter <- .rye_hygiene_gensym_env$counter + 1
  as.symbol(paste0(prefix, "__h", .rye_hygiene_gensym_env$counter))
}

#' Internal syntax wrapper for hygiene
#' @keywords internal
rye_hygiene_wrap <- function(expr, origin) {
  structure(list(expr = expr, origin = origin), class = "rye_syntax")
}

#' @keywords internal
rye_hygiene_is <- function(x) {
  inherits(x, "rye_syntax")
}

#' @keywords internal
rye_hygiene_origin <- function(x) {
  if (rye_hygiene_is(x)) {
    return(x$origin)
  }
  NULL
}

#' @keywords internal
rye_hygiene_expr <- function(x) {
  if (rye_hygiene_is(x)) {
    return(x$expr)
  }
  x
}

#' @keywords internal
rye_hygiene_unwrap <- function(expr) {
  if (rye_hygiene_is(expr)) {
    return(rye_hygiene_unwrap(expr$expr))
  }
  rye_map_expr(expr, rye_hygiene_unwrap)
}

#' @keywords internal
rye_map_expr <- function(expr, fn, ...) {
  if (is.call(expr)) {
    mapped <- lapply(as.list(expr), fn, ...)
    return(as.call(mapped))
  }
  if (is.list(expr) && is.null(attr(expr, "class", exact = TRUE))) {
    mapped <- lapply(expr, fn, ...)
    if (!is.null(names(expr))) {
      names(mapped) <- names(expr)
    }
    return(mapped)
  }
  expr
}

#' Mark occurrences of a symbol as introduced
#'
#' @param symbol A symbol or single string naming the identifier to capture
#' @param expr Expression to scan for occurrences
#' @keywords internal
rye_capture <- function(symbol, expr) {
  name <- NULL
  if (is.symbol(symbol)) {
    name <- as.character(symbol)
  } else if (is.character(symbol) && length(symbol) == 1) {
    name <- symbol
  }
  if (is.null(name) || !nzchar(name)) {
    stop("capture expects a symbol or single string name")
  }
  rye_capture_mark(expr, name)
}

#' @keywords internal
rye_capture_mark <- function(expr, name) {
  if (rye_hygiene_is(expr)) {
    origin <- rye_hygiene_origin(expr)
    inner <- rye_capture_mark(rye_hygiene_expr(expr), name)
    return(rye_hygiene_wrap(inner, origin))
  }
  if (is.symbol(expr) && identical(as.character(expr), name)) {
    return(rye_hygiene_wrap(expr, "introduced"))
  }
  rye_map_expr(expr, rye_capture_mark, name = name)
}

#' @keywords internal
rye_hygienize <- function(expr) {
  rye_hygienize_expr(expr, env = list(), protected = FALSE)
}

#' @keywords internal
rye_hygienize_expr <- function(expr, env, protected) {
  if (rye_hygiene_is(expr)) {
    origin <- rye_hygiene_origin(expr)
    inner <- rye_hygiene_expr(expr)
    if (identical(origin, "call_site")) {
      return(rye_hygienize_expr(inner, env, protected = TRUE))
    }
    if (identical(origin, "introduced")) {
      return(rye_hygienize_expr(inner, env, protected = FALSE))
    }
    return(rye_hygienize_expr(inner, env, protected = protected))
  }

  if (is.symbol(expr)) {
    if (isTRUE(protected)) {
      return(expr)
    }
    name <- as.character(expr)
    if (!is.null(env[[name]])) {
      return(env[[name]])
    }
    return(expr)
  }

  if (!is.call(expr)) {
    if (is.list(expr) && is.null(attr(expr, "class", exact = TRUE))) {
      updated <- lapply(expr, rye_hygienize_expr, env = env, protected = protected)
      if (!is.null(names(expr))) {
        names(updated) <- names(expr)
      }
      return(updated)
    }
    return(expr)
  }

  op <- expr[[1]]
  if (is.symbol(op)) {
    op_name <- as.character(op)
    if (op_name %in% c("quote", "quasiquote")) {
      return(expr)
    }
  }

  if (isTRUE(protected)) {
    updated <- lapply(as.list(expr), rye_hygienize_expr, env = env, protected = TRUE)
    return(as.call(updated))
  }

  if (is.symbol(op)) {
    op_name <- as.character(op)
    if (op_name == "begin") {
      return(rye_hygienize_begin(expr, env))
    }
    if (op_name == "define") {
      return(rye_hygienize_define(expr, env)$expr)
    }
    if (op_name == "lambda") {
      return(rye_hygienize_lambda(expr, env))
    }
    if (op_name %in% c("let", "let*", "letrec")) {
      return(rye_hygienize_let(expr, env, op_name))
    }
  }

  updated <- lapply(as.list(expr), rye_hygienize_expr, env = env, protected = FALSE)
  as.call(updated)
}

#' @keywords internal
rye_hygienize_begin <- function(expr, env) {
  result <- list(expr[[1]])
  current_env <- env
  if (length(expr) > 1) {
    for (i in 2:length(expr)) {
      form <- expr[[i]]
      if (is.call(form) && length(form) >= 2 && is.symbol(form[[1]]) &&
          as.character(form[[1]]) == "define") {
        out <- rye_hygienize_define(form, current_env)
        result[[i]] <- out$expr
        current_env <- out$env
      } else {
        result[[i]] <- rye_hygienize_expr(form, current_env, protected = FALSE)
      }
    }
  }
  as.call(result)
}

#' @keywords internal
rye_hygienize_define <- function(expr, env) {
  result <- list(expr[[1]])
  name_expr <- expr[[2]]
  name_origin <- rye_hygiene_origin(name_expr)
  name_expr <- rye_hygiene_expr(name_expr)
  new_env <- env
  if (is.symbol(name_expr) && !identical(name_origin, "call_site")) {
    name <- as.character(name_expr)
    fresh <- rye_hygiene_gensym(name)
    new_env[[name]] <- fresh
    result[[2]] <- fresh
  } else if (is.call(name_expr) || (is.list(name_expr) && is.null(attr(name_expr, "class", exact = TRUE)))) {
    pattern_out <- rye_hygienize_define_pattern(expr[[2]], new_env)
    result[[2]] <- pattern_out$expr
    new_env <- pattern_out$env
  } else {
    result[[2]] <- rye_hygienize_expr(expr[[2]], env, protected = FALSE)
  }
  if (length(expr) >= 3) {
    result[[3]] <- rye_hygienize_expr(expr[[3]], env, protected = FALSE)
  }
  list(expr = as.call(result), env = new_env)
}

#' @keywords internal
rye_hygienize_define_pattern <- function(pattern, env, protected = FALSE) {
  if (isTRUE(protected)) {
    return(list(expr = rye_hygienize_expr(pattern, env, protected = TRUE), env = env))
  }
  if (rye_hygiene_is(pattern)) {
    origin <- rye_hygiene_origin(pattern)
    inner <- rye_hygiene_expr(pattern)
    if (identical(origin, "call_site")) {
      return(rye_hygienize_define_pattern(inner, env, protected = TRUE))
    }
    if (identical(origin, "introduced")) {
      return(rye_hygienize_define_pattern(inner, env, protected = FALSE))
    }
    return(rye_hygienize_define_pattern(inner, env, protected = protected))
  }
  if (is.symbol(pattern)) {
    name <- as.character(pattern)
    if (identical(name, ".")) {
      return(list(expr = pattern, env = env))
    }
    fresh <- rye_hygiene_gensym(name)
    env[[name]] <- fresh
    return(list(expr = fresh, env = env))
  }
  if (is.call(pattern)) {
    parts <- as.list(pattern)
    updated <- list()
    for (i in seq_along(parts)) {
      out <- rye_hygienize_define_pattern(parts[[i]], env, protected = protected)
      updated[[i]] <- out$expr
      env <- out$env
    }
    return(list(expr = as.call(updated), env = env))
  }
  if (is.list(pattern) && is.null(attr(pattern, "class", exact = TRUE))) {
    updated <- list()
    for (i in seq_along(pattern)) {
      out <- rye_hygienize_define_pattern(pattern[[i]], env, protected = protected)
      updated[[i]] <- out$expr
      env <- out$env
    }
    if (!is.null(names(pattern))) {
      names(updated) <- names(pattern)
    }
    return(list(expr = updated, env = env))
  }
  list(expr = pattern, env = env)
}

#' @keywords internal
rye_hygienize_lambda <- function(expr, env) {
  if (length(expr) < 3) {
    return(expr)
  }
  args_expr <- expr[[2]]
  if (!is.call(args_expr)) {
    result <- list(expr[[1]], rye_hygienize_expr(args_expr, env, protected = TRUE))
    if (length(expr) > 2) {
      for (i in 3:length(expr)) {
        result[[i]] <- rye_hygienize_expr(expr[[i]], env, protected = FALSE)
      }
    }
    return(as.call(result))
  }
  args_list <- as.list(args_expr)
  new_args <- list()
  new_env <- env
  if (length(args_list) > 0) {
    for (i in seq_along(args_list)) {
      arg <- args_list[[i]]
      arg_origin <- rye_hygiene_origin(arg)
      arg_unwrapped <- rye_hygiene_expr(arg)
      if (is.symbol(arg_unwrapped) && as.character(arg_unwrapped) != "." &&
          !identical(arg_origin, "call_site")) {
        name <- as.character(arg_unwrapped)
        fresh <- rye_hygiene_gensym(name)
        new_env[[name]] <- fresh
        new_args[[i]] <- fresh
      } else {
        new_args[[i]] <- rye_hygienize_expr(arg, env, protected = TRUE)
      }
    }
  }
  args_out <- if (length(args_list) == 0) args_expr else as.call(new_args)
  result <- list(expr[[1]], args_out)
  if (length(expr) > 2) {
    for (i in 3:length(expr)) {
      result[[i]] <- rye_hygienize_expr(expr[[i]], new_env, protected = FALSE)
    }
  }
  as.call(result)
}

#' @keywords internal
rye_hygienize_binding_parts <- function(binding) {
  parts <- if (is.call(binding)) as.list(binding) else list()
  name_origin <- if (length(parts) >= 1) rye_hygiene_origin(parts[[1]]) else NULL
  name_expr <- if (length(parts) >= 1) rye_hygiene_expr(parts[[1]]) else NULL
  list(parts = parts, name_origin = name_origin, name_expr = name_expr)
}

#' @keywords internal
rye_hygienize_binding_value <- function(parts, env) {
  if (length(parts) >= 2) {
    return(rye_hygienize_expr(parts[[2]], env, protected = FALSE))
  }
  NULL
}

#' @keywords internal
rye_hygienize_let <- function(expr, env, op_name) {
  if (length(expr) < 3) {
    return(expr)
  }
  bindings_expr <- expr[[2]]
  bindings_list <- if (is.call(bindings_expr)) as.list(bindings_expr) else list()
  new_bindings <- list()
  if (op_name == "letrec") {
    value_env <- env
    body_env <- env
    if (length(bindings_list) > 0) {
      for (i in seq_along(bindings_list)) {
        binding <- bindings_list[[i]]
        info <- rye_hygienize_binding_parts(binding)
        parts <- info$parts
        if (length(parts) == 0) {
          next
        }
        name_origin <- info$name_origin
        name_expr <- info$name_expr
        if (is.symbol(name_expr) && !identical(name_origin, "call_site")) {
          name <- as.character(name_expr)
          body_env[[name]] <- rye_hygiene_gensym(name)
        }
      }
    }
    if (length(bindings_list) > 0) {
      for (i in seq_along(bindings_list)) {
        binding <- bindings_list[[i]]
        info <- rye_hygienize_binding_parts(binding)
        parts <- info$parts
        if (length(parts) == 0) {
          new_bindings[[i]] <- binding
          next
        }
        name_origin <- info$name_origin
        name_expr <- info$name_expr
        if (is.symbol(name_expr) && !identical(name_origin, "call_site") &&
            !is.null(body_env[[as.character(name_expr)]])) {
          renamed <- body_env[[as.character(name_expr)]]
          value <- rye_hygienize_binding_value(parts, body_env)
          new_bindings[[i]] <- as.call(list(renamed, value))
        } else {
          value <- rye_hygienize_binding_value(parts, value_env)
          new_bindings[[i]] <- as.call(c(list(parts[[1]]), list(value)))
        }
      }
    }
    bindings_out <- if (length(bindings_list) == 0) bindings_expr else as.call(new_bindings)
    body <- list(expr[[1]], bindings_out)
    if (length(expr) > 2) {
      for (i in 3:length(expr)) {
        body[[i]] <- rye_hygienize_expr(expr[[i]], body_env, protected = FALSE)
      }
    }
    return(as.call(body))
  }

  if (op_name == "let*") {
    current_env <- env
    if (length(bindings_list) > 0) {
      for (i in seq_along(bindings_list)) {
        binding <- bindings_list[[i]]
        info <- rye_hygienize_binding_parts(binding)
        parts <- info$parts
        if (length(parts) == 0) {
          new_bindings[[i]] <- binding
          next
        }
        name_origin <- info$name_origin
        name_expr <- info$name_expr
        value <- rye_hygienize_binding_value(parts, current_env)
        if (is.symbol(name_expr) && !identical(name_origin, "call_site")) {
          name <- as.character(name_expr)
          fresh <- rye_hygiene_gensym(name)
          current_env[[name]] <- fresh
          new_bindings[[i]] <- as.call(list(fresh, value))
        } else {
          new_bindings[[i]] <- as.call(c(list(parts[[1]]), list(value)))
        }
      }
    }
    bindings_out <- if (length(bindings_list) == 0) bindings_expr else as.call(new_bindings)
    body <- list(expr[[1]], bindings_out)
    if (length(expr) > 2) {
      for (i in 3:length(expr)) {
        body[[i]] <- rye_hygienize_expr(expr[[i]], current_env, protected = FALSE)
      }
    }
    return(as.call(body))
  }

  body_env <- env
  if (length(bindings_list) > 0) {
    for (i in seq_along(bindings_list)) {
      binding <- bindings_list[[i]]
      info <- rye_hygienize_binding_parts(binding)
      parts <- info$parts
      if (length(parts) == 0) {
        next
      }
      name_origin <- info$name_origin
      name_expr <- info$name_expr
      if (is.symbol(name_expr) && !identical(name_origin, "call_site")) {
        name <- as.character(name_expr)
        body_env[[name]] <- rye_hygiene_gensym(name)
      }
    }
  }
  if (length(bindings_list) > 0) {
    for (i in seq_along(bindings_list)) {
      binding <- bindings_list[[i]]
      info <- rye_hygienize_binding_parts(binding)
      parts <- info$parts
      if (length(parts) == 0) {
        new_bindings[[i]] <- binding
        next
      }
      name_origin <- info$name_origin
      name_expr <- info$name_expr
      if (is.symbol(name_expr) && !identical(name_origin, "call_site") &&
          !is.null(body_env[[as.character(name_expr)]])) {
        renamed <- body_env[[as.character(name_expr)]]
        value <- rye_hygienize_binding_value(parts, env)
        new_bindings[[i]] <- as.call(list(renamed, value))
      } else {
        value <- rye_hygienize_binding_value(parts, env)
        new_bindings[[i]] <- as.call(c(list(parts[[1]]), list(value)))
      }
    }
  }
  bindings_out <- if (length(bindings_list) == 0) bindings_expr else as.call(new_bindings)
  body <- list(expr[[1]], bindings_out)
  if (length(expr) > 2) {
    for (i in 3:length(expr)) {
      body[[i]] <- rye_hygienize_expr(expr[[i]], body_env, protected = FALSE)
    }
  }
  as.call(body)
}

#' Process quasiquote expressions
#'
#' @param expr The expression to process
#' @param env Environment for evaluation
#' @param depth Nesting depth of quasiquote (for nested quasiquotes)
#' @return The processed expression
#' @keywords internal
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
      return(rye_hygiene_wrap(rye_eval(expr[[2]], env), "call_site"))
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
            result <- c(result, list(rye_hygiene_wrap(item, "call_site")))
          }
        } else {
          stop("unquote-splicing requires a list")
        }
      } else {
        # Nested quasiquote
        result <- c(result, list(as.call(list(
          as.symbol("unquote-splicing"),
          rye_quasiquote(elem[[2]], env, depth - 1)
        ))))
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
#' @keywords internal
rye_defmacro <- function(name, params, body, env, docstring = NULL) {
  # Create a function that will expand the macro
  macro_fn <- function(...) {
    # Create environment for macro expansion
    macro_env <- new.env(parent = env)
    assign(".rye_macroexpanding", TRUE, envir = macro_env)

    # Bind parameters to arguments (unevaluated!)
    args <- match.call(expand.dots = FALSE)$...
    param_names <- as.character(params)

    # Handle rest parameters (param . rest)
    dot_idx <- which(param_names == ".")
    if (length(dot_idx) > 1) {
      stop("Dotted parameter list can only contain one '.'")
    }
    if (length(dot_idx) == 1) {
      if (dot_idx == length(param_names)) {
        # Legacy dotted-list style: (a b rest .)
        if (length(param_names) < 2) {
          stop("Dotted parameter list must have at least one parameter before .")
        }
        rest_param <- param_names[length(param_names) - 1]
        regular_params <- param_names[1:(length(param_names) - 2)]
      } else {
        # Standard dotted-list style: (a b . rest)
        if (dot_idx != length(param_names) - 1) {
          stop("Dotted parameter list must have exactly one parameter after '.'")
        }
        rest_param <- param_names[dot_idx + 1]
        if (dot_idx > 1) {
          regular_params <- param_names[1:(dot_idx - 1)]
        } else {
          regular_params <- character(0)
        }
      }

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

  attr(macro_fn, "rye_macro") <- list(params = as.character(params))
  if (!is.null(docstring)) {
    attr(macro_fn, "rye_doc") <- list(description = docstring)
  }

  # Store in macro registry (env-local)
  registry <- rye_env_macro_registry(env, create = TRUE)
  name_str <- as.character(name)
  if (exists(name_str, envir = registry, inherits = FALSE) && bindingIsLocked(name_str, registry)) {
    unlockBinding(name_str, registry)
  }
  registry[[name_str]] <- macro_fn
  lockBinding(name_str, registry)
  assign(name_str, macro_fn, envir = env)
}

#' Check if a symbol names a macro
#'
#' @param name Symbol to check
#' @return TRUE if it's a macro, FALSE otherwise
#' @keywords internal
is_macro <- function(name, env = parent.frame()) {
  if (!is.symbol(name)) {
    return(FALSE)
  }
  registry <- rye_env_macro_registry(env, create = FALSE)
  !is.null(registry) && exists(as.character(name), envir = registry, inherits = FALSE)
}

#' Get a macro expander function
#'
#' @param name Symbol naming the macro
#' @return The macro expander function
#' @keywords internal
get_macro <- function(name, env = parent.frame()) {
  registry <- rye_env_macro_registry(env, create = FALSE)
  if (is.null(registry)) {
    return(NULL)
  }
  registry[[as.character(name)]]
}

#' Expand macros in an expression
#'
#' Expands macros recursively until the expression no longer begins with a macro
#' call. Quoted and quasiquoted forms are not expanded.
#'
#' @param expr Expression to expand
#' @param env Environment for evaluation
#' @param preserve_src Logical. If TRUE, preserves source location information in the expanded expression. Defaults to FALSE.
#' @return Expanded expression
#' @examples
#' rye_eval(rye_read("(defmacro when (test body) `(if ,test ,body #nil))")[[1]])
#' rye_macroexpand(rye_read("(when #t 1)")[[1]])
#' @export
rye_macroexpand <- function(expr, env = parent.frame(), preserve_src = FALSE) {
  # Handle non-calls
  if (!is.call(expr) || length(expr) == 0) {
    if (isTRUE(preserve_src)) {
      return(expr)
    }
    return(rye_strip_src(expr))
  }

  op <- expr[[1]]

  # Check if it's a macro call
  if (is_macro(op, env = env)) {
    # Get the macro
    macro_fn <- get_macro(op, env = env)

    # Expand the macro with unevaluated arguments
    args <- as.list(expr[-1])
    expanded <- do.call(macro_fn, args)
    expanded <- rye_hygienize(expanded)
    expanded <- rye_hygiene_unwrap(expanded)
    expanded <- rye_src_inherit(expanded, expr)

    # Recursively expand the result
    return(rye_macroexpand(expanded, env, preserve_src = preserve_src))
  }

  # Not a macro - recursively expand subexpressions
  # But don't expand inside quote or quasiquote
  if (is.symbol(op)) {
    op_name <- as.character(op)
    if (op_name %in% c("quote", "defmacro")) {
      # Don't expand inside quote or defmacro
      if (isTRUE(preserve_src)) {
        return(expr)
      }
      return(rye_strip_src(expr))
    }
    if (op_name == "quasiquote") {
      # Only expand unquoted parts
      if (isTRUE(preserve_src)) {
        return(expr)
      }
      return(rye_strip_src(expr))
    }
  }

  # Recursively expand subexpressions
  result <- list(expr[[1]])
  if (length(expr) > 1) {
    for (i in 2:length(expr)) {
      result[[i]] <- rye_macroexpand(expr[[i]], env, preserve_src = preserve_src)
    }
  }

  expanded <- rye_src_inherit(as.call(result), expr)
  if (isTRUE(preserve_src)) {
    return(expanded)
  }
  rye_strip_src(expanded)
}

#' Expand a single macro layer in an expression
#'
#' @param expr Expression to expand
#' @param env Environment for evaluation
#' @return Expanded expression or the original expression
#' @keywords internal
rye_macroexpand_1 <- function(expr, env = parent.frame()) {
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
