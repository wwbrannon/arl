#' Evaluate Rye expressions
#'
#' Evaluates a parsed Rye expression, expanding macros and handling special
#' forms before applying functions in the provided environment.
#'
#' @param expr A parsed Rye expression
#' @param env Environment in which to evaluate (default: caller's environment)
#' @return The result of evaluation
#' @examples
#' exprs <- rye_read("(+ 1 2)")
#' rye_eval(exprs[[1]])
#' @export
rye_eval <- function(expr, env = parent.frame()) {
  if (!exists(".rye_env", envir = env, inherits = FALSE)) {
    assign(".rye_env", TRUE, envir = env)
  }
  withCallingHandlers({
    rye_trampoline(rye_eval_cps(expr, env, function(value) value))
  }, error = function(e) {
    stop(e)
  })
}

rye_thunk <- function(fn) {
  structure(fn, class = "rye_thunk")
}

rye_is_thunk <- function(value) {
  inherits(value, "rye_thunk")
}

rye_trampoline <- function(thunk) {
  current <- thunk
  while (rye_is_thunk(current)) {
    current <- current()
  }
  current
}

rye_call_k <- function(k, value) {
  rye_thunk(function() k(value))
}

rye_missing_default <- function() {
  structure(list(), class = "rye_missing_default")
}

rye_make_continuation <- function(k) {
  structure(list(k = k), class = "rye_continuation")
}

rye_make_builtin_callcc <- function() {
  structure(list(), class = "rye_builtin_callcc")
}

rye_eval_cps <- function(expr, env, k) {
  src <- rye_src_get(expr)
  if (!is.null(src)) {
    rye_src_stack_push(src)
    return(rye_eval_cps_inner(expr, env, function(value) {
      rye_src_stack_pop()
      k(value)
    }))
  }
  rye_eval_cps_inner(expr, env, k)
}

rye_eval_cps_inner <- function(expr, env, k) {
  # Handle NULL (empty list or #nil)
  if (is.null(expr)) {
    return(rye_call_k(k, NULL))
  }

  # Handle atoms (self-evaluating)
  if (!is.call(expr) && !is.symbol(expr)) {
    # Keywords are self-evaluating (return as-is for now)
    return(rye_call_k(k, rye_strip_src(expr)))
  }

  # Handle keywords (they pass through as-is for use in function calls)
  if (inherits(expr, "rye_keyword")) {
    return(rye_call_k(k, rye_strip_src(expr)))
  }

  # Handle symbols (variable lookup)
  if (is.symbol(expr)) {
    return(rye_call_k(k, eval(expr, envir = env)))
  }

  # Handle empty list
  if (is.call(expr) && length(expr) == 0) {
    return(rye_call_k(k, list()))
  }

  # Macro expansion (before special forms)
  # Must happen before we check special forms
  if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]])) {
    if (is_macro(expr[[1]])) {
      expanded <- rye_macroexpand(expr, env, preserve_src = TRUE)
      return(rye_eval_cps(expanded, env, k))
    }
  }

  # Special forms handling
  op <- expr[[1]]

  # quote - return argument unevaluated
  if (is.symbol(op) && as.character(op) == "quote") {
    if (length(expr) != 2) {
      stop("quote requires exactly 1 argument")
    }
    return(rye_call_k(k, rye_strip_src(expr[[2]])))
  }

  # quasiquote - template with selective evaluation
  if (is.symbol(op) && as.character(op) == "quasiquote") {
    if (length(expr) != 2) {
      stop("quasiquote requires exactly 1 argument")
    }
    result <- rye_quasiquote(expr[[2]], env)
    if (!exists(".rye_macroexpanding", envir = env, inherits = TRUE) ||
        !isTRUE(get(".rye_macroexpanding", envir = env, inherits = TRUE))) {
      result <- rye_hygiene_unwrap(result)
    }
    return(rye_call_k(k, rye_strip_src(result)))
  }

  # help - show docs without evaluating argument
  if (is.symbol(op) && as.character(op) == "help") {
    if (length(expr) != 2) {
      stop("help requires exactly 1 argument: (help topic)")
    }
    topic <- expr[[2]]
    if (is.symbol(topic)) {
      topic <- as.character(topic)
    }
    if (!is.character(topic) || length(topic) != 1) {
      stop("help requires a symbol or string")
    }
    rye_help(topic, env)
    return(rye_call_k(k, NULL))
  }

  # defmacro - define a macro
  if (is.symbol(op) && as.character(op) == "defmacro") {
    if (length(expr) < 4) {
      stop("defmacro requires at least 3 arguments: (defmacro name (params...) body...)")
    }

    name <- expr[[2]]
    if (!is.symbol(name)) {
      stop("defmacro requires a symbol as the first argument")
    }

    params_expr <- expr[[3]]
    # Convert params to list of symbols
    params <- list()
    if (!is.null(params_expr) && is.call(params_expr) && length(params_expr) > 0) {
      for (i in seq_along(params_expr)) {
        param <- params_expr[[i]]
        if (!is.symbol(param)) {
          stop("defmacro parameters must be symbols")
        }
        params[[i]] <- param
      }
    } else if (is.null(params_expr) || (is.call(params_expr) && length(params_expr) == 0)) {
      params <- list()
    } else {
      stop("defmacro parameters must be a list")
    }

    # Get body expressions
    body <- list()
    if (length(expr) >= 4) {
      for (i in 4:length(expr)) {
        body[[length(body) + 1]] <- expr[[i]]
      }
    }

    rye_defmacro(name, params, body, env)
    return(rye_call_k(k, NULL))
  }

  # call/cc - capture continuation
  if (is.symbol(op) && (as.character(op) == "call/cc" ||
                        as.character(op) == "call-with-current-continuation")) {
    if (length(expr) != 2) {
      stop("call/cc requires exactly 1 argument")
    }
    return(rye_eval_cps(expr[[2]], env, function(proc) {
      proc <- rye_strip_src(proc)
      rye_apply_callcc_cps(proc, k)
    }))
  }

  # if - conditional evaluation
  if (is.symbol(op) && as.character(op) == "if") {
    if (length(expr) < 3 || length(expr) > 4) {
      stop("if requires 2 or 3 arguments: (if test then [else])")
    }
    return(rye_eval_cps(expr[[2]], env, function(test) {
      test <- rye_strip_src(test)
      # R-style truthiness: NULL and FALSE are falsy
      if (identical(test, FALSE) || is.null(test)) {
        # Evaluate else branch if present
        if (length(expr) == 4) {
          return(rye_eval_cps(expr[[4]], env, k))
        }
        return(rye_call_k(k, NULL))
      }
      rye_eval_cps(expr[[3]], env, k)
    }))
  }

  # define - variable assignment
  if (is.symbol(op) && as.character(op) == "define") {
    if (length(expr) != 3) {
      stop("define requires exactly 2 arguments: (define name value)")
    }
    name <- expr[[2]]
    return(rye_eval_cps(expr[[3]], env, function(value) {
      value <- rye_strip_src(value)
      if (is.symbol(name)) {
        rye_assign(as.character(name), value, env)
        return(rye_call_k(k, NULL))
      }
      if (is.call(name) || (is.list(name) && is.null(attr(name, "class", exact = TRUE)))) {
        rye_destructure_bind(name, value, env, mode = "define")
        return(rye_call_k(k, NULL))
      }
      stop("define requires a symbol or list pattern as the first argument")
    }))
  }

  # set! - modify existing binding
  if (is.symbol(op) && as.character(op) == "set!") {
    if (length(expr) != 3) {
      stop("set! requires exactly 2 arguments: (set! name value)")
    }
    name <- expr[[2]]
    return(rye_eval_cps(expr[[3]], env, function(value) {
      value <- rye_strip_src(value)
      if (is.symbol(name)) {
        rye_assign_existing(as.character(name), value, env)
        return(rye_call_k(k, NULL))
      }
      if (is.call(name) || (is.list(name) && is.null(attr(name, "class", exact = TRUE)))) {
        rye_destructure_bind(name, value, env, mode = "set")
        return(rye_call_k(k, NULL))
      }
      stop("set! requires a symbol or list pattern as the first argument")
    }))
  }

  # load - evaluate a Rye source file or stdlib entry
  if (is.symbol(op) && as.character(op) == "load") {
    if (length(expr) != 2) {
      stop("load requires exactly 1 argument: (load \"path\")")
    }
    return(rye_eval_cps(expr[[2]], env, function(path) {
      path <- rye_strip_src(path)
      if (!is.character(path) || length(path) != 1) {
        stop("load requires a single file path string")
      }
      has_separator <- grepl("[/\\\\]", path)
      if (!has_separator) {
        stdlib_path <- rye_resolve_stdlib_path(path)
        if (!is.null(stdlib_path)) {
          return(rye_call_k(k, rye_strip_src(rye_load_file(stdlib_path, env))))
        }
        if (file.exists(path)) {
          return(rye_call_k(k, rye_strip_src(rye_load_file(path, env))))
        }
        stop(sprintf("File not found: %s", path))
      }
      rye_call_k(k, rye_strip_src(rye_load_file(path, env)))
    }))
  }

  # lambda - function creation
  if (is.symbol(op) && as.character(op) == "lambda") {
    if (length(expr) < 3) {
      stop("lambda requires at least 2 arguments: (lambda (args...) body...)")
    }

    # Parse argument list
    args_expr <- expr[[2]]

    # Handle argument list which may be a call or list
    arg_items <- list()
    if (!is.null(args_expr)) {
      if (is.call(args_expr)) {
        if (length(args_expr) > 0) {
          arg_items <- as.list(args_expr)
        }
      } else if (is.list(args_expr)) {
        arg_items <- args_expr
      } else {
        stop("lambda arguments must be a list")
      }
    }

    # Handle dotted rest parameter (x y . rest)
    rest_param <- NULL
    if (length(arg_items) > 0) {
      dot_idx <- which(vapply(arg_items, function(arg) {
        is.symbol(arg) && as.character(arg) == "."
      }, logical(1)))
      if (length(dot_idx) > 1) {
        stop("Dotted parameter list can only contain one '.'")
      }
      if (length(dot_idx) == 1) {
        if (dot_idx != length(arg_items) - 1) {
          stop("Dotted parameter list must have exactly one parameter after '.'")
        }
        rest_arg <- arg_items[[dot_idx + 1]]
        if (!is.symbol(rest_arg)) {
          stop("Rest parameter must be a symbol")
        }
        rest_param <- as.character(rest_arg)
        if (dot_idx > 1) {
          arg_items <- arg_items[1:(dot_idx - 1)]
        } else {
          arg_items <- list()
        }
      }
    }

    # Parse required and defaulted parameters
    param_names <- character(0)
    param_defaults <- list()
    param_default_exprs <- list()
    if (length(arg_items) > 0) {
      for (arg in arg_items) {
        if (is.symbol(arg)) {
          name <- as.character(arg)
          param_names <- c(param_names, name)
          param_defaults[[name]] <- quote(expr = )
          param_default_exprs[[name]] <- list(rye_missing_default())
        } else if (is.call(arg) || is.list(arg)) {
          arg_list <- if (is.call(arg)) as.list(arg) else arg
          if (length(arg_list) != 2) {
            stop("lambda default argument must be a 2-element list")
          }
          if (!is.symbol(arg_list[[1]])) {
            stop("lambda default argument name must be a symbol")
          }
          name <- as.character(arg_list[[1]])
          default_expr <- arg_list[[2]]
          if (is.null(default_expr)) {
            default_expr <- quote(NULL)
          }
          param_names <- c(param_names, name)
          param_defaults[[name]] <- default_expr
          param_default_exprs[[name]] <- list(default_expr)
        } else {
          stop("lambda arguments must be symbols or (name default) pairs")
        }
      }
    }

    all_names <- c(param_names, if (!is.null(rest_param)) rest_param)
    if (length(all_names) > 0 && any(duplicated(all_names))) {
      stop("lambda argument names must be unique")
    }

    # Create formals list (support defaults and rest args)
    formals_list <- list()
    if (length(param_names) > 0) {
      for (name in param_names) {
        formals_list[[name]] <- param_defaults[[name]]
      }
    }
    if (!is.null(rest_param)) {
      formals_list[["..."]] <- quote(expr = )
    }

    # Get body expressions (everything after the argument list)
    # Convert to a proper list
    body_exprs <- list()
    if (length(expr) >= 3) {
      for (i in 3:length(expr)) {
        body_exprs[[length(body_exprs) + 1]] <- expr[[i]]
      }
    }

    # Create a closure that evaluates the body in a new environment
    # Capture the current environment as the parent
    parent_env <- env

    fn <- function() {
      # Create new environment for function execution
      fn_env <- new.env(parent = parent_env)

      # Bind arguments
      if (length(formals_list) > 0) {
        for (param_name in names(formals_list)) {
          if (param_name == "...") {
            next
          }
          assign(param_name, get(param_name, inherits = FALSE), envir = fn_env)
        }
      }
      if (!is.null(rest_param)) {
        assign(rest_param, list(...), envir = fn_env)
      }

      # Evaluate body expressions in sequence
      result <- NULL
      if (length(body_exprs) > 0) {
        for (i in seq_along(body_exprs)) {
          result <- rye_strip_src(rye_eval(body_exprs[[i]], fn_env))
        }
      }
      result
    }

    # Set the formals
    formals(fn) <- formals_list
    class(fn) <- c("rye_closure", class(fn))
    attr(fn, "rye_closure") <- list(
      params = param_names,
      defaults = param_default_exprs,
      rest_param = rest_param,
      body_exprs = body_exprs,
      parent_env = parent_env
    )

    return(rye_call_k(k, fn))
  }

  # begin - sequence of expressions
  if (is.symbol(op) && as.character(op) == "begin") {
    if (length(expr) == 1) {
      return(rye_call_k(k, NULL))
    }
    return(rye_eval_seq_cps(as.list(expr)[-1], env, k))
  }

  # ~ - formula (don't evaluate arguments)
  if (is.symbol(op) && as.character(op) == "~") {
    # Build formula without evaluating arguments
    # R formulas are calls to ~, so we can use R's as.formula or just return the call
    # But we need to convert our parsed structure to R's formula structure
    formula_parts <- list(as.symbol("~"))
    if (length(expr) > 1) {
      for (i in 2:length(expr)) {
        formula_parts <- c(formula_parts, list(expr[[i]]))
      }
    }
    formula_call <- as.call(formula_parts)
    return(rye_call_k(k, eval(formula_call, envir = env)))
  }

  # :: and ::: - package accessors (don't evaluate arguments)
  if (is.symbol(op) && (as.character(op) == "::" || as.character(op) == ":::")) {
    if (length(expr) != 3) {
      stop(sprintf("%s requires exactly 2 arguments: (%s pkg name)", as.character(op), as.character(op)))
    }

    # Arguments should be symbols or can be strings
    pkg <- expr[[2]]
    name <- expr[[3]]

    # Convert to symbols if they aren't already
    if (!is.symbol(pkg)) {
      if (is.character(pkg)) {
        pkg <- as.symbol(pkg)
      } else {
        stop("Package name must be a symbol or string")
      }
    }

    if (!is.symbol(name)) {
      if (is.character(name)) {
        name <- as.symbol(name)
      } else {
        stop("Function/object name must be a symbol or string")
      }
    }

    # Build and evaluate the call using R's :: or :::
    access_call <- as.call(list(as.symbol(as.character(op)), pkg, name))
    return(rye_call_k(k, eval(access_call, envir = env)))
  }

  # Regular function application
  # Evaluate operator
  return(rye_eval_cps(op, env, function(fn) {
    fn <- rye_strip_src(fn)
    rye_eval_args_cps(expr, env, function(args_info) {
      args <- args_info$args
      if (length(args_info$arg_names) > 0) {
        names(args) <- args_info$arg_names
      }
      rye_apply_cps(fn, args, function(value) {
        rye_call_k(k, rye_strip_src(value))
      })
    })
  }))
}

rye_eval_seq_cps <- function(exprs, env, k) {
  if (length(exprs) == 0) {
    return(rye_call_k(k, NULL))
  }
  step <- function(idx) {
    if (idx >= length(exprs)) {
      return(rye_eval_cps(exprs[[idx]], env, k))
    }
    rye_eval_cps(exprs[[idx]], env, function(value) {
      value <- rye_strip_src(value)
      step(idx + 1)
    })
  }
  step(1)
}

rye_eval_args_cps <- function(expr, env, k) {
  args <- list()
  arg_names <- character(0)
  step <- function(i) {
    if (i > length(expr)) {
      return(rye_call_k(k, list(args = args, arg_names = arg_names)))
    }
    arg_expr <- expr[[i]]
    if (inherits(arg_expr, "rye_keyword")) {
      if (i + 1 > length(expr)) {
        stop(sprintf("Keyword :%s requires a value", arg_expr))
      }
      keyword_name <- as.character(arg_expr)
      return(rye_eval_cps(expr[[i + 1]], env, function(value) {
        args <<- c(args, list(rye_strip_src(value)))
        arg_names <<- c(arg_names, keyword_name)
        step(i + 2)
      }))
    }
    rye_eval_cps(arg_expr, env, function(value) {
      args <<- c(args, list(rye_strip_src(value)))
      arg_names <<- c(arg_names, "")
      step(i + 1)
    })
  }
  step(2)
}

rye_apply_cps <- function(fn, args, k) {
  if (inherits(fn, "rye_builtin_callcc")) {
    if (length(args) != 1) {
      stop("call/cc requires exactly 1 argument")
    }
    return(rye_apply_callcc_cps(args[[1]], k))
  }
  if (inherits(fn, "rye_continuation")) {
    if (length(args) != 1) {
      stop("continuation requires exactly 1 argument")
    }
    return(rye_call_k(fn$k, args[[1]]))
  }
  if (inherits(fn, "rye_closure")) {
    return(rye_apply_closure_cps(fn, args, k))
  }
  if (!is.function(fn)) {
    stop("attempt to call non-function")
  }
  result <- rye_do_call(fn, args)
  rye_call_k(k, result)
}

rye_apply_callcc_cps <- function(proc, k) {
  cont <- rye_make_continuation(k)
  rye_apply_cps(proc, list(cont), k)
}

rye_apply_closure_cps <- function(fn, args, k) {
  info <- attr(fn, "rye_closure")
  if (is.null(info)) {
    stop("invalid rye closure")
  }
  fn_env <- new.env(parent = info$parent_env)

  call <- as.call(c(list(quote(fn)), args))
  matched_call <- match.call(definition = fn, call = call, expand.dots = TRUE)
  matched_args <- as.list(matched_call)[-1]
  if (is.null(matched_args)) {
    matched_args <- list()
  }
  matched_names <- names(matched_args)
  if (is.null(matched_names)) {
    matched_names <- rep("", length(matched_args))
    names(matched_args) <- matched_names
  }

  rest_args <- list()
  if (!is.null(info$rest_param)) {
    for (idx in seq_along(matched_args)) {
      name <- matched_names[[idx]]
      if (!nzchar(name) || !(name %in% info$params)) {
        rest_args <- c(rest_args, list(rye_strip_src(matched_args[[idx]])))
        if (nzchar(name)) {
          names(rest_args)[length(rest_args)] <- name
        }
      }
    }
  } else {
    for (idx in seq_along(matched_args)) {
      name <- matched_names[[idx]]
      if (!nzchar(name) || !(name %in% info$params)) {
        stop("lambda got unexpected arguments")
      }
    }
  }

  bind_defaults <- function(idx) {
    if (idx > length(info$params)) {
      if (!is.null(info$rest_param)) {
        assign(info$rest_param, rest_args, envir = fn_env)
      }
      return(rye_eval_seq_cps(info$body_exprs, fn_env, k))
    }
    name <- info$params[[idx]]
    if (name %in% matched_names) {
      assign(name, rye_strip_src(matched_args[[name]]), envir = fn_env)
      return(bind_defaults(idx + 1))
    }
    default_expr <- info$defaults[[name]][[1]]
    if (inherits(default_expr, "rye_missing_default")) {
      stop(sprintf("lambda missing argument: %s", name))
    }
    rye_eval_cps(default_expr, fn_env, function(value) {
      assign(name, rye_strip_src(value), envir = fn_env)
      bind_defaults(idx + 1)
    })
  }

  bind_defaults(1)
}
