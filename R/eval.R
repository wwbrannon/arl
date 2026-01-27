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
  had_error <- FALSE
  src <- rye_src_get(expr)
  if (!is.null(src)) {
    rye_src_stack_push(src)
    on.exit({
      if (!had_error) {
        rye_src_stack_pop()
      }
    }, add = TRUE)
  }
  withCallingHandlers({
  # Handle NULL (empty list or #nil)
  if (is.null(expr)) {
    return(NULL)
  }

  # Handle atoms (self-evaluating)
  if (!is.call(expr) && !is.symbol(expr)) {
    # Keywords are self-evaluating (return as-is for now)
    return(rye_strip_src(expr))
  }

  # Handle keywords (they pass through as-is for use in function calls)
  if (inherits(expr, "rye_keyword")) {
    return(rye_strip_src(expr))
  }

  # Handle symbols (variable lookup)
  if (is.symbol(expr)) {
    return(eval(expr, envir = env))
  }

  # Handle empty list
  if (is.call(expr) && length(expr) == 0) {
    return(list())
  }

  # Macro expansion (before special forms)
  # Must happen before we check special forms
  if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]])) {
    if (is_macro(expr[[1]])) {
      expanded <- rye_macroexpand(expr, env, preserve_src = TRUE)
      return(rye_eval(expanded, env))
    }
  }

  # Special forms handling
  op <- expr[[1]]

  # quote - return argument unevaluated
  if (is.symbol(op) && as.character(op) == "quote") {
    if (length(expr) != 2) {
      stop("quote requires exactly 1 argument")
    }
    return(rye_strip_src(expr[[2]]))
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
    return(rye_strip_src(result))
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
    return(NULL)
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
    return(NULL)
  }

  # if - conditional evaluation
  if (is.symbol(op) && as.character(op) == "if") {
    if (length(expr) < 3 || length(expr) > 4) {
      stop("if requires 2 or 3 arguments: (if test then [else])")
    }
    test <- rye_strip_src(rye_eval(expr[[2]], env))
    # R-style truthiness: NULL and FALSE are falsy
    if (identical(test, FALSE) || is.null(test)) {
      # Evaluate else branch if present
      if (length(expr) == 4) {
        return(rye_strip_src(rye_eval(expr[[4]], env)))
      } else {
        return(NULL)
      }
    } else {
      return(rye_strip_src(rye_eval(expr[[3]], env)))
    }
  }

  # define - variable assignment
  if (is.symbol(op) && as.character(op) == "define") {
    if (length(expr) != 3) {
      stop("define requires exactly 2 arguments: (define name value)")
    }
    name <- expr[[2]]
    value <- rye_strip_src(rye_eval(expr[[3]], env))
    if (is.symbol(name)) {
      rye_assign(as.character(name), value, env)
      return(NULL)
    }
    if (is.call(name) || (is.list(name) && is.null(attr(name, "class", exact = TRUE)))) {
      rye_destructure_bind(name, value, env, mode = "define")
      return(NULL)
    }
    stop("define requires a symbol or list pattern as the first argument")
    return(NULL)
  }

  # set! - modify existing binding
  if (is.symbol(op) && as.character(op) == "set!") {
    if (length(expr) != 3) {
      stop("set! requires exactly 2 arguments: (set! name value)")
    }
    name <- expr[[2]]
    value <- rye_strip_src(rye_eval(expr[[3]], env))
    if (is.symbol(name)) {
      rye_assign_existing(as.character(name), value, env)
      return(NULL)
    }
    if (is.call(name) || (is.list(name) && is.null(attr(name, "class", exact = TRUE)))) {
      rye_destructure_bind(name, value, env, mode = "set")
      return(NULL)
    }
    stop("set! requires a symbol or list pattern as the first argument")
  }

  # load - evaluate a Rye source file or stdlib entry
  if (is.symbol(op) && as.character(op) == "load") {
    if (length(expr) != 2) {
      stop("load requires exactly 1 argument: (load \"path\")")
    }
    path <- rye_strip_src(rye_eval(expr[[2]], env))
    if (!is.character(path) || length(path) != 1) {
      stop("load requires a single file path string")
    }
    has_separator <- grepl("[/\\\\]", path)
    if (!has_separator) {
      stdlib_path <- rye_resolve_stdlib_path(path)
      if (!is.null(stdlib_path)) {
        return(rye_strip_src(rye_load_file(stdlib_path, env)))
      }
      if (file.exists(path)) {
        return(rye_strip_src(rye_load_file(path, env)))
      }
      stop(sprintf("File not found: %s", path))
    }
    return(rye_strip_src(rye_load_file(path, env)))
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
    if (length(arg_items) > 0) {
      for (arg in arg_items) {
        if (is.symbol(arg)) {
          name <- as.character(arg)
          param_names <- c(param_names, name)
          param_defaults[[name]] <- quote(expr = )
        } else if (is.call(arg) || is.list(arg)) {
          arg_list <- if (is.call(arg)) as.list(arg) else arg
          if (length(arg_list) != 2) {
            stop("lambda default argument must be a 2-element list")
          }
          if (!is.symbol(arg_list[[1]])) {
            stop("lambda default argument name must be a symbol")
          }
          name <- as.character(arg_list[[1]])
          param_names <- c(param_names, name)
          param_defaults[[name]] <- arg_list[[2]]
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

    return(fn)
  }

  # begin - sequence of expressions
  if (is.symbol(op) && as.character(op) == "begin") {
    result <- NULL
    for (i in 2:length(expr)) {
      result <- rye_strip_src(rye_eval(expr[[i]], env))
    }
    return(result)
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
    return(eval(formula_call, envir = env))
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
    return(eval(access_call, envir = env))
  }

  # Regular function application
  # Evaluate operator
  fn <- rye_strip_src(rye_eval(op, env))

  # Evaluate arguments, handling keywords for named parameters
  args <- list()
  arg_names <- character(0)
  i <- 2
  while (i <= length(expr)) {
    arg_expr <- expr[[i]]

    # Check if this is a keyword (for named arguments)
    if (inherits(arg_expr, "rye_keyword")) {
      # Next argument should be the value for this keyword
      if (i + 1 > length(expr)) {
        stop(sprintf("Keyword :%s requires a value", arg_expr))
      }
      keyword_name <- as.character(arg_expr)
      value <- rye_strip_src(rye_eval(expr[[i + 1]], env))

      args <- c(args, list(value))
      arg_names <- c(arg_names, keyword_name)
      i <- i + 2  # Skip both keyword and value
    } else {
      # Regular positional argument
      value <- rye_strip_src(rye_eval(arg_expr, env))
      args <- c(args, list(value))
      arg_names <- c(arg_names, "")
      i <- i + 1
    }
  }

  # Set names on args
  if (length(arg_names) > 0) {
    names(args) <- arg_names
  }

  # Apply function (arguments already evaluated by Rye)
  rye_strip_src(rye_do_call(fn, args))
  }, error = function(e) {
    had_error <<- TRUE
    stop(e)
  })
}
