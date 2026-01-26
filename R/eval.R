#' Evaluate Rye expressions
#'
#' @param expr A parsed Rye expression
#' @param env Environment in which to evaluate (default: caller's environment)
#' @return The result of evaluation
#' @export
rye_eval <- function(expr, env = parent.frame()) {
  # Handle NULL (empty list or #nil)
  if (is.null(expr)) {
    return(NULL)
  }

  # Handle atoms (self-evaluating)
  if (!is.call(expr) && !is.symbol(expr)) {
    # Keywords are self-evaluating (return as-is for now)
    return(expr)
  }

  # Handle keywords (they pass through as-is for use in function calls)
  if (inherits(expr, "rye_keyword")) {
    return(expr)
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
      expanded <- rye_macroexpand(expr, env)
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
    return(expr[[2]])
  }

  # quasiquote - template with selective evaluation
  if (is.symbol(op) && as.character(op) == "quasiquote") {
    if (length(expr) != 2) {
      stop("quasiquote requires exactly 1 argument")
    }
    return(rye_quasiquote(expr[[2]], env))
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
    test <- rye_eval(expr[[2]], env)
    # R's truthiness: NULL and FALSE are falsy, everything else is truthy
    if (identical(test, FALSE) || is.null(test)) {
      # Evaluate else branch if present
      if (length(expr) == 4) {
        return(rye_eval(expr[[4]], env))
      } else {
        return(NULL)
      }
    } else {
      return(rye_eval(expr[[3]], env))
    }
  }

  # define - variable assignment
  if (is.symbol(op) && as.character(op) == "define") {
    if (length(expr) != 3) {
      stop("define requires exactly 2 arguments: (define name value)")
    }
    name <- expr[[2]]
    if (!is.symbol(name)) {
      stop("define requires a symbol as the first argument")
    }
    value <- rye_eval(expr[[3]], env)
    assign(as.character(name), value, envir = env)
    return(NULL)
  }

  # lambda - function creation
  if (is.symbol(op) && as.character(op) == "lambda") {
    if (length(expr) < 3) {
      stop("lambda requires at least 2 arguments: (lambda (args...) body...)")
    }

    # Parse argument list
    args_expr <- expr[[2]]

    # Handle empty argument list () which is parsed as list()
    # or as a call of length 0
    arg_names <- character(0)
    if (!is.null(args_expr)) {
      if (is.call(args_expr) && length(args_expr) > 0) {
        # Non-empty argument list
        for (i in seq_along(args_expr)) {
          arg <- args_expr[[i]]
          if (!is.symbol(arg)) {
            stop("lambda arguments must be symbols")
          }
          arg_names <- c(arg_names, as.character(arg))
        }
      } else if (is.list(args_expr) && length(args_expr) == 0) {
        # Empty list() - no arguments
        arg_names <- character(0)
      } else if (!(is.call(args_expr) && length(args_expr) == 0)) {
        # It's not a call and not an empty list - error
        stop("lambda arguments must be a list")
      }
    }

    # Create formals list (all arguments are required, no defaults)
    if (length(arg_names) > 0) {
      formals_list <- setNames(rep(list(quote(expr = )), length(arg_names)), arg_names)
    } else {
      formals_list <- list()
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
      arg_values <- as.list(match.call()[-1])
      if (length(arg_values) > 0) {
        for (i in seq_along(arg_values)) {
          assign(names(formals_list)[i], eval(arg_values[[i]], envir = parent.frame()), envir = fn_env)
        }
      }

      # Evaluate body expressions in sequence
      result <- NULL
      if (length(body_exprs) > 0) {
        for (i in seq_along(body_exprs)) {
          result <- rye_eval(body_exprs[[i]], fn_env)
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
      result <- rye_eval(expr[[i]], env)
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

  # Regular function application
  # Evaluate operator
  fn <- rye_eval(op, env)

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
      value <- rye_eval(expr[[i + 1]], env)

      args <- c(args, list(value))
      arg_names <- c(arg_names, keyword_name)
      i <- i + 2  # Skip both keyword and value
    } else {
      # Regular positional argument
      value <- rye_eval(arg_expr, env)
      args <- c(args, list(value))
      arg_names <- c(arg_names, "")
      i <- i + 1
    }
  }

  # Set names on args
  if (length(arg_names) > 0) {
    names(args) <- arg_names
  }

  # Apply function
  do.call(fn, args)
}
