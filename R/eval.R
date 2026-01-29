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
    rye_eval_inner(expr, env)
  }, error = function(e) {
    stop(e)
  })
}

rye_missing_default <- function() {
  structure(list(), class = "rye_missing_default")
}

rye_collect_body <- function(expr, start_idx) {
  body <- list()
  if (length(expr) >= start_idx) {
    for (i in start_idx:length(expr)) {
      body[[length(body) + 1]] <- expr[[i]]
    }
  }
  body
}

rye_extract_docstring <- function(body_exprs) {
  docstring <- NULL
  if (length(body_exprs) > 0) {
    first_expr <- rye_strip_src(body_exprs[[1]])
    if (is.character(first_expr) && length(first_expr) == 1) {
      docstring <- first_expr
      body_exprs <- body_exprs[-1]
    }
  }
  list(body = body_exprs, docstring = docstring)
}

rye_lambda_arg_items <- function(args_expr) {
  if (is.null(args_expr)) {
    return(list())
  }
  if (is.call(args_expr)) {
    if (length(args_expr) > 0) {
      return(as.list(args_expr))
    }
    return(list())
  }
  if (is.list(args_expr)) {
    return(args_expr)
  }
  stop("lambda arguments must be a list")
}

rye_lambda_parse_params <- function(arg_items) {
  # Handle dotted rest parameter (x y . rest)
  rest_param <- NULL
  rest_param_spec <- NULL
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
      if (is.symbol(rest_arg)) {
        rest_param <- as.character(rest_arg)
        rest_param_spec <- list(type = "name", name = rest_param, pattern = NULL, display = rest_param)
      } else if (is.call(rest_arg) || (is.list(rest_arg) && is.null(attr(rest_arg, "class", exact = TRUE)))) {
        rest_list <- if (is.call(rest_arg)) as.list(rest_arg) else rest_arg
        is_pattern_wrapper <- length(rest_list) >= 2 &&
          is.symbol(rest_list[[1]]) &&
          as.character(rest_list[[1]]) %in% c("pattern", "destructure")
        if (!is_pattern_wrapper) {
          stop("Rest parameter must be a symbol or (pattern <pat>)")
        }
        if (length(rest_list) != 2) {
          stop("Rest pattern must be (pattern <pat>)")
        }
        rest_pattern <- rest_list[[2]]
        rest_display <- paste(deparse(rest_pattern, width.cutoff = 500), collapse = " ")
        rest_param_spec <- list(
          type = "pattern",
          name = NULL,
          pattern = rest_pattern,
          display = rest_display
        )
      } else {
        stop("Rest parameter must be a symbol or (pattern <pat>)")
      }
      if (dot_idx > 1) {
        arg_items <- arg_items[1:(dot_idx - 1)]
      } else {
        arg_items <- list()
      }
    }
  }

  # Parse required, defaulted, and destructured parameters
  param_specs <- list()
  param_names <- character(0)
  param_display <- character(0)
  param_defaults <- list()
  param_default_exprs <- list()
  if (length(arg_items) > 0) {
    for (arg in arg_items) {
      if (is.symbol(arg)) {
        name <- as.character(arg)
        param_names <- c(param_names, name)
        param_display <- c(param_display, name)
        param_defaults[[name]] <- quote(expr = )
        param_default_exprs[[name]] <- list(rye_missing_default())
        param_specs[[length(param_specs) + 1]] <- list(
          type = "name",
          formal = name,
          pattern = NULL,
          display = name
        )
      } else if (is.call(arg) || (is.list(arg) && is.null(attr(arg, "class", exact = TRUE)))) {
        arg_list <- if (is.call(arg)) as.list(arg) else arg
        is_pattern_wrapper <- length(arg_list) >= 2 &&
          is.symbol(arg_list[[1]]) &&
          as.character(arg_list[[1]]) %in% c("pattern", "destructure")
        is_default_pair <- length(arg_list) == 2 && is.symbol(arg_list[[1]])
        if (is_pattern_wrapper) {
          if (length(arg_list) != 2 && length(arg_list) != 3) {
            stop("pattern wrapper must be (pattern <pat>) or (pattern <pat> <default>)")
          }
          pattern <- arg_list[[2]]
          default_expr <- rye_missing_default()
          if (length(arg_list) == 3) {
            default_expr <- arg_list[[3]]
            if (is.null(default_expr)) {
              default_expr <- quote(NULL)
            }
          }
          tmp_name <- as.character(gensym(".__rye_arg"))
          display <- paste(deparse(pattern, width.cutoff = 500), collapse = " ")
          param_names <- c(param_names, tmp_name)
          param_display <- c(param_display, display)
          if (inherits(default_expr, "rye_missing_default")) {
            param_defaults[[tmp_name]] <- quote(expr = )
            param_default_exprs[[tmp_name]] <- list(rye_missing_default())
          } else {
            param_defaults[[tmp_name]] <- default_expr
            param_default_exprs[[tmp_name]] <- list(default_expr)
          }
          param_specs[[length(param_specs) + 1]] <- list(
            type = "pattern",
            formal = tmp_name,
            pattern = pattern,
            display = display
          )
        } else if (is_default_pair) {
          name <- as.character(arg_list[[1]])
          default_expr <- arg_list[[2]]
          if (is.null(default_expr)) {
            default_expr <- quote(NULL)
          }
          param_names <- c(param_names, name)
          param_display <- c(param_display, name)
          param_defaults[[name]] <- default_expr
          param_default_exprs[[name]] <- list(default_expr)
          param_specs[[length(param_specs) + 1]] <- list(
            type = "name",
            formal = name,
            pattern = NULL,
            display = name
          )
        } else {
          stop("lambda arguments must be symbols, (name default) pairs, or (pattern <pat> [default])")
        }
      } else {
        stop("lambda arguments must be symbols, (name default) pairs, or (pattern <pat> [default])")
      }
    }
  }

  list(
    param_specs = param_specs,
    param_names = param_names,
    param_display = param_display,
    param_defaults = param_defaults,
    param_default_exprs = param_default_exprs,
    rest_param = rest_param,
    rest_param_spec = rest_param_spec
  )
}

rye_eval_package_access <- function(expr, env, op_name) {
  if (length(expr) != 3) {
    stop(sprintf("%s requires exactly 2 arguments: (%s pkg name)", op_name, op_name))
  }

  pkg_name <- rye_symbol_or_string(expr[[2]], "Package name must be a symbol or string")
  obj_name <- rye_symbol_or_string(expr[[3]], "Function/object name must be a symbol or string")
  access_call <- as.call(list(as.symbol(op_name), as.symbol(pkg_name), as.symbol(obj_name)))
  eval(access_call, envir = env)
}

rye_special_forms <- list(
  quote = function(expr, env, op_name) {
    if (length(expr) != 2) {
      stop("quote requires exactly 1 argument")
    }
    rye_strip_src(expr[[2]])
  },
  quasiquote = function(expr, env, op_name) {
    if (length(expr) != 2) {
      stop("quasiquote requires exactly 1 argument")
    }
    result <- rye_quasiquote(expr[[2]], env)
    if (!exists(".rye_macroexpanding", envir = env, inherits = TRUE) ||
        !isTRUE(get(".rye_macroexpanding", envir = env, inherits = TRUE))) {
      result <- rye_hygiene_unwrap(result)
    }
    rye_strip_src(result)
  },
  delay = function(expr, env, op_name) {
    if (length(expr) != 2) {
      stop("delay requires exactly 1 argument")
    }
    rye_promise_new(rye_strip_src(expr[[2]]), env)
  },
  help = function(expr, env, op_name) {
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
    NULL
  },
  defmacro = function(expr, env, op_name) {
    if (length(expr) < 4) {
      stop("defmacro requires at least 3 arguments: (defmacro name (params...) body...)")
    }

    name <- expr[[2]]
    if (!is.symbol(name)) {
      stop("defmacro requires a symbol as the first argument")
    }

    params_expr <- expr[[3]]
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

    body <- rye_collect_body(expr, 4)
    doc_out <- rye_extract_docstring(body)
    body <- doc_out$body
    docstring <- doc_out$docstring

    rye_defmacro(name, params, body, env, docstring = docstring)
    NULL
  },
  `if` = function(expr, env, op_name) {
    if (length(expr) < 3 || length(expr) > 4) {
      stop("if requires 2 or 3 arguments: (if test then [else])")
    }
    test <- rye_eval_inner(expr[[2]], env)
    test <- rye_strip_src(test)
    if (identical(test, FALSE) || is.null(test)) {
      if (length(expr) == 4) {
        return(rye_eval_inner(expr[[4]], env))
      }
      return(NULL)
    }
    rye_eval_inner(expr[[3]], env)
  },
  define = function(expr, env, op_name) {
    if (length(expr) != 3) {
      stop("define requires exactly 2 arguments: (define name value)")
    }
    name <- expr[[2]]
    value <- rye_eval_inner(expr[[3]], env)
    value <- rye_strip_src(value)
    rye_assign_pattern(name, value, env, mode = "define", context = "define")
    NULL
  },
  `set!` = function(expr, env, op_name) {
    if (length(expr) != 3) {
      stop("set! requires exactly 2 arguments: (set! name value)")
    }
    name <- expr[[2]]
    value <- rye_eval_inner(expr[[3]], env)
    value <- rye_strip_src(value)
    rye_assign_pattern(name, value, env, mode = "set", context = "set!")
    NULL
  },
  load = function(expr, env, op_name) {
    if (length(expr) != 2) {
      stop("load requires exactly 1 argument: (load \"path\")")
    }
    path <- rye_eval_inner(expr[[2]], env)
    path <- rye_strip_src(path)
    if (!is.character(path) || length(path) != 1) {
      stop("load requires a single file path string")
    }
    if (!grepl("[/\\\\]", path) && rye_module_exists(path, env = env)) {
      return(NULL)
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
    rye_strip_src(rye_load_file(path, env))
  },
  module = function(expr, env, op_name) {
    if (length(expr) < 3) {
      stop("module requires at least 2 arguments: (module name (export ...) body...)")
    }
    module_name <- rye_symbol_or_string(expr[[2]], "module name must be a symbol or string")

    exports_expr <- expr[[3]]
    if (!is.call(exports_expr) ||
        length(exports_expr) < 1 ||
        !is.symbol(exports_expr[[1]])) {
      stop("module requires an export list: (module name (export ...) body...)")
    }

    export_tag <- as.character(exports_expr[[1]])
    export_all <- FALSE
    exports <- character(0)
    if (identical(export_tag, "export")) {
      if (length(exports_expr) > 1) {
        for (i in 2:length(exports_expr)) {
          item <- exports_expr[[i]]
          if (!is.symbol(item)) {
            stop("module exports must be symbols")
          }
          exports <- c(exports, as.character(item))
        }
      }
    } else if (identical(export_tag, "export-all")) {
      if (length(exports_expr) > 1) {
        stop("export-all does not take any arguments")
      }
      export_all <- TRUE
    } else {
      stop("module requires an export list: (module name (export ...) body...)")
    }

    module_env <- new.env(parent = env)
    assign(".rye_env", TRUE, envir = module_env)
    assign(".rye_module", TRUE, envir = module_env)
    rye_module_register(module_name, module_env, exports, registry_env = env)

    body_exprs <- rye_collect_body(expr, 4)
    if (length(body_exprs) == 0) {
      if (export_all) {
        exports <- setdiff(ls(module_env, all.names = TRUE), ".rye_env")
        rye_module_update_exports(module_name, exports, registry_env = env)
      }
      return(NULL)
    }
    result <- rye_eval_seq(body_exprs, module_env)
    if (export_all) {
      exports <- setdiff(ls(module_env, all.names = TRUE), ".rye_env")
      rye_module_update_exports(module_name, exports, registry_env = env)
    }
    result
  },
  import = function(expr, env, op_name) {
    if (length(expr) != 2) {
      stop("import requires exactly 1 argument: (import name)")
    }
    module_name <- rye_symbol_or_string(expr[[2]], "import requires a module name symbol or string")

    if (!rye_module_exists(module_name, env = env)) {
      module_path <- rye_resolve_module_path(module_name)
      if (is.null(module_path)) {
        stop(sprintf("Module not found: %s", module_name))
      }
      rye_load_file(module_path, env)
      if (!rye_module_exists(module_name, env = env)) {
        stop(sprintf("Module '%s' did not register itself", module_name))
      }
    }

    rye_module_attach(module_name, env)
    NULL
  },
  lambda = function(expr, env, op_name) {
    if (length(expr) < 3) {
      stop("lambda requires at least 2 arguments: (lambda (args...) body...)")
    }

    args_expr <- expr[[2]]
    arg_items <- rye_lambda_arg_items(args_expr)
    params <- rye_lambda_parse_params(arg_items)
    param_specs <- params$param_specs
    param_names <- params$param_names
    param_display <- params$param_display
    param_defaults <- params$param_defaults
    param_default_exprs <- params$param_default_exprs
    rest_param <- params$rest_param
    rest_param_spec <- params$rest_param_spec

    all_names <- c(param_names, if (!is.null(rest_param)) rest_param)
    if (length(all_names) > 0 && any(duplicated(all_names))) {
      stop("lambda argument names must be unique")
    }

    formals_list <- list()
    if (length(param_names) > 0) {
      for (name in param_names) {
        formals_list[[name]] <- param_defaults[[name]]
      }
    }
    if (!is.null(rest_param) || !is.null(rest_param_spec)) {
      formals_list[["..."]] <- quote(expr = )
    }

    body_exprs <- rye_collect_body(expr, 3)
    doc_out <- rye_extract_docstring(body_exprs)
    body_exprs <- doc_out$body
    docstring <- doc_out$docstring

    parent_env <- env

    fn <- function(...) {
      fn_env <- new.env(parent = parent_env)

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

      result <- NULL
      if (length(body_exprs) > 0) {
        for (i in seq_along(body_exprs)) {
          result <- rye_strip_src(rye_eval(body_exprs[[i]], fn_env))
        }
      }
      result
    }

    formals(fn) <- formals_list
    class(fn) <- c("rye_closure", class(fn))
    attr(fn, "rye_closure") <- list(
      params = param_names,
      param_specs = param_specs,
      param_display = param_display,
      defaults = param_default_exprs,
      rest_param = rest_param,
      rest_param_spec = rest_param_spec,
      body_exprs = body_exprs,
      parent_env = parent_env
    )
    if (!is.null(docstring)) {
      attr(fn, "rye_doc") <- list(description = docstring)
    }

    fn
  },
  begin = function(expr, env, op_name) {
    if (length(expr) == 1) {
      return(NULL)
    }
    rye_eval_seq(as.list(expr)[-1], env)
  },
  `~` = function(expr, env, op_name) {
    formula_parts <- list(as.symbol("~"))
    if (length(expr) > 1) {
      for (i in 2:length(expr)) {
        formula_parts <- c(formula_parts, list(expr[[i]]))
      }
    }
    formula_call <- as.call(formula_parts)
    eval(formula_call, envir = env)
  },
  `::` = function(expr, env, op_name) {
    rye_eval_package_access(expr, env, op_name)
  },
  `:::` = function(expr, env, op_name) {
    rye_eval_package_access(expr, env, op_name)
  }
)

rye_eval_inner <- function(expr, env) {
  # Source tracking:
  # We *must not* pop the src stack on error, because `rye_with_error_context()`
  # captures `rye_src_stack_get()` to include Location/Rye stack in errors.
  #
  # If we used `on.exit(pop())`, unwinding would pop before the error wrapper
  # runs, losing location context (see `tests/testthat/test-evaluator.R`).
  src <- rye_src_get(expr)
  if (is.null(src)) {
    return(rye_eval_inner_impl(expr, env))
  }
  rye_src_stack_push(src)
  result <- tryCatch(
    rye_eval_inner_impl(expr, env),
    error = function(e) stop(e)
  )
  rye_src_stack_pop()
  result
}

rye_eval_inner_impl <- function(expr, env) {

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
    if (is_macro(expr[[1]], env = env)) {
      expanded <- rye_macroexpand(expr, env, preserve_src = TRUE)
      return(rye_eval_inner(expanded, env))
    }
  }

  # Special forms handling
  op <- expr[[1]]
  op_name <- if (is.symbol(op)) as.character(op) else NULL

  handler <- if (!is.null(op_name)) rye_special_forms[[op_name]] else NULL
  if (!is.null(handler)) {
    return(handler(expr, env, op_name))
  }

  # Regular function application
  # Evaluate operator
  fn <- rye_eval_inner(op, env)
  fn <- rye_strip_src(fn)
  args_info <- rye_eval_args(expr, env)
  args <- args_info$args
  if (length(args_info$arg_names) > 0) {
    names(args) <- args_info$arg_names
  }
  result <- rye_apply(fn, args, env)
  return(rye_strip_src(result))
}

rye_eval_seq <- function(exprs, env) {
  if (length(exprs) == 0) {
    return(NULL)
  }
  result <- NULL
  for (i in seq_along(exprs)) {
    result <- rye_eval_inner(exprs[[i]], env)
  }
  return(result)
}

rye_eval_args <- function(expr, env) {
  # Pre-allocate to avoid O(n^2) vector growing, but handle NULL values correctly
  # Note: args[[i]] <- NULL doesn't work (removes element), so we pre-allocate
  max_args <- max(0, length(expr) - 1)
  args <- vector("list", max_args)
  arg_names <- character(max_args)
  arg_idx <- 1

  i <- 2
  while (i <= length(expr)) {
    arg_expr <- expr[[i]]
    if (inherits(arg_expr, "rye_keyword")) {
      if (i + 1 > length(expr)) {
        stop(sprintf("Keyword :%s requires a value", arg_expr))
      }
      keyword_name <- as.character(arg_expr)
      value <- rye_eval_inner(expr[[i + 1]], env)
      args[[arg_idx]] <- rye_strip_src(value)
      arg_names[[arg_idx]] <- keyword_name
      arg_idx <- arg_idx + 1
      i <- i + 2
    } else {
      value <- rye_eval_inner(arg_expr, env)
      args[[arg_idx]] <- rye_strip_src(value)
      arg_names[[arg_idx]] <- ""
      arg_idx <- arg_idx + 1
      i <- i + 1
    }
  }

  # Trim to actual size
  actual_size <- arg_idx - 1
  if (actual_size == 0) {
    return(list(args = list(), arg_names = character(0)))
  }
  args <- args[1:actual_size]
  arg_names <- arg_names[1:actual_size]
  return(list(args = args, arg_names = arg_names))
}

rye_apply <- function(fn, args, env) {
  if (inherits(fn, "rye_closure")) {
    return(rye_apply_closure(fn, args, env))
  }
  if (!is.function(fn)) {
    stop("attempt to call non-function")
  }
  result <- rye_do_call(fn, args)
  return(result)
}

rye_apply_closure <- function(fn, args, env) {
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
  formal_params <- info$params
  if (is.null(formal_params)) {
    formal_params <- character(0)
  }
  has_rest <- !is.null(info$rest_param_spec) || !is.null(info$rest_param)
  if (has_rest) {
    for (idx in seq_along(matched_args)) {
      name <- matched_names[[idx]]
      if (!nzchar(name) || !(name %in% formal_params)) {
        rest_args <- c(rest_args, list(rye_strip_src(matched_args[[idx]])))
        if (nzchar(name)) {
          names(rest_args)[length(rest_args)] <- name
        }
      }
    }
  } else {
    for (idx in seq_along(matched_args)) {
      name <- matched_names[[idx]]
      if (!nzchar(name) || !(name %in% formal_params)) {
        stop("lambda got unexpected arguments")
      }
    }
  }

  # Bind parameters with defaults
  param_specs <- info$param_specs
  if (is.null(param_specs)) {
    param_specs <- lapply(info$params, function(name) {
      list(type = "name", formal = name, pattern = NULL, display = name)
    })
  }

  for (idx in seq_along(param_specs)) {
    spec <- param_specs[[idx]]
    name <- spec$formal
    display <- spec$display
    if (is.null(display) || !nzchar(display)) {
      display <- name
    }

    if (name %in% matched_names) {
      value <- matched_args[[name]]
      value <- rye_strip_src(value)
      if (identical(spec$type, "pattern")) {
        rye_destructure_bind(spec$pattern, value, fn_env, mode = "define")
      } else {
        assign(name, value, envir = fn_env)
      }
    } else {
      default_expr <- info$defaults[[name]][[1]]
      if (inherits(default_expr, "rye_missing_default")) {
        stop(sprintf("lambda missing argument: %s", display))
      }
      value <- rye_eval_inner(default_expr, fn_env)
      value <- rye_strip_src(value)
      if (identical(spec$type, "pattern")) {
        rye_destructure_bind(spec$pattern, value, fn_env, mode = "define")
      } else {
        assign(name, value, envir = fn_env)
      }
    }
  }

  # Bind rest parameter if present
  if (has_rest) {
    rest_spec <- info$rest_param_spec
    if (is.null(rest_spec)) {
      assign(info$rest_param, rest_args, envir = fn_env)
    } else if (identical(rest_spec$type, "pattern")) {
      rye_destructure_bind(rest_spec$pattern, rest_args, fn_env, mode = "define")
    } else {
      assign(rest_spec$name, rest_args, envir = fn_env)
    }
  }

  # Evaluate body expressions
  return(rye_eval_seq(info$body_exprs, fn_env))
}
