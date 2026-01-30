rye_missing_default <- function() {
  structure(list(), class = "rye_missing_default")
}

rye_promise_value_key <- ".rye_promise_value"
rye_promise_expr_key <- ".rye_promise_expr"
rye_promise_env_key <- ".rye_promise_env"
rye_promise_eval_key <- ".rye_promise_eval"

#' @export
print.rye_promise <- function(x, ...) {
  cat("<promise>\n")
  invisible(x)
}

#' Evaluator for Rye expressions
#'
#' @keywords internal
#' @noRd
Evaluator <- R6::R6Class(
  "Evaluator",
  public = list(
    env = NULL,
    macro_expander = NULL,
    source_tracker = NULL,
    engine = NULL,
    special_forms = NULL,
    initialize = function(env, macro_expander, source_tracker, engine = NULL) {
      if (!inherits(env, "RyeEnv")) {
        stop("Evaluator requires a RyeEnv")
      }
      self$env <- env
      self$macro_expander <- macro_expander
      self$source_tracker <- source_tracker
      self$engine <- engine
      self$special_forms <- self$build_special_forms()
    },
    eval = function(expr) {
      self$eval_in_env(expr, self$env$env)
    },
    eval_in_env = function(expr, env) {
      if (inherits(env, "RyeEnv")) {
        env <- env$env
      } else if (is.null(env)) {
        env <- self$env$env
      }
      if (!is.environment(env)) {
        stop("Expected a RyeEnv or environment")
      }
      self$env$push_env(env)
      on.exit(self$env$pop_env(), add = TRUE)
      withCallingHandlers({
        self$eval_inner(expr, env)
      }, error = function(e) {
        stop(e)
      })
    },
    eval_seq = function(exprs) {
      self$eval_seq_in_env(exprs, self$env$env)
    },
    eval_seq_in_env = function(exprs, env) {
      if (inherits(env, "RyeEnv")) {
        env <- env$env
      } else if (is.null(env)) {
        env <- self$env$env
      }
      if (!is.environment(env)) {
        stop("Expected a RyeEnv or environment")
      }
      if (length(exprs) == 0) {
        return(NULL)
      }
      self$env$push_env(env)
      on.exit(self$env$pop_env(), add = TRUE)
      result <- NULL
      for (i in seq_along(exprs)) {
        result <- self$eval_inner(exprs[[i]], env)
      }
      result
    },
    eval_inner = function(expr, env) {
      # Source tracking:
      # We *must not* pop the src stack on error, because `SourceTracker$with_error_context()`
      # captures `source_tracker$get()` to include Location/Rye stack in errors.
      #
      # If we used `on.exit(pop())`, unwinding would pop before the error wrapper
      # runs, losing location context (see `tests/testthat/test-evaluator.R`).
      src <- self$source_tracker$src_get(expr)
      if (is.null(src)) {
        return(self$eval_inner_impl(expr, env))
      }
      self$source_tracker$push(src)
      result <- tryCatch(
        self$eval_inner_impl(expr, env),
        error = function(e) stop(e)
      )
      self$source_tracker$pop()
      result
    },
    eval_inner_impl = function(expr, env) {
      # Handle NULL (empty list or #nil)
      if (is.null(expr)) {
        return(NULL)
      }

      # Handle atoms (self-evaluating)
      if (!is.call(expr) && !is.symbol(expr)) {
        # Keywords are self-evaluating (return as-is for now)
        return(self$source_tracker$strip_src(expr))
      }

      # Handle keywords (they pass through as-is for use in function calls)
      if (inherits(expr, "rye_keyword")) {
        return(self$source_tracker$strip_src(expr))
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
        if (self$macro_expander$is_macro(expr[[1]], env = env)) {
          expanded <- self$macro_expander$macroexpand(expr, env = env, preserve_src = TRUE)
          return(self$eval_inner(expanded, env))
        }
      }

      # Special forms handling
      op <- expr[[1]]
      op_name <- if (is.symbol(op)) as.character(op) else NULL

      handler <- if (!is.null(op_name)) self$special_forms[[op_name]] else NULL
      if (!is.null(handler)) {
        return(handler(expr, env, op_name))
      }

      # Regular function application
      # Evaluate operator
      fn <- self$eval_inner(op, env)
      fn <- self$source_tracker$strip_src(fn)
      args_info <- self$eval_args(expr, env)
      args <- args_info$args
      if (length(args_info$arg_names) > 0) {
        names(args) <- args_info$arg_names
      }
      result <- self$apply(fn, args, env)
      return(self$source_tracker$strip_src(result))
    },
    eval_args = function(expr, env) {
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
          value <- self$eval_inner(expr[[i + 1]], env)
          args[[arg_idx]] <- self$source_tracker$strip_src(value)
          arg_names[[arg_idx]] <- keyword_name
          arg_idx <- arg_idx + 1
          i <- i + 2
        } else {
          value <- self$eval_inner(arg_expr, env)
          args[[arg_idx]] <- self$source_tracker$strip_src(value)
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
    },
    apply = function(fn, args, env) {
      if (inherits(fn, "rye_closure")) {
        return(self$apply_closure(fn, args, env))
      }
      if (!is.function(fn)) {
        stop("attempt to call non-function")
      }
      result <- private$do_call_impl(fn, args, env)
      return(result)
    },
    quote_arg = function(value, quote_symbols = TRUE) {
      private$quote_arg_impl(value, quote_symbols = quote_symbols)
    },
    do_call = function(fn, args, env = NULL) {
      if (is.null(env)) {
        env <- self$env$env
      }
      private$do_call_impl(fn, args, env)
    },
    promise_new = function(expr, env) {
      promise_env <- new.env(parent = emptyenv())
      assign(rye_promise_expr_key, expr, envir = promise_env)
      assign(rye_promise_env_key, env, envir = promise_env)
      assign(rye_promise_eval_key, self$eval_in_env, envir = promise_env)
      delayedAssign(
        rye_promise_value_key,
        .rye_promise_eval(.rye_promise_expr, .rye_promise_env),
        eval.env = promise_env,
        assign.env = promise_env
      )
      class(promise_env) <- c("rye_promise", class(promise_env))
      lockEnvironment(promise_env, bindings = FALSE)
      promise_env
    },
    apply_closure = function(fn, args, env) {
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
            rest_args <- c(rest_args, list(self$source_tracker$strip_src(matched_args[[idx]])))
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
          value <- self$source_tracker$strip_src(value)
          if (identical(spec$type, "pattern")) {
            RyeEnv$new(fn_env)$destructure_bind(spec$pattern, value, mode = "define")
          } else {
            assign(name, value, envir = fn_env)
          }
        } else {
          default_expr <- info$defaults[[name]][[1]]
          if (inherits(default_expr, "rye_missing_default")) {
            stop(sprintf("lambda missing argument: %s", display))
          }
          value <- self$eval_inner(default_expr, fn_env)
          value <- self$source_tracker$strip_src(value)
          if (identical(spec$type, "pattern")) {
            RyeEnv$new(fn_env)$destructure_bind(spec$pattern, value, mode = "define")
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
          RyeEnv$new(fn_env)$destructure_bind(rest_spec$pattern, rest_args, mode = "define")
        } else {
          assign(rest_spec$name, rest_args, envir = fn_env)
        }
      }

      # Evaluate body expressions
      return(self$eval_seq_in_env(info$body_exprs, fn_env))
    },
    eval_package_access = function(expr, env, op_name) {
      if (length(expr) != 3) {
        stop(sprintf("%s requires exactly 2 arguments: (%s pkg name)", op_name, op_name))
      }

      pkg_name <- RyeEnv$new(env)$symbol_or_string(expr[[2]], "Package name must be a symbol or string")
      obj_name <- RyeEnv$new(env)$symbol_or_string(expr[[3]], "Function/object name must be a symbol or string")
      access_call <- as.call(list(as.symbol(op_name), as.symbol(pkg_name), as.symbol(obj_name)))
      eval(access_call, envir = env)
    },
    build_special_forms = function() {
      list(
        quote = function(expr, env, op_name) {
          if (length(expr) != 2) {
            stop("quote requires exactly 1 argument")
          }
          self$source_tracker$strip_src(expr[[2]])
        },
        quasiquote = function(expr, env, op_name) {
          if (length(expr) != 2) {
            stop("quasiquote requires exactly 1 argument")
          }
          result <- self$macro_expander$quasiquote(expr[[2]], env)
          if (!exists(".rye_macroexpanding", envir = env, inherits = TRUE) ||
              !isTRUE(get(".rye_macroexpanding", envir = env, inherits = TRUE))) {
            result <- self$macro_expander$hygiene_unwrap(result)
          }
          self$source_tracker$strip_src(result)
        },
        delay = function(expr, env, op_name) {
          if (length(expr) != 2) {
            stop("delay requires exactly 1 argument")
          }
          self$promise_new(self$source_tracker$strip_src(expr[[2]]), env)
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
          if (is.null(self$engine)) {
            stop("help requires an engine")
          }
          self$engine$help_in_env(topic, env)
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

          body <- private$collect_body(expr, 4)
          doc_out <- private$extract_docstring(body)
          body <- doc_out$body
          docstring <- doc_out$docstring

          self$macro_expander$defmacro(name, params, body, docstring = docstring, env = env)
          NULL
        },
        `if` = function(expr, env, op_name) {
          if (length(expr) < 3 || length(expr) > 4) {
            stop("if requires 2 or 3 arguments: (if test then [else])")
          }
          test <- self$eval_inner(expr[[2]], env)
          test <- self$source_tracker$strip_src(test)
          if (identical(test, FALSE) || is.null(test)) {
            if (length(expr) == 4) {
              return(self$eval_inner(expr[[4]], env))
            }
            return(NULL)
          }
          self$eval_inner(expr[[3]], env)
        },
        define = function(expr, env, op_name) {
          if (length(expr) != 3) {
            stop("define requires exactly 2 arguments: (define name value)")
          }
          name <- expr[[2]]
          value <- self$eval_inner(expr[[3]], env)
          value <- self$source_tracker$strip_src(value)
          RyeEnv$new(env)$assign_pattern(name, value, mode = "define", context = "define")
          NULL
        },
        `set!` = function(expr, env, op_name) {
          if (length(expr) != 3) {
            stop("set! requires exactly 2 arguments: (set! name value)")
          }
          name <- expr[[2]]
          value <- self$eval_inner(expr[[3]], env)
          value <- self$source_tracker$strip_src(value)
          RyeEnv$new(env)$assign_pattern(name, value, mode = "set", context = "set!")
          NULL
        },
        load = function(expr, env, op_name) {
          if (length(expr) != 2) {
            stop("load requires exactly 1 argument: (load \"path\")")
          }
          path <- self$eval_inner(expr[[2]], env)
          path <- self$source_tracker$strip_src(path)
          if (!is.character(path) || length(path) != 1) {
            stop("load requires a single file path string")
          }
          if (!grepl("[/\\\\]", path) && RyeEnv$new(env)$module_registry$exists(path)) {
            return(NULL)
          }
          has_separator <- grepl("[/\\\\]", path)
          if (!has_separator) {
            stdlib_path <- rye_resolve_stdlib_path(path)
            if (!is.null(stdlib_path)) {
              return(self$source_tracker$strip_src(self$engine$load_file_in_env(stdlib_path, env)))
            }
            if (file.exists(path)) {
              return(self$source_tracker$strip_src(self$engine$load_file_in_env(path, env)))
            }
            stop(sprintf("File not found: %s", path))
          }
          self$source_tracker$strip_src(self$engine$load_file_in_env(path, env))
        },
        module = function(expr, env, op_name) {
          if (length(expr) < 3) {
            stop("module requires at least 2 arguments: (module name (export ...) body...)")
          }
          module_name <- RyeEnv$new(env)$symbol_or_string(expr[[2]], "module name must be a symbol or string")

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
          assign(".rye_module", TRUE, envir = module_env)
          RyeEnv$new(env)$module_registry$register(module_name, module_env, exports)

          body_exprs <- private$collect_body(expr, 4)
          if (length(body_exprs) == 0) {
            if (export_all) {
              exports <- setdiff(ls(module_env, all.names = TRUE), ".rye_module")
              RyeEnv$new(env)$module_registry$update_exports(module_name, exports)
            }
            return(NULL)
          }
          result <- self$eval_seq_in_env(body_exprs, module_env)
          if (export_all) {
            exports <- setdiff(ls(module_env, all.names = TRUE), ".rye_module")
            RyeEnv$new(env)$module_registry$update_exports(module_name, exports)
          }
          result
        },
        import = function(expr, env, op_name) {
          if (length(expr) != 2) {
            stop("import requires exactly 1 argument: (import name)")
          }
          module_name <- RyeEnv$new(env)$symbol_or_string(expr[[2]], "import requires a module name symbol or string")

          if (!RyeEnv$new(env)$module_registry$exists(module_name)) {
            module_path <- rye_resolve_module_path(module_name)
            if (is.null(module_path)) {
              stop(sprintf("Module not found: %s", module_name))
            }
            self$engine$load_file_in_env(module_path, env)
            if (!RyeEnv$new(env)$module_registry$exists(module_name)) {
              stop(sprintf("Module '%s' did not register itself", module_name))
            }
          }

          RyeEnv$new(env)$module_registry$attach(module_name)
          NULL
        },
        lambda = function(expr, env, op_name) {
          if (length(expr) < 3) {
            stop("lambda requires at least 2 arguments: (lambda (args...) body...)")
          }

          args_expr <- expr[[2]]
          arg_items <- private$lambda_arg_items(args_expr)
          params <- private$lambda_parse_params(arg_items)
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

          body_exprs <- private$collect_body(expr, 3)
          doc_out <- private$extract_docstring(body_exprs)
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
                result <- self$source_tracker$strip_src(self$eval_in_env(body_exprs[[i]], fn_env))
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
          self$eval_seq_in_env(as.list(expr)[-1], env)
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
          self$eval_package_access(expr, env, op_name)
        },
        `:::` = function(expr, env, op_name) {
          self$eval_package_access(expr, env, op_name)
        }
      )
    }
  ),
  private = list(
    collect_body = function(expr, start_idx) {
      body <- list()
      if (length(expr) >= start_idx) {
        for (i in start_idx:length(expr)) {
          body[[length(body) + 1]] <- expr[[i]]
        }
      }
      body
    },
    extract_docstring = function(body_exprs) {
      docstring <- NULL
      if (length(body_exprs) > 0) {
        first_expr <- self$source_tracker$strip_src(body_exprs[[1]])
        if (is.character(first_expr) && length(first_expr) == 1) {
          docstring <- first_expr
          body_exprs <- body_exprs[-1]
        }
      }
      list(body = body_exprs, docstring = docstring)
    },
    lambda_arg_items = function(args_expr) {
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
    },
    lambda_parse_params = function(arg_items) {
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
              tmp_name <- as.character(self$macro_expander$gensym(".__rye_arg"))
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
    },
    quote_arg_impl = function(value, quote_symbols = TRUE) {
      if (is.call(value) || (quote_symbols && is.symbol(value))) {
        return(as.call(list(as.symbol("quote"), value)))
      }
      value
    },
    do_call_impl = function(fn, args, env) {
      # Special handling for $, [, [[ - don't quote symbols
      if (identical(fn, base::`$`) || identical(fn, base::`[`) || identical(fn, base::`[[`)) {
        args <- lapply(args, private$quote_arg_impl, quote_symbols = FALSE)
        return(do.call(fn, args))
      }

      # For rye_no_quote functions, always quote symbols and calls to prevent
      # premature evaluation during promise forcing, but do it in the Rye environment
      # so symbols are looked up correctly.
      if (isTRUE(attr(fn, "rye_no_quote"))) {
        args <- lapply(args, private$quote_arg_impl, quote_symbols = TRUE)
        return(do.call(fn, args, envir = env))
      }

      # For other functions, quote symbols and calls as usual
      args <- lapply(args, private$quote_arg_impl, quote_symbols = TRUE)
      do.call(fn, args)
    }
  )
)
