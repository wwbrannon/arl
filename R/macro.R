# MacroExpander: Defines and expands Rye macros. Handles defmacro, macroexpand,
# macroexpand_1, quasiquote/unquote, gensym, hygiene. Shares EvalContext with Evaluator.
#
# @field context EvalContext (env, source_tracker).
#
#' @keywords internal
#' @noRd
MacroExpander <- R6::R6Class(
  "MacroExpander",
  public = list(
    context = NULL,
    # @description Create macro expander.
    # @param context EvalContext instance.
    initialize = function(context) {
      if (!r6_isinstance(context, "EvalContext")) {
        stop("MacroExpander requires an EvalContext")
      }
      self$context <- context
    },
    # @description Define a macro. Registers name in env's macro registry.
    # @param name Symbol or character macro name.
    # @param params Formal parameter list.
    # @param body Macro body (unevaluated).
    # @param docstring Optional string for (help name).
    # @param env Target environment or NULL for context$env$env.
    defmacro = function(name, params, body, docstring = NULL, env = NULL) {
      target_env <- private$normalize_env(env)
      private$define_macro(name, params, body, target_env, docstring = docstring)
      invisible(NULL)
    },
    # @description Recursively expand macros in expr.
    # @param expr Rye expression.
    # @param env Environment for macro lookup or NULL.
    # @param preserve_src Whether to keep source attributes.
    # @return Expanded expression.
    macroexpand = function(expr, env = NULL, preserve_src = FALSE) {
      target_env <- private$normalize_env(env)
      private$macroexpand_impl(expr, target_env, preserve_src, max_depth = Inf, walk = TRUE)
    },
    # @description Expand one layer of macros only.
    # @param expr Rye expression.
    # @param env Environment or NULL.
    # @param preserve_src Whether to keep source attributes.
    # @return Expanded expression.
    macroexpand_1 = function(expr, env = NULL, preserve_src = FALSE) {
      target_env <- private$normalize_env(env)
      private$macroexpand_impl(expr, target_env, preserve_src, max_depth = 1, walk = FALSE)
    },
    # @description Check if name is bound to a macro in env.
    # @param name Symbol or character.
    # @param env Environment or NULL.
    # @return Logical.
    is_macro = function(name, env = NULL) {
      target_env <- private$normalize_env(env)
      private$is_macro_impl(name, target_env)
    },
    # @description Get the macro function for name in env.
    # @param name Symbol or character.
    # @param env Environment or NULL.
    # @return Macro function or error.
    get_macro = function(name, env = NULL) {
      target_env <- private$normalize_env(env)
      private$get_macro_impl(name, target_env)
    },
    # @description Capture (macroexpand preserving one symbol). Used for macro hygiene.
    # @param symbol Symbol to preserve.
    # @param expr Expression to expand.
    # @return Expanded expression with symbol unquoted.
    capture = function(symbol, expr) {
      private$capture_impl(symbol, expr)
    },
    # @description Unwrap hygiene wrappers from an expression.
    # @param expr Expression possibly containing hygiene wrappers.
    # @return Expression.
    hygiene_unwrap = function(expr) {
      private$hygiene_unwrap_impl(expr)
    },
    # @description Generate a unique symbol (for macro hygiene).
    # @param prefix Character prefix for the symbol name.
    # @return New symbol.
    gensym = function(prefix = "G") {
      private$gensym_impl(prefix = prefix)
    },
    # @description Expand quasiquote (backtick) with unquote (comma) in expr.
    # @param expr Quasiquoted expression.
    # @param env Environment for unquote evaluation.
    # @param depth Unquote depth (for nested quasiquote).
    # @return Expanded expression.
    quasiquote = function(expr, env, depth = 1) {
      target_env <- private$normalize_env(env)
      private$quasiquote_impl(expr, target_env, depth = depth)
    }
  ),
  private = list(
    gensym_counter = 0,
    hygiene_counter = 0,
    eval_compiled_in_env = function(expr, env) {
      if (is.null(self$context$compiled_runtime) || is.null(self$context$compiler)) {
        stop("compiled runtime not initialized")
      }
      if (!is.null(self$context$macro_expander)) {
        expr <- self$context$macro_expander$macroexpand(expr, env = env, preserve_src = TRUE)
      }
      prev_strict <- self$context$compiler$strict
      prev_macro_eval <- self$context$compiler$macro_eval
      on.exit({
      self$context$compiler$strict <- prev_strict
      self$context$compiler$macro_eval <- prev_macro_eval
    }, add = TRUE)
    self$context$compiler$macro_eval <- TRUE
    compiled <- self$context$compiler$compile(expr, env, strict = TRUE)
    self$context$compiled_runtime$eval_compiled(compiled, env)
  },
    normalize_env = function(env) {
      if (r6_isinstance(env, "RyeEnv")) {
        return(env$env)
      }
      if (is.environment(env)) {
        return(env)
      }
      if (is.null(env)) {
        return(self$context$env$env)
      }
      stop("Expected a RyeEnv or environment")
    },
    macro_registry = function(env, create = TRUE) {
      self$context$env$get_registry(".rye_macros", env, create = create)
    },
    is_macro_impl = function(name, env) {
      if (!is.symbol(name)) {
        return(FALSE)
      }
      registry <- private$macro_registry(env, create = FALSE)
      !is.null(registry) && exists(as.character(name), envir = registry, inherits = FALSE)
    },
    get_macro_impl = function(name, env) {
      registry <- private$macro_registry(env, create = FALSE)
      if (is.null(registry)) {
        return(NULL)
      }
      registry[[as.character(name)]]
    },
    gensym_impl = function(prefix = "G") {
      private$gensym_counter <- private$gensym_counter + 1
      as.symbol(paste0(prefix, "__", private$gensym_counter))
    },
    hygiene_gensym = function(prefix = "H") {
      private$hygiene_counter <- private$hygiene_counter + 1
      as.symbol(paste0(prefix, "__h", private$hygiene_counter))
    },
    hygiene_wrap = function(expr, origin) {
      structure(list(expr = expr, origin = origin), class = "rye_syntax")
    },
    hygiene_is = function(x) {
      inherits(x, "rye_syntax")
    },
    hygiene_origin = function(x) {
      if (private$hygiene_is(x)) {
        return(x$origin)
      }
      NULL
    },
    hygiene_expr = function(x) {
      if (private$hygiene_is(x)) {
        return(x$expr)
      }
      x
    },
    hygiene_unwrap_impl = function(expr) {
      if (private$hygiene_is(expr)) {
        return(private$hygiene_unwrap_impl(expr$expr))
      }
      private$map_expr(expr, private$hygiene_unwrap_impl)
    },
    map_expr = function(expr, fn, ...) {
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
    },
    capture_impl = function(symbol, expr) {
      name <- NULL
      if (is.symbol(symbol)) {
        name <- as.character(symbol)
      } else if (is.character(symbol) && length(symbol) == 1) {
        name <- symbol
      }
      if (is.null(name) || !nzchar(name)) {
        stop("capture expects a symbol or single string name")
      }
      private$capture_mark(expr, name)
    },
    capture_mark = function(expr, name) {
      if (private$hygiene_is(expr)) {
        origin <- private$hygiene_origin(expr)
        inner <- private$capture_mark(private$hygiene_expr(expr), name)
        return(private$hygiene_wrap(inner, origin))
      }
      if (is.symbol(expr) && identical(as.character(expr), name)) {
        return(private$hygiene_wrap(expr, "introduced"))
      }
      private$map_expr(expr, private$capture_mark, name = name)
    },
    hygienize = function(expr) {
      private$hygienize_expr(expr, env = list(), protected = FALSE)
    },
    hygienize_expr = function(expr, env, protected) {
      if (private$hygiene_is(expr)) {
        origin <- private$hygiene_origin(expr)
        inner <- private$hygiene_expr(expr)
        if (identical(origin, "call_site")) {
          return(private$hygienize_expr(inner, env, protected = TRUE))
        }
        if (identical(origin, "introduced")) {
          return(private$hygienize_expr(inner, env, protected = FALSE))
        }
        return(private$hygienize_expr(inner, env, protected = protected))
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
          updated <- lapply(expr, private$hygienize_expr, env = env, protected = protected)
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
        updated <- lapply(as.list(expr), private$hygienize_expr, env = env, protected = TRUE)
        return(as.call(updated))
      }

      if (is.symbol(op)) {
        op_name <- as.character(op)
        if (op_name == "begin") {
          return(private$hygienize_begin(expr, env))
        }
        if (op_name == "define") {
          return(private$hygienize_define(expr, env)$expr)
        }
        if (op_name == "lambda") {
          return(private$hygienize_lambda(expr, env))
        }
        if (op_name %in% c("let", "let*", "letrec")) {
          return(private$hygienize_let(expr, env, op_name))
        }
      }

      updated <- lapply(as.list(expr), private$hygienize_expr, env = env, protected = FALSE)
      as.call(updated)
    },
    hygienize_begin = function(expr, env) {
      result <- list(expr[[1]])
      current_env <- env
      if (length(expr) > 1) {
        for (i in 2:length(expr)) {
          form <- expr[[i]]
          if (is.call(form) && length(form) >= 2 && is.symbol(form[[1]]) &&
              as.character(form[[1]]) == "define") {
            out <- private$hygienize_define(form, current_env)
            result[[i]] <- out$expr
            current_env <- out$env
          } else {
            result[[i]] <- private$hygienize_expr(form, current_env, protected = FALSE)
          }
        }
      }
      as.call(result)
    },
    hygienize_define = function(expr, env) {
      result <- list(expr[[1]])
      name_expr <- expr[[2]]
      name_origin <- private$hygiene_origin(name_expr)
      name_expr <- private$hygiene_expr(name_expr)
      new_env <- env
      if (is.symbol(name_expr) && !identical(name_origin, "call_site")) {
        name <- as.character(name_expr)
        fresh <- private$hygiene_gensym(name)
        new_env[[name]] <- fresh
        result[[2]] <- fresh
      } else if (is.call(name_expr) || (is.list(name_expr) && is.null(attr(name_expr, "class", exact = TRUE)))) {
        pattern_out <- private$hygienize_define_pattern(expr[[2]], new_env)
        result[[2]] <- pattern_out$expr
        new_env <- pattern_out$env
      } else {
        result[[2]] <- private$hygienize_expr(expr[[2]], env, protected = FALSE)
      }
      if (length(expr) >= 3) {
        result[[3]] <- private$hygienize_expr(expr[[3]], env, protected = FALSE)
      }
      list(expr = as.call(result), env = new_env)
    },
    hygienize_define_pattern = function(pattern, env, protected = FALSE) {
      if (isTRUE(protected)) {
        return(list(expr = private$hygienize_expr(pattern, env, protected = TRUE), env = env))
      }
      if (private$hygiene_is(pattern)) {
        origin <- private$hygiene_origin(pattern)
        inner <- private$hygiene_expr(pattern)
        if (identical(origin, "call_site")) {
          return(private$hygienize_define_pattern(inner, env, protected = TRUE))
        }
        if (identical(origin, "introduced")) {
          return(private$hygienize_define_pattern(inner, env, protected = FALSE))
        }
        return(private$hygienize_define_pattern(inner, env, protected = protected))
      }
      if (is.symbol(pattern)) {
        name <- as.character(pattern)
        if (identical(name, ".")) {
          return(list(expr = pattern, env = env))
        }
        fresh <- private$hygiene_gensym(name)
        env[[name]] <- fresh
        return(list(expr = fresh, env = env))
      }
      if (is.call(pattern)) {
        parts <- as.list(pattern)
        updated <- list()
        for (i in seq_along(parts)) {
          out <- private$hygienize_define_pattern(parts[[i]], env, protected = protected)
          updated[[i]] <- out$expr
          env <- out$env
        }
        return(list(expr = as.call(updated), env = env))
      }
      if (is.list(pattern) && is.null(attr(pattern, "class", exact = TRUE))) {
        updated <- list()
        for (i in seq_along(pattern)) {
          out <- private$hygienize_define_pattern(pattern[[i]], env, protected = protected)
          updated[[i]] <- out$expr
          env <- out$env
        }
        if (!is.null(names(pattern))) {
          names(updated) <- names(pattern)
        }
        return(list(expr = updated, env = env))
      }
      list(expr = pattern, env = env)
    },
    hygienize_lambda = function(expr, env) {
      if (length(expr) < 3) {
        return(expr)
      }
      args_expr <- expr[[2]]
      if (!is.call(args_expr)) {
        result <- list(expr[[1]], private$hygienize_expr(args_expr, env, protected = TRUE))
        if (length(expr) > 2) {
          for (i in 3:length(expr)) {
            result[[i]] <- private$hygienize_expr(expr[[i]], env, protected = FALSE)
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
          arg_origin <- private$hygiene_origin(arg)
          arg_unwrapped <- private$hygiene_expr(arg)
          if (is.symbol(arg_unwrapped) && as.character(arg_unwrapped) != "." &&
              !identical(arg_origin, "call_site")) {
            name <- as.character(arg_unwrapped)
            fresh <- private$hygiene_gensym(name)
            new_env[[name]] <- fresh
            new_args[[i]] <- fresh
          } else {
            new_args[[i]] <- private$hygienize_expr(arg, env, protected = TRUE)
          }
        }
      }
      args_out <- if (length(args_list) == 0) args_expr else as.call(new_args)
      result <- list(expr[[1]], args_out)
      if (length(expr) > 2) {
        for (i in 3:length(expr)) {
          result[[i]] <- private$hygienize_expr(expr[[i]], new_env, protected = FALSE)
        }
      }
      as.call(result)
    },
    hygienize_binding_parts = function(binding) {
      parts <- if (is.call(binding)) as.list(binding) else list()
      name_origin <- if (length(parts) >= 1) private$hygiene_origin(parts[[1]]) else NULL
      name_expr <- if (length(parts) >= 1) private$hygiene_expr(parts[[1]]) else NULL
      list(parts = parts, name_origin = name_origin, name_expr = name_expr)
    },
    hygienize_binding_value = function(parts, env) {
      if (length(parts) >= 2) {
        return(private$hygienize_expr(parts[[2]], env, protected = FALSE))
      }
      NULL
    },
    hygienize_let = function(expr, env, op_name) {
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
            info <- private$hygienize_binding_parts(binding)
            parts <- info$parts
            if (length(parts) == 0) {
              next
            }
            name_origin <- info$name_origin
            name_expr <- info$name_expr
            if (is.symbol(name_expr) && !identical(name_origin, "call_site")) {
              name <- as.character(name_expr)
              body_env[[name]] <- private$hygiene_gensym(name)
            }
          }
        }
        if (length(bindings_list) > 0) {
          for (i in seq_along(bindings_list)) {
            binding <- bindings_list[[i]]
            info <- private$hygienize_binding_parts(binding)
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
              value <- private$hygienize_binding_value(parts, body_env)
              new_bindings[[i]] <- as.call(list(renamed, value))
            } else {
              value <- private$hygienize_binding_value(parts, value_env)
              new_bindings[[i]] <- as.call(c(list(parts[[1]]), list(value)))
            }
          }
        }
        bindings_out <- if (length(bindings_list) == 0) bindings_expr else as.call(new_bindings)
        body <- list(expr[[1]], bindings_out)
        if (length(expr) > 2) {
          for (i in 3:length(expr)) {
            body[[i]] <- private$hygienize_expr(expr[[i]], body_env, protected = FALSE)
          }
        }
        return(as.call(body))
      }

      if (op_name == "let*") {
        current_env <- env
        if (length(bindings_list) > 0) {
          for (i in seq_along(bindings_list)) {
            binding <- bindings_list[[i]]
            info <- private$hygienize_binding_parts(binding)
            parts <- info$parts
            if (length(parts) == 0) {
              new_bindings[[i]] <- binding
              next
            }
            name_origin <- info$name_origin
            name_expr <- info$name_expr
            value <- private$hygienize_binding_value(parts, current_env)
            if (is.symbol(name_expr) && !identical(name_origin, "call_site")) {
              name <- as.character(name_expr)
              fresh <- private$hygiene_gensym(name)
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
            body[[i]] <- private$hygienize_expr(expr[[i]], current_env, protected = FALSE)
          }
        }
        return(as.call(body))
      }

      body_env <- env
      if (length(bindings_list) > 0) {
        for (i in seq_along(bindings_list)) {
          binding <- bindings_list[[i]]
          info <- private$hygienize_binding_parts(binding)
          parts <- info$parts
          if (length(parts) == 0) {
            next
          }
          name_origin <- info$name_origin
          name_expr <- info$name_expr
          if (is.symbol(name_expr) && !identical(name_origin, "call_site")) {
            name <- as.character(name_expr)
            body_env[[name]] <- private$hygiene_gensym(name)
          }
        }
      }
      if (length(bindings_list) > 0) {
        for (i in seq_along(bindings_list)) {
          binding <- bindings_list[[i]]
          info <- private$hygienize_binding_parts(binding)
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
            value <- private$hygienize_binding_value(parts, env)
            new_bindings[[i]] <- as.call(list(renamed, value))
          } else {
            value <- private$hygienize_binding_value(parts, env)
            new_bindings[[i]] <- as.call(c(list(parts[[1]]), list(value)))
          }
        }
      }
      bindings_out <- if (length(bindings_list) == 0) bindings_expr else as.call(new_bindings)
      body <- list(expr[[1]], bindings_out)
      if (length(expr) > 2) {
        for (i in 3:length(expr)) {
          body[[i]] <- private$hygienize_expr(expr[[i]], body_env, protected = FALSE)
        }
      }
      as.call(body)
    },
    quasiquote_impl = function(expr, env, depth = 1) {
      if (!is.call(expr)) {
        return(expr)
      }
      if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "unquote") {
        if (depth == 1) {
          if (length(expr) != 2) {
            stop("unquote requires exactly 1 argument")
          }
          value <- private$eval_compiled_in_env(expr[[2]], env)
          return(private$hygiene_wrap(value, "call_site"))
        }
        return(as.call(list(as.symbol("unquote"), private$quasiquote_impl(expr[[2]], env, depth - 1))))
      }
      if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "unquote-splicing") {
        stop("unquote-splicing can only appear in list context")
      }
      if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "quasiquote") {
        return(as.call(list(as.symbol("quasiquote"), private$quasiquote_impl(expr[[2]], env, depth + 1))))
      }
      result <- list()
      i <- 1
      while (i <= length(expr)) {
        elem <- expr[[i]]
        if (is.call(elem) && length(elem) > 0 && is.symbol(elem[[1]]) && as.character(elem[[1]]) == "unquote-splicing") {
          if (depth == 1) {
            if (length(elem) != 2) {
              stop("unquote-splicing requires exactly 1 argument")
            }
            spliced <- private$eval_compiled_in_env(elem[[2]], env)
            if (is.call(spliced)) {
              spliced <- as.list(spliced)
            }
            if (is.list(spliced)) {
              for (item in spliced) {
                result <- c(result, list(private$hygiene_wrap(item, "call_site")))
              }
            } else {
              stop("unquote-splicing requires a list")
            }
          } else {
            result <- c(result, list(as.call(list(
              as.symbol("unquote-splicing"),
              private$quasiquote_impl(elem[[2]], env, depth - 1)
            ))))
          }
        } else {
          processed <- private$quasiquote_impl(elem, env, depth)
          result <- c(result, list(processed))
        }
        i <- i + 1
      }
      as.call(result)
    },
    macro_parse_params = function(params) {
      # Handle rest parameters (. syntax) - similar to lambda
      rest_param <- NULL
      rest_param_spec <- NULL
      arg_items <- as.list(params)

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

          # Rest can be simple symbol or (pattern ...)
          if (is.symbol(rest_arg)) {
            rest_param <- as.character(rest_arg)
            rest_param_spec <- list(
              type = "name",
              name = rest_param,
              pattern = NULL
            )
          } else if (is.call(rest_arg) || is.list(rest_arg)) {
            rest_list <- if (is.call(rest_arg)) as.list(rest_arg) else rest_arg
            is_pattern <- length(rest_list) >= 2 &&
              is.symbol(rest_list[[1]]) &&
              as.character(rest_list[[1]]) %in% c("pattern", "destructure")

            if (!is_pattern) {
              stop("Rest parameter must be a symbol or (pattern <pat>)")
            }
            if (length(rest_list) != 2) {
              stop("Rest pattern must be (pattern <pat>), defaults not allowed")
            }

            rest_pattern <- rest_list[[2]]
            rest_param_spec <- list(
              type = "pattern",
              name = NULL,
              pattern = rest_pattern
            )
          } else {
            stop("Rest parameter must be a symbol or (pattern <pat>)")
          }

          # Remove . and rest param from regular params
          if (dot_idx > 1) {
            arg_items <- arg_items[1:(dot_idx - 1)]
          } else {
            arg_items <- list()
          }
        }
      }

      # Parse regular parameters
      param_specs <- list()
      param_names <- character(0)
      param_defaults <- list()

      for (arg in arg_items) {
        if (is.symbol(arg)) {
          # Simple required parameter: x
          name <- as.character(arg)
          param_names <- c(param_names, name)
          param_defaults[[name]] <- rye_missing_default()
          param_specs[[length(param_specs) + 1]] <- list(
            type = "name",
            formal = name,
            pattern = NULL
          )

        } else if (is.call(arg) || is.list(arg)) {
          arg_list <- if (is.call(arg)) as.list(arg) else arg

          # Check for incomplete pattern - (pattern) with no actual pattern
          if (length(arg_list) == 1 && is.symbol(arg_list[[1]]) &&
              as.character(arg_list[[1]]) %in% c("pattern", "destructure")) {
            stop("pattern must be (pattern <pat>) or (pattern <pat> <default>)")
          }

          # Check if pattern wrapper
          is_pattern <- length(arg_list) >= 2 &&
            is.symbol(arg_list[[1]]) &&
            as.character(arg_list[[1]]) %in% c("pattern", "destructure")

          # Check if simple default pair (name default)
          is_default_pair <- length(arg_list) == 2 &&
            is.symbol(arg_list[[1]]) &&
            !is_pattern

          if (is_pattern) {
            # Pattern: (pattern (a b)) or (pattern (a b) (list 1 2))
            if (length(arg_list) != 2 && length(arg_list) != 3) {
              stop("pattern must be (pattern <pat>) or (pattern <pat> <default>)")
            }

            pattern <- arg_list[[2]]
            default_expr <- rye_missing_default()

            if (length(arg_list) == 3) {
              default_expr <- arg_list[[3]]
              if (is.null(default_expr)) {
                default_expr <- quote(NULL)
              }
            }

            # Generate unique name for internal binding
            tmp_name <- as.character(self$gensym(".__rye_macro_arg"))
            param_names <- c(param_names, tmp_name)
            param_defaults[[tmp_name]] <- default_expr
            param_specs[[length(param_specs) + 1]] <- list(
              type = "pattern",
              formal = tmp_name,
              pattern = pattern
            )

          } else if (is_default_pair) {
            # Simple optional: (x 10)
            name <- as.character(arg_list[[1]])
            default_expr <- arg_list[[2]]
            if (is.null(default_expr)) {
              default_expr <- quote(NULL)
            }

            param_names <- c(param_names, name)
            param_defaults[[name]] <- default_expr
            param_specs[[length(param_specs) + 1]] <- list(
              type = "name",
              formal = name,
              pattern = NULL
            )

          } else {
            stop("defmacro parameters must be symbols, (name default), or (pattern ...)")
          }

        } else {
          stop("defmacro parameters must be symbols, (name default), or (pattern ...)")
        }
      }

      list(
        param_specs = param_specs,
        param_names = param_names,
        param_defaults = param_defaults,
        rest_param_spec = rest_param_spec
      )
    },
    define_macro = function(name, params, body, env, docstring = NULL) {
      # Debug: check body
      if (FALSE && as.character(name) == "empty-pat") {
        message("DEBUG define_macro for ", as.character(name))
        message("  body class: ", class(body))
        message("  body length: ", length(body))
        if (length(body) > 0) {
          message("  first body expr: ", deparse(body[[1]]))
        }
      }

      # Parse parameters
      parsed <- private$macro_parse_params(params)
      param_specs <- parsed$param_specs
      param_names <- parsed$param_names
      param_defaults <- parsed$param_defaults
      rest_param_spec <- parsed$rest_param_spec

      macro_fn <- function(...) {
        macro_env <- new.env(parent = env)
        assign(".rye_macroexpanding", TRUE, envir = macro_env)
        args <- match.call(expand.dots = FALSE)$...

        if (!is.null(rest_param_spec)) {
          # Extract regular param specs (before rest)
          regular_param_specs <- param_specs

          # Bind regular parameters
          for (i in seq_along(regular_param_specs)) {
            spec <- regular_param_specs[[i]]
            param_name <- spec$formal

            if (i <= length(args)) {
              # Argument provided
              value <- args[[i]]
            } else {
              # Check for default
              default_expr <- param_defaults[[param_name]]
              if (inherits(default_expr, "rye_missing_default")) {
                stop(sprintf("Missing required parameter (position %d)", i))
              }
              # Evaluate default
              value <- private$eval_compiled_in_env(default_expr, env)
            }

            # Bind the value
            if (spec$type == "pattern") {
              # Destructure into macro_env
              RyeEnv$new(macro_env)$destructure_bind(spec$pattern, value, mode = "define")
            } else {
              # Simple binding
              assign(param_name, value, envir = macro_env)
            }
          }

          # Bind rest parameter
          if (length(args) > length(regular_param_specs)) {
            rest_args <- args[(length(regular_param_specs) + 1):length(args)]
          } else {
            rest_args <- list()
          }

          if (rest_param_spec$type == "pattern") {
            # Destructure rest args
            RyeEnv$new(macro_env)$destructure_bind(rest_param_spec$pattern, rest_args, mode = "define")
          } else {
            # Simple rest binding
            assign(rest_param_spec$name, rest_args, envir = macro_env)
          }

        } else {
          # No rest parameter - bind all regular params
          for (i in seq_along(param_specs)) {
            spec <- param_specs[[i]]
            param_name <- spec$formal

            if (i <= length(args)) {
              # Argument provided
              value <- args[[i]]
            } else {
              # Check for default
              default_expr <- param_defaults[[param_name]]
              if (inherits(default_expr, "rye_missing_default")) {
                stop(sprintf("Missing required parameter (position %d)", i))
              }
              # Evaluate default
              value <- private$eval_compiled_in_env(default_expr, env)
            }

            # Bind the value
            if (spec$type == "pattern") {
              # Destructure into macro_env
              RyeEnv$new(macro_env)$destructure_bind(spec$pattern, value, mode = "define")
            } else {
              # Simple binding
              assign(param_name, value, envir = macro_env)
            }
          }

          # Check we didn't get too many args (no rest param to catch them)
          if (length(args) > length(param_specs)) {
            # Count required vs provided
            required_count <- sum(vapply(param_specs, function(spec) {
              inherits(param_defaults[[spec$formal]], "rye_missing_default")
            }, logical(1)))
            stop(sprintf("Macro %s expects %d-%d arguments, got %d",
                         as.character(name), required_count, length(param_specs), length(args)))
          }
        }

        result <- NULL
        for (expr in body) {
          result <- private$eval_compiled_in_env(expr, macro_env)
        }
        result
      }

      attr(macro_fn, "rye_macro") <- list(
        params = param_names,
        param_defaults = param_defaults
      )
      if (!is.null(docstring)) {
        attr(macro_fn, "rye_doc") <- list(description = docstring)
      }

      registry <- private$macro_registry(env, create = TRUE)
      name_str <- as.character(name)
      if (exists(name_str, envir = registry, inherits = FALSE)) {
        rye_unlock_binding(name_str, registry)
      }
      registry[[name_str]] <- macro_fn
      lockBinding(name_str, registry)
      assign(name_str, macro_fn, envir = env)
    },
    macroexpand_impl = function(expr, env, preserve_src, max_depth, walk) {
      if (!is.call(expr) || length(expr) == 0) {
        if (!walk || isTRUE(preserve_src)) {
          return(expr)
        }
        return(self$context$source_tracker$strip_src(expr))
      }

      op <- expr[[1]]

      if (private$is_macro_impl(op, env) && max_depth > 0) {
        macro_fn <- private$get_macro_impl(op, env)
        args <- as.list(expr[-1])
        expanded <- do.call(macro_fn, args)
        expanded <- private$hygienize(expanded)
        expanded <- private$hygiene_unwrap_impl(expanded)
        expanded <- self$context$source_tracker$src_inherit(expanded, expr)

        if (max_depth <= 1 || !walk) {
          if (isTRUE(preserve_src)) {
            return(expanded)
          }
          if (walk) {
            return(self$context$source_tracker$strip_src(expanded))
          }
          return(expanded)
        }

        return(private$macroexpand_impl(expanded, env, preserve_src, max_depth - 1, walk = TRUE))
      }

      if (!walk) {
        return(expr)
      }

      if (is.symbol(op)) {
        op_name <- as.character(op)
        if (op_name %in% c("quote", "defmacro")) {
          if (isTRUE(preserve_src)) {
            return(expr)
          }
          return(self$context$source_tracker$strip_src(expr))
        }
        if (op_name == "quasiquote") {
          if (isTRUE(preserve_src)) {
            return(expr)
          }
          return(self$context$source_tracker$strip_src(expr))
        }
      }

      # Recurse into all elements (including the operator) so that e.g. (and 1 2 3)
      # inside ((lambda (tmp) (if tmp (and 1 2 3) tmp)) #t) gets expanded.
      result <- list()
      for (i in seq_len(length(expr))) {
        result[[i]] <- private$macroexpand_impl(expr[[i]], env, preserve_src, max_depth, walk = TRUE)
      }

      expanded <- self$context$source_tracker$src_inherit(as.call(result), expr)
      if (isTRUE(preserve_src)) {
        return(expanded)
      }
      self$context$source_tracker$strip_src(expanded)
    }
  )
)
