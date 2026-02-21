# MacroExpander: Defines and expands Arl macros. Handles defmacro, macroexpand,
# macroexpand_1, quasiquote/unquote, gensym, hygiene. Shares EvalContext with compiled runtime.
#
# @field context EvalContext (env, source_tracker).

# S3 class for resolved cross-module references.
# When a macro introduces a free variable from its defining module,
# we capture the actual value at expansion time so it doesn't need
# to be in scope at the use site.
arl_resolved_ref <- function(value, source_symbol, module_name = NULL) {
  structure(
    list(value = value, source_symbol = source_symbol, module_name = module_name),
    class = "arl_resolved_ref"
  )
}

#' @exportS3Method
print.arl_resolved_ref <- function(x, ...) {
  label <- x$source_symbol
  if (!is.null(x$module_name)) label <- paste0(x$module_name, "/", label)
  cat(sprintf("<resolved:%s>", label), "\n")
}

is_resolved_ref <- function(x) inherits(x, "arl_resolved_ref")

#' @keywords internal
#' @noRd
MacroExpander <- R6::R6Class(
  "ArlMacroExpander",
  public = list(
    context = NULL,
    # @description Create macro expander.
    # @param context EvalContext instance.
    initialize = function(context) {
      if (!inherits(context, "ArlEvalContext")) {
        stop("MacroExpander requires an EvalContext")
      }
      self$context <- context
    },
    # @description Define a macro. Registers name in env's macro registry.
    # @param name Symbol or character macro name.
    # @param params Formal parameter list.
    # @param body Macro body (unevaluated).
    # @param doc_list Optional documentation list attached as arl_doc.
    # @param env Target environment or NULL for context$env$env.
    defmacro = function(name, params, body, doc_list = NULL, env = NULL) {
      target_env <- private$normalize_env(env)
      private$define_macro(name, params, body, target_env, doc_list = doc_list)
      invisible(NULL)
    },
    # @description Expand macros in expr.
    # @param expr Arl expression.
    # @param env Environment for macro lookup or NULL.
    # @param preserve_src Whether to keep source attributes.
    # @param depth NULL for full recursive expansion, or integer N for N expansion steps.
    # @return Expanded expression.
    macroexpand = function(expr, env = NULL, preserve_src = FALSE, depth = NULL) {
      target_env <- private$normalize_env(env)
      if (is.null(depth)) {
        # Full expansion (walk into subexpressions)
        # Collect macro names across the entire env chain
        macro_names <- private$get_all_macro_names(target_env)
        if (length(macro_names) == 0 || !private$contains_macro_head(expr, macro_names)) {
          if (isTRUE(preserve_src)) {
            return(expr)
          }
          return(self$context$source_tracker$strip_src(expr))
        }
        private$macroexpand_impl(expr, target_env, preserve_src, max_depth = Inf, walk = TRUE)
      } else {
        # Bounded expansion (no walk into subexpressions)
        private$macroexpand_impl(expr, target_env, preserve_src, max_depth = depth, walk = FALSE)
      }
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
    # @description Invalidate the macro names cache. Call after imports modify the env parent chain.
    invalidate_macro_cache = function() {
      private$macro_names_cache <- NULL
      private$macro_names_cache_env <- NULL
      private$macro_names_set <- NULL
    },
    # @description Expand quasiquote (backtick) with unquote (comma) in expr.
    # @param expr Quasiquoted expression.
    # @param env Environment for unquote evaluation.
    # @param depth Unquote depth (for nested quasiquote).
    # @return Expanded expression.
    quasiquote = function(expr, env, depth = 1) {
      target_env <- private$normalize_env(env)
      eval_fn <- function(inner, e) private$eval_compiled_in_env(inner, e)
      wrap_fn <- function(val) private$hygiene_wrap(val, "call_site")
      quasiquote_expand(expr, target_env, depth, eval_fn, wrap_fn,
                            skip_quote = FALSE)
    }
  ),
  private = list(
    gensym_counter = 0,
    hygiene_counter = 0,
    # Cache for macro registry names (optimization 1.3)
    macro_names_cache = NULL,
    macro_names_cache_env = NULL,
    # Cache for macro name lookup (optimization 1.4) - environment for O(1) lookup
    macro_names_set = NULL,
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
      resolve_env(env, self$context$env$env)
    },
    # Get or create a LOCAL macro registry for env (inherits = FALSE).
    # Used when defining macros — ensures each env owns its own registry.
    local_macro_registry = function(env) {
      registry <- get0(".__macros", envir = env, inherits = FALSE)
      if (is.null(registry)) {
        registry <- new.env(parent = emptyenv())
        assign(".__macros", registry, envir = env)
        lockBinding(".__macros", env)
      }
      # Invalidate cache — a new local registry changes the visible set
      private$macro_names_cache_env <- NULL
      private$macro_names_cache <- NULL
      private$macro_names_set <- NULL
      registry
    },
    # Collect ALL macro names visible from env by walking the env chain.
    # Each env may have its own .__macros registry; we aggregate names from all of them.
    get_all_macro_names = function(env) {
      # Cache: keyed on the env itself (identity)
      if (!is.null(private$macro_names_cache) &&
          identical(env, private$macro_names_cache_env)) {
        return(private$macro_names_cache)
      }
      names_acc <- character(0)
      e <- env
      base <- baseenv()
      empty <- emptyenv()
      while (!identical(e, empty) && !identical(e, base)) {
        reg <- get0(".__macros", envir = e, inherits = FALSE)
        if (!is.null(reg)) {
          names_acc <- c(names_acc, ls(reg, all.names = TRUE))
        }
        e <- parent.env(e)
      }
      macro_names <- unique(names_acc)
      # Update cache
      private$macro_names_cache <- macro_names
      private$macro_names_cache_env <- env
      # Build hash set for O(1) lookup
      if (length(macro_names) > 0) {
        set_list <- as.list(rep(TRUE, length(macro_names)))
        names(set_list) <- macro_names
        private$macro_names_set <- list2env(set_list, hash = TRUE, parent = emptyenv())
      } else {
        private$macro_names_set <- new.env(hash = TRUE, parent = emptyenv())
      }
      macro_names
    },
    contains_macro_head = function(expr, macro_names) {
      if (is.null(expr)) {
        return(FALSE)
      }
      if (inherits(expr, "ArlCons")) {
        if (private$contains_macro_head(expr$car, macro_names)) {
          return(TRUE)
        }
        return(private$contains_macro_head(expr$cdr, macro_names))
      }
      if (is.call(expr)) {
        if (length(expr) > 0 && is.symbol(expr[[1]])) {
          op_name <- as.character(expr[[1]])
          if (op_name %in% c("quote", "quasiquote", "defmacro")) {
            return(FALSE)
          }
          # Use O(1) hash set lookup (optimization 1.4)
          if (!is.null(private$macro_names_set) && exists(op_name, envir = private$macro_names_set, inherits = FALSE)) {
            return(TRUE)
          }
        }
        # Check for resolved ref wrapping a macro function in call position
        if (length(expr) > 0 && is_resolved_ref(expr[[1]]) &&
            is.function(expr[[1]]$value) && !is.null(attr(expr[[1]]$value, "arl_macro"))) {
          return(TRUE)
        }
        for (i in seq_along(expr)) {
          if (private$contains_macro_head(expr[[i]], macro_names)) {
            return(TRUE)
          }
        }
        return(FALSE)
      }
      if (is.list(expr) && is.null(attr(expr, "class", exact = TRUE))) {
        for (i in seq_along(expr)) {
          if (private$contains_macro_head(expr[[i]], macro_names)) {
            return(TRUE)
          }
        }
      }
      FALSE
    },
    is_macro_impl = function(name, env) {
      if (!is.symbol(name)) {
        return(FALSE)
      }
      name_str <- as.character(name)
      e <- env
      base <- baseenv()
      empty <- emptyenv()
      while (!identical(e, empty) && !identical(e, base)) {
        reg <- get0(".__macros", envir = e, inherits = FALSE)
        if (!is.null(reg) && exists(name_str, envir = reg, inherits = FALSE)) {
          return(TRUE)
        }
        e <- parent.env(e)
      }
      FALSE
    },
    get_macro_impl = function(name, env) {
      name_str <- as.character(name)
      e <- env
      base <- baseenv()
      empty <- emptyenv()
      while (!identical(e, empty) && !identical(e, base)) {
        reg <- get0(".__macros", envir = e, inherits = FALSE)
        if (!is.null(reg) && exists(name_str, envir = reg, inherits = FALSE)) {
          return(reg[[name_str]])
        }
        e <- parent.env(e)
      }
      NULL
    },
    gensym_impl = function(prefix = "G") {
      env <- self$context$env
      repeat {
        private$gensym_counter <- private$gensym_counter + 1
        candidate <- paste0(prefix, "__", private$gensym_counter)
        if (is.null(env) || !exists(candidate, envir = if (inherits(env, "ArlEnv")) env$env else env, inherits = TRUE)) {
          return(as.symbol(candidate))
        }
      }
    },
    hygiene_gensym = function(prefix = "H") {
      private$hygiene_counter <- private$hygiene_counter + 1
      as.symbol(paste0(prefix, "__h", private$hygiene_counter))
    },
    hygiene_wrap = function(expr, origin) {
      structure(list(expr = expr, origin = origin), class = "arl_syntax")
    },
    hygiene_is = function(x) {
      inherits(x, "arl_syntax")
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
    hygienize = function(expr, defining_env = NULL, use_site_env = NULL) {
      private$hygienize_expr(expr, env = list(), protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
    },
    hygienize_expr = function(expr, env, protected, defining_env = NULL, use_site_env = NULL) {
      if (private$hygiene_is(expr)) {
        origin <- private$hygiene_origin(expr)
        inner <- private$hygiene_expr(expr)
        if (identical(origin, "call_site")) {
          return(private$hygienize_expr(inner, env, protected = TRUE, defining_env = defining_env, use_site_env = use_site_env))
        }
        if (identical(origin, "introduced")) {
          return(private$hygienize_expr(inner, env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env))
        }
        return(private$hygienize_expr(inner, env, protected = protected, defining_env = defining_env, use_site_env = use_site_env))
      }

      if (is.symbol(expr)) {
        if (isTRUE(protected)) {
          return(expr)
        }
        name <- as.character(expr)
        if (!is.null(env[[name]])) {
          return(env[[name]])
        }

        # Resolve introduced free variables from defining module.
        # Only resolve if the symbol exists in the defining env but NOT in
        # the shared prelude/builtins chain. Skip if the use-site env
        # resolves to the identical value (same binding, no shadowing risk).
        if (!is.null(defining_env)) {
          prelude_env <- self$context$prelude_env
          if (exists(name, envir = defining_env, inherits = TRUE) &&
              (is.null(prelude_env) || !exists(name, envir = prelude_env, inherits = TRUE))) {
            value <- get(name, envir = defining_env, inherits = TRUE)
            # Skip if the use-site has the exact same binding
            if (!is.null(use_site_env) && exists(name, envir = use_site_env, inherits = TRUE) &&
                identical(value, get(name, envir = use_site_env, inherits = TRUE))) {
              return(expr)
            }
            module_name <- tryCatch(
              get(".__module_name", envir = defining_env, inherits = FALSE),
              error = function(e) NULL
            )
            return(arl_resolved_ref(value, name, module_name))
          }
        }

        return(expr)
      }

      if (!is.call(expr)) {
        if (is.list(expr) && is.null(attr(expr, "class", exact = TRUE))) {
          updated <- lapply(expr, private$hygienize_expr, env = env, protected = protected, defining_env = defining_env, use_site_env = use_site_env)
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
        updated <- lapply(as.list(expr), private$hygienize_expr, env = env, protected = TRUE, defining_env = defining_env, use_site_env = use_site_env)
        return(as.call(updated))
      }

      if (is.symbol(op)) {
        op_name <- as.character(op)
        if (op_name == "begin") {
          return(private$hygienize_begin(expr, env, defining_env = defining_env, use_site_env = use_site_env))
        }
        if (op_name == "define") {
          return(private$hygienize_define(expr, env, defining_env = defining_env, use_site_env = use_site_env)$expr)
        }
        if (op_name == "lambda") {
          return(private$hygienize_lambda(expr, env, defining_env = defining_env, use_site_env = use_site_env))
        }
        if (op_name %in% c("let", "let*", "letrec")) {
          return(private$hygienize_let(expr, env, op_name, defining_env = defining_env, use_site_env = use_site_env))
        }
      }

      updated <- lapply(as.list(expr), private$hygienize_expr, env = env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
      as.call(updated)
    },
    hygienize_begin = function(expr, env, defining_env = NULL, use_site_env = NULL) {
      result <- list(expr[[1]])
      current_env <- env
      if (length(expr) > 1) {
        for (i in 2:length(expr)) {
          form <- expr[[i]]
          if (is.call(form) && length(form) >= 2 && is.symbol(form[[1]]) &&
              as.character(form[[1]]) == "define") {
            out <- private$hygienize_define(form, current_env, defining_env = defining_env, use_site_env = use_site_env)
            result[[i]] <- out$expr
            current_env <- out$env
          } else {
            result[[i]] <- private$hygienize_expr(form, current_env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
          }
        }
      }
      as.call(result)
    },
    hygienize_define = function(expr, env, defining_env = NULL, use_site_env = NULL) {
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
        result[[2]] <- private$hygienize_expr(expr[[2]], env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
      }
      if (length(expr) >= 3) {
        result[[3]] <- private$hygienize_expr(expr[[3]], env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
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
    hygienize_lambda = function(expr, env, defining_env = NULL, use_site_env = NULL) {
      if (length(expr) < 3) {
        return(expr)
      }
      args_expr <- expr[[2]]
      if (!is.call(args_expr)) {
        result <- list(expr[[1]], private$hygienize_expr(args_expr, env, protected = TRUE, defining_env = defining_env, use_site_env = use_site_env))
        if (length(expr) > 2) {
          for (i in 3:length(expr)) {
            result[[i]] <- private$hygienize_expr(expr[[i]], env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
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
            new_args[[i]] <- private$hygienize_expr(arg, env, protected = TRUE, defining_env = defining_env, use_site_env = use_site_env)
          }
        }
      }
      args_out <- if (length(args_list) == 0) args_expr else as.call(new_args)
      result <- list(expr[[1]], args_out)
      if (length(expr) > 2) {
        for (i in 3:length(expr)) {
          result[[i]] <- private$hygienize_expr(expr[[i]], new_env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
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
    hygienize_binding_value = function(parts, env, defining_env = NULL, use_site_env = NULL) {
      if (length(parts) >= 2) {
        return(private$hygienize_expr(parts[[2]], env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env))
      }
      NULL
    },
    hygienize_let = function(expr, env, op_name, defining_env = NULL, use_site_env = NULL) {
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
              value <- private$hygienize_binding_value(parts, body_env, defining_env = defining_env, use_site_env = use_site_env)
              new_bindings[[i]] <- as.call(list(renamed, value))
            } else {
              value <- private$hygienize_binding_value(parts, value_env, defining_env = defining_env, use_site_env = use_site_env)
              new_bindings[[i]] <- as.call(c(list(parts[[1]]), list(value)))
            }
          }
        }
        bindings_out <- if (length(bindings_list) == 0) bindings_expr else as.call(new_bindings)
        body <- list(expr[[1]], bindings_out)
        if (length(expr) > 2) {
          for (i in 3:length(expr)) {
            body[[i]] <- private$hygienize_expr(expr[[i]], body_env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
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
            value <- private$hygienize_binding_value(parts, current_env, defining_env = defining_env, use_site_env = use_site_env)
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
            body[[i]] <- private$hygienize_expr(expr[[i]], current_env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
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
            value <- private$hygienize_binding_value(parts, env, defining_env = defining_env, use_site_env = use_site_env)
            new_bindings[[i]] <- as.call(list(renamed, value))
          } else {
            value <- private$hygienize_binding_value(parts, env, defining_env = defining_env, use_site_env = use_site_env)
            new_bindings[[i]] <- as.call(c(list(parts[[1]]), list(value)))
          }
        }
      }
      bindings_out <- if (length(bindings_list) == 0) bindings_expr else as.call(new_bindings)
      body <- list(expr[[1]], bindings_out)
      if (length(expr) > 2) {
        for (i in 3:length(expr)) {
          body[[i]] <- private$hygienize_expr(expr[[i]], body_env, protected = FALSE, defining_env = defining_env, use_site_env = use_site_env)
        }
      }
      as.call(body)
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
          param_defaults[[name]] <- missing_default()
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
            default_expr <- missing_default()

            if (length(arg_list) == 3) {
              default_expr <- arg_list[[3]]
              if (is.null(default_expr)) {
                default_expr <- quote(NULL)
              }
            }

            # Generate unique name for internal binding
            tmp_name <- as.character(self$gensym(".__macro_arg"))
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
    define_macro = function(name, params, body, env, doc_list = NULL) {

      # Parse parameters
      parsed <- private$macro_parse_params(params)
      param_specs <- parsed$param_specs
      param_names <- parsed$param_names
      param_defaults <- parsed$param_defaults
      rest_param_spec <- parsed$rest_param_spec

      # Bind a single macro parameter: use provided arg or fall back to default
      bind_param <- function(spec, args, i, macro_env) {
        param_name <- spec$formal
        if (i <= length(args)) {
          value <- args[[i]]
        } else {
          default_expr <- param_defaults[[param_name]]
          if (inherits(default_expr, "arl_missing_default")) {
            stop(sprintf("Missing required parameter (position %d)", i))
          }
          value <- private$eval_compiled_in_env(default_expr, env)
        }
        if (spec$type == "pattern") {
          Env$new(macro_env)$destructure_bind(spec$pattern, value, mode = "define")
        } else {
          assign(param_name, value, envir = macro_env)
        }
      }

      macro_fn <- function(...) {
        macro_env <- new.env(parent = env)
        assign(".__macroexpanding", TRUE, envir = macro_env)
        args <- match.call(expand.dots = FALSE)$...

        # Bind regular parameters
        for (i in seq_along(param_specs)) {
          bind_param(param_specs[[i]], args, i, macro_env)
        }

        if (!is.null(rest_param_spec)) {
          # Bind rest parameter
          if (length(args) > length(param_specs)) {
            rest_args <- args[(length(param_specs) + 1):length(args)]
          } else {
            rest_args <- list()
          }

          if (rest_param_spec$type == "pattern") {
            Env$new(macro_env)$destructure_bind(rest_param_spec$pattern, rest_args, mode = "define")
          } else {
            assign(rest_param_spec$name, rest_args, envir = macro_env)
          }
        } else {
          # Check we didn't get too many args (no rest param to catch them)
          if (length(args) > length(param_specs)) {
            required_count <- sum(vapply(param_specs, function(spec) {
              inherits(param_defaults[[spec$formal]], "arl_missing_default")
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

      rest_name <- if (!is.null(rest_param_spec) && !is.null(rest_param_spec$name)) rest_param_spec$name else NULL
      attr(macro_fn, "arl_macro") <- list(
        params = param_names,
        param_defaults = param_defaults,
        rest_param = rest_name
      )
      if (!is.null(doc_list) && length(doc_list) > 0) {
        attr(macro_fn, "arl_doc") <- doc_list
      }

      registry <- private$local_macro_registry(env)
      name_str <- as.character(name)
      if (exists(name_str, envir = registry, inherits = FALSE)) {
        unlock_binding(name_str, registry)
      }
      registry[[name_str]] <- macro_fn
      lockBinding(name_str, registry)
      assign(name_str, macro_fn, envir = env)
      # Invalidate cache after defining macro (optimization 1.3)
      private$macro_names_cache <- NULL
      private$macro_names_cache_env <- NULL
      private$macro_names_set <- NULL
    },
    macroexpand_impl = function(expr, env, preserve_src, max_depth, walk) {
      if (!is.call(expr) || length(expr) == 0) {
        if (!walk || isTRUE(preserve_src)) {
          return(expr)
        }
        return(self$context$source_tracker$strip_src(expr))
      }

      op <- expr[[1]]

      # Check if the operator is a resolved ref wrapping a macro function
      resolved_macro_fn <- NULL
      if (is_resolved_ref(op) && is.function(op$value) &&
          !is.null(attr(op$value, "arl_macro"))) {
        resolved_macro_fn <- op$value
      }

      if ((private$is_macro_impl(op, env) || !is.null(resolved_macro_fn)) && max_depth > 0) {
        macro_fn <- if (!is.null(resolved_macro_fn)) resolved_macro_fn else private$get_macro_impl(op, env)
        args <- as.list(expr[-1])
        expanded <- tryCatch(
          do.call(macro_fn, args),
          error = function(e) {
            if (isTRUE(.pkg_option("debug_macro", FALSE))) {
              op_name <- if (is.symbol(op)) as.character(op) else "<macro>"
              stop(sprintf("macro expansion failed for %s: %s", op_name, conditionMessage(e)), call. = FALSE)
            }
            stop(e)
          }
        )
        defining_env <- environment(macro_fn)$env  # macro_fn closes over `env` from define_macro
        expanded <- private$hygienize(expanded, defining_env, use_site_env = env)
        expanded <- private$hygiene_unwrap_impl(expanded)
        expanded <- self$context$source_tracker$src_inherit(expanded, expr)

        if (max_depth <= 1) {
          if (isTRUE(preserve_src)) {
            return(expanded)
          }
          if (walk) {
            return(self$context$source_tracker$strip_src(expanded))
          }
          return(expanded)
        }

        return(private$macroexpand_impl(expanded, env, preserve_src, max_depth - 1, walk = walk))
      }

      if (!walk) {
        return(expr)
      }

      if (is.symbol(op)) {
        op_name <- as.character(op)
        if (op_name == "lambda" && length(expr) >= 3) {
          args_expr <- expr[[2]]
          if (!isTRUE(preserve_src)) {
            args_expr <- self$context$source_tracker$strip_src(args_expr)
          }
          body_exprs <- lapply(as.list(expr)[-(1:2)], function(e) {
            private$macroexpand_impl(e, env, preserve_src, max_depth, walk = TRUE)
          })
          out <- as.call(c(list(op), list(args_expr), body_exprs))
          if (isTRUE(preserve_src)) {
            return(self$context$source_tracker$src_inherit(out, expr))
          }
          return(self$context$source_tracker$strip_src(out))
        }
        if (op_name == "define" && length(expr) >= 3 && is.call(expr[[2]])) {
          head_expr <- expr[[2]]
          if (!isTRUE(preserve_src)) {
            head_expr <- self$context$source_tracker$strip_src(head_expr)
          }
          body_exprs <- lapply(as.list(expr)[-(1:2)], function(e) {
            private$macroexpand_impl(e, env, preserve_src, max_depth, walk = TRUE)
          })
          out <- as.call(c(list(op), list(head_expr), body_exprs))
          if (isTRUE(preserve_src)) {
            return(self$context$source_tracker$src_inherit(out, expr))
          }
          return(self$context$source_tracker$strip_src(out))
        }
        if (op_name %in% c("quote", "defmacro", "import")) {
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
      for (i in seq_along(expr)) {
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
