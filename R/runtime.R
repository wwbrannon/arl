rye_missing_default <- function() {
  structure(list(), class = "rye_missing_default")
}

# Compiled-mode helpers: installed in env before eval(compiled, env).
# Rye truthiness: only #f (FALSE) and #nil (NULL) are false.
.rye_true_p <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  if (is.logical(x) && length(x) == 1L && !is.na(x) && identical(x[[1]], FALSE)) {
    return(FALSE)
  }
  TRUE
}

# Wrapper for define/set! from compiled code (including pattern destructuring).
# pattern can be a symbol, a string (converted to symbol for simple binding), or a list for destructuring.
.rye_assign_pattern <- function(env, pattern, value, mode) {
  if (is.character(pattern) && length(pattern) == 1L) {
    pattern <- as.symbol(pattern)
  }
  RyeEnv$new(env)$assign_pattern(pattern, value, mode = mode, context = if (identical(mode, "define")) "define" else "set!")
}

# EvalContext: Shared context for MacroExpander and CompiledRuntime. Holds env (RyeEnv)
# and source_tracker. Created once per engine; macro_expander and compiler are set after.
#
# @field env RyeEnv for the engine.
# @field source_tracker SourceTracker for error locations.
# @field macro_expander Set by RyeEngine after creation.
# @field compiled_runtime Set by RyeEngine after creation.
#
#' @keywords internal
#' @noRd
EvalContext <- R6::R6Class(
  "EvalContext",
  public = list(
    env = NULL,
    source_tracker = NULL,
    macro_expander = NULL,
    compiled_runtime = NULL,
    compiler = NULL,
    # @description Create context. macro_expander and compiler are assigned by the engine.
    # @param env RyeEnv instance.
    # @param source_tracker SourceTracker instance.
    initialize = function(env, source_tracker) {
      if (!r6_isinstance(env, "RyeEnv")) {
        stop("EvalContext requires a RyeEnv")
      }
      self$env <- env
      self$source_tracker <- source_tracker
    }
  )
)

# CompiledRuntime: Runtime helpers for compiled evaluation. Owns helper installation,
# eval_compiled, and compiled-only special form helpers (import/module/defmacro/etc.).
#
# @field context EvalContext (env, source_tracker, macro_expander, compiler).
# @field load_file_fn Function(path, env, create_scope) for load/run/import.
# @field help_fn Function(topic, env) for (help topic).
#
#' @keywords internal
#' @noRd
CompiledRuntime <- R6::R6Class(
  "CompiledRuntime",
  public = list(
    context = NULL,
    load_file_fn = NULL,
    help_fn = NULL,
    # @description Create compiled runtime.
    # @param context EvalContext instance.
    # @param load_file_fn Optional; required for load/run/import.
    # @param help_fn Optional; required for (help topic).
    initialize = function(context, load_file_fn = NULL, help_fn = NULL) {
      if (!r6_isinstance(context, "EvalContext")) {
        stop("CompiledRuntime requires an EvalContext")
      }
      self$context <- context
      self$load_file_fn <- load_file_fn
      self$help_fn <- help_fn
    },
    # @description Install bindings required for compiled code into env.
    install_helpers = function(env) {
      assign(".rye_env", env, envir = env)
      assign(".rye_quote", base::quote, envir = env)
      assign(".rye_true_p", .rye_true_p, envir = env)
      assign(".rye_assign_pattern", function(env, pattern, value, mode) {
        value <- self$context$source_tracker$strip_src(value)
        .rye_assign_pattern(env, pattern, value, mode)
      }, envir = env)
      assign(".rye_load", self$load_file_fn, envir = env)
      assign(".rye_help", function(topic, env) {
        if (is.symbol(topic)) topic <- as.character(topic)
        self$help_fn(topic, env)
      }, envir = env)
      assign(".rye_subscript_call", function(op_name, args, env) {
        self$subscript_call_compiled(op_name, args, env)
      }, envir = env)
      assign("quasiquote", function(expr) {
        if (exists(".rye_macroexpanding", envir = env, inherits = TRUE) &&
            isTRUE(get(".rye_macroexpanding", envir = env, inherits = TRUE))) {
          if (is.null(self$context$macro_expander)) {
            stop("macro expander not initialized")
          }
          return(self$context$macro_expander$quasiquote(expr, env))
        }
        self$quasiquote_compiled(expr, env)
      }, envir = env)
      assign(".rye_delay", function(compiled_expr, env) self$promise_new_compiled(compiled_expr, env), envir = env)
      assign(".rye_defmacro", function(name, params, body_arg, docstring, env) self$defmacro_compiled(name, params, body_arg, docstring, env), envir = env)
      assign(".rye_macro_quasiquote", function(expr, env) {
        if (is.null(self$context$macro_expander)) {
          stop("macro expander not initialized")
        }
        self$context$macro_expander$quasiquote(expr, env)
      }, envir = env)
      assign(".rye_module", function(module_name, exports, export_all, body_exprs, src_file, env) {
        self$module_compiled(module_name, exports, export_all, body_exprs, src_file, env)
      }, envir = env)
      assign(".rye_import", function(arg_value, env) {
        self$import_compiled(arg_value, env)
      }, envir = env)
      assign(".rye_pkg_access", function(op_name, pkg, name, env) {
        self$pkg_access_compiled(op_name, pkg, name, env)
      }, envir = env)
      invisible(NULL)
    },
    # @description Run compiled R expression in env (helpers must be installed).
    eval_compiled = function(compiled_expr, env) {
      if (!is.environment(env)) {
        stop("eval_compiled requires an environment")
      }
      self$context$env$push_env(env)
      on.exit(self$context$env$pop_env(), add = TRUE)
      self$install_helpers(env)
      result_with_vis <- withVisible(eval(compiled_expr, envir = env))
      value <- self$context$source_tracker$strip_src(result_with_vis$value)
      if (result_with_vis$visible) {
        value
      } else {
        invisible(value)
      }
    },
    # Import logic for compiled (import x): same semantics as import special form.
    import_compiled = function(arg_value, env) {
      is_path <- is.character(arg_value) && length(arg_value) == 1
      if (is_path) {
        path_str <- arg_value
        module_path <- rye_resolve_path_only(path_str)
        if (is.null(module_path)) {
          stop(sprintf("Module not found: %s", path_str))
        }
        registry_key <- rye_normalize_path_absolute(module_path)
      } else {
        module_name <- RyeEnv$new(env)$symbol_or_string(arg_value, "import requires a module name symbol or string")
        registry_key <- module_name
      }
      shared_registry <- self$context$env$module_registry
      if (!shared_registry$exists(registry_key)) {
        if (is_path) {
          if (is.null(self$load_file_fn)) {
            stop("import requires a load_file function")
          }
          self$load_file_fn(module_path, self$context$env$env, create_scope = FALSE)
        } else {
          module_path <- rye_resolve_module_path(registry_key)
          if (is.null(module_path)) {
            stop(sprintf("Module not found: %s", registry_key))
          }
          if (is.null(self$load_file_fn)) {
            stop("import requires a load_file function")
          }
          self$load_file_fn(module_path, self$context$env$env, create_scope = FALSE)
        }
        if (!shared_registry$exists(registry_key)) {
          stop(sprintf("Module '%s' did not register itself", registry_key))
        }
      }
      shared_registry$attach_into(registry_key, env)
      invisible(NULL)
    },
    # Package access (:: / :::) for compiled code. pkg and name are strings from the compiler.
    pkg_access_compiled = function(op_name, pkg_name, obj_name, env) {
      if (!is.character(pkg_name) || length(pkg_name) != 1 ||
          !is.character(obj_name) || length(obj_name) != 1) {
        stop("Package and object names must be length-1 character")
      }
      if (identical(op_name, "::")) {
        getExportedValue(pkg_name, obj_name)
      } else if (identical(op_name, ":::")) {
        getFromNamespace(obj_name, pkg_name)
      } else {
        stop("Unknown package access operator: ", op_name)
      }
    },
    subscript_call_compiled = function(op_name, args, env) {
      if (!is.character(op_name) || length(op_name) != 1) {
        stop("subscript operator name must be a single string")
      }
      if (!is.list(args)) {
        stop("subscript args must be a list")
      }
      fn <- get(op_name, envir = baseenv())
      args <- lapply(args, private$quote_arg_impl, quote_symbols = FALSE)
      do.call(fn, args)
    },
    # @description Apply a function to evaluated args (used in tests).
    do_call = function(fn, args, env = NULL) {
      if (is.null(env)) {
        env <- self$context$env$current_env()
      }
      private$do_call_impl(fn, args, env)
    },
    quasiquote_compiled = function(expr, env) {
      private$quasiquote_compiled_impl(expr, env, 1L)
    },
    promise_new_compiled = function(compiled_expr, env) {
      RyePromise$new(compiled_expr, env, self$eval_compiled)
    },
    # @description Quote an argument for compiled helpers (used in tests).
    quote_arg = function(value, quote_symbols = TRUE) {
      private$quote_arg_impl(value, quote_symbols = quote_symbols)
    },
    defmacro_compiled = function(name, params, body_arg, docstring, env) {
      body_list <- if (is.call(body_arg) && length(body_arg) >= 1 && identical(as.character(body_arg[[1]]), "begin")) {
        as.list(body_arg)[-1]
      } else {
        list(body_arg)
      }
      self$context$macro_expander$defmacro(name, params, body_list, docstring = docstring, env = env)
      invisible(NULL)
    },
    module_compiled = function(module_name, exports, export_all, body_exprs, src_file, env) {
      module_env <- new.env(parent = env)
      assign(".rye_module", TRUE, envir = module_env)
      self$context$env$module_registry$register(module_name, module_env, exports)
      if (!is.null(src_file) && is.character(src_file) && length(src_file) == 1L && nzchar(src_file) && grepl("[/\\\\]", src_file)) {
        absolute_path <- rye_normalize_path_absolute(src_file)
        self$context$env$module_registry$alias(absolute_path, module_name)
      }
      self$install_helpers(module_env)
      assign(".rye_env", module_env, envir = module_env)

      # Compile body expressions (for caching)
      should_cache <- !is.null(src_file) && is.character(src_file) &&
                      length(src_file) == 1L && nzchar(src_file) &&
                      file.exists(src_file)

      compiled_body <- NULL
      if (should_cache) {
        compiled_body <- lapply(body_exprs, function(expr) {
          expanded <- self$context$macro_expander$macroexpand(expr, env = module_env, preserve_src = TRUE)
          self$context$compiler$compile(expanded, module_env, strict = TRUE)
        })
      }

      # Evaluate (use compiled version if we made it, otherwise compile on-the-fly)
      if (!is.null(compiled_body)) {
        result <- NULL
        for (compiled_expr in compiled_body) {
          result <- self$eval_compiled(compiled_expr, module_env)
        }
      } else {
        result <- private$eval_seq_compiled(body_exprs, module_env)
      }

      if (export_all) {
        exports <- setdiff(ls(module_env, all.names = TRUE), ".rye_module")
        self$context$env$module_registry$update_exports(module_name, exports)
      }

      # Write caches after successful module load
      if (should_cache) {
        cache_paths <- get_cache_paths(src_file)
        if (!is.null(cache_paths)) {
          # Always write Option A (safe fallback)
          write_code_cache(module_name, compiled_body, exports, export_all, src_file, cache_paths$file_hash)

          # Try Option C if safe
          safety <- is_safe_to_cache(module_env, env)
          if (safety$safe) {
            write_env_cache(module_name, module_env, exports, src_file, cache_paths$file_hash)
          }
        }
      }

      result
    }
  ),
  private = list(
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
    },
    eval_seq_compiled = function(exprs, env) {
      if (is.null(exprs) || length(exprs) == 0) {
        return(invisible(NULL))
      }
      if (!is.list(exprs)) {
        exprs <- list(exprs)
      }
      result <- NULL
      for (expr in exprs) {
        expanded <- self$context$macro_expander$macroexpand(expr, env = env, preserve_src = TRUE)
        compiled <- self$context$compiler$compile(expanded, env, strict = TRUE)
        result <- self$eval_compiled(compiled, env)
      }
      result
    },
    quasiquote_compiled_impl = function(expr, env, depth) {
      if (!is.call(expr)) {
        return(expr)
      }
      if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "quote") {
        return(expr)
      }
      if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "unquote") {
        if (length(expr) != 2) {
          stop("unquote requires exactly 1 argument")
        }
        if (depth == 1) {
          compiled <- self$context$compiler$compile(expr[[2]], env)
          if (is.null(compiled)) {
            stop("unquote could not be compiled")
          }
          return(self$eval_compiled(compiled, env))
        }
        return(as.call(list(as.symbol("unquote"), private$quasiquote_compiled_impl(expr[[2]], env, depth - 1L))))
      }
      if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "unquote-splicing") {
        stop("unquote-splicing can only appear in list context")
      }
      if (is.call(expr) && length(expr) > 0 && is.symbol(expr[[1]]) && as.character(expr[[1]]) == "quasiquote") {
        if (length(expr) != 2) {
          stop("quasiquote requires exactly 1 argument")
        }
        return(as.call(list(as.symbol("quasiquote"), private$quasiquote_compiled_impl(expr[[2]], env, depth + 1L))))
      }
      result <- list()
      result_idx <- 1L
      i <- 1L
      while (i <= length(expr)) {
        elem <- expr[[i]]
        if (is.call(elem) && length(elem) > 0 && is.symbol(elem[[1]]) && as.character(elem[[1]]) == "unquote-splicing") {
          if (depth == 1) {
            if (length(elem) != 2) {
              stop("unquote-splicing requires exactly 1 argument")
            }
            compiled <- self$context$compiler$compile(elem[[2]], env)
            if (is.null(compiled)) {
              stop("unquote-splicing could not be compiled")
            }
            spliced <- self$eval_compiled(compiled, env)
            if (is.call(spliced)) {
              spliced <- as.list(spliced)
            }
            if (is.list(spliced)) {
              if (length(spliced) > 0) {
                for (item in spliced) {
                  result[[result_idx]] <- item
                  result_idx <- result_idx + 1L
                }
              }
            } else {
              stop("unquote-splicing requires a list")
            }
          } else {
            result[[result_idx]] <- as.call(list(
              as.symbol("unquote-splicing"),
              private$quasiquote_compiled_impl(elem[[2]], env, depth - 1L)
            ))
            result_idx <- result_idx + 1L
          }
        } else {
          processed <- private$quasiquote_compiled_impl(elem, env, depth)
          result[[result_idx]] <- processed
          result_idx <- result_idx + 1L
        }
        i <- i + 1L
      }
      as.call(result)
    }
  )
)
