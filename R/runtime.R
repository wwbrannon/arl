missing_default <- function() {
  structure(list(), class = "rye_missing_default")
}

# Compiled-mode helpers: installed in env before eval(compiled, env).
# Rye truthiness: #f (FALSE), #nil (NULL), and 0 are false.
.__true_p <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  if (is.logical(x) && length(x) == 1L && !is.na(x) && identical(x[[1]], FALSE)) {
    return(FALSE)
  }
  if (is.numeric(x) && length(x) == 1L && !is.na(x) && x == 0) {
    return(FALSE)
  }
  TRUE
}

# Wrapper for define/set! from compiled code (including pattern destructuring).
# pattern can be a symbol, a string (converted to symbol for simple binding), or a list for destructuring.
.__assign_pattern <- function(env, pattern, value, mode) {
  if (is.character(pattern) && length(pattern) == 1L) {
    pattern <- as.symbol(pattern)
  }
  # Fast path: inline simple symbol case to avoid Env$new() allocation
  if (is.symbol(pattern)) {
    name <- as.character(pattern)
    if (startsWith(name, ".__")) {
      stop(sprintf("%s cannot bind reserved name '%s' (names starting with '.__' are internal)",
                   mode, name), call. = FALSE)
    }
    if (identical(mode, "define")) {
      base::assign(name, value, envir = env)
    } else {
      # set! — walk parent chain (replicates Env$find_existing_env)
      if (!exists(name, envir = env, inherits = TRUE)) {
        stop(sprintf("set!: variable '%s' is not defined", name), call. = FALSE)
      }
      target <- env
      while (!exists(name, envir = target, inherits = FALSE)) {
        target <- parent.env(target)
      }
      base::assign(name, value, envir = target)
    }
    return(invisible(NULL))
  }
  # Slow path: destructuring (cons cells, lists, calls) — need full Env
  ctx <- if (identical(mode, "define")) "define" else "set!"
  Env$new(env)$assign_pattern(pattern, value, mode = mode, context = ctx)
}

# EvalContext: Shared context for MacroExpander and CompiledRuntime. Holds env (Env)
# and source_tracker. Created once per engine; macro_expander and compiler are set after.
#
# @field env Env for the engine.
# @field source_tracker SourceTracker for error locations.
# @field macro_expander Set by Engine after creation.
# @field compiled_runtime Set by Engine after creation.
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
    use_env_cache = NULL,
    coverage_tracker = NULL,
    # @description Create context. macro_expander and compiler are assigned by the engine.
    # @param env Env instance.
    # @param source_tracker SourceTracker instance.
    # @param use_env_cache Logical. If TRUE, enables the env cache.
    # @param coverage_tracker Optional CoverageTracker instance.
    initialize = function(env, source_tracker, use_env_cache = FALSE, coverage_tracker = NULL) {
      if (!r6_isinstance(env, "Env")) {
        stop("EvalContext requires a Env")
      }
      self$env <- env
      self$source_tracker <- source_tracker
      self$use_env_cache <- isTRUE(use_env_cache)
      self$coverage_tracker <- coverage_tracker
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
    module_cache = NULL,
    # @description Create compiled runtime.
    # @param context EvalContext instance.
    # @param load_file_fn Optional; required for load/run/import.
    # @param help_fn Optional; required for (help topic).
    # @param module_cache Optional ModuleCache instance.
    initialize = function(context, load_file_fn = NULL, help_fn = NULL, module_cache = NULL) {
      if (!r6_isinstance(context, "EvalContext")) {
        stop("CompiledRuntime requires an EvalContext")
      }
      self$context <- context
      self$load_file_fn <- load_file_fn
      self$help_fn <- help_fn
      self$module_cache <- module_cache
    },
    # @description Install bindings required for compiled code into env.
    install_helpers = function(env) {
      # Fast path: skip if already installed
      if (exists(".__helpers_installed", envir = env, inherits = FALSE)) {
        # .__env may point to the wrong environment when helpers were
        # copied from a module sub-environment during stdlib loading
        # (e.g. with use_env_cache = FALSE under coverage).
        if (!identical(get0(".__env", envir = env, inherits = FALSE), env)) {
          if (exists(".__env", envir = env, inherits = FALSE)) {
            unlock_binding(".__env", env)
          }
          assign(".__env", env, envir = env)
          lockBinding(".__env", env)
        }
        return(invisible(NULL))
      }

      # Helper to assign, document, and lock internal functions
      assign_and_lock <- function(name, value, description) {
        # Check if binding already exists and is locked
        if (exists(name, envir = env, inherits = FALSE) && bindingIsLocked(name, env)) {
          # Binding already locked (e.g., .__module marker in module envs)
          # Skip assignment to avoid conflict
          return(invisible(NULL))
        }

        assign(name, value, envir = env)
        # Only set rye_doc on non-primitive functions (primitives can't have attributes)
        if (is.function(value) && !is.primitive(value)) {
          obj <- get(name, envir = env)
          attr(obj, "rye_doc") <- list(
            description = paste0("INTERNAL: ", description, " This is part of Rye's compiled code implementation. Direct use is unsupported and may break in future versions.")
          )
          assign(name, obj, envir = env)
        }
        lockBinding(name, env)
      }

      # Environment reference
      assign_and_lock(".__env", env, "Current environment reference.")

      # Utility functions
      assign_and_lock(".__quote", base::quote, "Quote wrapper (base::quote).")
      assign_and_lock(".__true_p", .__true_p, "Truthiness checker.")

      # Core helpers
      assign_and_lock(".__assign_pattern", function(env, pattern, value, mode) {
        .__assign_pattern(env, pattern, value, mode)
      }, "Pattern assignment for define/set!.")

      assign_and_lock(".__load", self$load_file_fn, "File loader for load/run.")

      assign_and_lock(".__help", function(topic, env) {
        if (is.symbol(topic)) topic <- as.character(topic)
        self$help_fn(topic, env)
      }, "Help system accessor.")

      assign_and_lock(".__subscript_call", function(op_name, args, env) {
        self$subscript_call_compiled(op_name, args, env)
      }, "Subscript operator handler ($, [, [[).")

      assign_and_lock("quasiquote", function(expr) {
        if (exists(".__macroexpanding", envir = env, inherits = TRUE) &&
            isTRUE(get(".__macroexpanding", envir = env, inherits = TRUE))) {
          if (is.null(self$context$macro_expander)) {
            stop("macro expander not initialized")
          }
          return(self$context$macro_expander$quasiquote(expr, env))
        }
        self$quasiquote_compiled(expr, env)
      }, "Quasiquote template expander.")

      assign_and_lock(".__attach_doc", function(val, doc_list) {
        # Primitives don't support attributes; wrap in a regular function
        if (is.primitive(val)) {
          prim <- val
          val <- function(...) prim(...)
        }
        attr(val, "rye_doc") <- doc_list
        val
      }, "Attach rye_doc annotation to a value, wrapping primitives.")

      assign_and_lock(".__delay", function(compiled_expr, env) {
        self$promise_new_compiled(compiled_expr, env)
      }, "Promise/delay constructor.")

      assign_and_lock(".__defmacro", function(name, params, body_arg, docstring, env) {
        self$defmacro_compiled(name, params, body_arg, docstring, env)
      }, "Macro definition handler.")

      assign_and_lock(".__macro_quasiquote", function(expr, env) {
        if (is.null(self$context$macro_expander)) {
          stop("macro expander not initialized")
        }
        self$context$macro_expander$quasiquote(expr, env)
      }, "Quasiquote for macro expansion.")

      assign_and_lock(".__module", function(module_name, exports, export_all, body_exprs, src_file, env) {
        self$module_compiled(module_name, exports, export_all, body_exprs, src_file, env)
      }, "Module definition handler.")

      assign_and_lock(".__import", function(arg_value, env) {
        self$import_compiled(arg_value, env)
      }, "Module import handler.")

      assign_and_lock(".__pkg_access", function(op_name, pkg, name, env) {
        self$pkg_access_compiled(op_name, pkg, name, env)
      }, "R package access handler (r/pkg::fn).")

      # Coverage tracking hook: installed when coverage is enabled
      tracker <- self$context$coverage_tracker
      if (!is.null(tracker)) {
        assign_and_lock(".__coverage_track", function(file, start_line, end_line) {
          tracker$track(list(file = file, start_line = start_line, end_line = end_line))
        }, "Coverage tracking hook.")
      }

      # Mark helpers as installed for fast-path check
      assign(".__helpers_installed", TRUE, envir = env)
      lockBinding(".__helpers_installed", env)

      invisible(NULL)
    },
    # @description Run compiled R expression in env (helpers must be installed).
    eval_compiled = function(compiled_expr, env) {
      if (!is.environment(env)) {
        stop("eval_compiled requires an environment")
      }
      # Cache context chain to avoid repeated R6 field lookups
      ctx <- self$context
      rye_env <- ctx$env
      rye_env$push_env(env)
      on.exit(rye_env$pop_env(), add = TRUE)
      self$install_helpers(env)

      eval(compiled_expr, envir = env)
    },
    # Import logic for compiled (import x): same semantics as import special form.
    import_compiled = function(arg_value, env) {
      is_path <- is.character(arg_value) && length(arg_value) == 1
      if (is_path) {
        path_str <- arg_value
        module_path <- private$resolve_path_only(path_str)
        if (is.null(module_path)) {
          stop(sprintf("Module not found: %s", path_str))
        }
        registry_key <- normalize_path_absolute(module_path)
      } else {
        module_name <- Env$new(env)$symbol_or_string(arg_value, "import requires a module name symbol or string")
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
          module_path <- private$resolve_module_path(registry_key)
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
    quasiquote_compiled = function(expr, env) {
      eval_fn <- function(inner, e) {
        compiled <- self$context$compiler$compile(inner, e)
        if (is.null(compiled)) {
          stop("unquote could not be compiled")
        }
        self$eval_compiled(compiled, e)
      }
      quasiquote_expand(expr, env, 1L, eval_fn, wrap_fn = identity,
                            skip_quote = TRUE)
    },
    promise_new_compiled = function(compiled_expr, env) {
      Promise$new(compiled_expr, env, self$eval_compiled)
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
      assign(".__module", TRUE, envir = module_env)
      lockBinding(".__module", module_env)
      self$context$env$module_registry$register(module_name, module_env, exports)
      has_file_path <- !is.null(src_file) && is.character(src_file) &&
        length(src_file) == 1L && nzchar(src_file) && grepl("[/\\\\]", src_file)
      if (has_file_path) {
        absolute_path <- normalize_path_absolute(src_file)
        self$context$env$module_registry$alias(absolute_path, module_name)
      }
      self$install_helpers(module_env)

      # Parse ;;' annotations from source file (or raw text fallback)
      if (!is.null(src_file) && is.character(src_file) &&
          length(src_file) == 1L && nzchar(src_file) && file.exists(src_file)) {
        doc_parser <- DocParser$new()
        parsed_annotations <- doc_parser$parse_file(src_file)
        self$context$compiler$annotations <- parsed_annotations$functions
      } else if (!is.null(self$context$compiler$source_text)) {
        doc_parser <- DocParser$new()
        parsed_annotations <- doc_parser$parse_text(self$context$compiler$source_text)
        self$context$compiler$annotations <- parsed_annotations$functions
      }

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

      # Build evaluation body: interleave coverage calls if coverage is enabled
      coverage_tracker <- self$context$coverage_tracker
      coverage_active <- !is.null(coverage_tracker) && coverage_tracker$enabled
      eval_body <- compiled_body
      if (!is.null(eval_body) && coverage_active) {
        source_tracker <- self$context$source_tracker
        instrumented <- vector("list", length(eval_body) * 2L)
        idx <- 1L
        for (i in seq_along(eval_body)) {
          if (!is.null(source_tracker) && i <= length(body_exprs)) {
            rye_src <- source_tracker$src_get(body_exprs[[i]])
            if (!is.null(rye_src) && !is.null(rye_src$file) && !is.null(rye_src$start_line)) {
              # Narrow to start_line for forms whose sub-expressions are
              # instrumented separately (if, define/defmacro wrapping lambda).
              # For everything else, use full source span.
              end_line <- rye_src$start_line
              narrow <- should_narrow_coverage(body_exprs[[i]])
              if (!narrow && !is.null(rye_src$end_line)) {
                end_line <- rye_src$end_line
              }
              coverage_tracker$register_coverable(rye_src$file, rye_src$start_line, end_line)
              instrumented[[idx]] <- as.call(list(
                as.symbol(".__coverage_track"),
                rye_src$file,
                rye_src$start_line,
                end_line
              ))
              idx <- idx + 1L
            }
          }
          instrumented[[idx]] <- eval_body[[i]]
          idx <- idx + 1L
        }
        eval_body <- instrumented[seq_len(idx - 1L)]
      }

      # Evaluate (use compiled version if we made it, otherwise compile on-the-fly)
      if (!is.null(eval_body)) {
        if (length(eval_body) == 1L) {
          result <- self$eval_compiled(eval_body[[1L]], module_env)
        } else {
          block <- as.call(c(list(quote(`{`)), eval_body))
          result <- self$eval_compiled(block, module_env)
        }
      } else {
        result <- private$eval_seq_compiled(body_exprs, module_env)
      }

      if (export_all) {
        exports <- setdiff(ls(module_env, all.names = TRUE), ".__module")
        self$context$env$module_registry$update_exports(module_name, exports)
      }

      # Write caches after successful module load (skip when coverage is active
      # to avoid caching instrumented code)
      if (should_cache && !is.null(self$module_cache) && !coverage_active) {
        cache_paths <- self$module_cache$get_paths(src_file)
        if (!is.null(cache_paths)) {
          # Always write expr cache (safe fallback)
          self$module_cache$write_code(module_name, compiled_body, exports, export_all, src_file, cache_paths$file_hash)

          # Write env cache ONLY if enabled AND safe
          if (isTRUE(self$context$use_env_cache)) {
            safety <- self$module_cache$is_safe_to_cache(module_env, env)
            if (safety$safe) {
              self$module_cache$write_env(module_name, module_env, exports, src_file, cache_paths$file_hash)
            }
          }
        }
      }

      # Clear annotations after module compilation
      self$context$compiler$annotations <- NULL

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
    eval_seq_compiled = function(exprs, env) {
      if (is.null(exprs) || length(exprs) == 0) {
        return(invisible(NULL))
      }
      if (!is.list(exprs)) {
        exprs <- list(exprs)
      }
      # Cache R6 method references to avoid repeated context lookups in loop
      macro_expander <- self$context$macro_expander
      compiler <- self$context$compiler
      coverage_tracker <- self$context$coverage_tracker
      source_tracker <- self$context$source_tracker
      result <- NULL
      for (expr in exprs) {
        # Track top-level expression start line only (don't paint body ranges)
        if (!is.null(coverage_tracker) && coverage_tracker$enabled && !is.null(source_tracker)) {
          rye_src <- source_tracker$src_get(expr)
          if (!is.null(rye_src) && !is.null(rye_src$file) && !is.null(rye_src$start_line)) {
            coverage_tracker$track(list(
              file = rye_src$file,
              start_line = rye_src$start_line,
              end_line = rye_src$start_line
            ))
          }
        }
        expanded <- macro_expander$macroexpand(expr, env = env, preserve_src = TRUE)
        compiled <- compiler$compile(expanded, env, strict = TRUE)
        result <- self$eval_compiled(compiled, env)
      }
      result
    },
    resolve_module_path = function(name) {
      if (!is.character(name) || length(name) != 1) {
        return(NULL)
      }
      has_separator <- grepl("[/\\\\]", name)
      if (has_separator) {
        if (file.exists(name)) {
          return(name)
        }
        return(NULL)
      }
      stdlib_path <- resolve_stdlib_path(name)
      if (!is.null(stdlib_path)) {
        return(stdlib_path)
      }
      candidates <- c(name, paste0(name, ".rye"))
      for (candidate in candidates) {
        if (file.exists(candidate)) {
          return(candidate)
        }
      }
      NULL
    },
    # Path-only resolution: find file at path or path.rye (no stdlib lookup).
    # Used when import argument is a string (path). Returns NULL if not found.
    resolve_path_only = function(path) {
      if (!is.character(path) || length(path) != 1) {
        return(NULL)
      }
      if (file.exists(path)) {
        return(path)
      }
      with_ext <- paste0(path, ".rye")
      if (file.exists(with_ext)) {
        return(with_ext)
      }
      NULL
    }
  )
)
