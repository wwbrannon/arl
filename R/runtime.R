missing_default <- function() {
  structure(list(), class = "arl_missing_default")
}

# Compiled-mode helpers: installed in env before eval(compiled, env).
# Arl truthiness: #f (FALSE), #nil (NULL), and 0 are false.
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
      # Active bindings (from proxy imports) are read-only zero-arg functions;
      # base::assign on them triggers the binding function with the value as arg.
      # Remove active binding first to allow regular assignment.
      if (exists(name, envir = env, inherits = FALSE) &&
          bindingIsActive(name, env)) {
        if (bindingIsLocked(name, env)) unlock_binding(name, env)
        rm(list = name, envir = env)
      }
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
      # If the binding lives in a proxy env (active binding from import),
      # create a local shadow in the current env rather than mutating the proxy.
      if (bindingIsActive(name, target) &&
          isTRUE(get0(".__import_proxy", envir = target, inherits = FALSE))) {
        base::assign(name, value, envir = env)
      } else if (bindingIsActive(name, target)) {
        # Active binding in same env (squash mode) — remove and replace
        if (bindingIsLocked(name, target)) unlock_binding(name, target)
        rm(list = name, envir = target)
        base::assign(name, value, envir = target)
      } else {
        base::assign(name, value, envir = target)
      }
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
    coverage_tracker = NULL,
    builtins_env = NULL,
    prelude_env = NULL,
    current_source_file = NULL,
    loading_modules = NULL,
    squash_imports = FALSE,
    reload_env = NULL,
    # @description Create context. macro_expander and compiler are assigned by the engine.
    # @param env Env instance.
    # @param source_tracker SourceTracker instance.
    # @param coverage_tracker Optional CoverageTracker instance.
    initialize = function(env, source_tracker, coverage_tracker = NULL) {
      if (!r6_isinstance(env, "Env")) {
        stop("EvalContext requires a Env")
      }
      self$env <- env
      self$source_tracker <- source_tracker
      self$coverage_tracker <- coverage_tracker
      self$loading_modules <- character(0L)
    }
  )
)

# CompiledRuntime: Runtime helpers for compiled evaluation. Owns helper installation,
# eval_compiled, and compiled-only special form helpers (import/module/defmacro/etc.).
#
# @field context EvalContext (env, source_tracker, macro_expander, compiler).
# @field load_file_fn Function(path, env) for load/import (evaluate in env).
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
    # @param load_file_fn Optional; required for load/import (evaluate in env).
    # @param help_fn Optional; required for (help topic).
    # @param module_cache Optional ModuleCache instance.
    initialize = function(context, load_file_fn = NULL,
                          help_fn = NULL, module_cache = NULL) {
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
        # copied from a module sub-environment during stdlib loading.
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
        # Only set arl_doc on non-primitive functions (primitives can't have attributes)
        if (is.function(value) && !is.primitive(value)) {
          obj <- get(name, envir = env)
          attr(obj, "arl_doc") <- list(
            description = paste0("INTERNAL: ", description, " This is part of Arl's compiled code implementation. Direct use is unsupported and may break in future versions.")
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

      assign_and_lock(".__help", function(topic, env, package = NULL) {
        if (is.symbol(topic)) topic <- as.character(topic)
        if (is.symbol(package)) package <- as.character(package)
        self$help_fn(topic, env, package)
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
        attr(val, "arl_doc") <- doc_list
        val
      }, "Attach arl_doc annotation to a value, wrapping primitives.")

      assign_and_lock(".__delay", function(compiled_expr, env) {
        self$promise_new_compiled(compiled_expr, env)
      }, "Promise/delay constructor.")

      assign_and_lock(".__defmacro", function(name, params, body_arg, doc_list, env) {
        self$defmacro_compiled(name, params, body_arg, doc_list, env)
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

      assign_and_lock(".__import", function(arg_value, env, only = NULL, except = NULL, prefix = NULL, rename = NULL, reload = FALSE) {
        self$import_compiled(arg_value, env, only = only, except = except, prefix = prefix, rename = rename, reload = reload)
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
      arl_env <- ctx$env
      arl_env$push_env(env)
      on.exit(arl_env$pop_env(), add = TRUE)
      self$install_helpers(env)

      eval(compiled_expr, envir = env)
    },
    # Import logic for compiled (import x): same semantics as import special form.
    import_compiled = function(arg_value, env, only = NULL, except = NULL, prefix = NULL, rename = NULL, reload = FALSE) {
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

      if (isTRUE(reload)) {
        # Reload: module must already be registered
        if (!shared_registry$exists(registry_key)) {
          stop(sprintf("cannot reload: module '%s' has not been loaded", registry_key),
               call. = FALSE)
        }
        entry <- shared_registry$get(registry_key)
        if (is.null(entry$path)) {
          stop(sprintf("cannot reload: module '%s' has no source file", registry_key),
               call. = FALSE)
        }
        reload_path <- entry$path
        if (!file.exists(reload_path)) {
          stop(sprintf("cannot reload: file '%s' not found", reload_path),
               call. = FALSE)
        }
        old_env <- entry$env

        # Unregister all keys (name + path aliases)
        all_keys <- shared_registry$find_keys(registry_key)
        for (k in all_keys) {
          shared_registry$unregister(k)
        }

        # Clear module env (preserves env identity)
        clear_module_env(old_env)

        # Set reload context so module_compiled reuses old_env
        self$context$reload_env <- list(expected_env = old_env, active = TRUE)
        on.exit(self$context$reload_env <- NULL, add = TRUE)

        # Reload from file with cache bypass
        if (is.null(self$load_file_fn)) {
          stop("import requires a load_file function")
        }
        self$load_file_fn(reload_path, self$context$env$env, cache = FALSE)

        if (!shared_registry$exists(registry_key)) {
          stop(sprintf("Module '%s' did not register itself after reload", registry_key))
        }

        # Rebuild all existing proxies to reflect new exports
        reloaded_entry <- shared_registry$get(registry_key)
        shared_registry$rebuild_proxies(registry_key, self$context$env$env)

        # Also update squash-mode bindings in prelude_env if applicable
        prelude_env <- self$context$prelude_env
        if (!is.null(prelude_env)) {
          new_exports <- reloaded_entry$exports
          module_macro_registry <- get0(".__macros", envir = reloaded_entry$env, inherits = FALSE)
          # Check if this module has squash bindings in prelude_env
          # (squash bindings are active bindings directly in the env, not in proxies)
          has_squash <- any(vapply(new_exports, function(nm) {
            exists(nm, envir = prelude_env, inherits = FALSE) &&
              tryCatch(bindingIsActive(nm, prelude_env), error = function(e) FALSE)
          }, logical(1)))
          if (has_squash) {
            # Filter to names that exist as regular bindings or macros
            orig_names <- character(0)
            target_names <- character(0)
            for (export_name in new_exports) {
              is_macro <- !is.null(module_macro_registry) &&
                exists(export_name, envir = module_macro_registry, inherits = FALSE)
              if (exists(export_name, envir = reloaded_entry$env, inherits = FALSE) || is_macro) {
                orig_names <- c(orig_names, export_name)
                target_names <- c(target_names, export_name)
              }
            }
            squash_active_bindings(reloaded_entry$env, orig_names, target_names,
                                   module_macro_registry, prelude_env)
          }
        }
      } else {
        # Normal import (non-reload)
        # Cycle detection: check before registry lookup because modules register
        # themselves early (before body finishes evaluating)
        loading <- self$context$loading_modules
        if (registry_key %in% loading) {
          cycle <- c(loading[match(registry_key, loading):length(loading)], registry_key)
          stop(sprintf("Circular dependency detected: %s", paste(cycle, collapse = " -> ")),
               call. = FALSE)
        }

        if (!shared_registry$exists(registry_key)) {
          self$context$loading_modules <- c(loading, registry_key)
          on.exit({
            ctx_loading <- self$context$loading_modules
            self$context$loading_modules <- ctx_loading[ctx_loading != registry_key]
          }, add = TRUE)

          if (is_path) {
            if (is.null(self$load_file_fn)) {
              stop("import requires a load_file function")
            }
            self$load_file_fn(module_path, self$context$env$env)
          } else {
            module_path <- private$resolve_module_path(registry_key)
            if (is.null(module_path)) {
              stop(sprintf("Module not found: %s", registry_key))
            }
            if (is.null(self$load_file_fn)) {
              stop("import requires a load_file function")
            }
            self$load_file_fn(module_path, self$context$env$env)
          }
          if (!shared_registry$exists(registry_key)) {
            stop(sprintf("Module '%s' did not register itself", registry_key))
          }
        }
      }

      shared_registry$attach_into(registry_key, env, only = only, except = except,
                                   prefix = prefix, rename = rename,
                                   squash = isTRUE(self$context$squash_imports))
      # Invalidate macro names cache — import may add proxy envs with new macro registries
      if (!is.null(self$context$macro_expander)) {
        self$context$macro_expander$invalidate_macro_cache()
      }
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
    defmacro_compiled = function(name, params, body_arg, doc_list, env) {
      body_list <- if (is.call(body_arg) && length(body_arg) >= 1 && identical(as.character(body_arg[[1]]), "begin")) {
        as.list(body_arg)[-1]
      } else {
        list(body_arg)
      }
      self$context$macro_expander$defmacro(name, params, body_list, doc_list = doc_list, env = env)
      invisible(NULL)
    },
    module_compiled = function(module_name, exports, export_all, body_exprs, src_file, env) {
      # Module environments inherit from prelude_env (not the engine env
      # with all stdlib), so prelude bindings are visible but other stdlib
      # requires explicit import. Falls back to builtins_env then env.
      module_parent <- self$context$prelude_env
      if (is.null(module_parent)) module_parent <- self$context$builtins_env
      if (is.null(module_parent)) module_parent <- env

      # Check for reload: reuse existing env if reload_env is set
      reload_ctx <- self$context$reload_env
      if (!is.null(reload_ctx) && isTRUE(reload_ctx$active)) {
        module_env <- reload_ctx$expected_env
        reload_ctx$active <- FALSE
        parent.env(module_env) <- module_parent
        # Re-assign .__module marker (was cleared)
        assign(".__module", TRUE, envir = module_env)
        lockBinding(".__module", module_env)
      } else {
        module_env <- new.env(parent = module_parent)
        assign(".__module", TRUE, envir = module_env)
        lockBinding(".__module", module_env)
      }
      has_file_path <- !is.null(src_file) && is.character(src_file) &&
        length(src_file) == 1L && nzchar(src_file) && grepl("[/\\\\]", src_file)
      register_path <- if (has_file_path) normalize_path_absolute(src_file) else NULL
      self$context$env$module_registry$register(module_name, module_env, exports, path = register_path)
      if (has_file_path) {
        self$context$env$module_registry$alias(register_path, module_name)
      }
      self$install_helpers(module_env)

      # Parse ;;' annotations from source file (or raw text fallback)
      my_annotations <- NULL
      if (!is.null(src_file) && is.character(src_file) &&
          length(src_file) == 1L && nzchar(src_file) && file.exists(src_file)) {
        doc_parser <- DocParser$new()
        parsed_annotations <- doc_parser$parse_file(src_file)
        my_annotations <- parsed_annotations$functions
      } else if (!is.null(self$context$compiler$source_text)) {
        doc_parser <- DocParser$new()
        parsed_annotations <- doc_parser$parse_text(self$context$compiler$source_text)
        my_annotations <- parsed_annotations$functions
      }

      # Compile body expressions (for caching)
      should_cache <- !is.null(src_file) && is.character(src_file) &&
                      length(src_file) == 1L && nzchar(src_file) &&
                      file.exists(src_file)

      coverage_tracker <- self$context$coverage_tracker
      coverage_active <- !is.null(coverage_tracker) && coverage_tracker$enabled

      # Interleaved compile/eval: each expression is macro-expanded, compiled,
      # and evaluated before the next one is processed. This ensures that
      # (import X) runs before subsequent expressions try to use X's macros.
      # compiled_body is accumulated for caching.
      compiled_body <- if (should_cache) vector("list", length(body_exprs)) else NULL
      result <- NULL
      source_tracker <- self$context$source_tracker
      for (i in seq_along(body_exprs)) {
        # Coverage instrumentation for this expression
        if (coverage_active && !is.null(source_tracker)) {
          arl_src <- source_tracker$src_get(body_exprs[[i]])
          if (!is.null(arl_src) && !is.null(arl_src$file) && !is.null(arl_src$start_line)) {
            end_line <- arl_src$start_line
            narrow <- should_narrow_coverage(body_exprs[[i]])
            if (!narrow && !is.null(arl_src$end_line)) {
              end_line <- arl_src$end_line
            }
            coverage_tracker$register_coverable(arl_src$file, arl_src$start_line, end_line)
            coverage_tracker$track(list(
              file = arl_src$file,
              start_line = arl_src$start_line,
              end_line = end_line
            ))
          }
        }

        # Restore this module's annotations before each compile, because nested
        # module loads (triggered by import eval) overwrite compiler$annotations.
        self$context$compiler$annotations <- my_annotations
        expanded <- self$context$macro_expander$macroexpand(body_exprs[[i]], env = module_env, preserve_src = TRUE)
        compiled <- self$context$compiler$compile(expanded, module_env, strict = TRUE)
        if (!is.null(compiled_body)) compiled_body[[i]] <- compiled
        result <- self$eval_compiled(compiled, module_env)
      }

      if (export_all) {
        # ls() only returns immediate bindings — proxy-based imports live in
        # parent chain proxies, so they're naturally excluded.
        all_symbols <- ls(module_env, all.names = TRUE)
        # Exclude .__* internals
        all_symbols <- all_symbols[!grepl("^\\.__", all_symbols)]
        self$context$env$module_registry$update_exports(module_name, all_symbols)
      }

      # Write caches after successful module load (skip when coverage is active
      # to avoid caching instrumented code)
      if (should_cache && !is.null(self$module_cache) && !coverage_active) {
        cache_paths <- self$module_cache$get_paths(src_file)
        if (!is.null(cache_paths)) {
          # Always write expr cache (safe fallback)
          self$module_cache$write_code(module_name, compiled_body, exports, export_all, src_file, cache_paths$file_hash)

          # Env cache disabled: proxy-based imports use active bindings in
          # the parent chain which can't survive serialization/deserialization.
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
    eval_seq = function(exprs, env) {
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
          arl_src <- source_tracker$src_get(expr)
          if (!is.null(arl_src) && !is.null(arl_src$file) && !is.null(arl_src$start_line)) {
            coverage_tracker$track(list(
              file = arl_src$file,
              start_line = arl_src$start_line,
              end_line = arl_src$start_line
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
      candidates <- c(name, paste0(name, ".arl"))
      for (candidate in candidates) {
        if (file.exists(candidate)) {
          return(candidate)
        }
      }
      NULL
    },
    # Path-only resolution: find file at path or path.arl (no stdlib lookup).
    # Used when import argument is a string (path). Returns NULL if not found.
    # Relative paths resolve from current_source_file's directory if available,
    # otherwise from CWD.
    resolve_path_only = function(path) {
      if (!is.character(path) || length(path) != 1) {
        return(NULL)
      }
      # If relative and we have a source file context, resolve from its directory
      if (!grepl("^[/~]", path) && !grepl("^[A-Za-z]:", path)) {
        src_file <- self$context$current_source_file
        if (!is.null(src_file)) {
          base_dir <- dirname(src_file)
          resolved <- file.path(base_dir, path)
          if (file.exists(resolved)) return(resolved)
          resolved_ext <- paste0(resolved, ".arl")
          if (file.exists(resolved_ext)) return(resolved_ext)
        }
      }
      if (file.exists(path)) {
        return(path)
      }
      with_ext <- paste0(path, ".arl")
      if (file.exists(with_ext)) {
        return(with_ext)
      }
      NULL
    }
  )
)
