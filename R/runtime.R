missing_default <- function() {
  structure(list(), class = "arl_missing_default")
}

# Compiled-mode helpers: installed in env before eval(compiled, env).
# Arl truthiness: #f (FALSE), #nil (NULL), and 0 are false.
.__true_p <- compiler::cmpfun(function(x) {
  if (is.null(x) || isFALSE(x)) return(FALSE)
  if (is.numeric(x) && length(x) == 1L && !is.na(x) && x == 0) return(FALSE)
  TRUE
})

# Wrapper for define/set! from compiled code (including pattern destructuring).
# pattern can be a symbol, a string (converted to symbol for simple binding), or a list for destructuring.
# Shared set! assignment logic. Given the origin env, the target env where the
# binding was found, name, and value, performs the correct assignment handling
# proxy imports, squash-mode active bindings, locked bindings, and regular bindings.
.__set_into <- compiler::cmpfun(function(env, target, name, value) {
  if (bindingIsActive(name, target)) {
    if (isTRUE(get0(".__import_proxy", envir = target, inherits = FALSE))) {
      # Proxy env (import) — create local shadow, don't mutate module
      base::assign(name, value, envir = env)
    } else {
      # Active binding in same env (squash mode) — remove and replace
      unlock_binding(name, target)
      rm(list = name, envir = target)
      base::assign(name, value, envir = target)
    }
  } else if (bindingIsLocked(name, target)) {
    unlock_binding(name, target)
    base::assign(name, value, envir = target)
    lockBinding(as.symbol(name), target)
  } else {
    base::assign(name, value, envir = target)
  }
  invisible(NULL)
})

.__assign_pattern <- compiler::cmpfun(function(env, pattern, value, mode) {
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
      if (exists(name, envir = env, inherits = FALSE)) {
        if (bindingIsActive(name, env)) {
          # Active bindings (from proxy imports) are read-only zero-arg functions;
          # base::assign on them triggers the binding function with the value as arg.
          # Remove active binding first to allow regular assignment.
          unlock_binding(name, env)
          rm(list = name, envir = env)
        } else if (bindingIsLocked(name, env)) {
          # Locked bindings must be unlocked before reassignment
          unlock_binding(name, env)
        }
      }
      base::assign(name, value, envir = env)
    } else {
      # set! — walk parent chain to find existing binding, then delegate
      if (!exists(name, envir = env, inherits = TRUE)) {
        stop(sprintf("set!: variable '%s' is not defined", name), call. = FALSE)
      }
      target <- env
      while (!exists(name, envir = target, inherits = FALSE)) {
        target <- parent.env(target)
      }
      .__set_into(env, target, name, value)
    }
    return(invisible(NULL))
  }
  # Slow path: destructuring (cons cells, lists, calls) — need full Env
  ctx <- if (identical(mode, "define")) "define" else "set!"
  Env$new(env)$assign_pattern(pattern, value, mode = mode, context = ctx)
})

# Fast-path define for simple symbol names (no destructuring, no reserved-name check,
# no mode dispatch). The compiler emits calls to this directly for (define x val).
.__define <- compiler::cmpfun(function(env, name, value) {
  if (exists(name, envir = env, inherits = FALSE)) {
    if (bindingIsActive(name, env)) {
      # Active bindings (from proxy imports) must be removed before assignment
      unlock_binding(name, env)
      rm(list = name, envir = env)
    } else if (bindingIsLocked(name, env)) {
      # Locked bindings (module bindings) must be unlocked before reassignment
      unlock_binding(name, env)
    }
  }
  base::assign(name, value, envir = env)
  invisible(NULL)
})

# Fast-path set! for simple symbol names. Single bounded loop that stops at
# boundary (parent.env(builtins_env)) to avoid walking into R package envs.
.__set_impl <- compiler::cmpfun(function(env, name, value, boundary) {
  target <- env
  while (!identical(target, boundary) && !identical(target, emptyenv())) {
    if (exists(name, envir = target, inherits = FALSE)) {
      .__set_into(env, target, name, value)
      return(invisible(NULL))
    }
    target <- parent.env(target)
  }
  stop(sprintf("set!: variable '%s' is not defined", name), call. = FALSE)
})

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
  "ArlEvalContext",
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
    expected_module_name = NULL,
    cache_paths = NULL,
    # @description Create context. macro_expander and compiler are assigned by the engine.
    # @param env Env instance.
    # @param source_tracker SourceTracker instance.
    # @param coverage_tracker Optional CoverageTracker instance.
    initialize = function(env, source_tracker, coverage_tracker = NULL) {
      if (!inherits(env, "ArlEnv")) {
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
  "ArlCompiledRuntime",
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
      if (!inherits(context, "ArlEvalContext")) {
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

        # Set arl_doc on value before assigning (avoids get+reassign cycle)
        if (is.function(value) && !is.primitive(value)) {
          attr(value, "arl_doc") <- list(
            description = paste0(
              "INTERNAL: ", description,
              " This is part of Arl's compiled code implementation.",
              " Direct use is unsupported and may break in future versions."
            )
          )
        }
        assign(name, value, envir = env)
        lockBinding(name, env)
      }

      # Environment reference
      assign_and_lock(".__env", env, "Current environment reference.")

      # Utility functions
      assign_and_lock(".__quote", base::quote, "Quote wrapper (base::quote).")
      assign_and_lock(".__true_p", .__true_p, "Truthiness checker.")

      # Core helpers
      assign_and_lock(".__assign_pattern", .__assign_pattern,
        "Pattern assignment for define/set! (destructuring).")

      assign_and_lock(".__define", .__define,
        "Fast-path define for simple symbol names.")

      boundary <- parent.env(self$context$builtins_env)
      assign_and_lock(".__set", function(env, name, value) {
        .__set_impl(env, name, value, boundary)
      }, "Fast-path set! for simple symbol names.")

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

      assign_and_lock(".__module", function(module_name, exports, export_all, re_export, body_exprs, src_file, env) {
        self$module_compiled(module_name, exports, export_all, re_export, body_exprs, src_file, env)
      }, "Module definition handler.")

      assign_and_lock(".__import", function(arg_value, env, rename = NULL,
                                            reload = FALSE, as_alias = NULL,
                                            refer = NULL) {
        self$import_compiled(arg_value, env, rename = rename, reload = reload, as_alias = as_alias, refer = refer)
      }, "Module import handler.")

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
    import_compiled = function(arg_value, env, rename = NULL, reload = FALSE, as_alias = NULL, refer = NULL) {
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

          # Set expected_module_name so nameless modules can derive their name
          old_expected <- self$context$expected_module_name
          self$context$expected_module_name <- registry_key
          on.exit(self$context$expected_module_name <- old_expected, add = TRUE)

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

      squash <- isTRUE(self$context$squash_imports)

      # Bind module env to a name in the importing environment.
      # Skip for path-based imports (is_path) unless :as is specified.
      # Skip when squashing for prelude.
      # Skip when a referred export name collides with the module binding name
      # (e.g. dict module exports dict — the proxy binding would be shadowed).
      if (!squash && (!is_path || !is.null(as_alias))) {
        entry <- shared_registry$get(registry_key)
        if (!is.null(entry)) {
          if (!is.null(as_alias)) {
            local_name <- as_alias
          } else {
            # Use last /-component of module name, or full name if no /
            parts <- strsplit(registry_key, "/", fixed = TRUE)[[1]]
            local_name <- parts[length(parts)]
          }
          # Check for collision with referred exports
          skip_binding <- FALSE
          if (!is.null(refer)) {
            referred_names <- if (isTRUE(refer)) entry$exports else refer
            if (!is.null(rename)) {
              idx <- match(names(rename), referred_names)
              valid <- !is.na(idx)
              referred_names[idx[valid]] <- unname(rename[valid])
            }
            skip_binding <- local_name %in% referred_names
          }
          if (!skip_binding) {
            assign(local_name, entry$env, envir = env)
          }

          # Create namespace node for hierarchical names (if no :as and name has /)
          if (is.null(as_alias) && grepl("/", registry_key, fixed = TRUE)) {
            parts <- strsplit(registry_key, "/", fixed = TRUE)[[1]]
            top <- parts[1]
            if (nzchar(top)) {
              # Only create namespace node if top-level name isn't already bound to something else
              existing <- get0(top, envir = env, inherits = FALSE)
              if (is.null(existing) || inherits(existing, "arl_namespace")) {
                if (is.null(existing)) {
                  assign(top, make_namespace_node(top), envir = env)
                }
              }
            }
          }
        }
      }

      # Determine how to bind unqualified names (after module binding so exports win)
      if (!is.null(refer)) {
        if (isTRUE(refer)) {
          # :refer :all — attach all exports
          shared_registry$attach_into(registry_key, env, only = NULL,
                                       rename = rename, squash = squash)
        } else {
          # :refer (sym1 sym2 ...) — attach only listed symbols
          shared_registry$attach_into(registry_key, env, only = refer,
                                       rename = rename, squash = squash)
        }
      } else if (!is.null(as_alias)) {
        # :as without :refer — no unqualified imports (qualified access only)
        # Still need to attach for rename if specified
        if (!is.null(rename)) {
          shared_registry$attach_into(registry_key, env, only = NULL,
                                       rename = rename, squash = squash)
        }
        # Otherwise, no attach_into call — only module binding
      } else if (!is.null(rename)) {
        # :rename alone implies all exports
        shared_registry$attach_into(registry_key, env, only = NULL,
                                     rename = rename, squash = squash)
      } else if (squash) {
        # Squashed (prelude) imports: always dump exports
        shared_registry$attach_into(registry_key, env, squash = squash)
      } else {
        # Bare (import X): bind module only, no unqualified exports
        # Use :refer :all to dump exports into scope
      }

      # Invalidate macro names cache — import may add proxy envs with new macro registries
      if (!is.null(self$context$macro_expander)) {
        self$context$macro_expander$invalidate_macro_cache()
      }
      invisible(NULL)
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
    module_compiled = function(module_name, exports, export_all, re_export, body_exprs, src_file, env) {
      # Handle nameless modules: derive name from expected_module_name or file path
      if (identical(module_name, "")) {
        if (!is.null(self$context$expected_module_name)) {
          module_name <- self$context$expected_module_name
        } else if (!is.null(src_file) && is.character(src_file) &&
                   length(src_file) == 1L && nzchar(src_file)) {
          module_name <- private$derive_module_name_from_path(src_file)
        } else {
          stop("nameless module requires either a file context or expected_module_name", call. = FALSE)
        }
      }

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
        module_env <- arl_new_env(parent = module_parent)
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

      finalize_module_env(module_env, module_name, exports, export_all, re_export,
                         self$context$env$module_registry)

      # Write caches after successful module load (skip when coverage is active
      # to avoid caching instrumented code)
      if (should_cache && !is.null(self$module_cache) && !coverage_active) {
        # Use cache_paths from context (threaded from engine's initial
        # get_paths() call) to avoid TOCTOU races — if the file changed
        # between read and now, a fresh get_paths() would compute the new
        # hash and map it to the old compiled code.
        cache_paths <- self$context$cache_paths
        if (is.null(cache_paths)) {
          cache_paths <- self$module_cache$get_paths(src_file)
        }
        if (!is.null(cache_paths)) {
          # Extract compiler flags for cache validation
          compiler_flags <- NULL
          comp <- self$context$compiler
          if (!is.null(comp)) {
            compiler_flags <- comp$get_flags()
          }
          # Always write expr cache (safe fallback)
          self$module_cache$write_code(
            module_name, compiled_body, exports, export_all,
            re_export, src_file, cache_paths$file_hash,
            cache_paths = cache_paths, compiler_flags = compiler_flags
          )

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
    derive_module_name_from_path = function(src_file) {
      # Try to derive relative to stdlib root
      stdlib_root <- system.file("arl", package = "arl")
      if (nzchar(stdlib_root) && startsWith(normalizePath(src_file, mustWork = FALSE),
                                             normalizePath(stdlib_root, mustWork = FALSE))) {
        rel <- substring(normalizePath(src_file, mustWork = FALSE),
                         nchar(normalizePath(stdlib_root, mustWork = FALSE)) + 2)
        return(sub("\\.arl$", "", rel))
      }
      # Fallback: basename minus .arl
      sub("\\.arl$", "", basename(src_file))
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

# Finalize a module environment after evaluation: re-export forwardings,
# export_all symbol collection, registry update, and binding locking.
finalize_module_env <- function(module_env, module_name, exports, export_all, re_export, module_registry) {
  # Re-export forwarding for explicitly exported names that are imported
  if (!export_all && length(exports) > 0) {
    create_reexport_forwardings(module_env, exports, module_name)
  }

  if (export_all) {
    # ls() only returns immediate bindings — proxy-based imports live in
    # parent chain proxies, so they're naturally excluded.
    all_symbols <- ls(module_env, all.names = TRUE)
    all_symbols <- all_symbols[!grepl("^\\.__", all_symbols)]
    # Exclude _* user-private helpers (convention: _ prefix = module-private)
    all_symbols <- all_symbols[!grepl("^_", all_symbols)]

    if (isTRUE(re_export)) {
      imported_names <- collect_proxy_imported_names(module_env)
      new_names <- setdiff(imported_names, all_symbols)
      if (length(new_names) > 0) {
        create_reexport_forwardings(module_env, new_names, module_name)
        all_symbols <- c(all_symbols, new_names)
      }
    }

    module_registry$update_exports(module_name, all_symbols)
  }

  # Lock all individual bindings in module environment for immutability.
  # Uses lockBinding (not lockEnvironment) so reload can unlock them.
  all_binding_names <- ls(module_env, all.names = TRUE)
  for (nm in all_binding_names) {
    if (!bindingIsLocked(nm, module_env)) {
      lockBinding(as.symbol(nm), module_env)
    }
  }
}

# Collect imported symbol names from proxy environments in the parent chain.
# Walks from parent.env(module_env) upward, collecting .__import_target_names
# from each import proxy.
collect_proxy_imported_names <- function(module_env) {
  names <- character(0)
  p <- parent.env(module_env)
  while (!identical(p, emptyenv())) {
    if (isTRUE(get0(".__import_proxy", envir = p, inherits = FALSE))) {
      target_names <- get0(".__import_target_names", envir = p, inherits = FALSE)
      if (!is.null(target_names)) {
        names <- c(names, target_names)
      }
    }
    p <- parent.env(p)
  }
  unique(names)
}

# Create forwarding active bindings in module_env for re-exported names.
# For each name that is not an own binding in module_env but is accessible
# via the parent chain (import proxies), create a forwarding active binding
# so attach_into's inherits=FALSE lookup can find it.
# Also forwards macros from proxy .__macros registries.
create_reexport_forwardings <- function(module_env, names, module_name) {
  parent <- parent.env(module_env)
  for (nm in names) {
    if (!exists(nm, envir = module_env, inherits = FALSE)) {
      if (exists(nm, envir = parent, inherits = TRUE)) {
        # Create forwarding active binding that looks up from parent chain
        local({
          sym <- nm
          par <- parent
          makeActiveBinding(sym, function() get(sym, envir = par, inherits = TRUE), module_env)
        })
      } else {
        stop(sprintf("module '%s' exports '%s' but it is not defined or imported",
                      module_name, nm), call. = FALSE)
      }
    }
  }
  # Forward macros from proxy .__macros registries
  module_macro_registry <- get0(".__macros", envir = module_env, inherits = FALSE)
  p <- parent
  while (!identical(p, emptyenv())) {
    if (isTRUE(get0(".__import_proxy", envir = p, inherits = FALSE))) {
      proxy_macros <- get0(".__macros", envir = p, inherits = FALSE)
      if (!is.null(proxy_macros)) {
        for (nm in names) {
          if (exists(nm, envir = proxy_macros, inherits = FALSE)) {
            # Ensure module has its own macro registry
            if (is.null(module_macro_registry)) {
              module_macro_registry <- arl_new_env(parent = emptyenv())
              base::assign(".__macros", module_macro_registry, envir = module_env)
              lockBinding(".__macros", module_env)
            }
            if (!exists(nm, envir = module_macro_registry, inherits = FALSE)) {
              local({
                sym <- nm
                src_macros <- proxy_macros
                makeActiveBinding(sym, function() get(sym, envir = src_macros, inherits = FALSE), module_macro_registry)
              })
            }
          }
        }
      }
    }
    p <- parent.env(p)
  }
  invisible(NULL)
}
