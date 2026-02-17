# Module registry for Arl modules. Tracks loaded modules (name -> list(env, exports, path))
# and supports attach into a target environment. Used by compiled runtime for (import ...).

# Create a proxy environment with active bindings forwarding to module_env,
# and splice it into target_env's parent chain.
# orig_names: names in module_env to bind from
# target_names: names to bind as in the proxy (after rename/prefix)
# module_macro_registry: the module's .__macros env (or NULL)
# target_env: the importing environment
# module_name: name of the module (for idempotency markers)
create_import_proxy <- function(module_env, orig_names, target_names,
                                module_macro_registry, target_env, module_name) {
  proxy <- new.env(parent = parent.env(target_env))
  assign(".__import_proxy", TRUE, envir = proxy)
  lockBinding(".__import_proxy", proxy)
  assign(".__import_module_name", module_name, envir = proxy)
  lockBinding(".__import_module_name", proxy)

  # Create active bindings for regular exports
  for (i in seq_along(orig_names)) {
    local({
      oname <- orig_names[i]
      tname <- target_names[i]
      makeActiveBinding(tname, function() {
        get(oname, envir = module_env, inherits = FALSE)
      }, env = proxy)
    })
  }

  # Create macro registry in proxy for exported macros
  if (!is.null(module_macro_registry)) {
    proxy_macro_registry <- new.env(parent = emptyenv())
    has_macros <- FALSE
    for (i in seq_along(orig_names)) {
      local({
        oname <- orig_names[i]
        tname <- target_names[i]
        if (exists(oname, envir = module_macro_registry, inherits = FALSE)) {
          makeActiveBinding(tname, function() {
            get(oname, envir = module_macro_registry, inherits = FALSE)
          }, env = proxy_macro_registry)
          has_macros <<- TRUE
        }
      })
    }
    if (has_macros) {
      assign(".__macros", proxy_macro_registry, envir = proxy)
      lockBinding(".__macros", proxy)
    }
  }

  # Splice proxy into parent chain
  parent.env(target_env) <- proxy
  invisible(proxy)
}

# Create active bindings directly in target_env (no proxy, no chain modification).
# Used for prelude squash loading and toplevel_env test helper.
squash_active_bindings <- function(module_env, orig_names, target_names,
                                   module_macro_registry, target_env) {
  for (i in seq_along(orig_names)) {
    local({
      oname <- orig_names[i]
      tname <- target_names[i]
      # Remove existing binding if present (may be locked from previous squash)
      if (exists(tname, envir = target_env, inherits = FALSE)) {
        if (bindingIsLocked(tname, target_env)) {
          unlock_binding(tname, target_env)
        }
        rm(list = tname, envir = target_env)
      }
      makeActiveBinding(tname, function() {
        get(oname, envir = module_env, inherits = FALSE)
      }, env = target_env)
    })
  }

  # Handle macros: copy to target's macro registry
  if (!is.null(module_macro_registry)) {
    target_macro_registry <- get0(".__macros", envir = target_env, inherits = FALSE)
    if (is.null(target_macro_registry)) {
      target_macro_registry <- new.env(parent = emptyenv())
      base::assign(".__macros", target_macro_registry, envir = target_env)
      lockBinding(".__macros", target_env)
    }
    for (i in seq_along(orig_names)) {
      oname <- orig_names[i]
      tname <- target_names[i]
      if (exists(oname, envir = module_macro_registry, inherits = FALSE)) {
        macro_fn <- get(oname, envir = module_macro_registry, inherits = FALSE)
        if (exists(tname, envir = target_macro_registry, inherits = FALSE)) {
          unlock_binding(tname, target_macro_registry)
        }
        base::assign(tname, macro_fn, envir = target_macro_registry)
        lockBinding(tname, target_macro_registry)
      }
    }
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
ModuleRegistry <- R6::R6Class(
  "ModuleRegistry",
  public = list(
    arl_env = NULL,
    # @description Create a module registry for the given Env.
    # @param arl_env A Env instance.
    initialize = function(arl_env) {
      if (!r6_isinstance(arl_env, "Env")) {
        stop("ModuleRegistry requires a Env")
      }
      self$arl_env <- arl_env
      self$arl_env$module_registry_env(create = TRUE)
    },
    # @description Check whether a module is registered.
    # @param name Module name (single string).
    # @return Logical.
    exists = function(name) {
      if (!is.character(name) || length(name) != 1) {
        return(FALSE)
      }
      registry <- self$arl_env$module_registry_env(create = FALSE)
      !is.null(registry) && exists(name, envir = registry, inherits = FALSE)
    },
    # @description Get a module's registry entry (env, exports, path) or NULL.
    # @param name Module name (single string).
    # @return List with elements env, exports, path, or NULL.
    get = function(name) {
      if (!self$exists(name)) {
        return(NULL)
      }
      registry <- self$arl_env$module_registry_env(create = FALSE)
      get(name, envir = registry, inherits = FALSE)
    },
    # @description Register a loaded module.
    # @param name Module name (single string).
    # @param env Module environment.
    # @param exports Character vector of exported symbol names.
    # @param path Optional file path the module was loaded from.
    # @return The registry entry (invisible).
    register = function(name, env, exports, path = NULL) {
      if (!is.character(name) || length(name) != 1) {
        stop("module name must be a single string")
      }
      registry <- self$arl_env$module_registry_env(create = TRUE)
      if (self$exists(name)) {
        stop(sprintf("module '%s' is already defined", name))
      }
      # Create locked environment entry instead of list for true immutability
      entry_env <- new.env(parent = emptyenv())
      entry_env$env <- env
      entry_env$exports <- exports
      entry_env$path <- path
      lockEnvironment(entry_env, bindings = TRUE)
      assign(name, entry_env, envir = registry)
      lockBinding(name, registry)
      entry_env
    },
    # @description Update a module's exported symbols list.
    # @param name Module name (single string).
    # @param exports New character vector of exported symbol names.
    # @return The updated registry entry.
    update_exports = function(name, exports) {
      if (!is.character(name) || length(name) != 1) {
        stop("module name must be a single string")
      }
      old_entry <- self$get(name)
      if (is.null(old_entry)) {
        stop(sprintf("module '%s' is not loaded", name))
      }
      if (!is.character(exports)) {
        exports <- as.character(exports)
      }
      # Create new locked environment with updated exports (can't mutate old one)
      entry_env <- new.env(parent = emptyenv())
      entry_env$env <- old_entry$env
      entry_env$exports <- exports
      entry_env$path <- old_entry$path
      lockEnvironment(entry_env, bindings = TRUE)
      registry <- self$arl_env$module_registry_env(create = TRUE)
      unlock_binding(name, registry)
      assign(name, entry_env, envir = registry)
      lockBinding(name, registry)
      entry_env
    },
    # @description Register an alias: path (absolute) -> same entry as name.
    # Used so (import "path/to/file.arl") can find the module registered as (module X ...).
    # @param path Absolute path string (use normalize_path_absolute first).
    # @param name Module name (single string) already registered.
    # @return The registry entry (invisible). Idempotent if path already aliases same module.
    alias = function(path, name) {
      if (!is.character(path) || length(path) != 1 || !is.character(name) || length(name) != 1) {
        stop("alias requires path and name as single strings")
      }
      registry <- self$arl_env$module_registry_env(create = TRUE)
      entry <- self$get(name)
      if (is.null(entry)) {
        stop(sprintf("module '%s' is not loaded", name))
      }
      if (exists(path, envir = registry, inherits = FALSE)) {
        existing <- get(path, envir = registry, inherits = FALSE)
        if (identical(existing, entry)) {
          return(invisible(entry))
        }
        stop(sprintf("path '%s' is already bound to a different module", path))
      }
      assign(path, entry, envir = registry)
      lockBinding(path, registry)
      invisible(entry)
    },
    # @description Remove a module from the registry.
    # @param name Module name (single string).
    unregister = function(name) {
      if (!is.character(name) || length(name) != 1) {
        stop("module name must be a single string")
      }
      registry <- self$arl_env$module_registry_env(create = FALSE)
      if (!is.null(registry) && exists(name, envir = registry, inherits = FALSE)) {
        unlock_binding(name, registry)
        rm(list = name, envir = registry)
      }
      invisible(NULL)
    },
    # @description Attach a module's exports into the registry's Env.
    # @param name Module name (single string).
    # @param squash If TRUE, create active bindings directly in target (no proxy).
    attach = function(name, squash = FALSE) {
      entry <- self$get(name)
      if (is.null(entry)) {
        stop(sprintf("module '%s' is not loaded", name))
      }
      exports <- entry$exports
      module_env <- entry$env
      target_env <- self$arl_env$env
      module_macro_registry <- get0(".__macros", envir = module_env, inherits = FALSE)

      # Filter to names that exist as regular bindings or macros
      orig_names <- character(0)
      target_names <- character(0)
      for (export_name in exports) {
        is_macro <- !is.null(module_macro_registry) &&
          exists(export_name, envir = module_macro_registry, inherits = FALSE)
        if (exists(export_name, envir = module_env, inherits = FALSE)) {
          orig_names <- c(orig_names, export_name)
          target_names <- c(target_names, export_name)
        } else if (!is_macro) {
          stop(sprintf("module '%s' does not export '%s'", name, export_name))
        }
      }

      if (isTRUE(squash)) {
        squash_active_bindings(module_env, orig_names, target_names,
                               module_macro_registry, target_env)
      } else {
        # Idempotency: check if proxy for this module already exists
        p <- parent.env(target_env)
        while (!identical(p, emptyenv())) {
          if (isTRUE(get0(".__import_proxy", envir = p, inherits = FALSE)) &&
              identical(get0(".__import_module_name", envir = p, inherits = FALSE), name)) {
            return(invisible(NULL))
          }
          p <- parent.env(p)
        }
        create_import_proxy(module_env, orig_names, target_names,
                            module_macro_registry, target_env, name)
      }
      invisible(NULL)
    },
    # @description Attach a module's exports into an arbitrary target environment.
    # @param name Module name (single string).
    # @param target_env Environment to attach exports into.
    # @param only Character vector of names to import (NULL = all). Mutually exclusive with except.
    # @param except Character vector of names to exclude (NULL = none). Mutually exclusive with only.
    # @param prefix String to prepend to all imported names (NULL = no prefix).
    # @param rename Named character vector: names are original, values are new names (NULL = no rename).
    # @param squash If TRUE, create active bindings directly in target_env (no proxy).
    attach_into = function(name, target_env, only = NULL, except = NULL, prefix = NULL, rename = NULL, squash = FALSE) {
      entry <- self$get(name)
      if (is.null(entry)) {
        stop(sprintf("module '%s' is not loaded", name))
      }
      exports <- entry$exports
      module_env <- entry$env

      # Apply filtering: only/except
      if (!is.null(only)) {
        bad <- setdiff(only, exports)
        if (length(bad) > 0L) {
          stop(sprintf("module '%s' does not export '%s'", name, bad[1L]), call. = FALSE)
        }
        exports <- intersect(exports, only)
      } else if (!is.null(except)) {
        bad <- setdiff(except, exports)
        if (length(bad) > 0L) {
          stop(sprintf("module '%s' does not export '%s'", name, bad[1L]), call. = FALSE)
        }
        exports <- setdiff(exports, except)
      }

      # Build name mapping: original_name -> target_name
      # Apply rename first, then prefix
      target_names <- exports
      if (!is.null(rename)) {
        bad <- setdiff(names(rename), exports)
        if (length(bad) > 0L) {
          stop(sprintf("module '%s' does not export '%s'", name, bad[1L]), call. = FALSE)
        }
        idx <- match(names(rename), exports)
        valid <- !is.na(idx)
        target_names[idx[valid]] <- unname(rename[valid])
      }
      if (!is.null(prefix)) {
        target_names <- paste0(prefix, target_names)
      }

      module_macro_registry <- get0(".__macros", envir = module_env, inherits = FALSE)

      # Separate regular bindings from macro-only exports
      regular_orig <- character(0)
      regular_target <- character(0)
      for (j in seq_along(exports)) {
        export_name <- exports[j]
        mapped_name <- target_names[j]
        is_macro <- !is.null(module_macro_registry) &&
          exists(export_name, envir = module_macro_registry, inherits = FALSE)
        if (exists(export_name, envir = module_env, inherits = FALSE)) {
          regular_orig <- c(regular_orig, export_name)
          regular_target <- c(regular_target, mapped_name)
        } else if (!is_macro) {
          stop(sprintf("module '%s' does not export '%s'", name, export_name))
        }
      }

      if (isTRUE(squash)) {
        squash_active_bindings(module_env, regular_orig, regular_target,
                               module_macro_registry, target_env)
      } else {
        # Idempotency: check if proxy for this module already exists
        p <- parent.env(target_env)
        while (!identical(p, emptyenv())) {
          if (isTRUE(get0(".__import_proxy", envir = p, inherits = FALSE)) &&
              identical(get0(".__import_module_name", envir = p, inherits = FALSE), name)) {
            return(invisible(NULL))
          }
          p <- parent.env(p)
        }

        # Ensure target has a macro registry for macro-only exports
        # (create_import_proxy handles macros that also have regular bindings)
        create_import_proxy(module_env, regular_orig, regular_target,
                            module_macro_registry, target_env, name)

        # Handle macro-only exports (exist in macro registry but not as regular bindings)
        # These need to go into the target's own macro registry
        target_macro_registry <- get0(".__macros", envir = target_env, inherits = FALSE)
        if (is.null(target_macro_registry)) {
          target_macro_registry <- new.env(parent = emptyenv())
          base::assign(".__macros", target_macro_registry, envir = target_env)
          lockBinding(".__macros", target_env)
        }
        if (!is.null(module_macro_registry)) {
          for (j in seq_along(exports)) {
            export_name <- exports[j]
            mapped_name <- target_names[j]
            if (!exists(export_name, envir = module_env, inherits = FALSE) &&
                exists(export_name, envir = module_macro_registry, inherits = FALSE)) {
              macro_fn <- get(export_name, envir = module_macro_registry, inherits = FALSE)
              if (exists(mapped_name, envir = target_macro_registry, inherits = FALSE)) {
                unlock_binding(mapped_name, target_macro_registry)
              }
              base::assign(mapped_name, macro_fn, envir = target_macro_registry)
              lockBinding(mapped_name, target_macro_registry)
            }
          }
        }
      }
      invisible(NULL)
    }
  )
)
