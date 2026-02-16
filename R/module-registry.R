# Module registry for Arl modules. Tracks loaded modules (name -> list(env, exports, path))
# and supports attach into a target environment. Used by compiled runtime for (import ...).
#
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
    attach = function(name) {
      entry <- self$get(name)
      if (is.null(entry)) {
        stop(sprintf("module '%s' is not loaded", name))
      }
      exports <- entry$exports
      module_env <- entry$env
      target_env <- self$arl_env$env
      target_macro_registry <- get0(".__macros", envir = target_env, inherits = FALSE)
      if (is.null(target_macro_registry)) {
        target_macro_registry <- new.env(parent = emptyenv())
        base::assign(".__macros", target_macro_registry, envir = target_env)
        lockBinding(".__macros", target_env)
      }
      module_macro_registry <- get0(".__macros", envir = module_env, inherits = FALSE)

      for (export_name in exports) {
        is_macro <- !is.null(module_macro_registry) &&
          exists(export_name, envir = module_macro_registry, inherits = FALSE)
        if (is_macro) {
          macro_fn <- get(export_name, envir = module_macro_registry, inherits = FALSE)
          if (exists(export_name, envir = target_macro_registry, inherits = FALSE)) {
            unlock_binding(export_name, target_macro_registry)
          }
          assign(export_name, macro_fn, envir = target_macro_registry)
          lockBinding(export_name, target_macro_registry)
        }
        if (exists(export_name, envir = module_env, inherits = FALSE)) {
          assign(export_name, get(export_name, envir = module_env, inherits = FALSE), envir = target_env)
        } else if (!is_macro) {
          stop(sprintf("module '%s' does not export '%s'", name, export_name))
        }
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
    attach_into = function(name, target_env, only = NULL, except = NULL, prefix = NULL, rename = NULL) {
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

      # Get or create a LOCAL macro registry in the target env (not inherited)
      target_macro_registry <- get0(".__macros", envir = target_env, inherits = FALSE)
      if (is.null(target_macro_registry)) {
        target_macro_registry <- new.env(parent = emptyenv())
        base::assign(".__macros", target_macro_registry, envir = target_env)
        lockBinding(".__macros", target_env)
      }
      module_macro_registry <- get0(".__macros", envir = module_env, inherits = FALSE)

      # Copy exports with mapped names; macros go to macro registry AND as regular bindings
      regular_orig <- character(0)
      regular_target <- character(0)
      for (j in seq_along(exports)) {
        export_name <- exports[j]
        mapped_name <- target_names[j]
        is_macro <- !is.null(module_macro_registry) &&
          exists(export_name, envir = module_macro_registry, inherits = FALSE)
        if (is_macro) {
          # Copy macro to target macro registry
          macro_fn <- get(export_name, envir = module_macro_registry, inherits = FALSE)
          if (exists(mapped_name, envir = target_macro_registry, inherits = FALSE)) {
            unlock_binding(mapped_name, target_macro_registry)
          }
          base::assign(mapped_name, macro_fn, envir = target_macro_registry)
          lockBinding(mapped_name, target_macro_registry)
          # Also copy as regular binding (macros are callable values too)
          if (exists(export_name, envir = module_env, inherits = FALSE)) {
            regular_orig <- c(regular_orig, export_name)
            regular_target <- c(regular_target, mapped_name)
          }
        } else if (exists(export_name, envir = module_env, inherits = FALSE)) {
          regular_orig <- c(regular_orig, export_name)
          regular_target <- c(regular_target, mapped_name)
        } else {
          stop(sprintf("module '%s' does not export '%s'", name, export_name))
        }
      }

      # Bulk copy regular exports with mapped names
      if (length(regular_orig) > 0L) {
        vals <- mget(regular_orig, envir = module_env, inherits = FALSE)
        names(vals) <- regular_target
        list2env(vals, envir = target_env)
      }
      invisible(NULL)
    }
  )
)
