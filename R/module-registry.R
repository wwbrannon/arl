# Module registry for Rye modules. Tracks loaded modules (name -> list(env, exports, path))
# and supports attach into a target environment. Used by the evaluator for (import ...).
#
#' @keywords internal
#' @noRd
ModuleRegistry <- R6::R6Class(
  "ModuleRegistry",
  public = list(
    rye_env = NULL,
    # @description Create a module registry for the given RyeEnv.
    # @param rye_env A RyeEnv instance.
    initialize = function(rye_env) {
      if (!r6_isinstance(rye_env, "RyeEnv")) {
        stop("ModuleRegistry requires a RyeEnv")
      }
      self$rye_env <- rye_env
      self$rye_env$module_registry_env(create = TRUE)
    },
    # @description Check whether a module is registered.
    # @param name Module name (single string).
    # @return Logical.
    exists = function(name) {
      if (!is.character(name) || length(name) != 1) {
        return(FALSE)
      }
      registry <- self$rye_env$module_registry_env(create = FALSE)
      !is.null(registry) && exists(name, envir = registry, inherits = FALSE)
    },
    # @description Get a module's registry entry (env, exports, path) or NULL.
    # @param name Module name (single string).
    # @return List with elements env, exports, path, or NULL.
    get = function(name) {
      if (!self$exists(name)) {
        return(NULL)
      }
      registry <- self$rye_env$module_registry_env(create = FALSE)
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
      registry <- self$rye_env$module_registry_env(create = TRUE)
      if (self$exists(name)) {
        stop(sprintf("module '%s' is already defined", name))
      }
      entry <- list(env = env, exports = exports, path = path)
      assign(name, entry, envir = registry)
      lockBinding(name, registry)
      entry
    },
    # @description Update a module's exported symbols list.
    # @param name Module name (single string).
    # @param exports New character vector of exported symbol names.
    # @return The updated registry entry.
    update_exports = function(name, exports) {
      if (!is.character(name) || length(name) != 1) {
        stop("module name must be a single string")
      }
      entry <- self$get(name)
      if (is.null(entry)) {
        stop(sprintf("module '%s' is not loaded", name))
      }
      if (!is.character(exports)) {
        exports <- as.character(exports)
      }
      entry$exports <- exports
      registry <- self$rye_env$module_registry_env(create = TRUE)
      rye_unlock_binding(name, registry)
      assign(name, entry, envir = registry)
      lockBinding(name, registry)
      entry
    },
    # @description Remove a module from the registry.
    # @param name Module name (single string).
    unregister = function(name) {
      if (!is.character(name) || length(name) != 1) {
        stop("module name must be a single string")
      }
      registry <- self$rye_env$module_registry_env(create = FALSE)
      if (!is.null(registry) && exists(name, envir = registry, inherits = FALSE)) {
        rye_unlock_binding(name, registry)
        rm(list = name, envir = registry)
      }
      invisible(NULL)
    },
    # @description Attach a module's exports into the registry's RyeEnv.
    # @param name Module name (single string).
    attach = function(name) {
      entry <- self$get(name)
      if (is.null(entry)) {
        stop(sprintf("module '%s' is not loaded", name))
      }
      exports <- entry$exports
      module_env <- entry$env
      target_env <- self$rye_env$env
      target_macro_registry <- self$rye_env$macro_registry_env(create = TRUE)
      module_macro_registry <- self$rye_env$macro_registry_env(module_env, create = FALSE)

      for (export_name in exports) {
        if (!exists(export_name, envir = module_env, inherits = FALSE)) {
          if (!is.null(module_macro_registry) && exists(export_name, envir = module_macro_registry, inherits = FALSE)) {
            macro_fn <- get(export_name, envir = module_macro_registry, inherits = FALSE)
            assign(export_name, macro_fn, envir = target_macro_registry)
            lockBinding(export_name, target_macro_registry)
            next
          }
          stop(sprintf("module '%s' does not export '%s'", name, export_name))
        }
        assign(export_name, get(export_name, envir = module_env, inherits = FALSE), envir = target_env)
      }
      invisible(NULL)
    },
    # @description Attach a module's exports into an arbitrary target environment.
    # @param name Module name (single string).
    # @param target_env Environment to attach exports into.
    attach_into = function(name, target_env) {
      entry <- self$get(name)
      if (is.null(entry)) {
        stop(sprintf("module '%s' is not loaded", name))
      }
      exports <- entry$exports
      module_env <- entry$env
      target_rye <- RyeEnv$new(target_env)
      target_macro_registry <- target_rye$macro_registry_env(create = TRUE)
      module_macro_registry <- self$rye_env$macro_registry_env(module_env, create = FALSE)

      for (export_name in exports) {
        if (!exists(export_name, envir = module_env, inherits = FALSE)) {
          if (!is.null(module_macro_registry) && exists(export_name, envir = module_macro_registry, inherits = FALSE)) {
            macro_fn <- get(export_name, envir = module_macro_registry, inherits = FALSE)
            assign(export_name, macro_fn, envir = target_macro_registry)
            lockBinding(export_name, target_macro_registry)
            next
          }
          stop(sprintf("module '%s' does not export '%s'", name, export_name))
        }
        assign(export_name, get(export_name, envir = module_env, inherits = FALSE), envir = target_env)
      }
      invisible(NULL)
    }
  )
)
