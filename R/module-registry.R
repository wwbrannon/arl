#' Module registry wrapper
#'
#' @keywords internal
#' @noRd
ModuleRegistry <- R6::R6Class(
  "ModuleRegistry",
  public = list(
    rye_env = NULL,
    initialize = function(rye_env) {
      if (!inherits(rye_env, "RyeEnv")) {
        stop("ModuleRegistry requires a RyeEnv")
      }
      self$rye_env <- rye_env
      self$rye_env$module_registry_env(create = TRUE)
    },
    exists = function(name) {
      if (!is.character(name) || length(name) != 1) {
        return(FALSE)
      }
      registry <- self$rye_env$module_registry_env(create = FALSE)
      !is.null(registry) && exists(name, envir = registry, inherits = FALSE)
    },
    get = function(name) {
      if (!self$exists(name)) {
        return(NULL)
      }
      registry <- self$rye_env$module_registry_env(create = FALSE)
      get(name, envir = registry, inherits = FALSE)
    },
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
      if (bindingIsLocked(name, registry)) {
        unlockBinding(name, registry)
      }
      assign(name, entry, envir = registry)
      lockBinding(name, registry)
      entry
    },
    unregister = function(name) {
      if (!is.character(name) || length(name) != 1) {
        stop("module name must be a single string")
      }
      registry <- self$rye_env$module_registry_env(create = FALSE)
      if (!is.null(registry) && exists(name, envir = registry, inherits = FALSE)) {
        if (bindingIsLocked(name, registry)) {
          unlockBinding(name, registry)
        }
        rm(list = name, envir = registry)
      }
      invisible(NULL)
    },
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
    }
  )
)
