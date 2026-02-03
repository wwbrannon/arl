rye_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("rye")),
    error = function(...) "unknown"
  )
}

rye_env_registry <- function(env, name, create = TRUE) {
  if (is.null(env)) {
    env <- parent.frame()
  }
  registry <- get0(name, envir = env, inherits = TRUE)
  if (is.null(registry) && create) {
    registry <- new.env(parent = emptyenv())
    assign(name, registry, envir = env)
    lockBinding(name, env)
  }
  registry
}

# Wrapper for unlockBinding to avoid R CMD check NOTE
# R CMD check flags direct calls to unlockBinding as "possibly unsafe"
rye_unlock_binding <- function(sym, env) {
  if (bindingIsLocked(sym, env)) {
    do.call("unlockBinding", list(sym, env))
  }
}

rye_resolve_stdlib_path <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    return(NULL)
  }
  dir_path <- system.file("rye", package = "rye")
  if (identical(dir_path, "")) {
    return(NULL)
  }
  candidates <- c(
    file.path(dir_path, name),
    file.path(dir_path, paste0(name, ".rye"))
  )
  for (path in candidates) {
    if (file.exists(path)) {
      return(path)
    }
  }
  NULL
}

rye_resolve_module_path <- function(name) {
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
  stdlib_path <- rye_resolve_stdlib_path(name)
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
}

rye_error <- function(message, src_stack = list(), r_stack = list()) {
  structure(
    list(message = message, src_stack = src_stack, r_stack = r_stack),
    class = c("rye_error", "error", "condition")
  )
}

#' Test if an object is an R6 instance of a given class
#'
#' @param obj Any object.
#' @param cls Character class name (e.g. \code{"RyeCons"}).
#' @return Logical.
#' @keywords internal
#' @noRd
#' @importFrom R6 is.R6
r6_isinstance <- function(obj, cls) {
  R6::is.R6(obj) && inherits(obj, cls)
}

#' Test if an object is an R6 class generator for a given class name
#'
#' @param obj Any object.
#' @param cls Character class name (e.g. \code{"RyeCons"}).
#' @return Logical.
#' @keywords internal
#' @noRd
#' @importFrom R6 is.R6Class
r6_issubclass <- function(obj, cls) {
  R6::is.R6Class(obj) && identical(obj$classname, cls)
}
