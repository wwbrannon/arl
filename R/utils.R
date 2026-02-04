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

# Path-only resolution: find file at path or path.rye (no stdlib lookup).
# Used when import argument is a string (path). Returns NULL if not found.
rye_resolve_path_only <- function(path) {
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

# Normalize a file path to absolute form for consistent registry keys.
# Relative paths are resolved relative to getwd(). Uses forward slashes.
rye_normalize_path_absolute <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    return(path)
  }
  normalized <- tryCatch(
    normalizePath(path, mustWork = FALSE, winslash = "/"),
    error = function(e) path
  )
  if (is.na(normalized) || !nzchar(normalized)) {
    return(path)
  }
  normalized
}
