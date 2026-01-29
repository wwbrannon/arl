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

rye_promise_value_key <- ".rye_promise_value"
rye_promise_expr_key <- ".rye_promise_expr"
rye_promise_env_key <- ".rye_promise_env"
rye_promise_eval_key <- ".rye_promise_eval"

rye_promise_force <- function(promise_env) {
  if (!is.environment(promise_env) || !inherits(promise_env, "rye_promise")) {
    stop("not a Rye promise")
  }
  get(rye_promise_value_key, envir = promise_env, inherits = FALSE)
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

rye_env_resolve <- function(env, fallback) {
  if (inherits(env, "RyeEnv")) {
    return(env$env)
  }
  if (is.environment(env)) {
    return(env)
  }
  if (is.null(env)) {
    return(fallback$env)
  }
  stop("Expected a RyeEnv or environment")
}
