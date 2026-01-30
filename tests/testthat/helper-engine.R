stdlib_env <- function(engine, env = NULL) {
  if (is.null(env)) {
    return(engine$env$env)
  }
  if (!is.environment(env)) {
    stop("Expected an environment")
  }
  parent.env(env) <- engine$env$env
  loader_path <- rye_resolve_module_path("_stdlib_loader")
  if (is.null(loader_path)) {
    stop("stdlib loader not found")
  }
  expr <- engine$read(sprintf('(load "%s")', loader_path))[[1]]
  engine$eval_in_env(expr, env)
  core_env <- engine$env$env
  for (name in ls(core_env, all.names = TRUE)) {
    if (!exists(name, envir = env, inherits = FALSE)) {
      assign(name, get(name, envir = core_env, inherits = FALSE), envir = env)
    }
  }
  last_fn <- get0("last", envir = core_env, inherits = FALSE)
  if (is.function(last_fn)) {
    current_last <- get0("last", envir = env, inherits = FALSE)
    if (!is.function(current_last)) {
      assign("last", last_fn, envir = env)
    }
  }
  env
}
