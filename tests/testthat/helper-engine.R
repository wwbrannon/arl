make_engine <- function(...) {
  tracker <- getOption("arl.coverage_tracker")
  if (!is.null(tracker)) {
    Engine$new(..., use_env_cache = FALSE, coverage_tracker = tracker)
  } else {
    Engine$new(...)
  }
}

toplevel_env <- function(engine, env = NULL) {
  if (is.null(env)) {
    return(engine$env$env)
  }
  if (!is.environment(env)) {
    stop("Expected an environment")
  }
  parent.env(env) <- engine$env$env
  engine$load_stdlib_into_env(env)
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
