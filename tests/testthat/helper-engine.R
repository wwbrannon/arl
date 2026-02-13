make_engine <- function(..., coverage_tracker = getOption("arl.coverage_tracker")) {
  if (!is.null(coverage_tracker)) {
    Engine$new(..., coverage_tracker = coverage_tracker)
  } else {
    Engine$new(...)
  }
}

engine_field <- function(engine, name) {
  engine$.__enclos_env__$private[[paste0(".", name)]]
}

toplevel_env <- function(engine, env = NULL) {
  if (is.null(env)) {
    return(engine$get_env())
  }
  if (!is.environment(env)) {
    stop("Expected an environment")
  }
  parent.env(env) <- engine$get_env()
  # Access private .load_stdlib_into_env via R6 enclosure
  engine$.__enclos_env__$private$.load_stdlib_into_env(env)
  core_env <- engine$get_env()
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
