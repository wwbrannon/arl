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

rye_promise_new <- function(expr, env) {
  promise_env <- new.env(parent = emptyenv())
  assign(rye_promise_expr_key, expr, envir = promise_env)
  assign(rye_promise_env_key, env, envir = promise_env)
  assign(rye_promise_eval_key, rye_eval, envir = promise_env)
  delayedAssign(
    rye_promise_value_key,
    .rye_promise_eval(.rye_promise_expr, .rye_promise_env),
    eval.env = promise_env,
    assign.env = promise_env
  )
  class(promise_env) <- c("rye_promise", class(promise_env))
  lockEnvironment(promise_env, bindings = FALSE)
  promise_env
}

rye_promise_force <- function(promise_env) {
  if (!is.environment(promise_env) || !inherits(promise_env, "rye_promise")) {
    stop("not a Rye promise")
  }
  get(rye_promise_value_key, envir = promise_env, inherits = FALSE)
}

rye_eval_and_maybe_print <- function(fn, env, on_error, printer = NULL) {
  result <- tryCatch(
    fn(),
    error = function(e) {
      on_error(e)
      return(invisible(NULL))
    }
  )
  if (!is.null(result) && !is.null(printer)) {
    printer(result, env)
  }
  invisible(result)
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

rye_with_error_context <- function(fn, tracker = NULL) {
  if (is.null(tracker)) {
    tracker <- rye_default_engine()$source_tracker
  }
  prev_stack <- tracker$get()
  on.exit({
    tracker$reset()
    if (!is.null(prev_stack) && length(prev_stack) > 0) {
      for (src in prev_stack) {
        tracker$push(src)
      }
    }
  }, add = TRUE)
  tracker$reset()
  tryCatch(
    fn(),
    error = function(e) {
      if (inherits(e, "rye_error")) {
        stop(e)
      }
      cond <- rye_error(conditionMessage(e), tracker$get(), sys.calls())
      stop(cond)
    }
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

rye_do_call <- function(fn, args) {
  rye_default_engine()$evaluator$do_call(fn, args)
}

rye_load_file <- function(path, env = parent.frame()) {
  rye_default_engine()$load_file(path, env)
}

rye_hygiene_unwrap <- function(expr) {
  rye_default_engine()$macro_expander$hygiene_unwrap(expr)
}

rye_print_error <- function(e, file = stderr()) {
  rye_default_engine()$source_tracker$print_error(e, file = file)
}

rye_eval <- function(expr, env = parent.frame()) {
  rye_default_engine()$eval(expr, env)
}
