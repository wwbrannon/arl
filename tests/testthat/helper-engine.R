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
    env <- engine$get_env()
  } else {
    if (!is.environment(env)) {
      stop("Expected an environment")
    }
    parent.env(env) <- engine$get_env()
  }

  # Load all stdlib modules: first ensure they're all registered,
  # then attach each into the target env.
  load_order_path <- system.file("arl", "load-order.txt", package = "arl")
  if (nzchar(load_order_path) && file.exists(load_order_path)) {
    all_modules <- readLines(load_order_path, warn = FALSE)
    all_modules <- all_modules[nzchar(all_modules)]
  } else {
    stop("load-order.txt not found")
  }

  # Use the Env wrapper to access the shared module registry
  arl_env <- arl:::Env$new(engine$get_env())
  registry <- arl_env$module_registry

  # Ensure all modules are loaded (registered). The engine with prelude=TRUE
  # already has prelude modules loaded. Load any remaining via eval.
  for (mod in all_modules) {
    if (!registry$exists(mod)) {
      path <- arl:::resolve_stdlib_path(mod)
      if (!is.null(path)) {
        engine$load_file_in_env(path, engine$get_env())
      }
    }
  }

  # Attach all module exports into env using squash mode (active bindings
  # directly in env, maintaining flat-env behavior tests expect)
  for (mod in all_modules) {
    if (registry$exists(mod)) {
      registry$attach_into(mod, env, squash = TRUE)
    }
  }

  core_env <- engine$get_env()
  # Copy bindings from engine_env, prelude_env, and builtins_env
  prelude_env <- parent.env(core_env)
  builtins_env <- parent.env(prelude_env)
  for (src_env in list(core_env, prelude_env, builtins_env)) {
    for (name in ls(src_env, all.names = TRUE)) {
      if (!exists(name, envir = env, inherits = FALSE)) {
        assign(name, get(name, envir = src_env, inherits = FALSE), envir = env)
      }
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
