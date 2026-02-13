import_stdlib_modules <- function(engine, modules, env = NULL) {
  if (is.null(env)) {
    env <- engine$get_env()
  }
  for (module in modules) {
    exprs <- engine$read(sprintf("(import %s)", module))
    engine$eval(exprs[[1]], env = env)
  }
  env
}
