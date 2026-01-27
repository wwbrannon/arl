import_stdlib_modules <- function(env, modules) {
  for (module in modules) {
    exprs <- rye_read(sprintf("(import %s)", module))
    rye_eval(exprs[[1]], env)
  }
  env
}
