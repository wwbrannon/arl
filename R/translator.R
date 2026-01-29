rye_translate_env <- local({
  env <- NULL
  engine <- NULL
  function() {
    if (is.null(env)) {
      engine <- RyeEngine$new()
      env <- engine$load_stdlib()
      engine$eval(engine$read("(import translator)")[[1]], env)
    }
    env
  }
})

rye_expr_to_r <- function(expr, indent = 0) {
  env <- rye_translate_env()
  translator <- env$`translate-expr`
  translator(expr, indent)
}

rye_translate_str <- function(source) {
  env <- rye_translate_env()
  translator <- env$`translate-str`
  translator(source)
}

rye_translate_file <- function(path) {
  env <- rye_translate_env()
  translator <- env$`translate-file`
  translator(path)
}

# Internal translation helper (RyeEngine$translate delegates here).
rye_translate <- function(source, is_file = NULL) {
  env <- rye_translate_env()
  translator <- env$`translate-source`
  translator(source, is_file)
}
