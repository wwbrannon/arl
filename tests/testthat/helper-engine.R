new_engine <- function(load_stdlib = FALSE) {
  engine <- RyeEngine$new()
  if (isTRUE(load_stdlib)) {
    engine$load_stdlib()
  }
  engine
}

engine_env <- function(engine) {
  engine$env$env
}
