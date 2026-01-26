#' Load the Rye standard library
#'
#' Creates a new environment with access to R's base functions via the parent
#' environment chain. Lisp-specific functions are defined in this environment.
#'
#' @param env An environment to populate with stdlib functions. If NULL, creates a new one.
#' @return An environment containing the Rye standard library
#' @importFrom stats setNames
#' @export
rye_load_stdlib <- function(env = NULL) {
  # Create environment with baseenv() as parent if not provided
  # This gives automatic access to all R base functions
  if (is.null(env)) {
    env <- new.env(parent = baseenv())
  }
  # Define basic list functions
  env$car <- rye_stdlib_car
  env$cdr <- rye_stdlib_cdr
  env$cons <- rye_stdlib_cons

  # Higher-order functions
  env$map <- rye_stdlib_map
  env$filter <- rye_stdlib_filter
  env$reduce <- rye_stdlib_reduce

  # List predicates
  env$`list?` <- rye_stdlib_list_p
  env$`null?` <- rye_stdlib_null_p
  env$`symbol?` <- rye_stdlib_symbol_p
  env$`number?` <- rye_stdlib_number_p
  env$`string?` <- rye_stdlib_string_p

  # Boolean operations
  env$not <- rye_stdlib_not

  # Arithmetic operators
  # Provide single-character aliases for R operators
  env$`%` <- rye_stdlib_modulo

  # Lisp-style equality (can override base::== if needed)
  env$`=` <- rye_stdlib_equal

  # Output
  env$display <- rye_stdlib_display

  # Return the environment
  # All R base functions (+, -, *, /, <, >, print, etc.) are automatically
  # available via the parent environment chain
  env
}

rye_stdlib_car <- function(lst) {
  if (is.call(lst) && length(lst) > 0) {
    lst[[1]]
  } else if (is.list(lst) && length(lst) > 0) {
    lst[[1]]
  } else {
    NULL
  }
}

rye_stdlib_cdr <- function(lst) {
  if (is.call(lst) && length(lst) > 1) {
    as.call(as.list(lst)[-1])
  } else if (is.list(lst) && length(lst) > 1) {
    lst[-1]
  } else {
    list()
  }
}

rye_stdlib_cons <- function(item, lst) {
  if (is.call(lst)) {
    as.call(c(list(item), as.list(lst)))
  } else if (is.list(lst)) {
    c(list(item), lst)
  } else {
    list(item, lst)
  }
}

rye_stdlib_map <- function(fn, lst) {
  if (is.call(lst)) {
    lst <- as.list(lst)
  }
  lapply(lst, fn)
}

rye_stdlib_filter <- function(pred, lst) {
  if (is.call(lst)) {
    lst <- as.list(lst)
  }
  Filter(pred, lst)
}

rye_stdlib_reduce <- function(fn, lst, init = NULL) {
  if (is.call(lst)) {
    lst <- as.list(lst)
  }
  if (is.null(init)) {
    Reduce(fn, lst)
  } else {
    Reduce(fn, lst, init = init)
  }
}

rye_stdlib_list_p <- function(x) {
  is.list(x) || is.call(x)
}

rye_stdlib_null_p <- function(x) {
  is.null(x) || (is.list(x) && length(x) == 0) || (is.call(x) && length(x) == 0)
}

rye_stdlib_symbol_p <- function(x) {
  is.symbol(x)
}

rye_stdlib_number_p <- function(x) {
  is.numeric(x)
}

rye_stdlib_string_p <- function(x) {
  is.character(x)
}

rye_stdlib_not <- function(x) {
  !x
}

rye_stdlib_modulo <- function(x, y) {
  base::`%%`(x, y)
}

rye_stdlib_equal <- function(x, y) {
  base::`==`(x, y)
}

rye_stdlib_display <- function(x) {
  cat(as.character(x), "\n")
}
