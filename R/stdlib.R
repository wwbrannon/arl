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
  env$car <- function(lst) {
    if (is.call(lst) && length(lst) > 0) {
      lst[[1]]
    } else if (is.list(lst) && length(lst) > 0) {
      lst[[1]]
    } else {
      NULL
    }
  }

  env$cdr <- function(lst) {
    if (is.call(lst) && length(lst) > 1) {
      as.call(as.list(lst)[-1])
    } else if (is.list(lst) && length(lst) > 1) {
      lst[-1]
    } else {
      list()
    }
  }

  env$cons <- function(item, lst) {
    if (is.call(lst)) {
      as.call(c(list(item), as.list(lst)))
    } else if (is.list(lst)) {
      c(list(item), lst)
    } else {
      list(item, lst)
    }
  }

  # Higher-order functions
  env$map <- function(fn, lst) {
    if (is.call(lst)) {
      lst <- as.list(lst)
    }
    lapply(lst, fn)
  }

  env$filter <- function(pred, lst) {
    if (is.call(lst)) {
      lst <- as.list(lst)
    }
    Filter(pred, lst)
  }

  env$reduce <- function(fn, lst, init = NULL) {
    if (is.call(lst)) {
      lst <- as.list(lst)
    }
    if (is.null(init)) {
      Reduce(fn, lst)
    } else {
      Reduce(fn, lst, init = init)
    }
  }

  # List predicates
  env$`list?` <- function(x) {
    is.list(x) || is.call(x)
  }

  env$`null?` <- function(x) {
    is.null(x) || (is.list(x) && length(x) == 0) || (is.call(x) && length(x) == 0)
  }

  env$`symbol?` <- function(x) {
    is.symbol(x)
  }

  env$`number?` <- function(x) {
    is.numeric(x)
  }

  env$`string?` <- function(x) {
    is.character(x)
  }

  # Boolean operations
  env$not <- function(x) {
    !x
  }

  # Arithmetic operators
  # Provide single-character aliases for R operators
  env$`%` <- base::`%%`  # modulo

  # Lisp-style equality (can override base::== if needed)
  env$`=` <- base::`==`

  # Output
  env$display <- function(x) {
    cat(as.character(x), "\n")
  }

  # Return the environment
  # All R base functions (+, -, *, /, <, >, print, etc.) are automatically
  # available via the parent environment chain
  env
}
