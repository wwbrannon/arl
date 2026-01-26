#' Load the Rye standard library
#'
#' @param env Environment in which to load the library
#' @export
rye_load_stdlib <- function(env = .GlobalEnv) {
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

  # List utilities
  env$length <- function(x) {
    base::length(x)
  }

  env$list <- function(...) {
    base::list(...)
  }

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

  # Arithmetic and comparison (already available from R)
  # Just ensure they're in the environment
  env$`+` <- base::`+`
  env$`-` <- base::`-`
  env$`*` <- base::`*`
  env$`/` <- base::`/`
  env$`%` <- base::`%%`  # Modulo
  env$`<` <- base::`<`
  env$`>` <- base::`>`
  env$`<=` <- base::`<=`
  env$`>=` <- base::`>=`
  env$`=` <- base::`==`

  # Boolean operations
  env$not <- function(x) {
    !x
  }

  # Output
  env$print <- base::print
  env$display <- function(x) {
    cat(as.character(x), "\n")
  }

  invisible(NULL)
}
