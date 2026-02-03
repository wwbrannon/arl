#' Dotted-pair (improper list) support for Rye
#'
#' Minimal dotted-pair representation: a pair has a car and a cdr;
#' when cdr is not a proper list, the pair is represented as RyeCons (R6).
#'
#' @keywords internal
#' @noRd
RyeCons <- R6::R6Class("RyeCons",
  public = list(
    car = NULL,
    cdr = NULL,
    initialize = function(car, cdr) {
      self$car <- car
      self$cdr <- cdr
    },
    as_list = function() {
      out <- list()
      x <- self
      while (r6_isinstance(x, "RyeCons")) {
        out <- c(out, list(x$car))
        x <- x$cdr
      }
      out
    },
    parts = function() {
      prefix <- list()
      x <- self
      while (r6_isinstance(x, "RyeCons")) {
        prefix <- c(prefix, list(x$car))
        x <- x$cdr
      }
      list(prefix = prefix, tail = x)
    }
  )
)

# Thin wrappers for r/call lookups from Rye stdlib (get from globalenv finds package namespace).
# Engine also binds these names in the Rye env; R code uses r6_isinstance / RyeCons$new / $parts() directly.
rye_cons <- function(car, cdr) RyeCons$new(car, cdr)
rye_cons_p <- function(x) r6_isinstance(x, "RyeCons")
rye_cons_as_list <- function(x) if (r6_isinstance(x, "RyeCons")) x$as_list() else list()
rye_cons_parts <- function(x) if (r6_isinstance(x, "RyeCons")) x$parts() else list(prefix = list(), tail = x)
