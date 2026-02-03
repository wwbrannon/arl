#' Dotted-pair (improper list) support for Rye
#'
#' Minimal dotted-pair representation: a pair has a car and a cdr;
#' when cdr is not a proper list, the pair is represented as rye_cons.

#' Create a dotted pair (rye_cons)
#'
#' @param car First element (the "car").
#' @param cdr Second element (the "cdr"); may be any value.
#' @return Object with class \code{rye_cons} and components \code{car}, \code{cdr}.
#' @keywords internal
#' @noRd
rye_cons <- function(car, cdr) {
  structure(list(car = car, cdr = cdr), class = "rye_cons")
}

#' Test if an object is a rye_cons (dotted pair)
#'
#' @param x Any object.
#' @return Logical.
#' @keywords internal
#' @noRd
rye_cons_p <- function(x) {
  inherits(x, "rye_cons")
}

#' Extract the proper-list prefix from a rye_cons chain
#'
#' Walks the chain of rye_cons cells and collects the car of each
#' until the cdr is not a rye_cons (or is NULL). Used for functions
#' that expect a proper list (e.g. map, display).
#'
#' @param x A rye_cons chain (or any value; non-rye_cons returns empty list).
#' @return A proper R list of the cars, or empty list if not rye_cons.
#' @keywords internal
#' @noRd
rye_cons_as_list <- function(x) {
  out <- list()
  while (rye_cons_p(x)) {
    out <- c(out, list(x$car))
    x <- x$cdr
  }
  out
}

#' Split a rye_cons chain into prefix list and final cdr
#'
#' Used by display to format dotted pairs: prefix elements are formatted
#' then " . " then the tail.
#'
#' @param x A rye_cons chain.
#' @return List with components \code{prefix} (list of cars) and \code{tail} (final cdr).
#' @keywords internal
#' @noRd
rye_cons_parts <- function(x) {
  prefix <- list()
  while (rye_cons_p(x)) {
    prefix <- c(prefix, list(x$car))
    x <- x$cdr
  }
  list(prefix = prefix, tail = x)
}
