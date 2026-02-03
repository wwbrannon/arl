#' Value types for Rye: dotted pairs (RyeCons) and promises (RyePromise)
#'
#' RyeCons: minimal dotted-pair representation (car/cdr). RyePromise: lazy value
#' from (delay expr), with value() and get_expr().
#'
#' Load order: this file must be sourced before eval.R (alphabetical R/*.R order
#' when DESCRIPTION has no Collate). Evaluator in eval.R uses RyePromise; if
#' this file loaded later, promise_new() would reference an undefined class.
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

RyePromise <- R6::R6Class("RyePromise",
  lock_objects = FALSE,
  public = list(
    initialize = function(expr, env, eval_fn) {
      assign(".rye_promise_expr", expr, envir = self)
      assign(".rye_promise_env", env, envir = self)
      assign(".rye_promise_eval", eval_fn, envir = self)
      delayedAssign(
        ".rye_promise_value",
        .rye_promise_eval(.rye_promise_expr, .rye_promise_env),
        eval.env = self,
        assign.env = self
      )
    },
    value = function() {
      get(".rye_promise_value", envir = self, inherits = FALSE)
    },
    get_expr = function() {
      get(".rye_promise_expr", envir = self, inherits = FALSE)
    }
  )
)

