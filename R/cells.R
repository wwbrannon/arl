# RyeCons: Minimal dotted-pair representation for Rye lists (car, cdr). Load order: this
# file must be sourced before eval.R (Evaluator uses RyePromise).
#
# @field car First element of the pair.
# @field cdr Second element (rest of list or another RyeCons).
#
#' @keywords internal
#' @noRd
RyeCons <- R6::R6Class("RyeCons",
  public = list(
    car = NULL,
    cdr = NULL,
    # @description Create a dotted pair.
    # @param car First element.
    # @param cdr Second element (list, RyeCons, or other value).
    initialize = function(car, cdr) {
      self$car <- car
      self$cdr <- cdr
    },
    # @description Traverse cdr until non-RyeCons; return list of all car values.
    # @return R list.
    as_list = function() {
      out <- list()
      x <- self
      while (r6_isinstance(x, "RyeCons")) {
        out <- c(out, list(x$car))
        x <- x$cdr
      }
      out
    },
    # @description Split into prefix (list of car values) and tail (first non-RyeCons cdr).
    # @return List with elements prefix (list) and tail.
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

# RyePromise: Lazy value from (delay expr). Used by the delay special form in the evaluator.
#
# (No public fields; state in private .rye_promise_* bindings.)
#
RyePromise <- R6::R6Class("RyePromise",
  lock_objects = FALSE,
  public = list(
    # @description Create a promise. Evaluation is deferred until value() is called.
    # @param expr Unevaluated expression.
    # @param env Environment to evaluate expr in.
    # @param eval_fn Function(expr, env) used to evaluate (e.g. evaluator$eval_in_env).
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
    # @description Force the promise and return the value.
    # @return The evaluated result.
    value = function() {
      get(".rye_promise_value", envir = self, inherits = FALSE)
    },
    # @description Return the unevaluated expression stored in the promise.
    # @return Rye expression.
    get_expr = function() {
      get(".rye_promise_expr", envir = self, inherits = FALSE)
    }
  )
)

