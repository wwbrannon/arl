# Cons: Minimal dotted-pair representation for Rye lists (car, cdr). Load order: this
# file must be sourced before runtime/eval helpers that use Promise.
#
# @field car First element of the pair.
# @field cdr Second element (rest of list or another Cons).
#
#' @keywords internal
#' @noRd
Cons <- R6::R6Class("Cons",
  public = list(
    car = NULL,
    cdr = NULL,
    # @description Create a dotted pair.
    # @param car First element.
    # @param cdr Second element (list, Cons, or other value).
    initialize = function(car, cdr) {
      self$car <- car
      self$cdr <- cdr
    },
    # @description Traverse cdr until non-Cons; return list of all car values.
    # @return R list.
    as_list = function() {
      # Count length first, then fill pre-allocated vector
      n <- 0L
      x <- self
      while (r6_isinstance(x, "Cons")) { n <- n + 1L; x <- x$cdr }
      out <- vector("list", n)
      x <- self
      for (i in seq_len(n)) { out[[i]] <- x$car; x <- x$cdr }
      out
    },
    # @description Split into prefix (list of car values) and tail (first non-Cons cdr).
    # @return List with elements prefix (list) and tail.
    parts = function() {
      n <- 0L
      x <- self
      while (r6_isinstance(x, "Cons")) { n <- n + 1L; x <- x$cdr }
      prefix <- vector("list", n)
      x <- self
      for (i in seq_len(n)) { prefix[[i]] <- x$car; x <- x$cdr }
      list(prefix = prefix, tail = x)
    }
  )
)

# Promise: Lazy value from (delay expr). Used by delay in compiled/runtime code.
#
# (No public fields; state in private .rye_promise_* bindings.)
#
Promise <- R6::R6Class("Promise",
  private = list(
    expr = NULL,
    env = NULL,
    eval_fn = NULL,
    cached_value = NULL,
    evaluated = FALSE
  ),
  public = list(
    # @description Create a promise. Evaluation is deferred until value() is called.
    # @param expr Unevaluated expression.
    # @param env Environment to evaluate expr in.
    # @param eval_fn Function(expr, env) used to evaluate (e.g. compiled runtime).
    initialize = function(expr, env, eval_fn) {
      private$expr <- expr
      private$env <- env
      private$eval_fn <- eval_fn
    },
    # @description Force the promise and return the value.
    # @return The evaluated result.
    value = function() {
      if (!private$evaluated) {
        private$cached_value <- private$eval_fn(private$expr, private$env)
        private$evaluated <- TRUE
      }
      private$cached_value
    },
    # @description Return the unevaluated expression stored in the promise.
    # @return Rye expression.
    get_expr = function() {
      private$expr
    }
  )
)
