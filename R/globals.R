# This is to silence a stupid warning from R CMD check related to promise
# construction: there are dynamically evaluated variables that aren't lexically
# in scope when delayedAssign is called but will be when the promise it returns
# is forced. (see Evaluator$promise_new in R/eval.R)
if (getRversion() >= "2.15.1") utils::globalVariables(c(
  ".rye_promise_eval", ".rye_promise_expr", ".rye_promise_env"
))
