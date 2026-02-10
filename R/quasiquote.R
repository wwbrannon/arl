# Shared quasiquote expansion walker
#
# Parameterized recursive walker used by both CompiledRuntime and
# RyeMacroExpander to expand quasiquoted expressions.
#
# @param expr   The expression to expand.
# @param env    The environment for evaluating unquoted sub-expressions.
# @param depth  Current quasiquote nesting depth (starts at 1).
# @param eval_fn  function(inner_expr, env) — evaluates an unquoted expression.
# @param wrap_fn  function(value) — post-processes each evaluated value
#                 (identity for runtime, hygiene_wrap for macro).
# @param skip_quote  If TRUE, `(quote ...)` forms pass through unchanged
#                    (runtime behavior).
# @return The expanded expression.
rye_quasiquote_expand <- function(expr, env, depth, eval_fn, wrap_fn,
                                  skip_quote) {
  if (!is.call(expr)) {
    return(expr)
  }

  # Optionally skip recursion into (quote ...) forms
  if (skip_quote && length(expr) > 0 && is.symbol(expr[[1]]) &&
      as.character(expr[[1]]) == "quote") {
    return(expr)
  }

  if (length(expr) > 0 && is.symbol(expr[[1]])) {
    head <- as.character(expr[[1]])

    # (unquote x)
    if (head == "unquote") {
      if (length(expr) != 2) {
        stop("unquote requires exactly 1 argument")
      }
      if (depth == 1) {
        return(wrap_fn(eval_fn(expr[[2]], env)))
      }
      return(as.call(list(
        as.symbol("unquote"),
        rye_quasiquote_expand(expr[[2]], env, depth - 1L, eval_fn, wrap_fn,
                              skip_quote)
      )))
    }

    # (unquote-splicing x) at top level — always an error
    if (head == "unquote-splicing") {
      stop("unquote-splicing can only appear in list context")
    }

    # (quasiquote x)
    if (head == "quasiquote") {
      if (length(expr) != 2) {
        stop("quasiquote requires exactly 1 argument")
      }
      return(as.call(list(
        as.symbol("quasiquote"),
        rye_quasiquote_expand(expr[[2]], env, depth + 1L, eval_fn, wrap_fn,
                              skip_quote)
      )))
    }
  }

  # General list/call — iterate elements, handle splice
  n <- length(expr)
  result <- vector("list", n * 2L)
  k <- 0L

  for (i in seq_len(n)) {
    elem <- expr[[i]]
    is_splice <- is.call(elem) && length(elem) > 0 &&
      is.symbol(elem[[1]]) && as.character(elem[[1]]) == "unquote-splicing"

    if (is_splice) {
      if (depth == 1) {
        if (length(elem) != 2) {
          stop("unquote-splicing requires exactly 1 argument")
        }
        spliced <- eval_fn(elem[[2]], env)
        if (is.call(spliced)) {
          spliced <- as.list(spliced)
        }
        if (is.list(spliced)) {
          for (item in spliced) {
            k <- k + 1L
            if (k > length(result)) result <- c(result, vector("list", k))
            result[[k]] <- wrap_fn(item)
          }
        } else {
          stop("unquote-splicing requires a list")
        }
      } else {
        k <- k + 1L
        if (k > length(result)) result <- c(result, vector("list", k))
        result[[k]] <- as.call(list(
          as.symbol("unquote-splicing"),
          rye_quasiquote_expand(elem[[2]], env, depth - 1L, eval_fn, wrap_fn,
                                skip_quote)
        ))
      }
    } else {
      k <- k + 1L
      if (k > length(result)) result <- c(result, vector("list", k))
      result[[k]] <- rye_quasiquote_expand(elem, env, depth, eval_fn, wrap_fn,
                                           skip_quote)
    }
  }

  as.call(result[seq_len(k)])
}
