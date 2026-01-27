rye_trimws_compat <- function(x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") {
  which <- match.arg(which)
  left_pattern <- paste0("^", whitespace, "+")
  right_pattern <- paste0(whitespace, "+$")
  if (which == "left") {
    return(sub(left_pattern, "", x))
  }
  if (which == "right") {
    return(sub(right_pattern, "", x))
  }
  sub(left_pattern, "", sub(right_pattern, "", x))
}
rye_trimws_shim <- function(x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") {
  if (exists("trimws", mode = "function", inherits = TRUE)) {
    return(trimws(x, which = which, whitespace = whitespace))
  }
  rye_trimws_compat(x, which = which, whitespace = whitespace)
}

.rye_error_state <- new.env(parent = emptyenv())
.rye_error_state$src_stack <- list()
.rye_module_registry <- new.env(parent = emptyenv())

rye_src_new <- function(file, start_line, start_col, end_line = start_line, end_col = start_col) {
  structure(
    list(
      file = file,
      start_line = start_line,
      start_col = start_col,
      end_line = end_line,
      end_col = end_col
    ),
    class = "rye_src"
  )
}

rye_src_get <- function(expr) {
  if (is.null(expr)) {
    return(NULL)
  }
  attr(expr, "rye_src", exact = TRUE)
}

rye_src_set <- function(expr, src) {
  if (is.null(expr) || is.null(src)) {
    return(expr)
  }
  if (is.symbol(expr)) {
    return(expr)
  }
  attr(expr, "rye_src") <- src
  expr
}

rye_src_inherit <- function(expr, from) {
  src <- rye_src_get(from)
  if (is.null(src)) {
    return(expr)
  }
  if (!is.null(rye_src_get(expr))) {
    return(expr)
  }
  rye_src_set(expr, src)
}

rye_strip_src <- function(value) {
  if (is.null(value)) {
    return(value)
  }
  if (is.symbol(value)) {
    return(value)
  }
  if (!is.null(attr(value, "rye_src", exact = TRUE))) {
    attr(value, "rye_src") <- NULL
  }
  if (is.call(value)) {
    stripped <- lapply(as.list(value), rye_strip_src)
    return(as.call(stripped))
  }
  if (is.list(value) && is.null(attr(value, "class", exact = TRUE))) {
    stripped <- lapply(value, rye_strip_src)
    if (!is.null(names(value))) {
      names(stripped) <- names(value)
    }
    return(stripped)
  }
  value
}

rye_module_exists <- function(name) {
  is.character(name) && length(name) == 1 &&
    exists(name, envir = .rye_module_registry, inherits = FALSE)
}

rye_module_get <- function(name) {
  if (!rye_module_exists(name)) {
    return(NULL)
  }
  get(name, envir = .rye_module_registry, inherits = FALSE)
}

rye_module_register <- function(name, env, exports, path = NULL) {
  if (!is.character(name) || length(name) != 1) {
    stop("module name must be a single string")
  }
  if (rye_module_exists(name)) {
    stop(sprintf("module '%s' is already defined", name))
  }
  entry <- list(env = env, exports = exports, path = path)
  assign(name, entry, envir = .rye_module_registry)
  entry
}

rye_module_attach <- function(name, target_env) {
  entry <- rye_module_get(name)
  if (is.null(entry)) {
    stop(sprintf("module '%s' is not loaded", name))
  }
  exports <- entry$exports
  module_env <- entry$env
  for (export_name in exports) {
    if (!exists(export_name, envir = module_env, inherits = FALSE)) {
      stop(sprintf("module '%s' does not export '%s'", name, export_name))
    }
    assign(export_name, get(export_name, envir = module_env, inherits = FALSE), envir = target_env)
  }
  invisible(NULL)
}

rye_promise_value_key <- ".rye_promise_value"
rye_promise_expr_key <- ".rye_promise_expr"
rye_promise_env_key <- ".rye_promise_env"
rye_promise_eval_key <- ".rye_promise_eval"

rye_promise_new <- function(expr, env) {
  promise_env <- new.env(parent = emptyenv())
  assign(rye_promise_expr_key, expr, envir = promise_env)
  assign(rye_promise_env_key, env, envir = promise_env)
  assign(rye_promise_eval_key, rye_eval, envir = promise_env)
  delayedAssign(
    rye_promise_value_key,
    .rye_promise_eval(.rye_promise_expr, .rye_promise_env),
    eval.env = promise_env,
    assign.env = promise_env
  )
  class(promise_env) <- c("rye_promise", class(promise_env))
  lockEnvironment(promise_env, bindings = FALSE)
  promise_env
}

rye_promise_p <- function(x) {
  is.environment(x) && inherits(x, "rye_promise")
}

rye_promise_force <- function(x) {
  if (!rye_promise_p(x)) {
    return(x)
  }
  get(rye_promise_value_key, envir = x, inherits = FALSE)
}

print.rye_promise <- function(x, ...) {
  cat("<promise>\n")
  invisible(x)
}

rye_values_new <- function(values) {
  if (is.null(values)) {
    values <- list()
  }
  if (!is.list(values)) {
    values <- list(values)
  }
  structure(values, class = "rye_values")
}

rye_values_p <- function(x) {
  is.list(x) && inherits(x, "rye_values")
}

rye_values_list <- function(x) {
  if (!rye_values_p(x)) {
    return(list(x))
  }
  unclass(x)
}

rye_src_stack_get <- function() {
  .rye_error_state$src_stack
}

rye_src_stack_reset <- function() {
  .rye_error_state$src_stack <- list()
  invisible(NULL)
}

rye_src_stack_push <- function(src) {
  if (is.null(src)) {
    return(invisible(NULL))
  }
  .rye_error_state$src_stack <- c(.rye_error_state$src_stack, list(src))
  invisible(NULL)
}

rye_src_stack_pop <- function() {
  stack <- .rye_error_state$src_stack
  if (length(stack) == 0) {
    return(invisible(NULL))
  }
  .rye_error_state$src_stack <- stack[-length(stack)]
  invisible(NULL)
}

rye_eval_exprs <- function(exprs, env) {
  rye_with_error_context(function() {
    result <- NULL
    for (expr in exprs) {
      result <- rye_eval(expr, env)
    }
    result
  })
}

rye_eval_text <- function(text, env, source_name = "<eval>") {
  exprs <- rye_read(text, source_name = source_name)
  rye_eval_exprs(exprs, env)
}

rye_quote_arg <- function(value, quote_symbols = TRUE) {
  if (is.call(value) || (quote_symbols && is.symbol(value))) {
    return(as.call(list(as.symbol("quote"), value)))
  }
  value
}

rye_do_call <- function(fn, args) {
  quote_symbols <- !identical(fn, base::`$`) &&
    !identical(fn, base::`[`) &&
    !identical(fn, base::`[[`)
  args <- lapply(args, rye_quote_arg, quote_symbols = quote_symbols)
  do.call(fn, args)
}

rye_assign <- function(name, value, env) {
  target <- env
  repeat {
    if (exists(name, envir = target, inherits = FALSE)) {
      assign(name, value, envir = target)
      return(invisible(NULL))
    }
    parent_env <- parent.env(target)
    if (!exists(".rye_env", envir = parent_env, inherits = FALSE)) {
      break
    }
    target <- parent_env
  }
  assign(name, value, envir = env)
  invisible(NULL)
}

rye_assign_existing <- function(name, value, env) {
  if (!exists(name, envir = env, inherits = TRUE)) {
    stop(sprintf("set!: variable '%s' is not defined", name))
  }
  target_env <- env
  while (!exists(name, envir = target_env, inherits = FALSE)) {
    if (identical(target_env, emptyenv())) {
      stop(sprintf("set!: variable '%s' not found", name))
    }
    target_env <- parent.env(target_env)
  }
  assign(name, value, envir = target_env)
  invisible(NULL)
}

rye_pattern_symbols <- function(pattern) {
  if (is.symbol(pattern)) {
    name <- as.character(pattern)
    if (identical(name, ".")) {
      return(character(0))
    }
    return(name)
  }
  if (is.null(pattern)) {
    return(character(0))
  }
  if (is.call(pattern) || (is.list(pattern) && is.null(attr(pattern, "class", exact = TRUE)))) {
    parts <- if (is.call(pattern)) as.list(pattern) else pattern
    names <- character(0)
    for (part in parts) {
      names <- c(names, rye_pattern_symbols(part))
    }
    return(names)
  }
  character(0)
}

rye_destructure_bind <- function(pattern, value, env, mode = c("define", "set")) {
  mode <- match.arg(mode)
  bind_symbol <- function(symbol, val) {
    name <- as.character(symbol)
    if (identical(name, ".")) {
      stop("Invalid destructuring pattern: '.'")
    }
    if (identical(mode, "define")) {
      rye_assign(name, val, env)
    } else {
      rye_assign_existing(name, val, env)
    }
    invisible(NULL)
  }

  if (is.symbol(pattern)) {
    return(bind_symbol(pattern, value))
  }
  if (is.null(pattern)) {
    value_list <- rye_as_list(value)
    if (length(value_list) != 0) {
      stop(sprintf("Destructuring pattern expects empty list, got %d item(s)", length(value_list)))
    }
    return(invisible(NULL))
  }
  if (is.call(pattern) || (is.list(pattern) && is.null(attr(pattern, "class", exact = TRUE)))) {
    parts <- if (is.call(pattern)) as.list(pattern) else pattern
    dot_idx <- which(vapply(parts, function(x) {
      is.symbol(x) && identical(as.character(x), ".")
    }, logical(1)))
    if (length(dot_idx) > 1) {
      stop("Destructuring pattern can only contain one '.'")
    }
    value_list <- rye_as_list(value)
    if (length(dot_idx) == 1) {
      if (dot_idx == 1 || dot_idx == length(parts)) {
        stop("Destructuring '.' must appear between head and rest")
      }
      if (dot_idx != length(parts) - 1) {
        stop("Destructuring '.' must be followed by a single rest pattern")
      }
      head_patterns <- parts[1:(dot_idx - 1)]
      rest_pattern <- parts[[dot_idx + 1]]
      if (length(value_list) < length(head_patterns)) {
        stop(sprintf(
          "Destructuring pattern expects at least %d item(s), got %d",
          length(head_patterns),
          length(value_list)
        ))
      }
      for (i in seq_along(head_patterns)) {
        rye_destructure_bind(head_patterns[[i]], value_list[[i]], env, mode)
      }
      rest_values <- if (length(value_list) > length(head_patterns)) {
        value_list[(length(head_patterns) + 1):length(value_list)]
      } else {
        list()
      }
      rye_destructure_bind(rest_pattern, rest_values, env, mode)
      return(invisible(NULL))
    }
    if (length(value_list) != length(parts)) {
      stop(sprintf(
        "Destructuring pattern expects %d item(s), got %d",
        length(parts),
        length(value_list)
      ))
    }
    for (i in seq_along(parts)) {
      rye_destructure_bind(parts[[i]], value_list[[i]], env, mode)
    }
    return(invisible(NULL))
  }
  stop("Invalid destructuring pattern")
}

rye_resolve_stdlib_path <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    return(NULL)
  }
  dir_path <- system.file("rye", package = "rye")
  if (identical(dir_path, "")) {
    return(NULL)
  }
  candidates <- c(
    file.path(dir_path, name),
    file.path(dir_path, paste0(name, ".rye"))
  )
  for (path in candidates) {
    if (file.exists(path)) {
      return(path)
    }
  }
  NULL
}

rye_resolve_module_path <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    return(NULL)
  }
  has_separator <- grepl("[/\\\\]", name)
  if (has_separator) {
    if (file.exists(name)) {
      return(name)
    }
    return(NULL)
  }
  stdlib_path <- rye_resolve_stdlib_path(name)
  if (!is.null(stdlib_path)) {
    return(stdlib_path)
  }
  candidates <- c(name, paste0(name, ".rye"))
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(candidate)
    }
  }
  NULL
}

#' Load and evaluate a Rye source file
#'
#' Reads a `.rye` file, parses it, and evaluates each expression in order.
#'
#' @param path Path to a Rye source file
#' @param env Environment in which to evaluate the file
#' @return The result of the final expression in the file
#' @examples
#' tmp <- tempfile(fileext = ".rye")
#' writeLines("(+ 1 2)", tmp)
#' rye_load_file(tmp)
#' @export
rye_load_file <- function(path, env = parent.frame()) {
  if (!is.character(path) || length(path) != 1) {
    stop("load requires a single file path string")
  }
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path))
  }
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  rye_eval_text(text, env, source_name = path)
}

rye_error <- function(message, src_stack = list(), r_stack = list()) {
  structure(
    list(message = message, src_stack = src_stack, r_stack = r_stack),
    class = c("rye_error", "error", "condition")
  )
}

rye_with_error_context <- function(fn) {
  prev_stack <- rye_src_stack_get()
  on.exit({
    .rye_error_state$src_stack <- prev_stack
  }, add = TRUE)
  rye_src_stack_reset()
  tryCatch(
    fn(),
    error = function(e) {
      if (inherits(e, "rye_error")) {
        stop(e)
      }
      cond <- rye_error(conditionMessage(e), rye_src_stack_get(), sys.calls())
      stop(cond)
    }
  )
}

rye_format_src <- function(src) {
  if (is.null(src)) {
    return(NULL)
  }
  file <- src$file
  if (is.null(file) || !is.character(file) || length(file) != 1 || !nzchar(file)) {
    file <- "<input>"
  }
  start <- paste0(src$start_line, ":", src$start_col)
  end <- paste0(src$end_line, ":", src$end_col)
  if (identical(start, end)) {
    return(paste0(file, ":", start))
  }
  paste0(file, ":", start, "-", end)
}

rye_format_error <- function(e, include_r_stack = TRUE) {
  lines <- c(paste0("Error: ", conditionMessage(e)))
  if (inherits(e, "rye_error")) {
    src_stack <- e$src_stack
    if (!is.null(src_stack) && length(src_stack) > 0) {
      loc <- rye_format_src(src_stack[[length(src_stack)]])
      if (!is.null(loc)) {
        lines <- c(lines, paste0("Location: ", loc))
      }
      if (length(src_stack) > 1) {
        lines <- c(lines, "Rye stack:")
        for (src in rev(src_stack)) {
          loc <- rye_format_src(src)
          if (!is.null(loc)) {
            lines <- c(lines, paste0("  at ", loc))
          }
        }
      }
    }
    if (isTRUE(include_r_stack)) {
      r_stack <- e$r_stack
      if (!is.null(r_stack) && length(r_stack) > 0) {
        lines <- c(lines, "R stack:")
        max_frames <- 20
        calls <- r_stack
        if (length(calls) > max_frames) {
          calls <- tail(calls, max_frames)
        }
        for (call in rev(calls)) {
          lines <- c(lines, paste0("  ", paste(deparse(call), collapse = "")))
        }
        if (length(r_stack) > max_frames) {
          lines <- c(lines, "  ...")
        }
      }
    }
  }
  paste(lines, collapse = "\n")
}
