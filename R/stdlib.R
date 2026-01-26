#' Load the Rye standard library
#'
#' Loads the base Rye stdlib functions and, optionally, Rye stdlib source files
#' from the package's `inst/rye` directory.
#'
#' @param env An environment to populate. If NULL, creates a new one.
#' @param load_files Whether to load Rye stdlib source files from `inst/rye`.
#' @return An environment containing the Rye standard library
#' @importFrom stats setNames
#' @export
rye_load_stdlib <- function(env = NULL, load_files = FALSE) {
  env <- rye_load_stdlib_base(env)
  if (isTRUE(load_files)) {
    rye_load_stdlib_files(env)
  }
  env
}

rye_load_stdlib_base <- function(env = NULL) {
  # Create environment with baseenv() as parent if not provided
  # This gives automatic access to all R base functions
  if (is.null(env)) {
    env <- new.env(parent = baseenv())
  }
  # Define basic list functions
  env$car <- rye_stdlib_car
  env$cdr <- rye_stdlib_cdr
  env$cons <- rye_stdlib_cons
  env$call <- rye_stdlib_call
  env$`list*` <- rye_stdlib_list_star
  env$append <- rye_stdlib_append
  env$reverse <- rye_stdlib_reverse
  env$apply <- rye_stdlib_apply

  # Higher-order functions
  env$map <- rye_stdlib_map
  env$mapcat <- rye_stdlib_mapcat
  env$filter <- rye_stdlib_filter
  env$remove <- rye_stdlib_remove
  env$reduce <- rye_stdlib_reduce
  env$foldl <- rye_stdlib_foldl
  env$foldr <- rye_stdlib_foldr
  env$`every?` <- rye_stdlib_every_p
  env$`any?` <- rye_stdlib_any_p
  env$take <- rye_stdlib_take
  env$drop <- rye_stdlib_drop
  env$`take-while` <- rye_stdlib_take_while
  env$`drop-while` <- rye_stdlib_drop_while
  env$partition <- rye_stdlib_partition
  env$flatten <- rye_stdlib_flatten

  # List predicates
  env$`list?` <- rye_stdlib_list_p
  env$`pair?` <- rye_stdlib_pair_p
  env$`null?` <- rye_stdlib_null_p
  env$`nil?` <- rye_stdlib_null_p
  env$`symbol?` <- rye_stdlib_symbol_p
  env$`keyword?` <- rye_stdlib_keyword_p
  env$`number?` <- rye_stdlib_number_p
  env$`string?` <- rye_stdlib_string_p
  env$`vector?` <- rye_stdlib_vector_p
  env$`true?` <- rye_stdlib_true_p
  env$`false?` <- rye_stdlib_false_p
  env$`fn?` <- rye_stdlib_fn_p
  env$`callable?` <- rye_stdlib_callable_p

  # Boolean operations
  env$not <- rye_stdlib_not

  # Arithmetic operators
  # Provide single-character aliases for R operators
  env$`%` <- rye_stdlib_modulo

  # Lisp-style equality (can override base::== if needed)
  env$`=` <- rye_stdlib_equal

  # Output
  env$display <- rye_stdlib_display
  env$println <- rye_stdlib_display
  env$str <- rye_stdlib_str
  env$`string-join` <- rye_stdlib_string_join
  env$`string-split` <- rye_stdlib_string_split
  env$trim <- rye_stdlib_trim
  env$format <- rye_stdlib_format
  env$`read-line` <- rye_stdlib_read_line

  # Errors and debugging
  env$error <- rye_stdlib_error
  env$warn <- rye_stdlib_warn
  env$assert <- rye_stdlib_assert
  env$trace <- rye_stdlib_trace
  env$`try*` <- rye_stdlib_try

  # Macro and eval helpers
  env$gensym <- gensym
  env$`macro?` <- rye_stdlib_macro_p
  env$macroexpand <- rye_stdlib_macroexpand
  env$`macroexpand-1` <- rye_stdlib_macroexpand_1
  env$`macroexpand-all` <- rye_stdlib_macroexpand
  env$eval <- rye_stdlib_eval

  # Interop helpers
  env$dict <- rye_stdlib_dict
  env$hash <- rye_stdlib_dict
  env$`r/call` <- rye_stdlib_r_call

  # Return the environment
  # All R base functions (+, -, *, /, <, >, print, etc.) are automatically
  # available via the parent environment chain
  env
}

rye_load_stdlib_files <- function(env = parent.frame()) {
  dir_path <- system.file("rye", package = "rye")
  if (identical(dir_path, "")) {
    stop("Rye standard library directory not found in installed package")
  }
  paths <- list.files(dir_path, pattern = "\\.rye$", full.names = TRUE)
  if (length(paths) == 0) {
    stop("No Rye standard library files found in installed package")
  }
  ordered_files <- c(
    "binding.rye",
    "control.rye",
    "looping.rye",
    "threading.rye",
    "error.rye"
  )
  ordered_paths <- file.path(dir_path, ordered_files)
  ordered_paths <- ordered_paths[file.exists(ordered_paths)]
  remaining_paths <- setdiff(paths, ordered_paths)
  load_paths <- c(ordered_paths, sort(remaining_paths))

  result <- NULL
  for (path in load_paths) {
    result <- rye_load_file(path, env)
  }
  result
}

rye_stdlib_car <- function(lst) {
  if (is.call(lst) && length(lst) > 0) {
    lst[[1]]
  } else if (is.list(lst) && length(lst) > 0) {
    lst[[1]]
  } else {
    NULL
  }
}

rye_stdlib_cdr <- function(lst) {
  if (is.call(lst) && length(lst) > 1) {
    as.list(lst)[-1]
  } else if (is.list(lst) && length(lst) > 1) {
    lst[-1]
  } else {
    list()
  }
}

rye_stdlib_cons <- function(item, lst) {
  if (is.call(lst)) {
    as.call(c(list(item), as.list(lst)))
  } else if (is.list(lst)) {
    c(list(item), lst)
  } else {
    list(item, lst)
  }
}

rye_stdlib_call <- function(lst) {
  if (is.call(lst)) {
    lst
  } else {
    as.call(rye_as_list(lst))
  }
}

rye_stdlib_map <- function(fn, lst) {
  if (is.call(lst)) {
    lst <- as.list(lst)
  }
  lapply(lst, fn)
}

rye_stdlib_filter <- function(pred, lst) {
  if (is.call(lst)) {
    lst <- as.list(lst)
  }
  Filter(pred, lst)
}

rye_stdlib_reduce <- function(fn, lst, init = NULL) {
  if (is.call(lst)) {
    lst <- as.list(lst)
  }
  if (is.null(init)) {
    Reduce(fn, lst)
  } else {
    Reduce(fn, lst, init = init)
  }
}

rye_stdlib_list_p <- function(x) {
  is.list(x) || is.call(x)
}

rye_stdlib_null_p <- function(x) {
  is.null(x) || (is.list(x) && length(x) == 0) || (is.call(x) && length(x) == 0)
}

rye_stdlib_symbol_p <- function(x) {
  is.symbol(x)
}

rye_stdlib_number_p <- function(x) {
  is.numeric(x)
}

rye_stdlib_string_p <- function(x) {
  is.character(x)
}

rye_stdlib_not <- function(x) {
  !rye_is_truthy(x)
}

rye_stdlib_modulo <- function(x, y) {
  base::`%%`(x, y)
}

rye_stdlib_equal <- function(x, y) {
  base::`==`(x, y)
}

rye_stdlib_display <- function(x) {
  cat(as.character(x), "\n")
}

rye_is_truthy <- function(x) {
  !(identical(x, FALSE) || is.null(x))
}

rye_as_list <- function(x) {
  if (is.call(x)) {
    return(as.list(x))
  }
  if (is.list(x)) {
    return(x)
  }
  if (length(x) == 0) {
    return(list())
  }
  as.list(x)
}

rye_stdlib_list_star <- function(...) {
  args <- list(...)
  if (length(args) == 0) {
    return(list())
  }
  if (length(args) == 1) {
    return(args[[1]])
  }
  last <- args[[length(args)]]
  head <- args[1:(length(args) - 1)]
  if (is.list(last) || is.call(last)) {
    return(c(head, rye_as_list(last)))
  }
  c(head, list(last))
}

rye_stdlib_append <- function(x, y) {
  x_list <- rye_as_list(x)
  y_list <- rye_as_list(y)
  c(x_list, y_list)
}

rye_stdlib_reverse <- function(x) {
  rev(rye_as_list(x))
}

rye_stdlib_apply <- function(fn, args) {
  args <- rye_as_list(args)
  if (length(args) > 2 && (identical(fn, base::`+`) || identical(fn, base::`*`) ||
    identical(fn, base::`-`) || identical(fn, base::`/`))) {
    return(Reduce(fn, args))
  }
  rye_do_call(fn, args)
}

rye_stdlib_mapcat <- function(fn, lst) {
  lst <- rye_as_list(lst)
  results <- lapply(lst, fn)
  flat <- list()
  for (item in results) {
    flat <- c(flat, rye_as_list(item))
  }
  flat
}

rye_stdlib_remove <- function(pred, lst) {
  lst <- rye_as_list(lst)
  Filter(function(x) !isTRUE(pred(x)), lst)
}

rye_stdlib_foldl <- function(fn, lst, init = NULL) {
  lst <- rye_as_list(lst)
  if (is.null(init)) {
    Reduce(fn, lst)
  } else {
    Reduce(fn, lst, init = init)
  }
}

rye_stdlib_foldr <- function(fn, lst, init = NULL) {
  lst <- rye_as_list(lst)
  if (is.null(init)) {
    Reduce(fn, lst, right = TRUE)
  } else {
    Reduce(fn, lst, init = init, right = TRUE)
  }
}

rye_stdlib_every_p <- function(pred, lst) {
  lst <- rye_as_list(lst)
  all(vapply(lst, function(x) isTRUE(pred(x)), logical(1)))
}

rye_stdlib_any_p <- function(pred, lst) {
  lst <- rye_as_list(lst)
  any(vapply(lst, function(x) isTRUE(pred(x)), logical(1)))
}

rye_stdlib_take <- function(n, lst) {
  lst <- rye_as_list(lst)
  if (n <= 0) {
    return(list())
  }
  lst[seq_len(min(n, length(lst)))]
}

rye_stdlib_drop <- function(n, lst) {
  lst <- rye_as_list(lst)
  if (n <= 0) {
    return(lst)
  }
  if (n >= length(lst)) {
    return(list())
  }
  lst[(n + 1):length(lst)]
}

rye_stdlib_take_while <- function(pred, lst) {
  lst <- rye_as_list(lst)
  result <- list()
  for (item in lst) {
    if (!isTRUE(pred(item))) {
      break
    }
    result <- c(result, list(item))
  }
  result
}

rye_stdlib_drop_while <- function(pred, lst) {
  lst <- rye_as_list(lst)
  idx <- 1
  while (idx <= length(lst) && isTRUE(pred(lst[[idx]]))) {
    idx <- idx + 1
  }
  if (idx > length(lst)) {
    list()
  } else {
    lst[idx:length(lst)]
  }
}

rye_stdlib_partition <- function(n, lst, step = n) {
  lst <- rye_as_list(lst)
  if (n <= 0 || step <= 0) {
    stop("partition requires positive n and step")
  }
  parts <- list()
  i <- 1
  while (i <= length(lst)) {
    chunk <- lst[i:min(i + n - 1, length(lst))]
    if (length(chunk) < n) {
      break
    }
    parts <- c(parts, list(chunk))
    i <- i + step
  }
  parts
}

rye_stdlib_flatten <- function(lst) {
  lst <- rye_as_list(lst)
  result <- list()
  for (item in lst) {
    if (is.list(item) || is.call(item)) {
      result <- c(result, rye_stdlib_flatten(item))
    } else {
      result <- c(result, list(item))
    }
  }
  result
}

rye_stdlib_pair_p <- function(x) {
  (is.list(x) || is.call(x)) && length(x) > 0
}

rye_stdlib_keyword_p <- function(x) {
  inherits(x, "rye_keyword")
}

rye_stdlib_vector_p <- function(x) {
  is.atomic(x) && !is.character(x) && !is.list(x)
}

rye_stdlib_true_p <- function(x) {
  isTRUE(x)
}

rye_stdlib_false_p <- function(x) {
  identical(x, FALSE)
}

rye_stdlib_fn_p <- function(x) {
  is.function(x)
}

rye_stdlib_callable_p <- function(x) {
  is.function(x)
}

rye_stdlib_str <- function(...) {
  paste0(...)
}

rye_stdlib_string_join <- function(x, sep = "") {
  x <- rye_as_list(x)
  paste(unlist(x), collapse = sep)
}

rye_stdlib_string_split <- function(x, sep = "") {
  unname(strsplit(x, split = sep, fixed = TRUE)[[1]])
}

rye_stdlib_trim <- function(x) {
  trimws(x)
}

rye_stdlib_format <- function(fmt, ...) {
  sprintf(fmt, ...)
}

rye_stdlib_read_line <- function(prompt = "") {
  con <- getOption("rye.stdin")
  if (is.null(con)) {
    con <- stdin()
  }
  if (nzchar(prompt)) {
    cat(prompt)
  }
  readLines(con = con, n = 1, warn = FALSE)
}

rye_stdlib_error <- function(msg) {
  stop(msg, call. = FALSE)
}

rye_stdlib_warn <- function(msg) {
  warning(msg, call. = FALSE)
}

rye_stdlib_assert <- function(cond, msg = "Assertion failed") {
  if (!rye_is_truthy(cond)) {
    stop(msg, call. = FALSE)
  }
  TRUE
}

rye_stdlib_trace <- function(x, label = NULL) {
  if (!is.null(label)) {
    cat(as.character(label), ": ", sep = "")
  }
  cat(as.character(x), "\n")
  x
}

rye_stdlib_try <- function(thunk, error_handler = NULL, finally_handler = NULL) {
  if (!is.function(thunk)) {
    stop("try* expects a function as first argument")
  }
  if (!is.null(error_handler) && !is.function(error_handler)) {
    stop("try* error handler must be a function")
  }
  if (!is.null(finally_handler) && !is.function(finally_handler)) {
    stop("try* finally handler must be a function")
  }
  if (!is.null(finally_handler)) {
    on.exit(finally_handler(), add = TRUE)
  }
  if (is.null(error_handler)) {
    return(thunk())
  }
  tryCatch(
    thunk(),
    error = function(e) {
      error_handler(e)
    }
  )
}

rye_stdlib_macro_p <- function(x) {
  if (is.symbol(x)) {
    is_macro(x)
  } else {
    FALSE
  }
}

rye_stdlib_macroexpand <- function(expr, env = parent.frame()) {
  rye_macroexpand(expr, env)
}

rye_stdlib_macroexpand_1 <- function(expr, env = parent.frame()) {
  if (!is.call(expr) || length(expr) == 0) {
    return(expr)
  }
  op <- expr[[1]]
  if (is.symbol(op) && is_macro(op)) {
    macro_fn <- get_macro(op)
    args <- as.list(expr[-1])
    return(do.call(macro_fn, args))
  }
  expr
}

rye_stdlib_eval <- function(expr, env = parent.frame()) {
  rye_eval(expr, env)
}

rye_stdlib_dict <- function(...) {
  args <- list(...)
  args
}

rye_stdlib_r_call <- function(fn, args = list()) {
  if (is.symbol(fn)) {
    fn <- get(as.character(fn), envir = baseenv())
  } else if (is.character(fn)) {
    fn <- get(fn, envir = baseenv())
  }
  rye_do_call(fn, rye_as_list(args))
}
