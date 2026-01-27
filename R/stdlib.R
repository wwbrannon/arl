#' Load the Rye standard library
#'
#' Loads the base Rye stdlib functions and, optionally, Rye stdlib source files
#' from the package's `inst/rye` directory.
#'
#' @details
#' The returned environment uses `baseenv()` as its parent so that core R
#' functions remain available alongside Rye helpers.
#'
#' @param env An environment to populate. If NULL, creates a new one.
#' @param load_files Whether to load Rye stdlib source files from `inst/rye`.
#' @return An environment containing the Rye standard library
#' @examples
#' env <- rye_load_stdlib()
#' env$`+`(1, 2)
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
  env$values <- rye_stdlib_values
  env$`call-with-values` <- rye_stdlib_call_with_values

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
  # Provide aliases for common R operators
  env$`+` <- base::`+`
  env$`-` <- base::`-`
  env$`*` <- base::`*`
  env$`/` <- base::`/`
  env$`<` <- base::`<`
  env$`<=` <- base::`<=`
  env$`>` <- base::`>`
  env$`>=` <- base::`>=`
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
  env$`read-file` <- rye_stdlib_read_file
  env$`read-lines` <- rye_stdlib_read_lines
  env$`write-file` <- rye_stdlib_write_file
  env$`write-lines` <- rye_stdlib_write_lines
  env$`append-file` <- rye_stdlib_append_file
  env$`file-exists?` <- rye_stdlib_file_exists_p
  env$`string-contains?` <- rye_stdlib_string_contains_p
  env$`string-match?` <- rye_stdlib_string_match_p
  env$`string-find` <- rye_stdlib_string_find
  env$`string-replace` <- rye_stdlib_string_replace
  env$`string-replace-all` <- rye_stdlib_string_replace_all

  # Errors and debugging
  env$error <- rye_stdlib_error
  env$warn <- rye_stdlib_warn
  env$assert <- rye_stdlib_assert
  env$trace <- rye_stdlib_trace
  env$`try*` <- rye_stdlib_try

  # Macro and eval helpers
  env$gensym <- gensym
  env$capture <- rye_capture
  env$`macro?` <- rye_stdlib_macro_p
  env$macroexpand <- rye_stdlib_macroexpand
  env$`macroexpand-1` <- rye_stdlib_macroexpand_1
  env$`macroexpand-all` <- rye_stdlib_macroexpand
  env$eval <- rye_stdlib_eval

  # Interop helpers
  env$dict <- rye_stdlib_dict
  env$hash <- rye_stdlib_dict
  env$`dict?` <- rye_stdlib_dict_p
  env$`dict-get` <- rye_stdlib_dict_get
  env$`dict-set` <- rye_stdlib_dict_set
  env$`dict-remove` <- rye_stdlib_dict_remove
  env$`dict-keys` <- rye_stdlib_dict_keys
  env$`dict-values` <- rye_stdlib_dict_values
  env$`dict-has?` <- rye_stdlib_dict_has_p
  env$`dict-merge` <- rye_stdlib_dict_merge
  env$set <- rye_stdlib_set
  env$`set?` <- rye_stdlib_set_p
  env$`set-add` <- rye_stdlib_set_add
  env$`set-remove` <- rye_stdlib_set_remove
  env$`set-contains?` <- rye_stdlib_set_contains_p
  env$`set-union` <- rye_stdlib_set_union
  env$`set-intersection` <- rye_stdlib_set_intersection
  env$`set-difference` <- rye_stdlib_set_difference
  env$`r/call` <- rye_stdlib_r_call

  # Convenience functions
  env$identity <- rye_stdlib_identity
  env$first <- rye_stdlib_first
  env$rest <- rye_stdlib_rest
  env$last <- rye_stdlib_last
  env$nth <- rye_stdlib_nth
  env$complement <- rye_stdlib_complement
  env$compose <- rye_stdlib_compose
  env$repeatedly <- rye_stdlib_repeatedly
  env$`repeat` <- rye_stdlib_repeat
  env$zip <- rye_stdlib_zip
  env$partial <- rye_stdlib_partial

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
    "struct.rye",
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
  cat(rye_stdlib_format_value(x), "\n")
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
  if (length(args) > 2 &&
        (identical(fn, base::`+`) || identical(fn, base::`*`) ||
           identical(fn, base::`-`) || identical(fn, base::`/`))) {
    return(Reduce(fn, args))
  }
  rye_do_call(fn, args)
}

rye_stdlib_values <- function(...) {
  rye_values_new(list(...))
}

rye_stdlib_call_with_values <- function(producer, consumer) {
  if (!is.function(producer)) {
    stop("call-with-values expects a function as the producer")
  }
  if (!is.function(consumer)) {
    stop("call-with-values expects a function as the consumer")
  }
  produced <- producer()
  args <- rye_values_list(produced)
  rye_do_call(consumer, args)
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
  args <- list(...)
  formatted <- lapply(args, function(arg) {
    if (is.symbol(arg)) {
      return(as.character(arg))
    }
    if (is.environment(arg) && inherits(arg, "rye_dict")) {
      return(as.character(rye_stdlib_dict_to_list(arg)))
    }
    if (is.environment(arg) && inherits(arg, "rye_set")) {
      return(as.character(unname(as.list(arg, all.names = TRUE))))
    }
    if (is.list(arg) && any(vapply(arg, function(item) is.environment(item) &&
        (inherits(item, "rye_dict") || inherits(item, "rye_set")), logical(1)))) {
      converted <- lapply(arg, function(item) {
        if (is.environment(item) && inherits(item, "rye_dict")) {
          return(rye_stdlib_dict_to_list(item))
        }
        if (is.environment(item) && inherits(item, "rye_set")) {
          return(unname(as.list(item, all.names = TRUE)))
        }
        item
      })
      return(as.character(converted))
    }
    if (is.call(arg)) {
      return(as.character(arg))
    }
    arg
  })
  do.call(paste0, formatted)
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

rye_stdlib_read_file <- function(path, encoding = "UTF-8") {
  con <- file(path, open = "r", encoding = encoding)
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  paste(lines, collapse = "\n")
}

rye_stdlib_read_lines <- function(path, encoding = "UTF-8") {
  con <- file(path, open = "r", encoding = encoding)
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  as.list(lines)
}

rye_stdlib_write_file <- function(path, content, sep = "\n", encoding = "UTF-8") {
  text <- rye_stdlib_normalize_lines(content, sep = sep)
  con <- file(path, open = "w", encoding = encoding)
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = FALSE)
  invisible(TRUE)
}

rye_stdlib_write_lines <- function(path, lines, encoding = "UTF-8") {
  lines <- rye_stdlib_normalize_line_vector(lines)
  con <- file(path, open = "w", encoding = encoding)
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con, useBytes = FALSE)
  invisible(TRUE)
}

rye_stdlib_append_file <- function(path, content, sep = "\n", encoding = "UTF-8") {
  text <- rye_stdlib_normalize_lines(content, sep = sep)
  con <- file(path, open = "a", encoding = encoding)
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = FALSE, sep = "")
  invisible(TRUE)
}

rye_stdlib_file_exists_p <- function(path) {
  isTRUE(file.exists(path))
}

rye_stdlib_string_contains_p <- function(str, pattern, fixed = TRUE) {
  isTRUE(grepl(pattern, str, fixed = fixed))
}

rye_stdlib_string_match_p <- function(str, pattern, fixed = FALSE) {
  isTRUE(grepl(pattern, str, fixed = fixed))
}

rye_stdlib_string_find <- function(str, pattern, fixed = TRUE) {
  match <- regexpr(pattern, str, fixed = fixed)
  if (match[1] < 0) {
    return(NULL)
  }
  match[1] - 1
}

rye_stdlib_string_replace <- function(str, pattern, replacement, fixed = FALSE) {
  sub(pattern, replacement, str, fixed = fixed)
}

rye_stdlib_string_replace_all <- function(str, pattern, replacement, fixed = FALSE) {
  gsub(pattern, replacement, str, fixed = fixed)
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
  cat(rye_stdlib_format_value(x), "\n")
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
    expanded <- do.call(macro_fn, args)
    expanded <- rye_hygienize(expanded)
    expanded <- rye_hygiene_unwrap(expanded)
    expanded <- rye_src_inherit(expanded, expr)
    return(expanded)
  }
  expr
}

rye_stdlib_eval <- function(expr, env = parent.frame()) {
  rye_eval(expr, env)
}

rye_stdlib_dict <- function(...) {
  args <- list(...)
  dict <- rye_stdlib_dict_new()
  if (length(args) == 0) {
    return(dict)
  }
  arg_names <- names(args)
  if (is.null(arg_names) || any(!nzchar(arg_names))) {
    stop("dict requires named arguments")
  }
  for (name in arg_names) {
    assign(name, args[[name]], envir = dict)
  }
  assign(".rye_keys", arg_names, envir = dict)
  dict
}

rye_stdlib_dict_p <- function(x) {
  is.environment(x) && inherits(x, "rye_dict")
}

rye_stdlib_dict_get <- function(dict, key, default = NULL) {
  name <- rye_stdlib_dict_key_to_name(key)
  if (is.null(name) || !rye_stdlib_dict_p(dict)) {
    return(default)
  }
  if (exists(name, envir = dict, inherits = FALSE)) {
    return(get(name, envir = dict, inherits = FALSE))
  }
  default
}

rye_stdlib_dict_set <- function(dict, key, value) {
  name <- rye_stdlib_dict_key_to_name(key)
  if (is.null(name)) {
    stop("dict-set requires a string, symbol, or keyword key")
  }
  if (!rye_stdlib_dict_p(dict)) {
    stop("dict-set requires a dict")
  }
  keys <- rye_stdlib_dict_keys_ordered(dict)
  if (!name %in% keys) {
    assign(".rye_keys", c(keys, name), envir = dict)
  }
  assign(name, value, envir = dict)
  dict
}

rye_stdlib_dict_remove <- function(dict, key) {
  name <- rye_stdlib_dict_key_to_name(key)
  if (is.null(name) || !rye_stdlib_dict_p(dict)) {
    return(dict)
  }
  if (!exists(name, envir = dict, inherits = FALSE)) {
    return(dict)
  }
  rm(list = name, envir = dict, inherits = FALSE)
  keys <- rye_stdlib_dict_keys_ordered(dict)
  if (length(keys) > 0) {
    assign(".rye_keys", keys[keys != name], envir = dict)
  }
  dict
}

rye_stdlib_dict_keys <- function(dict) {
  if (!rye_stdlib_dict_p(dict)) {
    return(list())
  }
  as.list(rye_stdlib_dict_keys_ordered(dict))
}

rye_stdlib_dict_values <- function(dict) {
  if (!rye_stdlib_dict_p(dict)) {
    return(list())
  }
  keys <- rye_stdlib_dict_keys_ordered(dict)
  if (length(keys) == 0) {
    return(list())
  }
  rye_as_list(unname(mget(keys, envir = dict, inherits = FALSE)))
}

rye_stdlib_dict_has_p <- function(dict, key) {
  name <- rye_stdlib_dict_key_to_name(key)
  if (is.null(name) || !rye_stdlib_dict_p(dict)) {
    return(FALSE)
  }
  exists(name, envir = dict, inherits = FALSE)
}

rye_stdlib_dict_merge <- function(...) {
  dicts <- list(...)
  result <- rye_stdlib_dict_new()
  result_keys <- character(0)
  for (dict in dicts) {
    if (!rye_stdlib_dict_p(dict)) {
      next
    }
    keys <- rye_stdlib_dict_keys_ordered(dict)
    if (length(keys) == 0) {
      next
    }
    for (name in keys) {
      assign(name, get(name, envir = dict, inherits = FALSE), envir = result)
      if (!name %in% result_keys) {
        result_keys <- c(result_keys, name)
      }
    }
  }
  assign(".rye_keys", result_keys, envir = result)
  result
}

rye_stdlib_set <- function(...) {
  args <- list(...)
  set <- rye_stdlib_set_new()
  if (length(args) == 1 && (is.list(args[[1]]) || is.call(args[[1]]))) {
    items <- rye_as_list(args[[1]])
  } else {
    items <- rye_as_list(args)
  }
  for (item in items) {
    key <- rye_stdlib_set_key(item)
    if (!exists(key, envir = set, inherits = FALSE)) {
      assign(key, item, envir = set)
    }
  }
  set
}

rye_stdlib_set_p <- function(x) {
  is.environment(x) && inherits(x, "rye_set")
}

rye_stdlib_set_add <- function(set, item) {
  if (!rye_stdlib_set_p(set)) {
    stop("set-add requires a set")
  }
  key <- rye_stdlib_set_key(item)
  if (!exists(key, envir = set, inherits = FALSE)) {
    assign(key, item, envir = set)
  }
  set
}

rye_stdlib_set_remove <- function(set, item) {
  if (!rye_stdlib_set_p(set)) {
    return(set)
  }
  key <- rye_stdlib_set_key(item)
  if (exists(key, envir = set, inherits = FALSE)) {
    rm(list = key, envir = set, inherits = FALSE)
  }
  set
}

rye_stdlib_set_contains_p <- function(set, item) {
  if (!rye_stdlib_set_p(set)) {
    return(FALSE)
  }
  key <- rye_stdlib_set_key(item)
  exists(key, envir = set, inherits = FALSE)
}

rye_stdlib_set_union <- function(a, b) {
  result <- rye_stdlib_set_new()
  rye_stdlib_set_copy_into(result, a)
  rye_stdlib_set_copy_into(result, b)
  result
}

rye_stdlib_set_intersection <- function(a, b) {
  result <- rye_stdlib_set_new()
  if (!rye_stdlib_set_p(a) || !rye_stdlib_set_p(b)) {
    return(result)
  }
  keys <- ls(envir = a, all.names = TRUE, sorted = FALSE)
  for (key in keys) {
    if (exists(key, envir = b, inherits = FALSE)) {
      assign(key, get(key, envir = a, inherits = FALSE), envir = result)
    }
  }
  result
}

rye_stdlib_set_difference <- function(a, b) {
  result <- rye_stdlib_set_new()
  if (!rye_stdlib_set_p(a)) {
    return(result)
  }
  keys <- ls(envir = a, all.names = TRUE, sorted = FALSE)
  for (key in keys) {
    if (!rye_stdlib_set_p(b) || !exists(key, envir = b, inherits = FALSE)) {
      assign(key, get(key, envir = a, inherits = FALSE), envir = result)
    }
  }
  result
}

rye_stdlib_r_call <- function(fn, args = list()) {
  if (is.symbol(fn)) {
    fn <- get(as.character(fn), envir = baseenv())
  } else if (is.character(fn)) {
    fn <- get(fn, envir = baseenv())
  }
  rye_do_call(fn, rye_as_list(args))
}

# ============================================================================
# Convenience Functions
# Internal stdlib helper functions - not exported
# ============================================================================

# Identity function - returns its argument unchanged
rye_stdlib_identity <- function(x) {
  x
}

# Alias for car - returns first element of list
rye_stdlib_first <- function(lst) {
  rye_stdlib_car(lst)
}

# Alias for cdr - returns rest of list
rye_stdlib_rest <- function(lst) {
  rye_stdlib_cdr(lst)
}

# Get last element of a list
rye_stdlib_last <- function(lst) {
  if (is.call(lst)) {
    lst <- as.list(lst)
  }
  if (length(lst) == 0) {
    NULL
  } else {
    lst[[length(lst)]]
  }
}

# Get nth element of a list (0-indexed)
rye_stdlib_nth <- function(lst, n) {
  if (is.call(lst)) {
    lst <- as.list(lst)
  }
  if (n < 0 || n >= length(lst)) {
    stop(sprintf("Index %d out of bounds for list of length %d", n, length(lst)))
  }
  lst[[n + 1]]  # R is 1-indexed
}

# Negate a predicate function
rye_stdlib_complement <- function(pred) {
  function(...) !pred(...)
}

# Compose two functions (right to left)
rye_stdlib_compose <- function(f, g) {
  function(...) f(g(...))
}

# Call a function n times and collect results
rye_stdlib_repeatedly <- function(n, fn) {
  lapply(seq_len(n), function(i) fn())
}

# Repeat a value n times
rye_stdlib_repeat <- function(n, value) {
  replicate(n, value, simplify = FALSE)
}

# Zip multiple lists together
rye_stdlib_zip <- function(...) {
  lists <- list(...)
  if (length(lists) == 0) {
    return(list())
  }
  # Convert calls to lists
  lists <- lapply(lists, function(x) {
    if (is.call(x)) as.list(x) else x
  })
  # Find minimum length
  min_len <- min(vapply(lists, length, integer(1)))
  if (min_len == 0) {
    return(list())
  }
  # Zip together
  lapply(seq_len(min_len), function(i) {
    lapply(lists, function(lst) lst[[i]])
  })
}

# Partial application of a function
rye_stdlib_partial <- function(fn, ...) {
  captured_args <- list(...)
  function(...) {
    all_args <- c(captured_args, list(...))
    do.call(fn, all_args)
  }
}

rye_stdlib_normalize_line_vector <- function(lines) {
  if (is.null(lines)) {
    return(character(0))
  }
  if (is.call(lines)) {
    lines <- as.list(lines)
  }
  if (is.list(lines)) {
    return(vapply(lines, as.character, character(1)))
  }
  as.character(lines)
}

rye_stdlib_normalize_lines <- function(content, sep = "\n") {
  lines <- rye_stdlib_normalize_line_vector(content)
  if (length(lines) <= 1) {
    return(as.character(lines))
  }
  paste(lines, collapse = sep)
}

rye_stdlib_dict_key_to_name <- function(key) {
  if (is.character(key)) {
    return(key)
  }
  if (inherits(key, "rye_keyword") || is.symbol(key)) {
    return(as.character(key))
  }
  NULL
}

rye_stdlib_dict_new <- function() {
  dict <- new.env(hash = TRUE, parent = emptyenv())
  class(dict) <- c("rye_dict", class(dict))
  assign(".rye_keys", character(0), envir = dict)
  dict
}

rye_stdlib_set_new <- function() {
  set <- new.env(hash = TRUE, parent = emptyenv())
  class(set) <- c("rye_set", class(set))
  set
}

rye_stdlib_set_key <- function(value) {
  raw <- serialize(value, NULL, ascii = TRUE)
  paste(as.integer(raw), collapse = ",")
}

rye_stdlib_set_copy_into <- function(target, source) {
  if (!rye_stdlib_set_p(target) || !rye_stdlib_set_p(source)) {
    return(target)
  }
  keys <- ls(envir = source, all.names = TRUE, sorted = FALSE)
  for (key in keys) {
    if (!exists(key, envir = target, inherits = FALSE)) {
      assign(key, get(key, envir = source, inherits = FALSE), envir = target)
    }
  }
  target
}

rye_stdlib_dict_keys_ordered <- function(dict) {
  if (!rye_stdlib_dict_p(dict)) {
    return(character(0))
  }
  keys <- get0(".rye_keys", envir = dict, inherits = FALSE)
  if (is.null(keys)) {
    return(sort(ls(envir = dict, all.names = FALSE)))
  }
  keys
}

rye_stdlib_dict_to_list <- function(dict) {
  keys <- rye_stdlib_dict_keys_ordered(dict)
  if (length(keys) == 0) {
    return(list())
  }
  values <- mget(keys, envir = dict, inherits = FALSE)
  names(values) <- keys
  values
}

rye_stdlib_deparse_single <- function(x) {
  paste(deparse(x, width.cutoff = 500), collapse = " ")
}

rye_stdlib_format_value <- function(x) {
  if (rye_values_p(x)) {
    inner <- unclass(x)
    if (length(inner) == 0) {
      return("(values)")
    }
    return(paste(c("(values", as.character(inner), ")"), collapse = " "))
  }
  if (is.environment(x) && inherits(x, "rye_dict")) {
    return(paste(as.character(rye_stdlib_dict_to_list(x)), collapse = " "))
  }
  if (is.environment(x) && inherits(x, "rye_set")) {
    return(rye_stdlib_deparse_single(unname(as.list(x, all.names = TRUE))))
  }
  if (is.list(x)) {
    if (length(x) == 0) {
      return("")
    }
    if (any(vapply(x, function(item) is.environment(item) &&
        (inherits(item, "rye_dict") || inherits(item, "rye_set")), logical(1)))) {
      converted <- lapply(x, function(item) {
        if (is.environment(item) && inherits(item, "rye_dict")) {
          return(rye_stdlib_dict_to_list(item))
        }
        if (is.environment(item) && inherits(item, "rye_set")) {
          return(unname(as.list(item, all.names = TRUE)))
        }
        item
      })
      return(paste(as.character(converted), collapse = " "))
    }
    return(paste(as.character(x), collapse = " "))
  }
  if (is.call(x)) {
    return(paste(as.character(x), collapse = " "))
  }
  as.character(x)
}

attr(rye_stdlib_map, "rye_doc") <- list(
  usage = "(map fn lst)",
  description = "Apply fn to each element of lst and return a list."
)
attr(rye_stdlib_mapcat, "rye_doc") <- list(
  usage = "(mapcat fn lst)",
  description = "Map fn over lst and concatenate the results."
)
attr(rye_stdlib_filter, "rye_doc") <- list(
  usage = "(filter pred lst)",
  description = "Return elements of lst for which pred returns truthy."
)
attr(rye_stdlib_remove, "rye_doc") <- list(
  usage = "(remove pred lst)",
  description = "Return elements of lst for which pred returns falsy."
)
attr(rye_stdlib_reduce, "rye_doc") <- list(
  usage = "(reduce fn lst [init])",
  description = "Reduce lst by repeatedly applying fn."
)
attr(rye_stdlib_apply, "rye_doc") <- list(
  usage = "(apply fn lst)",
  description = "Apply fn to the elements of lst as arguments."
)
attr(rye_stdlib_values, "rye_doc") <- list(
  usage = "(values ...)",
  description = "Return multiple values to a call-with-values consumer."
)
attr(rye_stdlib_call_with_values, "rye_doc") <- list(
  usage = "(call-with-values producer consumer)",
  description = "Call producer and pass its values to consumer."
)

attr(rye_stdlib_car, "rye_doc") <- list(
  usage = "(car lst)",
  description = "Return the first element of lst."
)
attr(rye_stdlib_cdr, "rye_doc") <- list(
  usage = "(cdr lst)",
  description = "Return the rest of lst after the first element."
)
attr(rye_stdlib_cons, "rye_doc") <- list(
  usage = "(cons item lst)",
  description = "Prepend item to lst."
)
attr(rye_stdlib_call, "rye_doc") <- list(
  usage = "(call lst)",
  description = "Convert a list to a callable form."
)
attr(rye_stdlib_list_star, "rye_doc") <- list(
  usage = "(list* item ... tail)",
  description = "Build a list with tail as the final cdr."
)
attr(rye_stdlib_append, "rye_doc") <- list(
  usage = "(append lst ...)",
  description = "Concatenate lists."
)
attr(rye_stdlib_reverse, "rye_doc") <- list(
  usage = "(reverse lst)",
  description = "Return a reversed list."
)
attr(rye_stdlib_take, "rye_doc") <- list(
  usage = "(take n lst)",
  description = "Return the first n elements of lst."
)
attr(rye_stdlib_drop, "rye_doc") <- list(
  usage = "(drop n lst)",
  description = "Return lst without the first n elements."
)
attr(rye_stdlib_take_while, "rye_doc") <- list(
  usage = "(take-while pred lst)",
  description = "Take elements while pred returns truthy."
)
attr(rye_stdlib_drop_while, "rye_doc") <- list(
  usage = "(drop-while pred lst)",
  description = "Drop elements while pred returns truthy."
)
attr(rye_stdlib_partition, "rye_doc") <- list(
  usage = "(partition n lst)",
  description = "Partition lst into sublists of size n."
)
attr(rye_stdlib_flatten, "rye_doc") <- list(
  usage = "(flatten lst)",
  description = "Flatten a nested list."
)

attr(rye_stdlib_list_p, "rye_doc") <- list(
  usage = "(list? x)",
  description = "Return TRUE if x is a list or call."
)
attr(rye_stdlib_pair_p, "rye_doc") <- list(
  usage = "(pair? x)",
  description = "Return TRUE if x is a non-empty list or call."
)
attr(rye_stdlib_null_p, "rye_doc") <- list(
  usage = "(null? x)",
  description = "Return TRUE if x is NULL or an empty list."
)
attr(rye_stdlib_symbol_p, "rye_doc") <- list(
  usage = "(symbol? x)",
  description = "Return TRUE if x is a symbol."
)
attr(rye_stdlib_keyword_p, "rye_doc") <- list(
  usage = "(keyword? x)",
  description = "Return TRUE if x is a keyword."
)
attr(rye_stdlib_number_p, "rye_doc") <- list(
  usage = "(number? x)",
  description = "Return TRUE if x is numeric."
)
attr(rye_stdlib_string_p, "rye_doc") <- list(
  usage = "(string? x)",
  description = "Return TRUE if x is a string."
)
attr(rye_stdlib_vector_p, "rye_doc") <- list(
  usage = "(vector? x)",
  description = "Return TRUE if x is a vector."
)
attr(rye_stdlib_true_p, "rye_doc") <- list(
  usage = "(true? x)",
  description = "Return TRUE if x is truthy."
)
attr(rye_stdlib_false_p, "rye_doc") <- list(
  usage = "(false? x)",
  description = "Return TRUE if x is falsy."
)
attr(rye_stdlib_fn_p, "rye_doc") <- list(
  usage = "(fn? x)",
  description = "Return TRUE if x is a function."
)
attr(rye_stdlib_callable_p, "rye_doc") <- list(
  usage = "(callable? x)",
  description = "Return TRUE if x can be called."
)

attr(rye_stdlib_not, "rye_doc") <- list(
  usage = "(not x)",
  description = "Logical negation."
)
attr(rye_stdlib_modulo, "rye_doc") <- list(
  usage = "(% a b)",
  description = "Remainder of a divided by b."
)
attr(rye_stdlib_equal, "rye_doc") <- list(
  usage = "(= a b)",
  description = "Lisp-style equality."
)

attr(rye_stdlib_display, "rye_doc") <- list(
  usage = "(display x)",
  description = "Print x without formatting."
)
attr(rye_stdlib_str, "rye_doc") <- list(
  usage = "(str x)",
  description = "Display structure of x."
)
attr(rye_stdlib_string_join, "rye_doc") <- list(
  usage = "(string-join parts [sep])",
  description = "Join strings with sep."
)
attr(rye_stdlib_string_split, "rye_doc") <- list(
  usage = "(string-split s [sep])",
  description = "Split string s on sep."
)
attr(rye_stdlib_trim, "rye_doc") <- list(
  usage = "(trim s)",
  description = "Trim whitespace from s."
)
attr(rye_stdlib_format, "rye_doc") <- list(
  usage = "(format fmt args...)",
  description = "Format a string."
)
attr(rye_stdlib_read_line, "rye_doc") <- list(
  usage = "(read-line)",
  description = "Read a single line from stdin."
)
attr(rye_stdlib_read_file, "rye_doc") <- list(
  usage = "(read-file path [:encoding enc])",
  description = "Read an entire file into a single string."
)
attr(rye_stdlib_read_lines, "rye_doc") <- list(
  usage = "(read-lines path [:encoding enc])",
  description = "Read a file into a list of lines."
)
attr(rye_stdlib_write_file, "rye_doc") <- list(
  usage = "(write-file path content [:sep s] [:encoding enc])",
  description = "Write content to a file, overwriting if it exists."
)
attr(rye_stdlib_write_lines, "rye_doc") <- list(
  usage = "(write-lines path lines [:encoding enc])",
  description = "Write a list of lines to a file."
)
attr(rye_stdlib_append_file, "rye_doc") <- list(
  usage = "(append-file path content [:sep s] [:encoding enc])",
  description = "Append content to a file."
)
attr(rye_stdlib_file_exists_p, "rye_doc") <- list(
  usage = "(file-exists? path)",
  description = "Return TRUE if path exists."
)
attr(rye_stdlib_string_contains_p, "rye_doc") <- list(
  usage = "(string-contains? s pattern [:fixed #t])",
  description = "Return TRUE if pattern occurs in s."
)
attr(rye_stdlib_string_match_p, "rye_doc") <- list(
  usage = "(string-match? s pattern [:fixed #f])",
  description = "Return TRUE if regex pattern matches s."
)
attr(rye_stdlib_string_find, "rye_doc") <- list(
  usage = "(string-find s pattern [:fixed #t])",
  description = "Return 0-based index of first match or #nil."
)
attr(rye_stdlib_string_replace, "rye_doc") <- list(
  usage = "(string-replace s pattern replacement [:fixed #f])",
  description = "Replace the first match in s."
)
attr(rye_stdlib_string_replace_all, "rye_doc") <- list(
  usage = "(string-replace-all s pattern replacement [:fixed #f])",
  description = "Replace all matches in s."
)

attr(rye_stdlib_error, "rye_doc") <- list(
  usage = "(error message)",
  description = "Raise an error with message."
)
attr(rye_stdlib_warn, "rye_doc") <- list(
  usage = "(warn message)",
  description = "Emit a warning with message."
)
attr(rye_stdlib_assert, "rye_doc") <- list(
  usage = "(assert test [message])",
  description = "Raise error if test is falsy."
)
attr(rye_stdlib_trace, "rye_doc") <- list(
  usage = "(trace x)",
  description = "Print x and return it."
)
attr(rye_stdlib_try, "rye_doc") <- list(
  usage = "(try* thunk [:error_handler fn] [:finally_handler fn])",
  description = "Evaluate thunk with error/finally handlers."
)

attr(rye_stdlib_macroexpand, "rye_doc") <- list(
  usage = "(macroexpand expr)",
  description = "Recursively expand macros in expr."
)
attr(rye_stdlib_macroexpand_1, "rye_doc") <- list(
  usage = "(macroexpand-1 expr)",
  description = "Expand a single macro layer in expr."
)
attr(rye_stdlib_eval, "rye_doc") <- list(
  usage = "(eval expr)",
  description = "Evaluate expr in the current environment."
)

attr(rye_stdlib_dict, "rye_doc") <- list(
  usage = "(dict key val ...)",
  description = "Create a hash-backed dictionary from key/value pairs."
)
attr(rye_stdlib_dict_p, "rye_doc") <- list(
  usage = "(dict? x)",
  description = "Return TRUE if x is a dictionary."
)
attr(rye_stdlib_dict_get, "rye_doc") <- list(
  usage = "(dict-get dict key [default])",
  description = "Get value for key or default if missing."
)
attr(rye_stdlib_dict_set, "rye_doc") <- list(
  usage = "(dict-set dict key value)",
  description = "Set key to value in dict and return dict."
)
attr(rye_stdlib_dict_remove, "rye_doc") <- list(
  usage = "(dict-remove dict key)",
  description = "Remove key from dict and return dict."
)
attr(rye_stdlib_dict_keys, "rye_doc") <- list(
  usage = "(dict-keys dict)",
  description = "Return a list of dict keys."
)
attr(rye_stdlib_dict_values, "rye_doc") <- list(
  usage = "(dict-values dict)",
  description = "Return a list of dict values."
)
attr(rye_stdlib_dict_has_p, "rye_doc") <- list(
  usage = "(dict-has? dict key)",
  description = "Return TRUE if dict contains key."
)
attr(rye_stdlib_dict_merge, "rye_doc") <- list(
  usage = "(dict-merge dict ...)",
  description = "Merge dicts, later values override earlier."
)
attr(rye_stdlib_set, "rye_doc") <- list(
  usage = "(set item ...)",
  description = "Create a hash-backed set of unique items."
)
attr(rye_stdlib_set_p, "rye_doc") <- list(
  usage = "(set? x)",
  description = "Return TRUE if x is a set."
)
attr(rye_stdlib_set_add, "rye_doc") <- list(
  usage = "(set-add set item)",
  description = "Add item to set and return set."
)
attr(rye_stdlib_set_remove, "rye_doc") <- list(
  usage = "(set-remove set item)",
  description = "Remove item from set and return set."
)
attr(rye_stdlib_set_contains_p, "rye_doc") <- list(
  usage = "(set-contains? set item)",
  description = "Return TRUE if set contains item."
)
attr(rye_stdlib_set_union, "rye_doc") <- list(
  usage = "(set-union a b)",
  description = "Return union of two sets."
)
attr(rye_stdlib_set_intersection, "rye_doc") <- list(
  usage = "(set-intersection a b)",
  description = "Return intersection of two sets."
)
attr(rye_stdlib_set_difference, "rye_doc") <- list(
  usage = "(set-difference a b)",
  description = "Return items in a that are not in b."
)
attr(rye_stdlib_r_call, "rye_doc") <- list(
  usage = "(r/call fn [:args list])",
  description = "Call an R function with list arguments."
)

attr(rye_stdlib_identity, "rye_doc") <- list(
  usage = "(identity x)",
  description = "Return x unchanged."
)
attr(rye_stdlib_first, "rye_doc") <- list(
  usage = "(first lst)",
  description = "Return the first element of lst."
)
attr(rye_stdlib_rest, "rye_doc") <- list(
  usage = "(rest lst)",
  description = "Return the rest of lst."
)
attr(rye_stdlib_last, "rye_doc") <- list(
  usage = "(last lst)",
  description = "Return the last element of lst."
)
attr(rye_stdlib_nth, "rye_doc") <- list(
  usage = "(nth lst n)",
  description = "Return the nth (0-indexed) element of lst."
)
attr(rye_stdlib_complement, "rye_doc") <- list(
  usage = "(complement pred)",
  description = "Return a predicate that negates pred."
)
attr(rye_stdlib_compose, "rye_doc") <- list(
  usage = "(compose f g)",
  description = "Compose f after g."
)
attr(rye_stdlib_repeatedly, "rye_doc") <- list(
  usage = "(repeatedly n fn)",
  description = "Call fn n times and collect results."
)
attr(rye_stdlib_repeat, "rye_doc") <- list(
  usage = "(repeat n value)",
  description = "Repeat value n times."
)
attr(rye_stdlib_zip, "rye_doc") <- list(
  usage = "(zip lst ...)",
  description = "Zip lists together."
)
attr(rye_stdlib_partial, "rye_doc") <- list(
  usage = "(partial fn args...)",
  description = "Return fn with args pre-applied."
)
