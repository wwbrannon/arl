#' Load the Rye standard library
#'
#' Loads the base Rye stdlib functions and Rye stdlib source files from
#' the package's `inst/rye` directory.
#'
#' @details
#' The returned environment uses `baseenv()` as its parent so that core R
#' functions remain available alongside Rye helpers.
#'
#' @param env An environment to populate. If NULL, creates a new one.
#' @return An environment containing the Rye standard library
#' @examples
#' env <- rye_load_stdlib()
#' env$`+`(1, 2)
#' @importFrom stats setNames
#' @export
rye_load_stdlib <- function(env = NULL) {
  env <- rye_load_stdlib_base(env)
  rye_load_stdlib_files(env)
  env
}

rye_load_stdlib_base <- function(env = NULL) {
  # Create environment with baseenv() as parent if not provided
  # This gives automatic access to all R base functions
  if (is.null(env)) {
    env <- new.env(parent = baseenv())
  }
  # Core helpers implemented in R
  env$call <- rye_stdlib_call
  env$apply <- rye_stdlib_apply
  env$values <- rye_stdlib_values
  env$`call-with-values` <- rye_stdlib_call_with_values
  env$`call/cc` <- rye_make_builtin_callcc()
  env$`call-with-current-continuation` <- env$`call/cc`

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
  # Additional arithmetic/equality helpers come from stdlib files

  # Output
  env$display <- rye_stdlib_display
  env$println <- rye_stdlib_display
  env$str <- rye_stdlib_str

  # Errors and debugging
  # Error helpers provided by stdlib files
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
    "stdlib-core.rye",
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


rye_stdlib_display <- function(x) {
  cat(rye_stdlib_format_value(x), "\n")
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

rye_stdlib_call <- function(lst) {
  if (is.call(lst)) {
    lst
  } else {
    as.call(rye_as_list(lst))
  }
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

attr(rye_stdlib_call, "rye_doc") <- list(
  usage = "(call lst)",
  description = "Convert a list to a callable form."
)

attr(rye_stdlib_display, "rye_doc") <- list(
  usage = "(display x)",
  description = "Print x without formatting."
)
attr(rye_stdlib_str, "rye_doc") <- list(
  usage = "(str x)",
  description = "Display structure of x."
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

