# Help utilities for Rye
rye_special_forms_help <- list(
  quote = list(
    usage = "(quote expr)",
    description = "Return expr without evaluation."
  ),
  quasiquote = list(
    usage = "(quasiquote expr)",
    description = "Template with selective evaluation via unquote."
  ),
  delay = list(
    usage = "(delay expr)",
    description = "Return a promise that evaluates expr when forced."
  ),
  unquote = list(
    usage = "(unquote expr)",
    description = "Evaluate expr inside quasiquote."
  ),
  `unquote-splicing` = list(
    usage = "(unquote-splicing expr)",
    description = "Splice a list into a quasiquoted list."
  ),
  `if` = list(
    usage = "(if test then [else])",
    description = "Evaluate then or else depending on test."
  ),
  lambda = list(
    usage = "(lambda (args...) body...)",
    description = "Create an anonymous function."
  ),
  define = list(
    usage = "(define name value)",
    description = "Bind name to value in the current environment."
  ),
  `set!` = list(
    usage = "(set! name value)",
    description = "Update an existing binding."
  ),
  help = list(
    usage = "(help topic)",
    description = "Show help for a topic without evaluating it."
  ),
  begin = list(
    usage = "(begin expr...)",
    description = "Evaluate expressions in sequence, returning the last."
  ),
  load = list(
    usage = "(load \"path\")",
    description = "Load and evaluate a Rye source file."
  ),
  defmacro = list(
    usage = "(defmacro name (params...) body...)",
    description = "Define a macro."
  ),
  module = list(
    usage = "(module name (export ...) body...)",
    description = "Define a module with explicit exports. Use (export-all) to export all definitions."
  ),
  import = list(
    usage = "(import name)",
    description = "Load a module and attach its exports."
  ),
  `::` = list(
    usage = "(:: pkg name)",
    description = "Access an exported object from an R package."
  ),
  `:::` = list(
    usage = "(::: pkg name)",
    description = "Access a non-exported object from an R package."
  ),
  `~` = list(
    usage = "(~ lhs rhs)",
    description = "Build an R formula without evaluating arguments."
  )
)

rye_help_print <- function(topic, doc) {
  lines <- c(paste0("Topic: ", topic))
  if (!is.null(doc$usage) && doc$usage != "") {
    lines <- c(lines, paste0("Usage: ", doc$usage))
  }
  if (!is.null(doc$description) && doc$description != "") {
    lines <- c(lines, paste0("Description: ", doc$description))
  }
  cat(paste(lines, collapse = "\n"), "\n")
}

rye_help_format_default <- function(expr) {
  if (identical(expr, quote(expr = ))) {
    return(NULL)
  }
  if (inherits(expr, "rye_missing_default")) {
    return(NULL)
  }
  paste(deparse(expr, width.cutoff = 500), collapse = " ")
}

rye_help_usage_from_closure <- function(fn, topic) {
  info <- attr(fn, "rye_closure", exact = TRUE)
  if (is.null(info)) {
    return(NULL)
  }
  args <- character(0)
  if (length(info$params) > 0) {
    param_specs <- info$param_specs
    if (is.null(param_specs)) {
      param_specs <- lapply(info$params, function(name) {
        list(type = "name", formal = name, display = name)
      })
    }
    for (spec in param_specs) {
      name <- spec$formal
      display <- spec$display
      if (is.null(display) || !nzchar(display)) {
        display <- name
      }
      default_expr <- info$defaults[[name]][[1]]
      default_text <- rye_help_format_default(default_expr)
      if (is.null(default_text)) {
        args <- c(args, display)
      } else {
        args <- c(args, paste0("(", display, " ", default_text, ")"))
      }
    }
  }
  rest_spec <- info$rest_param_spec
  if (!is.null(rest_spec)) {
    rest_display <- rest_spec$display
    if (is.null(rest_display) || !nzchar(rest_display)) {
      rest_display <- rest_spec$name
    }
    args <- c(args, ".", rest_display)
  } else if (!is.null(info$rest_param)) {
    args <- c(args, ".", info$rest_param)
  }
  paste0("(", topic, if (length(args) > 0) paste0(" ", paste(args, collapse = " ")) else "", ")")
}

rye_help_usage_from_formals <- function(fn, topic) {
  signature <- args(fn)
  if (is.null(signature)) {
    return(paste0("(", topic, ")"))
  }
  args_text <- paste(deparse(signature), collapse = " ")
  args_text <- sub("\\)\\s*NULL\\s*$", ")", args_text)
  args_text <- gsub(",\\s*", " ", args_text)
  args_text <- gsub("\\s+", " ", args_text)
  args_text <- sub("^function\\s*\\(", "", args_text)
  args_text <- sub("\\)$", "", args_text)
  args_text <- trimws(args_text)
  paste0("(", topic, if (nzchar(args_text)) paste0(" ", args_text) else "", ")")
}

rye_help_usage_from_macro <- function(fn, topic) {
  info <- attr(fn, "rye_macro", exact = TRUE)
  if (is.null(info) || is.null(info$params)) {
    return(paste0("(", topic, ")"))
  }
  params <- info$params
  paste0("(", topic, if (length(params) > 0) paste0(" ", paste(params, collapse = " ")) else "", ")")
}

rye_help <- function(topic, env = parent.frame()) {
  if (!is.character(topic) || length(topic) != 1) {
    stop("help requires a symbol or string")
  }

  doc <- rye_special_forms_help[[topic]]
  if (!is.null(doc)) {
    rye_help_print(topic, doc)
    return(invisible(NULL))
  }

  macro_symbol <- as.symbol(topic)
  if (is_macro(macro_symbol)) {
    macro_fn <- get_macro(macro_symbol)
    macro_doc <- attr(macro_fn, "rye_doc", exact = TRUE)
    usage <- rye_help_usage_from_macro(macro_fn, topic)
    if (!is.null(macro_doc)) {
      if (is.character(macro_doc)) {
        macro_doc <- list(description = paste(macro_doc, collapse = "\n"))
      }
      if (is.null(macro_doc$usage) && !is.null(usage)) {
        macro_doc$usage <- usage
      }
      rye_help_print(topic, macro_doc)
      return(invisible(NULL))
    }
    if (!is.null(usage)) {
      rye_help_print(topic, list(usage = usage))
      return(invisible(NULL))
    }
  }

  if (exists(topic, envir = env, inherits = TRUE)) {
    obj <- get(topic, envir = env, inherits = TRUE)
    obj_doc <- attr(obj, "rye_doc", exact = TRUE)
    usage <- NULL
    if (inherits(obj, "rye_closure")) {
      usage <- rye_help_usage_from_closure(obj, topic)
    } else if (is.function(obj)) {
      usage <- rye_help_usage_from_formals(obj, topic)
    }
    if (!is.null(obj_doc)) {
      if (is.character(obj_doc)) {
        obj_doc <- list(description = paste(obj_doc, collapse = "\n"))
      }
      if (is.null(obj_doc$usage) && !is.null(usage)) {
        obj_doc$usage <- usage
      }
      rye_help_print(topic, obj_doc)
      return(invisible(NULL))
    }
    if (!is.null(usage)) {
      rye_help_print(topic, list(usage = usage))
      return(invisible(NULL))
    }
  }

  utils::help(topic, help_type = "text")
  invisible(NULL)
}
