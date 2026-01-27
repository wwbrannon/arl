# ---
# title: "Translator Reference (R Implementation)"
# output:
#   rmarkdown::html_vignette:
#     highlight: null
#     pandoc_args: ["--syntax-highlighting=tango"]
# vignette: >
#   %\VignetteIndexEntry{Translator Reference (R Implementation)}
#   %\VignetteEngine{knitr::rmarkdown}
#   %\VignetteEncoding{UTF-8}
# ---

# This file preserves the original R implementation of the Rye translator
# (`rye_expr_to_r`) as a reference. The production code now uses the `.rye`
# version in `inst/rye/translator.rye`.

# ```r
# Translate a Rye AST expression to R code
#
# @param expr A parsed Rye expression (from rye_parse)
# @param indent Current indentation level (used internally)
# @return A character string containing R code
# @keywords internal
rye_expr_to_r <- function(expr, indent = 0) {
  indent_str <- paste(rep("  ", indent), collapse = "")

  # Handle NULL
  if (is.null(expr)) {
    return("NULL")
  }

  # Handle keywords
  if (inherits(expr, "rye_keyword")) {
    return(paste0(as.character(expr), " ="))
  }

  # Handle atomic values
  if (is.numeric(expr)) {
    return(as.character(expr))
  }

  if (is.character(expr) && !is.symbol(expr)) {
    # Escape quotes in the string
    escaped <- gsub('"', '\\\\"', expr, fixed = TRUE)
    return(paste0('"', escaped, '"'))
  }

  if (is.logical(expr)) {
    return(as.character(expr))
  }

  # Handle symbols
  if (is.symbol(expr)) {
    sym_name <- as.character(expr)
    if (sym_name == "#inf") {
      return("Inf")
    }
    if (sym_name == "#-inf") {
      return("-Inf")
    }
    if (sym_name == "#nan") {
      return("NaN")
    }
    # Handle R operators that need special treatment
    if (sym_name %in% c("+", "-", "*", "/", "%%", "%/%", "^",
                        "<", ">", "<=", ">=", "==", "!=",
                        "&", "|", "!")) {
      return(paste0("`", sym_name, "`"))
    }
    return(sym_name)
  }

  # Handle empty list
  if (is.call(expr) && length(expr) == 0) {
    return("list()")
  }

  # Handle calls
  if (is.call(expr)) {
    op <- expr[[1]]

    # Handle quote
    if (is.symbol(op) && as.character(op) == "quote") {
      if (length(expr) != 2) {
        stop("quote requires exactly 1 argument")
      }
      return(paste0("quote(", rye_expr_to_r(expr[[2]], indent), ")"))
    }

    # Handle quasiquote
    if (is.symbol(op) && as.character(op) == "quasiquote") {
      if (length(expr) != 2) {
        stop("quasiquote requires exactly 1 argument")
      }
      return(paste0("bquote(", rye_expr_to_r(expr[[2]], indent), ")"))
    }

    # Handle unquote
    if (is.symbol(op) && as.character(op) == "unquote") {
      if (length(expr) != 2) {
        stop("unquote requires exactly 1 argument")
      }
      return(paste0(".(", rye_expr_to_r(expr[[2]], indent), ")"))
    }

    # Handle unquote-splicing
    if (is.symbol(op) && as.character(op) == "unquote-splicing") {
      if (length(expr) != 2) {
        stop("unquote-splicing requires exactly 1 argument")
      }
      return(paste0(".(", rye_expr_to_r(expr[[2]], indent), ")"))
    }

    # Handle lambda
    if (is.symbol(op) && as.character(op) == "lambda") {
      if (length(expr) < 2) {
        stop("lambda requires an argument list")
      }

      # Get arguments
      args_expr <- expr[[2]]
      arg_items <- list()
      if (!is.null(args_expr)) {
        if (is.call(args_expr)) {
          if (length(args_expr) > 0) {
            arg_items <- as.list(args_expr)
          }
        } else if (is.list(args_expr)) {
          arg_items <- args_expr
        } else {
          stop("lambda arguments must be a list")
        }
      }

      rest_param <- NULL
      if (length(arg_items) > 0) {
        dot_idx <- which(vapply(arg_items, function(arg) {
          is.symbol(arg) && as.character(arg) == "."
        }, logical(1)))
        if (length(dot_idx) > 1) {
          stop("Dotted parameter list can only contain one '.'")
        }
        if (length(dot_idx) == 1) {
          if (dot_idx != length(arg_items) - 1) {
            stop("Dotted parameter list must have exactly one parameter after '.'")
          }
          rest_arg <- arg_items[[dot_idx + 1]]
          if (!is.symbol(rest_arg)) {
            stop("Rest parameter must be a symbol")
          }
          rest_param <- as.character(rest_arg)
          if (dot_idx > 1) {
            arg_items <- arg_items[1:(dot_idx - 1)]
          } else {
            arg_items <- list()
          }
        }
      }

      arg_parts <- character(0)
      param_names <- character(0)
      if (length(arg_items) > 0) {
        for (arg in arg_items) {
          if (is.symbol(arg)) {
            name <- as.character(arg)
            param_names <- c(param_names, name)
            arg_parts <- c(arg_parts, name)
          } else if (is.call(arg) || is.list(arg)) {
            arg_list <- if (is.call(arg)) as.list(arg) else arg
            if (length(arg_list) != 2) {
              stop("lambda default argument must be a 2-element list")
            }
            if (!is.symbol(arg_list[[1]])) {
              stop("lambda default argument name must be a symbol")
            }
            name <- as.character(arg_list[[1]])
            param_names <- c(param_names, name)
            default_expr <- rye_expr_to_r(arg_list[[2]], indent)
            arg_parts <- c(arg_parts, paste0(name, " = ", default_expr))
          } else {
            stop("lambda arguments must be symbols or (name default) pairs")
          }
        }
      }

      all_names <- c(param_names, if (!is.null(rest_param)) rest_param)
      if (length(all_names) > 0 && any(duplicated(all_names))) {
        stop("lambda argument names must be unique")
      }
      if (!is.null(rest_param)) {
        arg_parts <- c(arg_parts, "...")
      }

      # Get body
      body_parts <- character(0)
      has_body <- length(expr) >= 3
      if (has_body) {
        for (i in 3:length(expr)) {
          body_parts <- c(body_parts, rye_expr_to_r(expr[[i]], indent + 1))
        }
      }

      if (!is.null(rest_param)) {
        rest_binding <- paste0(rest_param, " <- list(...)")
        body_parts <- c(rest_binding, body_parts)
        if (!has_body) {
          body_parts <- c(body_parts, "NULL")
        }
      }

      args_str <- paste(arg_parts, collapse = ", ")
      body_str <- paste(body_parts, collapse = "\n")

      if (length(body_parts) == 0) {
        return(paste0("function(", args_str, ") NULL"))
      }
      if (length(body_parts) > 1 || !is.null(rest_param)) {
        indented_body <- paste0(
          paste(rep("  ", indent + 1), collapse = ""),
          gsub("\n", paste0("\n", paste(rep("  ", indent + 1), collapse = "")), body_str)
        )
        return(paste0(
          "function(", args_str, ") {\n",
          indented_body, "\n", indent_str, "}"
        ))
      }
      return(paste0("function(", args_str, ") ", body_str))
    }

    # Handle define
    if (is.symbol(op) && as.character(op) == "define") {
      if (length(expr) != 3) {
        stop("define requires exactly 2 arguments")
      }
      name <- as.character(expr[[2]])
      value <- rye_expr_to_r(expr[[3]], indent)
      return(paste0(name, " <- ", value))
    }

    # Handle if
    if (is.symbol(op) && as.character(op) == "if") {
      if (length(expr) < 3 || length(expr) > 4) {
        stop("if requires 2 or 3 arguments")
      }
      test <- rye_expr_to_r(expr[[2]], indent)
      then_expr <- rye_expr_to_r(expr[[3]], indent)

      if (length(expr) == 4) {
        else_expr <- rye_expr_to_r(expr[[4]], indent)
        return(paste0("if (", test, ") ", then_expr, " else ", else_expr))
      } else {
        return(paste0("if (", test, ") ", then_expr))
      }
    }

    # Handle begin
    if (is.symbol(op) && as.character(op) == "begin") {
      parts <- character(0)
      for (i in 2:length(expr)) {
        parts <- c(parts, rye_expr_to_r(expr[[i]], indent))
      }
      return(paste0("{\n", paste(paste0(indent_str, "  ", parts), collapse = "\n"), "\n", indent_str, "}"))
    }

    # Handle defmacro (translate to a comment since macros don't exist in R)
    if (is.symbol(op) && as.character(op) == "defmacro") {
      if (length(expr) < 4) {
        stop("defmacro requires at least 3 arguments")
      }
      name <- as.character(expr[[2]])
      return(paste0("# Macro: ", name, " (not translated)"))
    }

    # Handle :: and :::
    if (is.symbol(op) && (as.character(op) == "::" || as.character(op) == ":::")) {
      if (length(expr) != 3) {
        stop(paste0(as.character(op), " requires exactly 2 arguments"))
      }
      pkg <- rye_expr_to_r(expr[[2]], indent)
      name <- rye_expr_to_r(expr[[3]], indent)
      return(paste0(pkg, as.character(op), name))
    }

    # Handle formula (~)
    if (is.symbol(op) && as.character(op) == "~") {
      parts <- character(0)
      for (i in 2:length(expr)) {
        parts <- c(parts, rye_expr_to_r(expr[[i]], indent))
      }
      return(paste(c("~", parts), collapse = " "))
    }

    # Infix operators
    if (is.symbol(op) && as.character(op) %in% c(
      "+", "-", "*", "/", "%%", "%/%", "^",
      "<", ">", "<=", ">=", "==", "!=",
      "&&", "||", "&", "|", "!"
    )) {
      op_name <- as.character(op)
      args <- character(0)
      for (i in 2:length(expr)) {
        arg_expr <- expr[[i]]
        arg_text <- rye_expr_to_r(arg_expr, indent)
        if (is.call(arg_expr)) {
          arg_text <- paste0("(", arg_text, ")")
        }
        args <- c(args, arg_text)
      }
      if (length(args) == 0) {
        return(op_name)
      }
      if (length(args) == 1 && op_name %in% c("+", "-", "!")) {
        return(paste0(op_name, args[[1]]))
      }
      return(paste(args, collapse = paste0(" ", op_name, " ")))
    }

    # Regular function call
    fn_name <- rye_expr_to_r(op, indent)

    # Handle arguments (including keywords for named arguments)
    args <- character(0)
    i <- 2
    while (i <= length(expr)) {
      arg_expr <- expr[[i]]

      # Check if this is a keyword (for named arguments)
      if (inherits(arg_expr, "rye_keyword")) {
        if (i + 1 > length(expr)) {
          stop(sprintf("Keyword :%s requires a value", arg_expr))
        }
        keyword_name <- as.character(arg_expr)
        value <- rye_expr_to_r(expr[[i + 1]], indent)
        args <- c(args, paste0(keyword_name, " = ", value))
        i <- i + 2
      } else {
        value <- rye_expr_to_r(arg_expr, indent)
        args <- c(args, value)
        i <- i + 1
      }
    }

    args_str <- paste(args, collapse = ", ")
    return(paste0(fn_name, "(", args_str, ")"))
  }

  # Fallback
  as.character(expr)
}
# ```
