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
      arg_names <- character(0)
      if (!is.null(args_expr) && is.call(args_expr) && length(args_expr) > 0) {
        for (i in seq_along(args_expr)) {
          arg_names <- c(arg_names, as.character(args_expr[[i]]))
        }
      }

      # Get body
      body_parts <- character(0)
      if (length(expr) >= 3) {
        for (i in 3:length(expr)) {
          body_parts <- c(body_parts, rye_expr_to_r(expr[[i]], indent + 1))
        }
      }

      args_str <- paste(arg_names, collapse = ", ")
      body_str <- paste(body_parts, collapse = "\n")

      if (length(body_parts) == 0) {
        return(paste0("function(", args_str, ") NULL"))
      }
      if (length(body_parts) > 1) {
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

#' Translate Rye code to R code
#'
#' Translates Rye source into equivalent R code without evaluating it.
#'
#' @param source Either a file path to a .rye file or a string containing Rye code
#' @param is_file Logical indicating whether source is a file path (default: TRUE if source ends with .rye)
#' @return A character string containing the translated R code
#' @examples
#' rye_translate("(+ 1 2)", is_file = FALSE)
#' #> [1] "1 + 2"
#' @export
rye_translate <- function(source, is_file = NULL) {
  # Auto-detect if source is a file path
  if (is.null(is_file)) {
    is_file <- grepl("\\.rye$", source) && file.exists(source)
  }

  # Read source
  if (is_file) {
    if (!file.exists(source)) {
      stop(sprintf("cannot open file '%s'", source))
    }
    text <- paste(readLines(source, warn = FALSE), collapse = "\n")
  } else {
    text <- source
  }

  # Parse to AST
  exprs <- rye_read(text)

  # Translate each expression
  r_code_parts <- character(0)
  for (expr in exprs) {
    r_code_parts <- c(r_code_parts, rye_expr_to_r(expr, indent = 0))
  }

  # Join with newlines
  paste(r_code_parts, collapse = "\n\n")
}
