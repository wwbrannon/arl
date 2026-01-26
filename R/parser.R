#' Parse Rye tokens into R data structures
#'
#' @param tokens A list of tokens from rye_tokenize
#' @return A list of parsed expressions as R calls
#' @export
rye_parse <- function(tokens) {
  pos <- 1
  expressions <- list()

  parse_expr <- function() {
    if (pos > length(tokens)) {
      stop("Unexpected end of input")
    }

    token <- tokens[[pos]]

    # Quote sugar
    if (token$type == "QUOTE") {
      pos <<- pos + 1
      quoted <- parse_expr()
      return(as.call(list(as.symbol("quote"), quoted)))
    }

    # Quasiquote sugar
    if (token$type == "QUASIQUOTE") {
      pos <<- pos + 1
      quoted <- parse_expr()
      return(as.call(list(as.symbol("quasiquote"), quoted)))
    }

    # Unquote sugar
    if (token$type == "UNQUOTE") {
      pos <<- pos + 1
      unquoted <- parse_expr()
      return(as.call(list(as.symbol("unquote"), unquoted)))
    }

    # Unquote-splicing sugar
    if (token$type == "UNQUOTE_SPLICING") {
      pos <<- pos + 1
      unquoted <- parse_expr()
      return(as.call(list(as.symbol("unquote-splicing"), unquoted)))
    }

    # Atoms
    if (token$type == "NUMBER") {
      pos <<- pos + 1
      return(token$value)
    }

    if (token$type == "STRING") {
      pos <<- pos + 1
      return(token$value)
    }

    if (token$type == "BOOLEAN") {
      pos <<- pos + 1
      return(token$value)
    }

    if (token$type == "NIL") {
      pos <<- pos + 1
      return(NULL)
    }

    if (token$type == "SYMBOL") {
      pos <<- pos + 1
      return(as.symbol(token$value))
    }

    if (token$type == "KEYWORD") {
      pos <<- pos + 1
      # Store keywords as a special structure
      # We'll use this in the evaluator to convert to named arguments
      return(structure(token$value, class = "rye_keyword"))
    }

    # Lists (S-expressions)
    if (token$type == "LPAREN") {
      pos <<- pos + 1
      elements <- list()

      while (pos <= length(tokens) && tokens[[pos]]$type != "RPAREN") {
        # Use c() with list() to preserve NULL values
        elem <- parse_expr()
        elements <- c(elements, list(elem))
      }

      if (pos > length(tokens)) {
        stop(sprintf("Unclosed parenthesis at line %d, column %d", token$line, token$col))
      }

      pos <<- pos + 1  # Skip RPAREN

      # Empty list
      if (length(elements) == 0) {
        return(list())
      }

      # Convert to R call
      return(as.call(elements))
    }

    if (token$type == "RPAREN") {
      stop(sprintf("Unexpected ')' at line %d, column %d", token$line, token$col))
    }

    stop(sprintf("Unexpected token type: %s", token$type))
  }

  while (pos <= length(tokens)) {
    expressions[[length(expressions) + 1]] <- parse_expr()
  }

  expressions
}

#' Read Rye source code into parsed expressions
#'
#' @param source A string containing Rye source code
#' @return A list of parsed expressions
#' @export
rye_read <- function(source) {
  tokens <- rye_tokenize(source)
  rye_parse(tokens)
}
