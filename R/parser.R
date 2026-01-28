#' Parse Rye tokens into R data structures
#'
#' Produces R calls and atomic values that represent the Rye AST. Quote and
#' quasiquote sugar are expanded into explicit `(quote ...)` and
#' `(quasiquote ...)` forms during parsing.
#'
#' @param tokens A list of tokens from rye_tokenize
#' @param source_name Optional character string naming the source (e.g., filename) for error messages and source tracking. Defaults to NULL.
#' @return A list of parsed expressions as R calls
#' @examples
#' tokens <- rye_tokenize("(+ 1 2)")
#' rye_parse(tokens)
#' @export
rye_parse <- function(tokens, source_name = NULL) {
  pos <- 1
  expressions <- list()

  parse_expr <- function() {
    if (pos > length(tokens)) {
      stop("Unexpected end of input")
    }

    token <- tokens[[pos]]

    make_src <- function(start_token, end_src = NULL) {
      end_line <- start_token$line
      end_col <- start_token$col
      if (!is.null(end_src)) {
        if (!is.null(end_src$end_line)) {
          end_line <- end_src$end_line
        }
        if (!is.null(end_src$end_col)) {
          end_col <- end_src$end_col
        }
      }
      rye_src_new(source_name, start_token$line, start_token$col, end_line, end_col)
    }

    # Quote sugar
    if (token$type == "QUOTE") {
      pos <<- pos + 1
      quoted <- parse_expr()
      expr <- as.call(list(as.symbol("quote"), quoted))
      return(rye_src_set(expr, make_src(token, rye_src_get(quoted))))
    }

    # Quasiquote sugar
    if (token$type == "QUASIQUOTE") {
      pos <<- pos + 1
      quoted <- parse_expr()
      expr <- as.call(list(as.symbol("quasiquote"), quoted))
      return(rye_src_set(expr, make_src(token, rye_src_get(quoted))))
    }

    # Unquote sugar
    if (token$type == "UNQUOTE") {
      pos <<- pos + 1
      unquoted <- parse_expr()
      expr <- as.call(list(as.symbol("unquote"), unquoted))
      return(rye_src_set(expr, make_src(token, rye_src_get(unquoted))))
    }

    # Unquote-splicing sugar
    if (token$type == "UNQUOTE_SPLICING") {
      pos <<- pos + 1
      unquoted <- parse_expr()
      expr <- as.call(list(as.symbol("unquote-splicing"), unquoted))
      return(rye_src_set(expr, make_src(token, rye_src_get(unquoted))))
    }

    # Atoms
    if (token$type == "NUMBER") {
      pos <<- pos + 1
      return(rye_src_set(token$value, make_src(token)))
    }

    if (token$type == "STRING") {
      pos <<- pos + 1
      return(rye_src_set(token$value, make_src(token)))
    }

    if (token$type == "BOOLEAN") {
      pos <<- pos + 1
      return(rye_src_set(token$value, make_src(token)))
    }

    if (token$type == "NIL") {
      pos <<- pos + 1
      return(NULL)
    }

    if (token$type == "SYMBOL") {
      pos <<- pos + 1

      # Handle :: and ::: syntactic sugar
      # Check for ::: first (3 colons) as it's more specific
      if (grepl(":::", token$value, fixed = TRUE)) {
        parts <- strsplit(token$value, ":::", fixed = TRUE)[[1]]
        if (length(parts) == 2 && nzchar(parts[1]) && nzchar(parts[2])) {
          # Convert pkg:::name to (::: pkg name)
          expr <- as.call(list(as.symbol(":::"), as.symbol(parts[1]), as.symbol(parts[2])))
          return(rye_src_set(expr, make_src(token)))
        }
        # Invalid format, fall through to return as-is
      } else if (grepl("::", token$value, fixed = TRUE)) {
        parts <- strsplit(token$value, "::", fixed = TRUE)[[1]]
        if (length(parts) == 2 && nzchar(parts[1]) && nzchar(parts[2])) {
          # Convert pkg::name to (:: pkg name)
          expr <- as.call(list(as.symbol("::"), as.symbol(parts[1]), as.symbol(parts[2])))
          return(rye_src_set(expr, make_src(token)))
        }
        # Invalid format, fall through to return as-is
      }

      return(rye_src_set(as.symbol(token$value), make_src(token)))
    }

    if (token$type == "KEYWORD") {
      pos <<- pos + 1
      # Store keywords as a special structure
      # We'll use this in the evaluator to convert to named arguments
      keyword <- structure(token$value, class = "rye_keyword")
      return(rye_src_set(keyword, make_src(token)))
    }

    # Lists (S-expressions)
    if (token$type == "LPAREN") {
      pos <<- pos + 1
      start_token <- token
      # Use c() with list() to preserve NULL values (elements[[i]] <- NULL doesn't work)
      # But collect in chunks to avoid O(n^2) behavior
      elements <- list()
      chunk <- vector("list", 32)  # Collect in chunks of 32
      chunk_idx <- 1

      while (pos <= length(tokens) && tokens[[pos]]$type != "RPAREN") {
        elem <- parse_expr()
        chunk[[chunk_idx]] <- elem
        chunk_idx <- chunk_idx + 1

        # Flush chunk when full
        if (chunk_idx > 32) {
          elements <- c(elements, chunk)
          chunk <- vector("list", 32)
          chunk_idx <- 1
        }
      }

      # Flush remaining chunk
      if (chunk_idx > 1) {
        elements <- c(elements, chunk[1:(chunk_idx - 1)])
      }

      if (pos > length(tokens)) {
        stop(sprintf("Unclosed parenthesis at line %d, column %d", token$line, token$col))
      }

      end_token <- tokens[[pos]]
      pos <<- pos + 1  # Skip RPAREN

      # Empty list
      if (length(elements) == 0) {
        return(rye_src_set(list(), rye_src_new(source_name, start_token$line, start_token$col, end_token$line, end_token$col)))
      }

      # Convert to R call
      expr <- as.call(elements)
      return(rye_src_set(expr, rye_src_new(source_name, start_token$line, start_token$col, end_token$line, end_token$col)))
    }

    if (token$type == "RPAREN") {
      stop(sprintf("Unexpected ')' at line %d, column %d", token$line, token$col))
    }

    stop(sprintf("Unexpected token type: %s", token$type))
  }

  while (pos <= length(tokens)) {
    expr <- parse_expr()
    expressions <- c(expressions, list(expr))
  }

  expressions
}

#' Read Rye source code into parsed expressions
#'
#' Convenience helper that combines tokenization and parsing.
#'
#' @param source A string containing Rye source code
#' @param source_name Optional character string naming the source (e.g., filename) for error messages and source tracking. Defaults to NULL.
#' @return A list of parsed expressions
#' @examples
#' rye_read("(define x 10) (+ x 5)")
#' @export
rye_read <- function(source, source_name = NULL) {
  tokens <- rye_tokenize(source)
  rye_parse(tokens, source_name = source_name)
}
