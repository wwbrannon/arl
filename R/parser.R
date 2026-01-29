#' Parser for Rye tokens
#'
#' @keywords internal
#' @noRd
Parser <- R6::R6Class(
  "Parser",
  public = list(
    source_tracker = NULL,
    initialize = function(source_tracker = NULL) {
      self$source_tracker <- source_tracker
    },
    parse = function(tokens, source_name = NULL) {
      pos <- 1
      expressions <- list()
      tracker <- self$source_tracker
      if (is.null(tracker)) {
        tracker <- SourceTracker$new()
      }

      parse_expr <- function() {
        if (pos > length(tokens)) {
          stop("Unexpected end of input")
        }

        token <- tokens[[pos]]
        sugar_map <- list(
          QUOTE = "quote",
          QUASIQUOTE = "quasiquote",
          UNQUOTE = "unquote",
          UNQUOTE_SPLICING = "unquote-splicing"
        )

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
          tracker$src_new(source_name, start_token$line, start_token$col, end_line, end_col)
        }

        # Quote/quasiquote/unquote sugar
        if (token$type %in% names(sugar_map)) {
          pos <<- pos + 1
          quoted <- parse_expr()
          op <- sugar_map[[token$type]]
          expr <- as.call(list(as.symbol(op), quoted))
          return(tracker$src_set(expr, make_src(token, tracker$src_get(quoted))))
        }

        # Atoms
        if (token$type == "NUMBER") {
          pos <<- pos + 1
          return(tracker$src_set(token$value, make_src(token)))
        }

        if (token$type == "STRING") {
          pos <<- pos + 1
          return(tracker$src_set(token$value, make_src(token)))
        }

        if (token$type == "BOOLEAN") {
          pos <<- pos + 1
          return(tracker$src_set(token$value, make_src(token)))
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
              return(tracker$src_set(expr, make_src(token)))
            }
            # Invalid format, fall through to return as-is
          } else if (grepl("::", token$value, fixed = TRUE)) {
            parts <- strsplit(token$value, "::", fixed = TRUE)[[1]]
            if (length(parts) == 2 && nzchar(parts[1]) && nzchar(parts[2])) {
              # Convert pkg::name to (:: pkg name)
              expr <- as.call(list(as.symbol("::"), as.symbol(parts[1]), as.symbol(parts[2])))
              return(tracker$src_set(expr, make_src(token)))
            }
            # Invalid format, fall through to return as-is
          }

          return(tracker$src_set(as.symbol(token$value), make_src(token)))
        }

        if (token$type == "KEYWORD") {
          pos <<- pos + 1
          # Store keywords as a special structure
          # We'll use this in the evaluator to convert to named arguments
          keyword <- structure(token$value, class = "rye_keyword")
          return(tracker$src_set(keyword, make_src(token)))
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
            return(tracker$src_set(list(), tracker$src_new(source_name, start_token$line, start_token$col, end_token$line, end_token$col)))
          }

          # Convert to R call
          expr <- as.call(elements)
          return(tracker$src_set(expr, tracker$src_new(source_name, start_token$line, start_token$col, end_token$line, end_token$col)))
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
  )
)
