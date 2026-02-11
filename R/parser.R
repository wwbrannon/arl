# Parser: Converts token lists from Tokenizer into Rye S-expressions (R calls). Expands
# quote/quasiquote sugar into explicit forms. Uses source_tracker for source locations.
#
# @field source_tracker SourceTracker for attaching source info to expressions.
#
#' @keywords internal
#' @noRd
Parser <- R6::R6Class(
  "Parser",
  public = list(
    source_tracker = NULL,
    # @description Create parser with optional source tracker.
    # @param source_tracker Optional SourceTracker; used for src on parsed expressions.
    initialize = function(source_tracker = NULL) {
      self$source_tracker <- source_tracker
    },
    # @description Parse a list of tokens into a list of Rye expressions (R calls/symbols/atoms).
    # @param tokens List of tokens from Tokenizer$tokenize().
    # @param source_name Optional name for error reporting and source attachment.
    # @return List of expressions.
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
          # Use a sentinel symbol so #nil survives in (as.call elements); NULL would be dropped.
          return(tracker$src_set(as.symbol(".rye_nil"), make_src(token)))
        }

        if (token$type == "NA") {
          pos <<- pos + 1
          return(tracker$src_set(token$value, make_src(token)))
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
          # We'll use this in the compiler/runtime to convert to named arguments
          keyword <- structure(token$value, class = "rye_keyword")
          return(tracker$src_set(keyword, make_src(token)))
        }

        # Standalone dot: treat as symbol . (e.g. lambda (. args) rest parameter)
        if (token$type == "DOT") {
          pos <<- pos + 1
          return(tracker$src_set(as.symbol("."), make_src(token)))
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
          seen_dot <- FALSE
          dot_cdr <- NULL

          dotted_heads <- NULL

          while (pos <= length(tokens) && tokens[[pos]]$type != "RPAREN") {
            elem <- parse_expr()
            # After first element(s), DOT means dotted pair: (a . b) or (a b . c)
            if (pos <= length(tokens) && tokens[[pos]]$type == "DOT") {
              # (a .) with no cdr: parse as list (a .) so lambda/macro can error on invalid formals
              if (pos + 1 <= length(tokens) && tokens[[pos + 1]]$type == "RPAREN") {
                chunk[[chunk_idx]] <- elem
                chunk_idx <- chunk_idx + 1
                next
              }
              pos <<- pos + 1  # consume DOT
              if (pos > length(tokens) || tokens[[pos]]$type == "RPAREN") {
                tok <- tokens[[pos - 1]]
                stop(sprintf("Missing cdr after '.' at line %d, column %d", tok$line, tok$col))
              }
              dot_cdr <- parse_expr()
              if (chunk_idx > 1) {
                elements <- c(elements, chunk[1:(chunk_idx - 1)])
              }
              dotted_heads <- c(elements, list(elem))
              seen_dot <- TRUE
              break
            }
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

          span <- tracker$src_new(
            source_name, start_token$line, start_token$col,
            end_token$line, end_token$col)

          # Dotted pair: (a . b) or (a b . c)
          if (seen_dot && !is.null(dot_cdr) && length(dotted_heads) > 0) {
            result <- dot_cdr
            for (j in rev(seq_along(dotted_heads))) {
              result <- Cons$new(dotted_heads[[j]], result)
            }
            return(tracker$src_set(result, span))
          }

          # Empty list
          if (length(elements) == 0) {
            return(tracker$src_set(list(), span))
          }

          # Convert to R call
          expr <- as.call(elements)
          return(tracker$src_set(expr, span))
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
    },

    # @description Convert a Rye expression back to its string representation.
    # Inverse of parse(): the output can be parsed back with read().
    # @param expr A Rye expression (symbol, call, atomic, Cons, keyword, etc.).
    # @return Character string.
    write = function(expr) {
      # Strip source tracking attribute so deparse() is clean
      attr(expr, "rye_src") <- NULL

      # NULL -> #nil
      if (is.null(expr)) return("#nil")

      # Cons (dotted pair) -> (a b . c)
      if (r6_isinstance(expr, "Cons")) {
        p <- expr$parts()
        elems <- vapply(p$prefix, function(e) self$write(e), character(1))
        tail_str <- self$write(p$tail)
        return(paste0("(", paste(elems, collapse = " "), " . ", tail_str, ")"))
      }

      # Keyword -> :foo
      if (inherits(expr, "rye_keyword")) {
        return(paste0(":", as.character(expr)))
      }

      # Symbol
      if (is.symbol(expr)) {
        name <- as.character(expr)
        if (name == ".rye_nil") return("#nil")
        return(name)
      }

      # Logical (#t, #f, NA)
      if (is.logical(expr) && length(expr) == 1L) {
        if (is.na(expr)) return("NA")
        return(if (expr) "#t" else "#f")
      }

      # String (including NA_character_)
      if (is.character(expr) && length(expr) == 1L) {
        if (is.na(expr)) return("NA_character_")
        return(paste0('"', .rye_unescape_string(expr), '"'))
      }

      # Integer (including NA_integer_)
      if (is.integer(expr) && length(expr) == 1L) {
        if (is.na(expr)) return("NA_integer_")
        return(paste0(as.character(expr), "L"))
      }

      # Double (NaN, Inf, -Inf, NA_real_, regular)
      if (is.double(expr) && length(expr) == 1L) {
        if (is.nan(expr)) return("NaN")
        if (is.infinite(expr)) return(if (expr > 0) "Inf" else "-Inf")
        if (is.na(expr)) return("NA_real_")
        return(deparse(expr))
      }

      # Complex (including NA_complex_)
      if (is.complex(expr) && length(expr) == 1L) {
        if (is.na(expr)) return("NA_complex_")
        return(deparse(expr))
      }

      # Empty list -> ()
      if (is.list(expr) && length(expr) == 0L) {
        return("()")
      }

      # Call (S-expression)
      if (is.call(expr)) {
        if (is.symbol(expr[[1L]])) {
          head_name <- as.character(expr[[1L]])

          # Sugar: (quote x) -> 'x, etc. (2-element forms)
          if (length(expr) == 2L) {
            sugar <- switch(head_name,
              "quote" = "'",
              "quasiquote" = "`",
              "unquote" = ",",
              "unquote-splicing" = ",@",
              NULL
            )
            if (!is.null(sugar)) {
              return(paste0(sugar, self$write(expr[[2L]])))
            }
          }

          # Sugar: (:: pkg name) -> pkg::name, (::: pkg name) -> pkg:::name
          if (head_name %in% c("::", ":::") && length(expr) == 3L &&
              is.symbol(expr[[2L]]) && is.symbol(expr[[3L]])) {
            return(paste0(
              as.character(expr[[2L]]), head_name, as.character(expr[[3L]])
            ))
          }
        }

        # General call: (f a b ...)
        elems <- vapply(
          seq_along(expr),
          function(i) self$write(expr[[i]]),
          character(1)
        )
        return(paste0("(", paste(elems, collapse = " "), ")"))
      }

      stop(sprintf("write: cannot serialize object of class %s",
                   paste(class(expr), collapse = "/")), call. = FALSE)
    }
  )
)
