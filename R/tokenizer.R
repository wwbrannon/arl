#' Tokenizer for Rye source code
#'
#' @keywords internal
#' @noRd
Tokenizer <- R6::R6Class(
  "Tokenizer",
  public = list(
    delimiters = NULL,
    initialize = function(delimiters = NULL) {
      if (is.null(delimiters)) {
        delimiters <- c(" ", "\t", "\n", "\r", "(", ")", "'", "`", ",", ";", "@", '"', ":")
      }
      self$delimiters <- delimiters
    },
    tokenize = function(source) {
      tokens <- list()
      line <- 1
      col <- 1
      i <- 1
      n <- nchar(source)
      delimiters <- self$delimiters
      add_token <- function(type, value, line_pos = line, col_pos = col) {
        tokens[[length(tokens) + 1]] <<- list(type = type, value = value, line = line_pos, col = col_pos)
      }

      while (i <= n) {
        char <- substr(source, i, i)

        # Skip whitespace
        if (char %in% c(" ", "\t", "\r")) {
          col <- col + 1
          i <- i + 1
          next
        }

        # Handle newlines
        if (char == "\n") {
          line <- line + 1
          col <- 1
          i <- i + 1
          next
        }

        # Skip comments (semicolon to end of line)
        if (char == ";") {
          while (i <= n && substr(source, i, i) != "\n") {
            i <- i + 1
          }
          next
        }

        # Left paren
        if (char == "(") {
          add_token("LPAREN", "(")
          col <- col + 1
          i <- i + 1
          next
        }

        # Right paren
        if (char == ")") {
          add_token("RPAREN", ")")
          col <- col + 1
          i <- i + 1
          next
        }

        # Quote
        if (char == "'") {
          add_token("QUOTE", "'")
          col <- col + 1
          i <- i + 1
          next
        }

        # Quasiquote
        if (char == "`") {
          add_token("QUASIQUOTE", "`")
          col <- col + 1
          i <- i + 1
          next
        }

        # Unquote/unquote-splicing
        if (char == ",") {
          if (i + 1 <= n && substr(source, i + 1, i + 1) == "@") {
            add_token("UNQUOTE_SPLICING", ",@")
            col <- col + 2
            i <- i + 2
          } else {
            add_token("UNQUOTE", ",")
            col <- col + 1
            i <- i + 1
          }
          next
        }

        # String literals
        if (char == '"') {
          start_col <- col
          i <- i + 1
          col <- col + 1
          str_chars <- list()
          str_idx <- 1

          while (i <= n && substr(source, i, i) != '"') {
            ch <- substr(source, i, i)
            if (ch == "\\") {
              # Handle escape sequences
              if (i + 1 <= n) {
                next_ch <- substr(source, i + 1, i + 1)
                if (next_ch == "n") {
                  str_chars[[str_idx]] <- "\n"
                  str_idx <- str_idx + 1
                } else if (next_ch == "t") {
                  str_chars[[str_idx]] <- "\t"
                  str_idx <- str_idx + 1
                } else if (next_ch == "r") {
                  str_chars[[str_idx]] <- "\r"
                  str_idx <- str_idx + 1
                } else if (next_ch == '"') {
                  str_chars[[str_idx]] <- '"'
                  str_idx <- str_idx + 1
                } else if (next_ch == "\\") {
                  str_chars[[str_idx]] <- "\\"
                  str_idx <- str_idx + 1
                } else {
                  # Preserve unknown escapes as literal backslash + char
                  str_chars[[str_idx]] <- "\\"
                  str_chars[[str_idx + 1]] <- next_ch
                  str_idx <- str_idx + 2
                }
                i <- i + 2
                col <- col + 2
              } else {
                stop(sprintf("Unterminated string at line %d, column %d", line, start_col))
              }
            } else {
              if (ch == "\n") {
                line <- line + 1
                col <- 1
              } else {
                col <- col + 1
              }
              str_chars[[str_idx]] <- ch
              str_idx <- str_idx + 1
              i <- i + 1
            }
          }

          if (i > n) {
            stop(sprintf("Unterminated string at line %d, column %d", line, start_col))
          }

          tokens[[length(tokens) + 1]] <- list(
            type = "STRING",
            value = paste(unlist(str_chars), collapse = ""),
            line = line,
            col = start_col
          )
          i <- i + 1  # Skip closing quote
          col <- col + 1
          next
        }

        # Keywords (colon-prefixed symbols like :key) or :: and ::: operators
        if (char == ":") {
          start_col <- col
          start <- i

          # Check if this is :: or ::: (package accessor operators)
          if (i + 1 <= n && substr(source, i + 1, i + 1) == ":") {
            # It's :: or :::, treat as a symbol
            i <- i + 2
            col <- col + 2
            # Check for third colon
            if (i <= n && substr(source, i, i) == ":") {
              i <- i + 1
              col <- col + 1
              tokens[[length(tokens) + 1]] <- list(type = "SYMBOL", value = ":::", line = line, col = start_col)
            } else {
              tokens[[length(tokens) + 1]] <- list(type = "SYMBOL", value = "::", line = line, col = start_col)
            }
            next
          }

          # It's a keyword
          i <- i + 1  # Skip the colon
          col <- col + 1

          # Read the keyword name
          while (i <= n && !substr(source, i, i) %in% delimiters) {
            i <- i + 1
            col <- col + 1
          }

          if (i == start + 1) {
            stop(sprintf("Empty keyword at line %d, column %d", line, start_col))
          }

          keyword_name <- substr(source, start + 1, i - 1)
          tokens[[length(tokens) + 1]] <- list(type = "KEYWORD", value = keyword_name, line = line, col = start_col)
          next
        }

        # Numbers and symbols (atoms)
        # Check if this is the start of an atom (not a delimiter)
        if (!char %in% delimiters) {
          start_col <- col
          start <- i

          # Read until we hit a delimiter, but handle :: and ::: specially
          while (i <= n) {
            curr_char <- substr(source, i, i)

            # Check for :: or ::: within the symbol
            if (curr_char == ":") {
              # Look ahead to see if this is :: or :::
              if (i + 1 <= n && substr(source, i + 1, i + 1) == ":") {
                # It's at least ::, include it
                i <- i + 2
                col <- col + 2
                # Check for third colon (:::)
                if (i <= n && substr(source, i, i) == ":") {
                  i <- i + 1
                  col <- col + 1
                }
                # Continue reading the rest of the symbol
                next
              } else {
                # Single colon - treat as delimiter
                break
              }
            }

            # Check for other delimiters
            if (curr_char %in% delimiters) {
              break
            }

            i <- i + 1
            col <- col + 1
          }

          token_text <- substr(source, start, i - 1)
          token_type <- "SYMBOL"
          token_value <- token_text

          # Try to parse as number
          if (grepl("^[-+]?\\d+(\\.\\d+)?$", token_text)) {
            token_type <- "NUMBER"
            token_value <- as.numeric(token_text)
          }

          # Booleans
          if (token_text == "#t" || token_text == "#f") {
            token_type <- "BOOLEAN"
            token_value <- token_text == "#t"
          }

          # nil
          if (token_text == "#nil") {
            token_type <- "NIL"
            token_value <- NULL
          }

          tokens[[length(tokens) + 1]] <- list(
            type = token_type,
            value = token_value,
            line = line,
            col = start_col
          )
          next
        }

        stop(sprintf("Unexpected character: '%s' at line %d, column %d", char, line, col))
      }

      tokens
    }
  )
)
