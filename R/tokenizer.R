#' Tokenize Rye source code
#'
#' @param source A string containing Rye source code
#' @return A list of tokens, each with type, value, line, and column
#' @export
rye_tokenize <- function(source) {
  tokens <- list()
  line <- 1
  col <- 1
  i <- 1
  n <- nchar(source)

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
      tokens[[length(tokens) + 1]] <- list(type = "LPAREN", value = "(", line = line, col = col)
      col <- col + 1
      i <- i + 1
      next
    }

    # Right paren
    if (char == ")") {
      tokens[[length(tokens) + 1]] <- list(type = "RPAREN", value = ")", line = line, col = col)
      col <- col + 1
      i <- i + 1
      next
    }

    # Quote
    if (char == "'") {
      tokens[[length(tokens) + 1]] <- list(type = "QUOTE", value = "'", line = line, col = col)
      col <- col + 1
      i <- i + 1
      next
    }

    # Quasiquote
    if (char == "`") {
      tokens[[length(tokens) + 1]] <- list(type = "QUASIQUOTE", value = "`", line = line, col = col)
      col <- col + 1
      i <- i + 1
      next
    }

    # Unquote/unquote-splicing
    if (char == ",") {
      if (i + 1 <= n && substr(source, i + 1, i + 1) == "@") {
        tokens[[length(tokens) + 1]] <- list(type = "UNQUOTE_SPLICING", value = ",@", line = line, col = col)
        col <- col + 2
        i <- i + 2
      } else {
        tokens[[length(tokens) + 1]] <- list(type = "UNQUOTE", value = ",", line = line, col = col)
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
      str_chars <- character(0)

      while (i <= n && substr(source, i, i) != '"') {
        ch <- substr(source, i, i)
        if (ch == "\\") {
          # Handle escape sequences
          if (i + 1 <= n) {
            next_ch <- substr(source, i + 1, i + 1)
            if (next_ch == "n") {
              str_chars <- c(str_chars, "\n")
            } else if (next_ch == "t") {
              str_chars <- c(str_chars, "\t")
            } else if (next_ch == "r") {
              str_chars <- c(str_chars, "\r")
            } else if (next_ch == '"') {
              str_chars <- c(str_chars, '"')
            } else if (next_ch == "\\") {
              str_chars <- c(str_chars, "\\")
            } else {
              # Preserve unknown escapes as literal backslash + char
              str_chars <- c(str_chars, "\\", next_ch)
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
          str_chars <- c(str_chars, ch)
          i <- i + 1
        }
      }

      if (i > n) {
        stop(sprintf("Unterminated string at line %d, column %d", line, start_col))
      }

      tokens[[length(tokens) + 1]] <- list(
        type = "STRING",
        value = paste(str_chars, collapse = ""),
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
      delimiters <- c(" ", "\t", "\n", "\r", "(", ")", "'", "`", ",", ";", "@", '"', ":")
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
    delimiters <- c(" ", "\t", "\n", "\r", "(", ")", "'", "`", ",", ";", "@", '"', ":")
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

      value <- substr(source, start, i - 1)

      # Determine if it's a number or symbol
      if (grepl("^[+-]?[0-9]+\\.?[0-9]*$", value) || grepl("^[+-]?\\.[0-9]+$", value)) {
        tokens[[length(tokens) + 1]] <- list(type = "NUMBER", value = as.numeric(value), line = line, col = start_col)
      } else if (value == "#t" || value == "#T") {
        tokens[[length(tokens) + 1]] <- list(type = "BOOLEAN", value = TRUE, line = line, col = start_col)
      } else if (value == "#f" || value == "#F") {
        tokens[[length(tokens) + 1]] <- list(type = "BOOLEAN", value = FALSE, line = line, col = start_col)
      } else if (value == "#nil") {
        tokens[[length(tokens) + 1]] <- list(type = "NIL", value = NULL, line = line, col = start_col)
      } else {
        tokens[[length(tokens) + 1]] <- list(type = "SYMBOL", value = value, line = line, col = start_col)
      }
      next
    }

    stop(sprintf("Unexpected character '%s' at line %d, column %d", char, line, col))
  }

  tokens
}
