# Tokenizer: Lexes Rye source code into tokens (LPAREN, RPAREN, SYMBOL, NUMBER, STRING,
# QUOTE, QUASIQUOTE, etc.) with line/col. Delimiters control what counts as a token boundary.
#
# @field delimiters Character vector of single-character delimiters.
#
#' @keywords internal
#' @noRd
Tokenizer <- R6::R6Class(
  "Tokenizer",
  public = list(
    delimiters = NULL,
    # @description Create tokenizer with default or custom delimiters.
    # @param delimiters Optional character vector; default includes space, parens, quote, comma, etc.
    initialize = function(delimiters = NULL) {
      if (is.null(delimiters)) {
        delimiters <- c(" ", "\t", "\n", "\r", "(", ")", "'", "`", ",", ";", "@", '"', ":")
      }
      self$delimiters <- delimiters
    },
    # @description Lex source string into a list of tokens (each list(type, value, line, col)).
    # @param source Character string of Rye source code.
    # @return List of token lists.
    tokenize = function(source) {
      tokens <- list()
      line <- 1
      col <- 1
      i <- 1
      chars <- strsplit(source, "")[[1]]
      n <- length(chars)
      delimiters <- self$delimiters
      # O(1) delimiter lookup via environment
      is_delim <- new.env(parent = emptyenv())
      for (d in delimiters) assign(d, TRUE, envir = is_delim)
      add_token <- function(type, value, line_pos = line, col_pos = col) {
        tokens[[length(tokens) + 1]] <<- list(type = type, value = value, line = line_pos, col = col_pos)
      }

      while (i <= n) {
        char <- chars[i]

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
          while (i <= n && chars[i] != "\n") {
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
          if (i + 1 <= n && chars[i + 1L] == "@") {
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

        # Standalone dot (dotted-pair notation): emit DOT only when . is alone
        if (char == ".") {
          next_char <- if (i + 1L <= n) chars[i + 1L] else ""
          if (next_char == "" || next_char %in% delimiters) {
            add_token("DOT", ".")
            col <- col + 1
            i <- i + 1
            next
          }
        }

        # String literals
        if (char == '"') {
          start_col <- col
          i <- i + 1
          col <- col + 1
          str_chars <- list()
          str_idx <- 1

          while (i <= n && chars[i] != '"') {
            ch <- chars[i]
            if (ch == "\\") {
              # Handle escape sequences
              if (i + 1 <= n) {
                next_ch <- chars[i + 1L]
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
          if (i + 1L <= n && chars[i + 1L] == ":") {
            # It's :: or :::, treat as a symbol
            i <- i + 2
            col <- col + 2
            # Check for third colon
            if (i <= n && chars[i] == ":") {
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
          while (i <= n && !exists(chars[i], envir = is_delim, inherits = FALSE)) {
            i <- i + 1
            col <- col + 1
          }

          if (i == start + 1) {
            stop(sprintf("Empty keyword at line %d, column %d", line, start_col))
          }

          keyword_name <- paste(chars[(start + 1L):(i - 1L)], collapse = "")
          tokens[[length(tokens) + 1]] <- list(type = "KEYWORD", value = keyword_name, line = line, col = start_col)
          next
        }

        # Numbers and symbols (atoms)
        # Check if this is the start of an atom (not a delimiter)
        if (!exists(char, envir = is_delim, inherits = FALSE)) {
          start_col <- col
          start <- i

          # Read until we hit a delimiter, but handle :: and ::: specially
          while (i <= n) {
            curr_char <- chars[i]

            # Check for :: or ::: within the symbol
            if (curr_char == ":") {
              # Look ahead to see if this is :: or :::
              if (i + 1L <= n && chars[i + 1L] == ":") {
                # It's at least ::, include it
                i <- i + 2
                col <- col + 2
                # Check for third colon (:::)
                if (i <= n && chars[i] == ":") {
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
            if (exists(curr_char, envir = is_delim, inherits = FALSE)) {
              break
            }

            i <- i + 1
            col <- col + 1
          }

          token_text <- paste(chars[start:(i - 1L)], collapse = "")
          token_type <- "SYMBOL"
          token_value <- token_text

          # Try to parse as integer literal (e.g., 4L, 42L)
          if (grepl("^[-+]?\\d+L$", token_text)) {
            token_type <- "NUMBER"
            # Remove the L suffix and convert to integer
            num_str <- substr(token_text, 1, nchar(token_text) - 1)
            token_value <- as.integer(num_str)
          }
          # Try to parse as complex number with real and imaginary parts (e.g., 2+4i, 3.14-2.5i)
          else if (grepl("^[-+]?\\d+(\\.\\d+)?([eE][-+]?\\d+)?[+-]\\d+(\\.\\d+)?([eE][-+]?\\d+)?i$", token_text)) {
            token_type <- "NUMBER"
            # Find the position of the +/- that separates real and imaginary parts
            # Need to skip the leading sign if present
            start_pos <- 1
            if (substr(token_text, 1, 1) %in% c("+", "-")) {
              start_pos <- 2
            }
            # Find the +/- separator (must be after the first character)
            sep_pos <- NULL
            for (j in start_pos:nchar(token_text)) {
              if (substr(token_text, j, j) %in% c("+", "-")) {
                sep_pos <- j
                break
              }
            }
            # Extract real and imaginary parts
            real_str <- substr(token_text, 1, sep_pos - 1)
            # Imaginary part includes the sign but not the 'i'
            imag_str <- substr(token_text, sep_pos, nchar(token_text) - 1)
            token_value <- complex(real = as.numeric(real_str), imaginary = as.numeric(imag_str))
          }
          # Try to parse as pure imaginary number (e.g., 4i, 3.14i)
          else if (grepl("^[-+]?\\d+(\\.\\d+)?([eE][-+]?\\d+)?i$", token_text)) {
            token_type <- "NUMBER"
            # Remove the i suffix and convert to complex
            num_str <- substr(token_text, 1, nchar(token_text) - 1)
            # Create complex number with imaginary part
            token_value <- complex(real = 0, imaginary = as.numeric(num_str))
          }
          # Try to parse as regular number (including scientific notation)
          else if (grepl("^[-+]?\\d+(\\.\\d+)?([eE][-+]?\\d+)?$", token_text)) {
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

          # R special constants (reserved words that are literals)
          if (token_text == "Inf") {
            token_type <- "NUMBER"
            token_value <- Inf
          }
          if (token_text == "-Inf") {
            token_type <- "NUMBER"
            token_value <- -Inf
          }
          if (token_text == "NaN") {
            token_type <- "NUMBER"
            token_value <- NaN
          }
          if (token_text == "NA") {
            token_type <- "NA"
            token_value <- NA
          }
          if (token_text == "NA_real_") {
            token_type <- "NA"
            token_value <- NA_real_
          }
          if (token_text == "NA_integer_") {
            token_type <- "NA"
            token_value <- NA_integer_
          }
          if (token_text == "NA_character_") {
            token_type <- "NA"
            token_value <- NA_character_
          }
          if (token_text == "NA_complex_") {
            token_type <- "NA"
            token_value <- NA_complex_
          }
          if (token_text == "TRUE") {
            token_type <- "BOOLEAN"
            token_value <- TRUE
          }
          if (token_text == "FALSE") {
            token_type <- "BOOLEAN"
            token_value <- FALSE
          }
          if (token_text == "NULL") {
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
