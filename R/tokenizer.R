# Tokenizer: Lexes Arl source code into tokens (LPAREN, RPAREN, SYMBOL, NUMBER, STRING,
# QUOTE, QUASIQUOTE, etc.) with line/col. Delimiters control what counts as a token boundary.
#
# @field delimiters Character vector of single-character delimiters.
#
#' @keywords internal
#' @noRd

# Module-level regex pattern for tokenization (compiled once at package load)
# Ordered alternation: PCRE tries each branch in order, first match wins.
.TOKEN_PATTERN <- paste0(
  '"(?:[^"\\\\]|\\\\.)*"',         # (1)  Complete string
  '|"(?:[^"\\\\]|\\\\.)*',         # (2)  Unterminated string (error detect)
  '|;[^\\n]*',                     # (3)  Comment
  '|,@',                           # (4)  Unquote-splicing
  '|[()]',                         # (5)  Parens
  "|'",                            # (6)  Quote
  '|`',                            # (7)  Quasiquote
  '|,',                            # (8)  Unquote
  '|:::',                          # (9)  Triple colon (before ::)
  '|::',                           # (10) Double colon (before :keyword)
  "|:[^ \\t\\n\\r()'`,;@\":]+",   # (11) Keyword
  "|[^ \\t\\n\\r()'`,;@\":]+(?::{2,3}[^ \\t\\n\\r()'`,;@\":]+)*",  # (12) Atom
  '|:'                             # (13) Lone colon
)

# Process string escape sequences using fixed gsub (C-level)
# Sentinel approach ensures \\n -> \ + n (not newline)
.process_string_escapes <- function(s) {
  s <- gsub("\\\\", "\001", s, fixed = TRUE)   # \\ -> sentinel
  s <- gsub("\\n",  "\n",   s, fixed = TRUE)   # \n -> newline
  s <- gsub("\\t",  "\t",   s, fixed = TRUE)   # \t -> tab
  s <- gsub("\\r",  "\r",   s, fixed = TRUE)   # \r -> CR
  s <- gsub("\\\"", "\"",   s, fixed = TRUE)   # \" -> quote
  s <- gsub("\001", "\\",   s, fixed = TRUE)   # sentinel -> backslash
  s
}

# Reverse of .process_string_escapes: convert literal special chars to escape sequences
unescape_string <- function(s) {
  s <- gsub("\\",   "\001", s, fixed = TRUE)   # backslash -> sentinel
  s <- gsub("\"",   "\\\"", s, fixed = TRUE)   # quote -> \"
  s <- gsub("\n",   "\\n",  s, fixed = TRUE)   # newline -> \n
  s <- gsub("\t",   "\\t",  s, fixed = TRUE)   # tab -> \t
  s <- gsub("\r",   "\\r",  s, fixed = TRUE)   # CR -> \r
  s <- gsub("\001", "\\\\", s, fixed = TRUE)   # sentinel -> \\
  s
}

# Classify an atom token text into its type and value
.classify_atom <- function(text) {
  # Standalone dot
  if (text == ".") {
    return(list(type = "DOT", value = "."))
  }

  # Booleans
  if (text == "#t") return(list(type = "BOOLEAN", value = TRUE))
  if (text == "#f") return(list(type = "BOOLEAN", value = FALSE))
  if (text == "TRUE") return(list(type = "BOOLEAN", value = TRUE))
  if (text == "FALSE") return(list(type = "BOOLEAN", value = FALSE))

  # Nil
  if (text == "#nil") return(list(type = "NIL", value = NULL))
  if (text == "NULL") return(list(type = "NIL", value = NULL))

  # Special numeric constants
  if (text == "Inf") return(list(type = "NUMBER", value = Inf))
  if (text == "-Inf") return(list(type = "NUMBER", value = -Inf))
  if (text == "NaN") return(list(type = "NUMBER", value = NaN))

  # NA variants
  if (text == "NA") return(list(type = "NA", value = NA))
  if (text == "NA_real_") return(list(type = "NA", value = NA_real_))
  if (text == "NA_integer_") return(list(type = "NA", value = NA_integer_))
  if (text == "NA_character_") return(list(type = "NA", value = NA_character_))
  if (text == "NA_complex_") return(list(type = "NA", value = NA_complex_))

  # Integer literal (e.g., 4L, 42L, -10L)
  if (grepl("^[-+]?\\d+L$", text)) {
    num_str <- substr(text, 1, nchar(text) - 1)
    return(list(type = "NUMBER", value = as.integer(num_str)))
  }

  # Full complex number (e.g., 2+4i, 3.14-2.5i)
  if (grepl("^[-+]?\\d+(\\.\\d+)?([eE][-+]?\\d+)?[+-]\\d+(\\.\\d+)?([eE][-+]?\\d+)?i$", text)) {
    start_pos <- 1L
    ch1 <- substr(text, 1, 1)
    if (ch1 == "+" || ch1 == "-") start_pos <- 2L
    sep_pos <- NULL
    nc <- nchar(text)
    for (j in start_pos:nc) {
      ch <- substr(text, j, j)
      if (ch == "+" || ch == "-") {
        sep_pos <- j
        break
      }
    }
    real_str <- substr(text, 1, sep_pos - 1)
    imag_str <- substr(text, sep_pos, nc - 1)
    return(list(type = "NUMBER", value = complex(real = as.numeric(real_str), imaginary = as.numeric(imag_str))))
  }

  # Pure imaginary number (e.g., 4i, 3.14i, -2.5i)
  if (grepl("^[-+]?\\d+(\\.\\d+)?([eE][-+]?\\d+)?i$", text)) {
    num_str <- substr(text, 1, nchar(text) - 1)
    return(list(type = "NUMBER", value = complex(real = 0, imaginary = as.numeric(num_str))))
  }

  # Regular number (including scientific notation)
  if (grepl("^[-+]?\\d+(\\.\\d+)?([eE][-+]?\\d+)?$", text)) {
    return(list(type = "NUMBER", value = as.numeric(text)))
  }

  # Default: symbol
  list(type = "SYMBOL", value = text)
}

Tokenizer <- R6::R6Class(
  "ArlTokenizer",
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
    # @param source Character string of Arl source code.
    # @return List of token lists.
    tokenize = function(source) {
      if (nchar(source) == 0L) return(list())

      # Phase 1: Single C-level regex pass
      m <- gregexpr(.TOKEN_PATTERN, source, perl = TRUE)[[1]]

      # No matches
      if (length(m) == 1L && m[1L] == -1L) return(list())

      match_starts <- as.integer(m)
      match_lengths <- attr(m, "match.length")
      n_matches <- length(match_starts)

      # Extract matched texts
      texts <- substring(source, match_starts, match_starts + match_lengths - 1L)

      # Phase 2: Vectorized line/col computation
      newline_pos <- gregexpr("\n", source, fixed = TRUE)[[1]]
      if (length(newline_pos) == 1L && newline_pos[1L] == -1L) {
        # No newlines - everything is on line 1
        lines <- rep(1L, n_matches)
        cols <- match_starts
      } else {
        line_starts <- c(1L, newline_pos + 1L)
        lines <- findInterval(match_starts, line_starts)
        cols <- match_starts - line_starts[lines] + 1L
      }

      # Phase 3: Classification loop
      tokens <- vector("list", n_matches)  # pre-allocate
      tok_idx <- 0L

      for (k in seq_len(n_matches)) {
        txt <- texts[k]
        ln <- lines[k]
        cl <- cols[k]
        ch1 <- substr(txt, 1L, 1L)

        if (ch1 == ";") {
          # Comment - skip
          next
        }

        if (ch1 == "\"") {
          # String - check if terminated
          if (substr(txt, nchar(txt), nchar(txt)) != "\"" || nchar(txt) < 2L) {
            stop(sprintf("Unterminated string at line %d, column %d", ln, cl))
          }
          # Strip quotes and process escapes
          inner <- substr(txt, 2L, nchar(txt) - 1L)
          val <- .process_string_escapes(inner)
          tok_idx <- tok_idx + 1L
          tokens[[tok_idx]] <- list(type = "STRING", value = val, line = ln, col = cl)
          next
        }

        if (ch1 == "(") {
          tok_idx <- tok_idx + 1L
          tokens[[tok_idx]] <- list(type = "LPAREN", value = "(", line = ln, col = cl)
          next
        }

        if (ch1 == ")") {
          tok_idx <- tok_idx + 1L
          tokens[[tok_idx]] <- list(type = "RPAREN", value = ")", line = ln, col = cl)
          next
        }

        if (ch1 == "'") {
          tok_idx <- tok_idx + 1L
          tokens[[tok_idx]] <- list(type = "QUOTE", value = "'", line = ln, col = cl)
          next
        }

        if (ch1 == "`") {
          tok_idx <- tok_idx + 1L
          tokens[[tok_idx]] <- list(type = "QUASIQUOTE", value = "`", line = ln, col = cl)
          next
        }

        if (txt == ",@") {
          tok_idx <- tok_idx + 1L
          tokens[[tok_idx]] <- list(type = "UNQUOTE_SPLICING", value = ",@", line = ln, col = cl)
          next
        }

        if (ch1 == ",") {
          tok_idx <- tok_idx + 1L
          tokens[[tok_idx]] <- list(type = "UNQUOTE", value = ",", line = ln, col = cl)
          next
        }

        if (ch1 == ":") {
          # :: or ::: operators
          if (txt == "::") {
            tok_idx <- tok_idx + 1L
            tokens[[tok_idx]] <- list(type = "SYMBOL", value = "::", line = ln, col = cl)
            next
          }
          if (txt == ":::") {
            tok_idx <- tok_idx + 1L
            tokens[[tok_idx]] <- list(type = "SYMBOL", value = ":::", line = ln, col = cl)
            next
          }
          # Lone colon (empty keyword)
          if (txt == ":") {
            stop(sprintf("Empty keyword at line %d, column %d", ln, cl))
          }
          # Keyword - strip leading colon
          tok_idx <- tok_idx + 1L
          tokens[[tok_idx]] <- list(type = "KEYWORD", value = substr(txt, 2L, nchar(txt)), line = ln, col = cl)
          next
        }

        # Atom (everything else) - classify
        atom <- .classify_atom(txt)
        tok_idx <- tok_idx + 1L
        tokens[[tok_idx]] <- list(type = atom$type, value = atom$value, line = ln, col = cl)
      }

      # Trim pre-allocated list to actual size
      if (tok_idx == 0L) return(list())
      tokens[seq_len(tok_idx)]
    }
  )
)
