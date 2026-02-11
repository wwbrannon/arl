# RyeDocParser: Parses ;;' annotation blocks from .rye source files.
# Extracts @description, @examples, @seealso, @note, @section, @signature
# for use by the compiler (baking docs into compiled code) and the
# vignette generator (tools/generate-stdlib-docs.R).

#' @importFrom R6 R6Class
#' @keywords internal
#' @noRd
RyeDocParser <- R6::R6Class(
  "RyeDocParser",
  public = list(
    #' @description Parse ;;' annotations from a .rye source file.
    #' @param file Path to a .rye source file.
    #' @return List with $functions (named list), $sections (list), $file (basename).
    parse_file = function(file) {
      lines <- readLines(file, warn = FALSE)
      result <- private$parse_lines(lines)
      result$file <- basename(file)
      result
    },

    #' @description Parse ;;' annotations from source text (string).
    #' @param text Character string of Rye source code.
    #' @return List with $functions (named list), $sections (list), $file (NULL).
    parse_text = function(text) {
      lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
      result <- private$parse_lines(lines)
      result$file <- NULL
      result
    },

    #' @description Extract exported symbol names from module source text.
    #' @param file Path to a .rye source file.
    #' @return Character vector of exported symbol names.
    get_exports = function(file) {
      text <- paste(readLines(file, warn = FALSE), collapse = "\n")
      private$extract_exports(text)
    }
  ),
  private = list(
    parse_lines = function(lines) {
      n <- length(lines)

      functions <- list()
      sections <- list()
      current_section <- NULL
      current_section_prose <- NULL

      i <- 1L
      while (i <= n) {
        if (!grepl("^\\s*;;'", lines[i])) {
          i <- i + 1L
          next
        }

        block_start <- i
        block_lines <- character()
        while (i <= n && grepl("^\\s*;;'", lines[i])) {
          line <- sub("^\\s*;;'\\s?", "", lines[i])
          block_lines <- c(block_lines, line)
          i <- i + 1L
        }

        tags <- private$parse_annotation_tags(block_lines)

        if (!is.null(tags$section) && !private$is_followed_by_definition(lines, i, n)) {
          current_section <- tags$section
          current_section_prose <- tags$section_prose
          sections[[length(sections) + 1L]] <- list(
            name = tags$section,
            prose = tags$section_prose
          )
          next
        }

        if (!is.null(tags$section)) {
          current_section <- tags$section
          current_section_prose <- tags$section_prose
          sections[[length(sections) + 1L]] <- list(
            name = tags$section,
            prose = tags$section_prose
          )
        }

        def <- private$find_following_definition(lines, i, n)
        if (is.null(def)) next

        func_name <- def$name
        def_line <- def$line
        i <- def_line

        description <- tags$description
        if (is.null(description) || description == "") {
          line1 <- trimws(lines[def_line])
          is_direct <- !grepl("\\(lambda", line1) && grepl("^\\(define\\s+\\S+\\s+\\S+\\)$", line1)
          if (is_direct) {
            description <- private$find_doc_bang(lines, def_line, n, func_name)
          } else {
            description <- private$extract_docstring(lines, def_line, n)
            if (is.null(description) || description == "") {
              description <- private$find_doc_bang(lines, def_line, n, func_name)
            }
          }
        }

        signature <- tags$signature
        if (is.null(signature) || signature == "") {
          signature <- private$extract_signature(lines, def_line, n, func_name, def$type)
        }

        functions[[func_name]] <- list(
          name = func_name,
          description = if (is.null(description)) "" else description,
          signature = if (is.null(signature)) "" else signature,
          examples = tags$examples,
          seealso = tags$seealso,
          note = tags$note,
          section = current_section,
          section_prose = current_section_prose,
          source_line = block_start
        )

        i <- i + 1L
      }

      list(functions = functions, sections = sections)
    },
    parse_annotation_tags = function(block_lines) {
      tags <- list(
        description = NULL,
        examples = NULL,
        seealso = NULL,
        note = NULL,
        section = NULL,
        section_prose = NULL,
        signature = NULL
      )

      current_tag <- NULL
      current_content <- character()
      section_prose_lines <- character()

      flush_tag <- function() {
        if (is.null(current_tag)) return()
        content <- paste(current_content, collapse = "\n")
        content <- trimws(content)
        if (current_tag == "examples") {
          tags$examples <<- content
        } else if (current_tag == "seealso") {
          tags$seealso <<- content
        } else if (current_tag == "note") {
          tags$note <<- content
        } else if (current_tag == "description") {
          tags$description <<- content
        } else if (current_tag == "signature") {
          tags$signature <<- content
        }
      }

      for (line in block_lines) {
        if (grepl("^@section\\s+", line)) {
          flush_tag()
          current_tag <- NULL
          current_content <- character()
          tags$section <- trimws(sub("^@section\\s+", "", line))
          section_prose_lines <- character()
          current_tag <- "__section_prose"
          next
        }
        if (grepl("^@examples\\s*$", line) || grepl("^@examples\\s", line)) {
          flush_tag()
          current_tag <- "examples"
          rest <- trimws(sub("^@examples\\s*", "", line))
          current_content <- if (nchar(rest) > 0) rest else character()
          next
        }
        if (grepl("^@seealso\\s", line)) {
          flush_tag()
          current_tag <- "seealso"
          current_content <- trimws(sub("^@seealso\\s+", "", line))
          next
        }
        if (grepl("^@note\\s", line) || grepl("^@note$", line)) {
          flush_tag()
          current_tag <- "note"
          rest <- trimws(sub("^@note\\s*", "", line))
          current_content <- if (nchar(rest) > 0) rest else character()
          next
        }
        if (grepl("^@description\\s", line) || grepl("^@description$", line)) {
          flush_tag()
          current_tag <- "description"
          rest <- trimws(sub("^@description\\s*", "", line))
          current_content <- if (nchar(rest) > 0) rest else character()
          next
        }
        if (grepl("^@signature\\s", line)) {
          flush_tag()
          current_tag <- "signature"
          current_content <- trimws(sub("^@signature\\s+", "", line))
          next
        }
        if (grepl("^@", line)) {
          flush_tag()
          current_tag <- NULL
          current_content <- character()
          next
        }

        if (!is.null(current_tag)) {
          if (current_tag == "__section_prose") {
            section_prose_lines <- c(section_prose_lines, line)
          } else {
            current_content <- c(current_content, line)
          }
        }
      }
      flush_tag()

      if (length(section_prose_lines) > 0) {
        tags$section_prose <- paste(section_prose_lines, collapse = "\n")
      }

      tags
    },

    is_followed_by_definition = function(lines, i, n) {
      j <- i
      while (j <= n) {
        line <- trimws(lines[j])
        if (line == "" || grepl("^;;[^']", line) || grepl("^;$", line) || line == ";;") {
          j <- j + 1L
          next
        }
        return(grepl("^\\(define\\s", line) || grepl("^\\(defmacro\\s", line))
      }
      FALSE
    },

    find_following_definition = function(lines, i, n) {
      j <- i
      while (j <= n) {
        line <- trimws(lines[j])
        if (line == "" || grepl("^;;[^']", line) || grepl("^;$", line) || line == ";;") {
          j <- j + 1L
          next
        }
        m <- regmatches(line, regexpr("^\\(define\\s+([a-zA-Z0-9_.?/<>!=*+%@~^&|-]+)", line, perl = TRUE))
        if (length(m) == 1 && nchar(m) > 0) {
          name <- sub("^\\(define\\s+", "", m)
          return(list(name = name, line = j, type = "define"))
        }
        m2 <- regmatches(line, regexpr('^\\(define\\s+"([^"]+)"', line, perl = TRUE))
        if (length(m2) == 1 && nchar(m2) > 0) {
          name <- sub('^\\(define\\s+"', "", m2)
          name <- sub('"$', "", name)
          return(list(name = name, line = j, type = "define"))
        }
        m3 <- regmatches(line, regexpr("^\\(defmacro\\s+([a-zA-Z0-9_.?/<>!=*+%@~^&|-]+)", line, perl = TRUE))
        if (length(m3) == 1 && nchar(m3) > 0) {
          name <- sub("^\\(defmacro\\s+", "", m3)
          return(list(name = name, line = j, type = "defmacro"))
        }
        return(NULL)
      }
      NULL
    },

    extract_docstring = function(lines, def_line, n) {
      end_line <- min(def_line + 15, n)
      for (k in (def_line + 1):end_line) {
        if (grepl("^\\s*;;'", lines[k])) {
          end_line <- k - 1L
          break
        }
      }
      if (end_line < def_line) return(NULL)

      chunk <- paste(lines[def_line:end_line], collapse = "\n")

      extract_string_after <- function(text, prefix_re) {
        m <- regmatches(text, regexpr(
          paste0(prefix_re, '\\s*\n?\\s*"((?:[^"\\\\]|\\\\.)*)"'),
          text, perl = TRUE
        ))
        if (length(m) == 1 && nchar(m) > 0) {
          inner <- sub(paste0('.*', prefix_re, '\\s*\n?\\s*"'), "", m, perl = TRUE)
          inner <- sub('"$', "", inner)
          inner <- gsub('\\\\"', '"', inner)
          return(inner)
        }
        NULL
      }

      result <- extract_string_after(chunk, '\\(defmacro\\s+\\S+\\s+\\([^)]*\\)')
      if (!is.null(result)) return(result)

      result <- extract_string_after(chunk, '\\(lambda\\s+\\([^)]*\\)')
      if (!is.null(result)) return(result)

      result <- extract_string_after(chunk, '\\(lambda\\s+\\(\\. [^)]+\\)')
      if (!is.null(result)) return(result)

      NULL
    },

    find_doc_bang = function(lines, def_line, n, func_name) {
      for (j in (def_line + 1):min(def_line + 5, n)) {
        line <- trimws(lines[j])
        escaped_name <- gsub("([.?*+^${}()|\\[\\]\\\\])", "\\\\\\1", func_name)
        pattern <- paste0('^\\(doc!\\s+("?', escaped_name, '"?)\\s+"((?:[^"\\\\]|\\\\.)*)"\\)')
        m <- regmatches(line, regexpr(pattern, line, perl = TRUE))
        if (length(m) == 1 && nchar(m) > 0) {
          inner <- sub(paste0('^\\(doc!\\s+"?', escaped_name, '"?\\s+"'), "", m)
          inner <- sub('"\\)$', "", inner)
          inner <- gsub('\\\\"', '"', inner)
          return(inner)
        }
      }
      NULL
    },

    extract_signature = function(lines, def_line, n, func_name, def_type = "define") {
      chunk <- paste(lines[def_line:min(def_line + 10, n)], collapse = "\n")

      if (def_type == "defmacro") {
        m <- regmatches(chunk, regexpr(
          paste0("\\(defmacro\\s+", gsub("([.?*+^${}()|\\[\\]\\\\])", "\\\\\\1", func_name),
                 "\\s+\\(([^)]*)\\)"),
          chunk, perl = TRUE
        ))
        if (length(m) == 1 && nchar(m) > 0) {
          params_str <- sub(".*\\(defmacro\\s+\\S+\\s+\\(", "", m)
          params_str <- sub("\\)$", "", params_str)
          params <- private$format_params(params_str)
          return(paste0("(", func_name, if (nchar(params) > 0) paste0(" ", params) else "", ")"))
        }
        return(NULL)
      }

      line1 <- trimws(lines[def_line])
      if (grepl("^\\(define\\s+\\S+\\s+__r", line1) ||
          (grepl("^\\(define\\s+\\S+\\s+\\S+\\)$", line1) &&
           !grepl("\\(lambda", line1))) {
        return(NULL)
      }

      m <- regmatches(chunk, regexpr(
        "\\(lambda\\s+\\(([^)]*)\\)",
        chunk, perl = TRUE
      ))
      if (length(m) == 1 && nchar(m) > 0) {
        params_str <- sub("^\\(lambda\\s+\\(", "", m)
        params_str <- sub("\\)$", "", params_str)
        params <- private$format_params(params_str)
        return(paste0("(", func_name, if (nchar(params) > 0) paste0(" ", params) else "", ")"))
      }

      NULL
    },

    format_params = function(params_str) {
      params_str <- trimws(params_str)
      if (nchar(params_str) == 0) return("")

      tokens <- private$tokenize_params(params_str)
      parts <- character()
      for (tok in tokens) {
        if (grepl("^\\.", tok)) {
          rest_name <- trimws(sub("^\\.", "", tok))
          if (nchar(rest_name) == 0) rest_name <- "args"
          parts <- c(parts, paste0(rest_name, "..."))
        } else if (grepl("^\\(", tok)) {
          inner <- sub("^\\(", "", tok)
          inner <- sub("\\)$", "", inner)
          parts <- c(parts, paste0("[", inner, "]"))
        } else {
          parts <- c(parts, tok)
        }
      }
      paste(parts, collapse = " ")
    },

    tokenize_params = function(s) {
      s <- trimws(s)
      tokens <- character()
      i <- 1L
      n <- nchar(s)
      while (i <= n) {
        ch <- substr(s, i, i)
        if (ch == " " || ch == "\t" || ch == "\n") {
          i <- i + 1L
          next
        }
        if (ch == "(") {
          depth <- 1L
          j <- i + 1L
          while (j <= n && depth > 0L) {
            c2 <- substr(s, j, j)
            if (c2 == "(") depth <- depth + 1L
            else if (c2 == ")") depth <- depth - 1L
            j <- j + 1L
          }
          tokens <- c(tokens, substr(s, i, j - 1L))
          i <- j
        } else if (ch == ".") {
          j <- i + 1L
          while (j <= n && substr(s, j, j) %in% c(" ", "\t")) j <- j + 1L
          k <- j
          while (k <= n && !substr(s, k, k) %in% c(" ", "\t", ")", "\n")) k <- k + 1L
          tokens <- c(tokens, paste0(".", substr(s, j, k - 1L)))
          i <- k
        } else {
          j <- i
          while (j <= n && !substr(s, j, j) %in% c(" ", "\t", "(", ")", "\n")) j <- j + 1L
          tokens <- c(tokens, substr(s, i, j - 1L))
          i <- j
        }
      }
      tokens
    },

    extract_exports = function(text) {
      lines <- strsplit(text, "\n")[[1]]
      clean_lines <- character()
      for (line in lines) {
        in_str <- FALSE
        quote_char <- ""
        ii <- 1L
        nn <- nchar(line)
        while (ii <= nn) {
          ch <- substr(line, ii, ii)
          if (in_str) {
            if (ch == quote_char && (ii == 1L || substr(line, ii - 1L, ii - 1L) != "\\"))
              in_str <- FALSE
            ii <- ii + 1L
            next
          }
          if (ch %in% c('"', "'")) {
            in_str <- TRUE
            quote_char <- ch
            ii <- ii + 1L
            next
          }
          if (ch == ";") {
            line <- substr(line, 1L, ii - 1L)
            break
          }
          ii <- ii + 1L
        }
        clean_lines <- c(clean_lines, line)
      }
      clean_text <- paste(clean_lines, collapse = "\n")

      exp_match <- regexpr("\\(export\\s+", clean_text, perl = TRUE)
      if (exp_match == -1L) return(character())

      start <- exp_match + attr(exp_match, "match.length")
      i <- as.integer(start)
      n <- nchar(clean_text)
      depth <- 1L
      while (i <= n && depth > 0L) {
        ch <- substr(clean_text, i, i)
        if (ch == "(") depth <- depth + 1L
        else if (ch == ")") depth <- depth - 1L
        i <- i + 1L
      }
      if (depth != 0L) return(character())

      exp_body <- substr(clean_text, start, i - 2L)
      exp_body <- gsub("\\s+", " ", exp_body)

      syms <- character()
      rest <- trimws(exp_body)
      while (nchar(rest) > 0) {
        rest <- trimws(rest)
        if (substr(rest, 1, 1) == '"') {
          m <- regexpr('"[^"]*"', rest)
          if (m == -1) break
          syms <- c(syms, substr(rest, 2, attr(m, "match.length") - 1))
          rest <- substr(rest, attr(m, "match.length") + 1, nchar(rest))
        } else {
          m <- regexpr("^[a-zA-Z0-9_.?/<>!=*+-]+", rest)
          if (m == -1) break
          syms <- c(syms, substr(rest, 1, attr(m, "match.length")))
          rest <- substr(rest, attr(m, "match.length") + 1, nchar(rest))
        }
      }
      syms
    }
  )
)
