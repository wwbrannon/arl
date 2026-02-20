# DocParser: Parses ;;' annotation blocks from .arl source files.
# Extracts @description, @param, @examples, @assert, @seealso, @note,
# @section, @signature for use by the compiler (baking docs into compiled
# code) and the vignette generator (tools/docs/generate-lang-docs.R).

#' @importFrom R6 R6Class
#' @keywords internal
#' @noRd
DocParser <- R6::R6Class(
  "ArlDocParser",
  public = list(
    #' @description Parse ;;' annotations from a .arl source file.
    #' @param file Path to a .arl source file.
    #' @return List with $functions (named list), $sections (list), $file (basename).
    parse_file = function(file) {
      lines <- readLines(file, warn = FALSE)
      result <- private$parse_lines(lines)
      result$file <- basename(file)
      result
    },

    #' @description Parse ;;' annotations from source text (string).
    #' @param text Character string of Arl source code.
    #' @return List with $functions (named list), $sections (list), $file (NULL).
    parse_text = function(text) {
      lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
      result <- private$parse_lines(lines)
      result$file <- NULL
      result
    },

    #' @description Extract exported symbol names from module source text.
    #' @param file Path to a .arl source file.
    #' @return Character vector of exported symbol names.
    get_exports = function(file) {
      text <- paste(readLines(file, warn = FALSE), collapse = "\n")
      private$extract_exports(text)
    },

    #' @description Convert an Arl function name to a stable HTML anchor slug.
    #' Special characters are mapped to readable alternatives so that
    #' names like `string<?` and `string>?` produce distinct slugs.
    #' @param name Character string — an Arl function name.
    #' @return A lowercase, hyphen-separated slug safe for use as an HTML id.
    slugify = function(name) {
      # Pure operator names
      op_map <- list(
        "+"  = "plus",  "-"  = "minus", "*"  = "star", "/" = "div",
        "%"  = "percent", "="  = "num-eq", "==" = "num-eq-eq",
        "<"  = "lt",    ">"  = "gt",    "<=" = "lte",  ">=" = "gte"
      )
      if (name %in% names(op_map)) return(op_map[[name]])

      # Threading macro names
      if (name == "->>") return("thread-last")
      if (name == "->")  return("thread-first")

      s <- name

      # Arrow conversions (must come before single-char replacements)
      s <- gsub("->", "-to-", s, fixed = TRUE)
      s <- gsub(":::", "-ns-internal-", s, fixed = TRUE)
      s <- gsub("::", "-ns-", s, fixed = TRUE)

      # Comparison operators in names (longer patterns first)
      s <- gsub("<=", "-lte", s, fixed = TRUE)
      s <- gsub(">=", "-gte", s, fixed = TRUE)
      s <- gsub("==", "-eq-eq", s, fixed = TRUE)
      s <- gsub("<",  "-lt",  s, fixed = TRUE)
      s <- gsub(">",  "-gt",  s, fixed = TRUE)
      s <- gsub("=",  "-eq",  s, fixed = TRUE)

      # Other special chars
      s <- gsub("!",  "-bang", s, fixed = TRUE)
      s <- gsub("?",  "-p",    s, fixed = TRUE)
      s <- gsub("*",  "-star", s, fixed = TRUE)
      s <- gsub("~",  "-tilde-", s, fixed = TRUE)
      s <- gsub("/",  "-",     s, fixed = TRUE)
      s <- gsub(".",  "-",     s, fixed = TRUE)

      # Clean up runs of hyphens and leading/trailing hyphens
      s <- gsub("-+", "-", s)
      s <- gsub("^-|-$", "", s)
      tolower(s)
    },

    #' @description Load reference documentation entries from a DCF file.
    #' @param path Path to reference docs DCF file.
    #' @param kind Optional kind filter (e.g., "builtin", "special-form").
    #' @return A named list of function doc entries. Each entry has name,
    #'   description, signature, examples, seealso, note, section,
    #'   section_prose, vignette, and kind fields.
    load_reference_docs = function(path = "inst/reference-docs.dcf", kind = NULL) {
      if (!file.exists(path)) return(list())

      lines <- readLines(path, warn = FALSE)
      lines <- lines[!grepl("^\\s*#", lines)]
      m <- read.dcf(textConnection(paste(lines, collapse = "\n")))
      entries <- list()

      for (i in seq_len(nrow(m))) {
        func_name <- m[i, "Name"]

        dcf_field <- function(field) {
          if (!field %in% colnames(m)) return(NULL)
          val <- unname(m[i, field])
          if (is.na(val) || !nzchar(val)) NULL else val
        }

        examples <- dcf_field("Examples")
        if (!is.null(examples)) examples <- sub("\\s+$", "", examples)

        vname <- dcf_field("Vignette")
        sec_name <- dcf_field("Section")

        noeval_val <- dcf_field("Noeval")
        noeval <- !is.null(noeval_val) && tolower(noeval_val) %in% c("yes", "true")

        assert_val <- dcf_field("Assert")
        if (!is.null(assert_val)) assert_val <- sub("\\s+$", "", assert_val)

        kind_val <- dcf_field("Kind")
        if (is.null(kind_val) || !nzchar(trimws(kind_val))) {
          kind_val <- "builtin"
        } else {
          kind_val <- tolower(trimws(kind_val))
        }
        kind_val <- unname(kind_val)
        # Special forms are language constructs, not callable objects; their
        # examples are primarily illustrative and often context-dependent.
        # Default them to non-evaluated vignette examples unless explicitly set.
        if (kind_val == "special-form" && is.null(noeval_val)) {
          noeval <- TRUE
        }
        if (!is.null(kind) && kind_val != tolower(trimws(kind))) {
          next
        }

        entries[[func_name]] <- list(
          name         = func_name,
          kind         = unname(kind_val),
          description  = if (is.null(dcf_field("Description"))) "" else dcf_field("Description"),
          signature    = if (is.null(dcf_field("Signature"))) "" else dcf_field("Signature"),
          examples     = examples,
          assert       = assert_val,
          seealso      = dcf_field("Seealso"),
          note         = dcf_field("Note"),
          section      = sec_name,
          section_prose = NULL,
          vignette     = vname,
          noeval       = noeval
        )
      }

      entries
    },

    #' @description Load documentation for R-defined builtins from a DCF file.
    #' @param path Path to reference docs DCF file.
    #' @return A named list of builtin doc entries.
    load_builtins = function(path = "inst/reference-docs.dcf") {
      self$load_reference_docs(path = path, kind = "builtin")
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

        tags <- private$parse_annotation_tags(block_lines, block_start)

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

        signature <- tags$signature
        if (is.null(signature) || signature == "") {
          signature <- private$extract_signature(lines, def_line, n, func_name, def$type)
        }

        functions[[func_name]] <- list(
          name = func_name,
          description = if (is.null(description)) "" else description,
          signature = if (is.null(signature)) "" else signature,
          params = tags$params,
          examples = tags$examples,
          assert = tags$assert,
          seealso = tags$seealso,
          note = tags$note,
          internal = tags$internal,
          noeval = tags$noeval,
          section = current_section,
          section_prose = current_section_prose,
          source_line = block_start
        )

        i <- i + 1L
      }

      list(functions = functions, sections = sections)
    },
    parse_annotation_tags = function(block_lines, block_start = NA_integer_) {
      tags <- list(
        description = NULL,
        params = list(),
        examples = NULL,
        assert = NULL,
        seealso = NULL,
        note = NULL,
        section = NULL,
        section_prose = NULL,
        signature = NULL,
        internal = FALSE,
        noeval = FALSE
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
        } else if (current_tag == "assert") {
          tags$assert <<- content
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
        if (grepl("^@assert\\s*$", line) || grepl("^@assert\\s", line)) {
          flush_tag()
          current_tag <- "assert"
          rest <- trimws(sub("^@assert\\s*", "", line))
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
        if (grepl("^@internal\\s*$", line) || grepl("^@internal\\s", line)) {
          tags$internal <- TRUE
          next
        }
        if (grepl("^@noeval\\s*$", line) || grepl("^@noeval\\s", line)) {
          tags$noeval <- TRUE
          next
        }
        if (grepl("^@param\\s+", line)) {
          flush_tag()
          current_tag <- NULL
          current_content <- character()
          param_name <- sub("^@param\\s+(\\S+).*", "\\1", line)
          param_desc <- trimws(sub("^@param\\s+\\S+\\s*", "", line))
          tags$params <- c(tags$params, list(list(name = param_name, description = param_desc)))
          next
        }
        if (grepl("^@returns?\\s", line) || grepl("^@returns?$", line)) {
          flush_tag()
          current_tag <- NULL
          current_content <- character()
          next
        }
        if (grepl("^@", line)) {
          flush_tag()
          tag_name <- sub("^@(\\S+).*", "\\1", line)
          warning(sprintf("Unknown doc tag '@%s' in annotation block starting at line %d",
                          tag_name, block_start), call. = FALSE)
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
      if (grepl("^\\(define\\s+\\S+\\s+\\S+\\)$", line1) &&
          !grepl("\\(lambda", line1)) {
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
          m <- regexpr("^[a-zA-Z0-9_.?/<>!=*+%@~^&|-]+", rest)
          if (m == -1) break
          syms <- c(syms, substr(rest, 1, attr(m, "match.length")))
          rest <- substr(rest, attr(m, "match.length") + 1, nchar(rest))
        }
      }
      syms
    }
  )
)
