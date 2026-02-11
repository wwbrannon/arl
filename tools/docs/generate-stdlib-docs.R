#!/usr/bin/env Rscript
# generate-stdlib-docs.R — Auto-generate stdlib reference vignettes from .rye source files.
#
# Usage: Rscript tools/docs/generate-stdlib-docs.R
#        make stdlib-docs
#
# Reads tools/docs/stdlib-docs.yml for module-to-vignette mapping, parses ;;' annotations
# from inst/rye/*.rye, and writes vignettes/stdlib-*.Rmd files.

# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required. Install with: install.packages('yaml')")
}

# Load RyeDocParser (shared annotation parser)
if (requireNamespace("rye", quietly = TRUE)) {
  .doc_parser <- rye:::RyeDocParser$new()
} else {
  # Fallback: source directly when running outside installed package
  source(file.path("R", "doc-parser.R"))
  .doc_parser <- RyeDocParser$new()
}

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

AUTOGEN_HEADER <- paste0(
  "<!-- AUTO-GENERATED from inst/rye/ source files. Do not edit manually. -->\n",
  "<!-- Regenerate with: make stdlib-docs -->\n"
)

# ---------------------------------------------------------------------------
# Anchor / slug helpers
# ---------------------------------------------------------------------------

#' Convert a function name to a stable HTML anchor ID.
#' Special characters are mapped to readable alternatives so that
#' headings like `string<?` and `string>?` get distinct anchors.
slugify_func_name <- function(name) {
  # Pure operator names
  op_map <- list(
    "+"  = "plus",  "-"  = "minus", "*"  = "star", "/" = "div",
    "%"  = "modulo", "="  = "num-eq", "==" = "num-eq-eq",
    "<"  = "lt",    ">"  = "gt",    "<=" = "lte",  ">=" = "gte"
  )
  if (name %in% names(op_map)) return(op_map[[name]])

  # Threading macro names
  if (name == "->>") return("thread-last")
  if (name == "->")  return("thread-first")

  s <- name

  # Arrow conversions (must come before single-char replacements)
  s <- gsub("->", "-to-", s, fixed = TRUE)

  # Comparison operators in names (longer patterns first)
  s <- gsub("<=", "-lte", s, fixed = TRUE)
  s <- gsub(">=", "-gte", s, fixed = TRUE)
  s <- gsub("==", "-eq-eq", s, fixed = TRUE)
  s <- gsub("<",  "-lt",  s, fixed = TRUE)
  s <- gsub(">",  "-gt",  s, fixed = TRUE)
  s <- gsub("=",  "-eq",  s, fixed = TRUE)

  # Other special chars
  s <- gsub("!",  "-bang", s, fixed = TRUE)
  s <- gsub("?",  "",      s, fixed = TRUE)
  s <- gsub("*",  "-star", s, fixed = TRUE)
  s <- gsub("/",  "-",     s, fixed = TRUE)
  s <- gsub(".",  "-",     s, fixed = TRUE)

  # Clean up runs of hyphens and leading/trailing hyphens
  s <- gsub("-+", "-", s)
  s <- gsub("^-|-$", "", s)
  tolower(s)
}

#' Build a global lookup: func_name -> list(vignette, slug).
#' Used to resolve cross-vignette "See also" links.
build_func_index <- function(vignettes_config, all_parsed) {
  index <- list()
  for (vname in names(vignettes_config)) {
    vconfig <- vignettes_config[[vname]]
    for (mod_name in vconfig$modules) {
      parsed <- all_parsed[[mod_name]]
      if (is.null(parsed)) next
      for (fn in parsed$functions) {
        index[[fn$name]] <- list(
          vignette = vname,
          slug     = slugify_func_name(fn$name)
        )
      }
    }
  }
  index
}

#' Turn a "See also" string into markdown with hyperlinks.
#'
#' Input examples:
#'   "car, cdr, cons"
#'   "exact? (in `math` module), inexact? (in `math` module)"
#'
#' Each recognised function name becomes a markdown link; parenthetical
#' notes and unknown names are left as plain text.
linkify_seealso <- function(seealso, func_index, current_vignette) {
  # Split on commas
  parts <- strsplit(seealso, ",")[[1]]
  linked <- vapply(parts, function(part) {
    part <- trimws(part)
    if (nchar(part) == 0) return("")

    # Separate optional parenthetical note:  "exact? (in `math` module)"
    paren_note <- ""
    m <- regexpr("\\s+\\(.*\\)$", part, perl = TRUE)
    if (m != -1L) {
      paren_note <- substr(part, m, nchar(part))
      part_name  <- trimws(substr(part, 1L, m - 1L))
    } else {
      part_name <- part
    }

    # Look up in index
    entry <- func_index[[part_name]]
    if (is.null(entry)) {
      return(paste0(part_name, paren_note))
    }

    # Build link
    if (entry$vignette == current_vignette) {
      link <- paste0("[", part_name, "](#", entry$slug, ")")
    } else {
      link <- paste0("[", part_name, "](", entry$vignette, ".html#", entry$slug, ")")
    }
    paste0(link, paren_note)
  }, character(1), USE.NAMES = FALSE)

  paste(linked, collapse = ", ")
}

# ---------------------------------------------------------------------------
# Annotation parser — delegates to shared RyeDocParser (R/doc-parser.R)
# ---------------------------------------------------------------------------

#' Parse ;;' annotations from a .rye source file.
#' Delegates to the shared RyeDocParser class.
parse_rye_annotations <- function(file) {
  .doc_parser$parse_file(file)
}

#' Legacy inline parser (kept as unused reference; all calls go through RyeDocParser).
.parse_rye_annotations_legacy <- function(file) {
  lines <- readLines(file, warn = FALSE)
  n <- length(lines)

  # Track results
  functions <- list()
  sections <- list()
  current_section <- NULL
  current_section_prose <- NULL

  i <- 1L
  while (i <= n) {
    # Look for start of a ;;' block
    if (!grepl("^\\s*;;'", lines[i])) {
      i <- i + 1L
      next
    }

    # Collect the ;;' block
    block_start <- i
    block_lines <- character()
    while (i <= n && grepl("^\\s*;;'", lines[i])) {
      # Strip the ;;' prefix and one optional space
      line <- sub("^\\s*;;'\\s?", "", lines[i])
      block_lines <- c(block_lines, line)
      i <- i + 1L
    }

    # Parse tags from block
    tags <- parse_annotation_tags(block_lines)

    # Check if this block is a standalone @section (not followed by a definition)
    if (!is.null(tags$section) && !is_followed_by_definition(lines, i, n)) {
      current_section <- tags$section
      current_section_prose <- tags$section_prose
      sections[[length(sections) + 1L]] <- list(
        name = tags$section,
        prose = tags$section_prose
      )
      next
    }

    # If block contains @section before a definition, update current section
    if (!is.null(tags$section)) {
      current_section <- tags$section
      current_section_prose <- tags$section_prose
      sections[[length(sections) + 1L]] <- list(
        name = tags$section,
        prose = tags$section_prose
      )
    }

    # Look for a following definition
    def <- find_following_definition(lines, i, n)
    if (is.null(def)) next

    func_name <- def$name
    def_line <- def$line
    i <- def_line  # advance past definition

    # Extract description from inline docstring or doc! call, unless @description overrides
    description <- tags$description
    if (is.null(description) || description == "") {
      # For direct assignments (define f __rfunc), try doc! first
      # For lambda definitions, try inline docstring first
      line1 <- trimws(lines[def_line])
      is_direct <- !grepl("\\(lambda", line1) && grepl("^\\(define\\s+\\S+\\s+\\S+\\)$", line1)
      if (is_direct) {
        description <- find_doc_bang(lines, def_line, n, func_name)
      } else {
        description <- extract_docstring(lines, def_line, n)
        if (is.null(description) || description == "") {
          description <- find_doc_bang(lines, def_line, n, func_name)
        }
      }
    }

    # Extract signature from lambda params
    signature <- tags$signature
    if (is.null(signature) || signature == "") {
      signature <- extract_signature(lines, def_line, n, func_name, def$type)
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

  list(functions = functions, sections = sections, file = basename(file))
}

#' Parse @tag content from a block of annotation lines (already stripped of ;;' prefix).
parse_annotation_tags <- function(block_lines) {
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
    # Check for @tag at start of line
    if (grepl("^@section\\s+", line)) {
      flush_tag()
      current_tag <- NULL
      current_content <- character()
      tags$section <- trimws(sub("^@section\\s+", "", line))
      # Remaining lines (until next @tag) are section prose
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
      # Unknown tag — flush and ignore
      flush_tag()
      current_tag <- NULL
      current_content <- character()
      next
    }

    # Continuation line for current tag
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
}

#' Check if the lines following position i contain a (define ...) or (defmacro ...) form,
#' allowing blank lines and ;; comments between.
is_followed_by_definition <- function(lines, i, n) {
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
}

#' Find the definition (define/defmacro) following position i.
#' Returns list(name, line, type) or NULL.
find_following_definition <- function(lines, i, n) {
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
    # Also match quoted define names like (define "string<?" ...)
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
}

#' Extract inline docstring from a lambda/defmacro body starting at line def_line.
#' Looks for the first string literal inside the lambda or defmacro.
#' Stops scanning at the next ;;' annotation block to avoid cross-contamination.
extract_docstring <- function(lines, def_line, n) {
  # Collect lines until next ;;' block or up to 10 lines, whichever is less
  end_line <- min(def_line + 15, n)
  for (k in (def_line + 1):end_line) {
    if (grepl("^\\s*;;'", lines[k])) {
      end_line <- k - 1L
      break
    }
  }
  if (end_line < def_line) return(NULL)

  chunk <- paste(lines[def_line:end_line], collapse = "\n")

  # Helper: extract first string literal (handling escaped quotes) after a pattern
  extract_string_after <- function(text, prefix_re) {
    m <- regmatches(text, regexpr(
      paste0(prefix_re, '\\s*\n?\\s*"((?:[^"\\\\]|\\\\.)*)"'),
      text, perl = TRUE
    ))
    if (length(m) == 1 && nchar(m) > 0) {
      inner <- sub(paste0('.*', prefix_re, '\\s*\n?\\s*"'), "", m, perl = TRUE)
      inner <- sub('"$', "", inner)
      # Unescape backslash-escaped quotes
      inner <- gsub('\\\\"', '"', inner)
      return(inner)
    }
    NULL
  }

  # Pattern: (defmacro name (params) "docstring" ...)
  result <- extract_string_after(chunk, '\\(defmacro\\s+\\S+\\s+\\([^)]*\\)')
  if (!is.null(result)) return(result)

  # Pattern: (define name (lambda (...) "docstring" ...))
  result <- extract_string_after(chunk, '\\(lambda\\s+\\([^)]*\\)')
  if (!is.null(result)) return(result)

  # Pattern for variadic: (lambda (. args) "docstring")
  result <- extract_string_after(chunk, '\\(lambda\\s+\\(\\. [^)]+\\)')
  if (!is.null(result)) return(result)

  NULL
}

#' Find a (doc! name "docstring") call after a definition.
find_doc_bang <- function(lines, def_line, n, func_name) {
  # Check next few lines for doc!
  for (j in (def_line + 1):min(def_line + 5, n)) {
    line <- trimws(lines[j])
    # Match (doc! name "docstring") - name may or may not be quoted
    escaped_name <- gsub("([.?*+^${}()|\\[\\]\\\\])", "\\\\\\1", func_name)
    pattern <- paste0('^\\(doc!\\s+("?', escaped_name, '"?)\\s+"((?:[^"\\\\]|\\\\.)*)"\\)')
    m <- regmatches(line, regexpr(pattern, line, perl = TRUE))
    if (length(m) == 1 && nchar(m) > 0) {
      # Extract the docstring
      inner <- sub(paste0('^\\(doc!\\s+"?', escaped_name, '"?\\s+"'), "", m)
      inner <- sub('"\\)$', "", inner)
      inner <- gsub('\\\\"', '"', inner)
      return(inner)
    }
  }
  NULL
}

#' Extract function signature from lambda parameter list.
extract_signature <- function(lines, def_line, n, func_name, def_type = "define") {
  chunk <- paste(lines[def_line:min(def_line + 10, n)], collapse = "\n")

  if (def_type == "defmacro") {
    # (defmacro name (params...) ...)
    m <- regmatches(chunk, regexpr(
      paste0("\\(defmacro\\s+", gsub("([.?*+^${}()|\\[\\]\\\\])", "\\\\\\1", func_name),
             "\\s+\\(([^)]*)\\)"),
      chunk, perl = TRUE
    ))
    if (length(m) == 1 && nchar(m) > 0) {
      params_str <- sub(".*\\(defmacro\\s+\\S+\\s+\\(", "", m)
      params_str <- sub("\\)$", "", params_str)
      params <- format_params(params_str)
      return(paste0("(", func_name, if (nchar(params) > 0) paste0(" ", params) else "", ")"))
    }
    return(NULL)
  }

  # Check for direct assignment: (define name __rfunc) or (define name other-name)
  line1 <- trimws(lines[def_line])
  if (grepl("^\\(define\\s+\\S+\\s+__r", line1) ||
      (grepl("^\\(define\\s+\\S+\\s+\\S+\\)$", line1) &&
       !grepl("\\(lambda", line1))) {
    # Direct assignment — no signature extractable from source
    return(NULL)
  }

  # (define name (lambda (params...) ...))
  m <- regmatches(chunk, regexpr(
    "\\(lambda\\s+\\(([^)]*)\\)",
    chunk, perl = TRUE
  ))
  if (length(m) == 1 && nchar(m) > 0) {
    params_str <- sub("^\\(lambda\\s+\\(", "", m)
    params_str <- sub("\\)$", "", params_str)
    params <- format_params(params_str)
    return(paste0("(", func_name, if (nchar(params) > 0) paste0(" ", params) else "", ")"))
  }

  NULL
}

#' Format parameter string from lambda params.
#' Handles: plain args, (arg default), and (. rest).
format_params <- function(params_str) {
  params_str <- trimws(params_str)
  if (nchar(params_str) == 0) return("")

  # Tokenize respecting nested parens
  tokens <- tokenize_params(params_str)
  parts <- character()
  for (tok in tokens) {
    if (grepl("^\\.", tok)) {
      # Rest parameter: . args -> "value..."
      rest_name <- trimws(sub("^\\.", "", tok))
      if (nchar(rest_name) == 0) rest_name <- "args"
      parts <- c(parts, paste0(rest_name, "..."))
    } else if (grepl("^\\(", tok)) {
      # Optional parameter: (name default) -> [name default]
      inner <- sub("^\\(", "", tok)
      inner <- sub("\\)$", "", inner)
      parts <- c(parts, paste0("[", inner, "]"))
    } else {
      parts <- c(parts, tok)
    }
  }
  paste(parts, collapse = " ")
}

#' Tokenize parameter string into individual params.
#' Groups parenthesized params like (x default) as single tokens.
tokenize_params <- function(s) {
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
      # Find matching close paren
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
      # Rest parameter: . name
      j <- i + 1L
      # Skip whitespace
      while (j <= n && substr(s, j, j) %in% c(" ", "\t")) j <- j + 1L
      # Read name
      k <- j
      while (k <= n && !substr(s, k, k) %in% c(" ", "\t", ")", "\n")) k <- k + 1L
      tokens <- c(tokens, paste0(".", substr(s, j, k - 1L)))
      i <- k
    } else {
      # Regular token
      j <- i
      while (j <= n && !substr(s, j, j) %in% c(" ", "\t", "(", ")", "\n")) j <- j + 1L
      tokens <- c(tokens, substr(s, i, j - 1L))
      i <- j
    }
  }
  tokens
}

# ---------------------------------------------------------------------------
# Builtins loader
# ---------------------------------------------------------------------------

#' Load documentation for R-defined builtins from builtins-docs.yml.
#'
#' Returns a list keyed by vignette name. Each entry has:
#'   $functions — named list of function entries (same format as parse_rye_annotations output)
#'   $sections  — list of section entries in order: list(name, prose)
load_builtins_docs <- function(path = "tools/docs/builtins-docs.yml") {
  if (!file.exists(path)) return(list())

  raw <- yaml::yaml.load_file(path)
  if (is.null(raw$builtins)) return(list())

  by_vignette <- list()

  for (func_name in names(raw$builtins)) {
    entry <- raw$builtins[[func_name]]
    vname <- entry$vignette
    if (is.null(vname)) next

    if (is.null(by_vignette[[vname]])) {
      by_vignette[[vname]] <- list(functions = list(), sections = list())
    }

    sec_name <- entry$section
    if (!is.null(sec_name)) {
      existing_secs <- vapply(
        by_vignette[[vname]]$sections,
        function(s) s$name, character(1)
      )
      if (length(existing_secs) == 0 || !sec_name %in% existing_secs) {
        by_vignette[[vname]]$sections[[length(by_vignette[[vname]]$sections) + 1L]] <- list(
          name = sec_name, prose = NULL
        )
      }
    }

    # Trim trailing whitespace from multi-line YAML strings
    examples <- entry$examples
    if (!is.null(examples)) examples <- sub("\\s+$", "", examples)

    by_vignette[[vname]]$functions[[func_name]] <- list(
      name         = func_name,
      description  = if (is.null(entry$description)) "" else entry$description,
      signature    = if (is.null(entry$signature)) "" else entry$signature,
      examples     = examples,
      seealso      = entry$seealso,
      note         = entry$note,
      section      = sec_name,
      section_prose = NULL
    )
  }

  by_vignette
}

# ---------------------------------------------------------------------------
# Rmd generator
# ---------------------------------------------------------------------------

#' Generate a single vignette Rmd from parsed annotation data.
#'
#' @param vignette_name Basename of the vignette (e.g., "stdlib-math")
#' @param config Config entry with title, modules, preamble
#' @param all_parsed List of parse results from parse_rye_annotations(), keyed by module name
#' @return Character string with the full Rmd content
generate_rmd <- function(vignette_name, config, all_parsed, func_index = list(),
                         builtin_funcs = NULL) {
  title <- config$title
  preamble <- config$preamble
  if (is.null(preamble)) preamble <- ""
  preamble <- trimws(preamble)

  out <- character()

  # YAML frontmatter (--- must be on line 1 for Rmd title rendering)
  out <- c(out, "---")
  out <- c(out, paste0('title: "', title, '"'))
  out <- c(out, "output:")
  out <- c(out, "  rmarkdown::html_vignette:")
  out <- c(out, "    highlight: null")
  out <- c(out, "vignette: >")
  out <- c(out, paste0("  %\\VignetteIndexEntry{", title, "}"))
  out <- c(out, "  %\\VignetteEngine{knitr::rmarkdown}")
  out <- c(out, "  %\\VignetteEncoding{UTF-8}")
  out <- c(out, "---")
  out <- c(out, "")
  out <- c(out, AUTOGEN_HEADER)
  out <- c(out, "```{r setup, include = FALSE}")
  out <- c(out, 'knitr::opts_chunk$set(collapse = TRUE, comment = "#>")')
  out <- c(out, "```")
  out <- c(out, "")

  if (nchar(preamble) > 0) {
    out <- c(out, preamble)
    out <- c(out, "")
  }

  # Collect all functions across modules, preserving order
  all_funcs <- list()
  all_sections <- list()
  for (mod_name in config$modules) {
    parsed <- all_parsed[[mod_name]]
    if (is.null(parsed)) {
      message("Warning: No parsed data for module '", mod_name, "'")
      next
    }
    for (sec in parsed$sections) {
      all_sections[[length(all_sections) + 1L]] <- sec
    }
    for (fn in parsed$functions) {
      all_funcs[[length(all_funcs) + 1L]] <- fn
    }
  }

  # Merge R-defined builtin entries into the appropriate sections
  if (!is.null(builtin_funcs) && length(builtin_funcs$functions) > 0) {
    # Add new sections that don't already exist
    existing_sec_names <- vapply(all_sections, function(s) s$name, character(1))
    for (bsec in builtin_funcs$sections) {
      if (!bsec$name %in% existing_sec_names) {
        all_sections[[length(all_sections) + 1L]] <- bsec
      }
    }
    # Insert each builtin after the last function in its section (or at end)
    for (bfn in builtin_funcs$functions) {
      insert_pos <- length(all_funcs) + 1L
      for (idx in rev(seq_along(all_funcs))) {
        if (identical(all_funcs[[idx]]$section, bfn$section)) {
          insert_pos <- idx + 1L
          break
        }
      }
      if (insert_pos > length(all_funcs)) {
        all_funcs[[length(all_funcs) + 1L]] <- bfn
      } else {
        all_funcs <- append(all_funcs, list(bfn), after = insert_pos - 1L)
      }
    }
  }

  # Group functions by section and emit
  current_section <- NULL
  for (idx in seq_along(all_funcs)) {
    fn <- all_funcs[[idx]]

    # Emit section header if changed
    if (!identical(fn$section, current_section)) {
      current_section <- fn$section
      if (!is.null(current_section)) {
        out <- c(out, paste0("## ", current_section))
        out <- c(out, "")
        # Find section prose
        for (sec in all_sections) {
          if (sec$name == current_section && !is.null(sec$prose) && nchar(trimws(sec$prose)) > 0) {
            out <- c(out, trimws(sec$prose))
            out <- c(out, "")
            break
          }
        }
      }
    }

    # Emit function entry with explicit anchor
    slug <- slugify_func_name(fn$name)
    out <- c(out, paste0("### ", fn$name, " {#", slug, "}"))
    out <- c(out, "")

    if (nchar(fn$description) > 0) {
      out <- c(out, fn$description)
      out <- c(out, "")
    }

    if (nchar(fn$signature) > 0) {
      out <- c(out, paste0("**Signature:** `", fn$signature, "`"))
      out <- c(out, "")
    }

    if (!is.null(fn$examples) && nchar(fn$examples) > 0) {
      out <- c(out, "**Examples:**")
      out <- c(out, "```lisp")
      out <- c(out, fn$examples)
      out <- c(out, "```")
      out <- c(out, "")
    }

    if (!is.null(fn$note) && nchar(fn$note) > 0) {
      out <- c(out, paste0("**Note:** ", fn$note))
      out <- c(out, "")
    }

    if (!is.null(fn$seealso) && nchar(fn$seealso) > 0) {
      linked_seealso <- linkify_seealso(fn$seealso, func_index, vignette_name)
      out <- c(out, paste0("**See also:** ", linked_seealso))
      out <- c(out, "")
    }

    out <- c(out, "---")
    out <- c(out, "")
  }

  # Emit any standalone sections that had no associated functions
  emitted_sections <- unique(vapply(all_funcs, function(fn) {
    if (is.null(fn$section)) "" else fn$section
  }, character(1)))
  for (sec in all_sections) {
    if (!sec$name %in% emitted_sections) {
      out <- c(out, paste0("## ", sec$name))
      out <- c(out, "")
      if (!is.null(sec$prose) && nchar(trimws(sec$prose)) > 0) {
        out <- c(out, trimws(sec$prose))
        out <- c(out, "")
      }
    }
  }

  paste(out, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

#' Generate all stdlib documentation vignettes.
#'
#' @param config_path Path to stdlib-docs.yml
#' @param rye_dir Path to inst/rye/ directory
#' @param output_dir Path to vignettes/ directory
generate_all <- function(
  config_path = "tools/docs/stdlib-docs.yml",
  rye_dir = "inst/rye",
  output_dir = "vignettes"
) {
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }
  if (!dir.exists(rye_dir)) {
    stop("Rye source directory not found: ", rye_dir)
  }
  if (!dir.exists(output_dir)) {
    stop("Output directory not found: ", output_dir)
  }

  config <- yaml::yaml.load_file(config_path)
  vignettes <- config$vignettes

  # Collect all module names needed
  all_modules <- unique(unlist(lapply(vignettes, function(v) v$modules)))
  message("Modules to process: ", paste(all_modules, collapse = ", "))

  # Parse all needed .rye files
  all_parsed <- list()
  for (mod_name in all_modules) {
    rye_file <- file.path(rye_dir, paste0(mod_name, ".rye"))
    if (!file.exists(rye_file)) {
      message("Warning: Module file not found: ", rye_file)
      next
    }
    message("Parsing: ", rye_file)
    all_parsed[[mod_name]] <- parse_rye_annotations(rye_file)
  }

  # Check for undocumented exports
  check_undocumented(all_parsed, rye_dir, all_modules)

  # Load R-defined builtin documentation
  builtins_path <- file.path(dirname(config_path), "builtins-docs.yml")
  builtins_by_vignette <- load_builtins_docs(builtins_path)
  n_builtins <- sum(vapply(builtins_by_vignette, function(v) length(v$functions), integer(1)))
  if (n_builtins > 0) {
    message("Loaded ", n_builtins, " R-defined builtin entries from ", builtins_path)
  }

  # Build global function index for cross-vignette links
  func_index <- build_func_index(vignettes, all_parsed)
  # Add builtins to the func index
  for (vname in names(builtins_by_vignette)) {
    for (fn in builtins_by_vignette[[vname]]$functions) {
      func_index[[fn$name]] <- list(
        vignette = vname,
        slug     = slugify_func_name(fn$name)
      )
    }
  }
  message("Indexed ", length(func_index), " functions for cross-linking")

  # Generate each vignette
  for (vname in names(vignettes)) {
    vconfig <- vignettes[[vname]]
    message("Generating: ", vname, ".Rmd")
    rmd <- generate_rmd(vname, vconfig, all_parsed, func_index,
                        builtin_funcs = builtins_by_vignette[[vname]])
    output_file <- file.path(output_dir, paste0(vname, ".Rmd"))
    writeLines(rmd, output_file, useBytes = TRUE)
    message("  Wrote: ", output_file)
  }

  message("Done. Generated ", length(vignettes), " vignettes.")
  invisible(NULL)
}

#' Check for exported functions that don't have ;;' annotations.
check_undocumented <- function(all_parsed, rye_dir, modules) {
  for (mod_name in modules) {
    rye_file <- file.path(rye_dir, paste0(mod_name, ".rye"))
    if (!file.exists(rye_file)) next

    exports <- .doc_parser$get_exports(rye_file)
    parsed <- all_parsed[[mod_name]]
    if (is.null(parsed)) next

    documented <- names(parsed$functions)
    undocumented <- setdiff(exports, documented)
    if (length(undocumented) > 0) {
      message("  Warning: Undocumented exports in ", mod_name, ": ",
              paste(undocumented, collapse = ", "))
    }
  }
}

#' Extract exported symbol names from module source text.
extract_exports <- function(text) {
  # Strip comments (respecting strings)
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

  # Find (export ...) form
  exp_match <- regexpr("\\(export\\s+", clean_text, perl = TRUE)
  if (exp_match == -1L) return(character())

  start <- exp_match + attr(exp_match, "match.length")
  # Find matching paren
  depth <- 1L
  i <- as.integer(exp_match)
  # Skip past "(export "
  i <- as.integer(start)
  n <- nchar(clean_text)
  while (i <= n && depth > 0L) {
    ch <- substr(clean_text, i, i)
    if (ch == "(") depth <- depth + 1L
    else if (ch == ")") depth <- depth - 1L
    i <- i + 1L
  }
  if (depth != 0L) return(character())

  exp_body <- substr(clean_text, start, i - 2L)
  exp_body <- gsub("\\s+", " ", exp_body)

  # Extract symbols
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

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------

if (!interactive()) {
  generate_all()
}
