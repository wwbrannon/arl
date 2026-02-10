#!/usr/bin/env Rscript
# generate-stdlib-docs.R — Auto-generate stdlib reference vignettes from .rye source files.
#
# Usage: Rscript tools/generate-stdlib-docs.R
#        make stdlib-docs
#
# Reads tools/stdlib-docs.yml for module-to-vignette mapping, parses ;;' annotations
# from inst/rye/*.rye, and writes vignettes/stdlib-*.Rmd files.

# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required. Install with: install.packages('yaml')")
}

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

AUTOGEN_HEADER <- paste0(
  "<!-- AUTO-GENERATED from inst/rye/ source files. Do not edit manually. -->\n",
  "<!-- Regenerate with: make stdlib-docs -->\n"
)

# ---------------------------------------------------------------------------
# Annotation parser
# ---------------------------------------------------------------------------

#' Parse ;;' annotations from a .rye source file.
#'
#' Returns a list with two components:
#'   $functions — named list keyed by function name, each with:
#'     name, description, signature, examples, seealso, note, section, section_prose
#'   $sections — list of section entries in order: list(name, prose)
#'   $file — basename of the source file
parse_rye_annotations <- function(file) {
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
# Rmd generator
# ---------------------------------------------------------------------------

#' Generate a single vignette Rmd from parsed annotation data.
#'
#' @param vignette_name Basename of the vignette (e.g., "stdlib-math")
#' @param config Config entry with title, modules, preamble
#' @param all_parsed List of parse results from parse_rye_annotations(), keyed by module name
#' @return Character string with the full Rmd content
generate_rmd <- function(vignette_name, config, all_parsed) {
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

    # Emit function entry
    out <- c(out, paste0("### ", fn$name))
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
      out <- c(out, paste0("**See also:** ", fn$seealso))
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
  config_path = "tools/stdlib-docs.yml",
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

  # Generate each vignette
  for (vname in names(vignettes)) {
    vconfig <- vignettes[[vname]]
    message("Generating: ", vname, ".Rmd")
    rmd <- generate_rmd(vname, vconfig, all_parsed)
    output_file <- file.path(output_dir, paste0(vname, ".Rmd"))
    writeLines(rmd, output_file, useBytes = TRUE)
    message("  Wrote: ", output_file)
  }

  message("Done. Generated ", length(vignettes), " vignettes.")
  invisible(NULL)
}

#' Check for exported functions that don't have ;;' annotations.
check_undocumented <- function(all_parsed, rye_dir, modules) {
  # Use a simplified export extraction (similar to FileDeps)
  for (mod_name in modules) {
    rye_file <- file.path(rye_dir, paste0(mod_name, ".rye"))
    if (!file.exists(rye_file)) next

    text <- paste(readLines(rye_file, warn = FALSE), collapse = "\n")
    exports <- extract_exports(text)
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
