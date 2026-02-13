#!/usr/bin/env Rscript
# generate-stdlib-docs.R — Auto-generate stdlib reference vignettes from .arl source files.
#
# Usage: Rscript tools/docs/generate-stdlib-docs.R
#        make stdlib-docs
#
# Reads tools/docs/stdlib-docs.dcf for module-to-vignette mapping, parses ;;' annotations
# from inst/arl/*.arl, and writes vignettes/stdlib-*.Rmd files.

# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------

# Load DocParser (shared annotation parser)
if (requireNamespace("arl", quietly = TRUE)) {
  .doc_parser <- arl:::DocParser$new()
} else {
  # Fallback: source directly when running outside installed package
  source(file.path("R", "doc-parser.R"))
  .doc_parser <- DocParser$new()
}

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

AUTOGEN_HEADER <- paste0(
  "<!-- AUTO-GENERATED from inst/arl/ source files. Do not edit manually. -->\n",
  "<!-- Regenerate with: make stdlib-docs -->\n"
)

# ---------------------------------------------------------------------------
# Cross-referencing helpers
# ---------------------------------------------------------------------------

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
          slug     = .doc_parser$slugify(fn$name)
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
# Builtins grouping
# ---------------------------------------------------------------------------

#' Group flat builtin entries (from DocParser$load_builtins) by vignette.
#'
#' Returns a list keyed by vignette name.  Each entry has:
#'   $functions — named list of function doc entries
#'   $sections  — list of unique section entries in order: list(name, prose)
group_builtins_by_vignette <- function(builtins) {
  by_vignette <- list()
  for (fn in builtins) {
    vname <- fn$vignette
    if (is.null(vname)) next

    if (is.null(by_vignette[[vname]])) {
      by_vignette[[vname]] <- list(functions = list(), sections = list())
    }

    if (!is.null(fn$section)) {
      existing_secs <- vapply(
        by_vignette[[vname]]$sections,
        function(s) s$name, character(1)
      )
      if (length(existing_secs) == 0 || !fn$section %in% existing_secs) {
        by_vignette[[vname]]$sections[[length(by_vignette[[vname]]$sections) + 1L]] <- list(
          name = fn$section, prose = NULL
        )
      }
    }

    by_vignette[[vname]]$functions[[fn$name]] <- fn
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
#' @param all_parsed List of parse results from parse_arl_annotations(), keyed by module name
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
  out <- c(out, "output: arl::arl_html_vignette")
  out <- c(out, "pkgdown:")
  out <- c(out, "  as_is: true")
  out <- c(out, "vignette: >")
  out <- c(out, paste0("  %\\VignetteIndexEntry{", title, "}"))
  out <- c(out, "  %\\VignetteEngine{knitr::rmarkdown}")
  out <- c(out, "  %\\VignetteEncoding{UTF-8}")
  out <- c(out, "---")
  out <- c(out, "")
  out <- c(out, AUTOGEN_HEADER)
  out <- c(out, "```{r setup, include = FALSE}")
  out <- c(out, 'knitr::opts_chunk$set(collapse = TRUE, comment = "#>")')
  out <- c(out, "arl::register_knitr_engine()")
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
      if (isTRUE(fn$internal)) next
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
  emitted_slugs <- character(0)
  for (idx in seq_along(all_funcs)) {
    fn <- all_funcs[[idx]]

    # Emit section header if changed
    if (!identical(fn$section, current_section)) {
      current_section <- fn$section
      if (!is.null(current_section)) {
        section_slug <- paste0("section-", gsub("[^a-z0-9]+", "-", tolower(current_section)))
        section_slug <- gsub("-+", "-", section_slug)
        section_slug <- gsub("^-|-$", "", section_slug)
        if (section_slug %in% emitted_slugs) {
          n_dup <- sum(emitted_slugs == section_slug)
          section_slug <- paste0(section_slug, "-", n_dup + 1L)
        }
        emitted_slugs <- c(emitted_slugs, section_slug)
        out <- c(out, paste0("## ", current_section, " {#", section_slug, "}"))
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
    slug <- .doc_parser$slugify(fn$name)
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
      if (isTRUE(fn$noeval)) {
        out <- c(out, "```{arl, eval=FALSE}")
      } else {
        out <- c(out, "```{arl}")
      }
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
      sec_slug <- paste0("section-", gsub("[^a-z0-9]+", "-", tolower(sec$name)))
      sec_slug <- gsub("-+", "-", sec_slug)
      sec_slug <- gsub("^-|-$", "", sec_slug)
      out <- c(out, paste0("## ", sec$name, " {#", sec_slug, "}"))
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
# Overview page generator (stdlib-reference.Rmd)
# ---------------------------------------------------------------------------

GITHUB_BASE <- "https://github.com/wwbrannon/arl/blob/main"

#' List of .arl module source files in load order, with optional descriptions.
MODULE_SOURCE_FILES <- list(
  list(file = "_r.arl",          desc = "R operator/function aliases, internal"),
  list(file = "core.arl",        desc = NULL),
  list(file = "list.arl",        desc = NULL),
  list(file = "types.arl",       desc = "type predicates"),
  list(file = "logic.arl",       desc = "logical operations"),
  list(file = "conversions.arl", desc = "type conversions"),
  list(file = "equality.arl",    desc = "equality and S3 dispatch"),
  list(file = "control.arl",     desc = NULL),
  list(file = "functional.arl",  desc = NULL),
  list(file = "sequences.arl",   desc = NULL),
  list(file = "sort.arl",        desc = NULL),
  list(file = "struct.arl",      desc = NULL),
  list(file = "error.arl",       desc = NULL),
  list(file = "threading.arl",   desc = NULL),
  list(file = "binding.arl",     desc = NULL),
  list(file = "looping.arl",     desc = NULL),
  list(file = "dict.arl",        desc = NULL),
  list(file = "math.arl",        desc = "includes numeric type predicates"),
  list(file = "set.arl",         desc = NULL),
  list(file = "strings.arl",     desc = NULL),
  list(file = "display.arl",     desc = NULL),
  list(file = "io.arl",          desc = NULL),
  list(file = "assert.arl",      desc = NULL),
  list(file = "r-interop.arl",   desc = NULL)
)

#' Collect all exported function names for a vignette, combining .arl exports
#' and R-defined builtins.  Returns a character vector (preserving export order,
#' with internal names starting with __ filtered out).
collect_vignette_exports <- function(vconfig, arl_dir, builtins_functions) {
  exports <- character()
  for (mod_name in vconfig$modules) {
    arl_file <- file.path(arl_dir, paste0(mod_name, ".arl"))
    if (file.exists(arl_file)) {
      mod_exports <- .doc_parser$get_exports(arl_file)
      mod_exports <- mod_exports[!grepl("^__", mod_exports)]
      exports <- c(exports, mod_exports)
    }
  }
  # Append R-defined builtins for this vignette
  if (!is.null(builtins_functions) && length(builtins_functions$functions) > 0) {
    exports <- c(exports, names(builtins_functions$functions))
  }
  unique(exports)
}

#' Format a character vector of function names as backtick-wrapped,
#' comma-separated inline code, wrapping at ~80 characters.
format_func_list <- function(func_names) {
  items <- paste0("`", func_names, "`")
  lines <- character()
  current <- ""
  for (item in items) {
    candidate <- if (nchar(current) == 0) item else paste0(current, ", ", item)
    if (nchar(candidate) > 78 && nchar(current) > 0) {
      lines <- c(lines, paste0(current, ","))
      current <- item
    } else {
      current <- candidate
    }
  }
  if (nchar(current) > 0) lines <- c(lines, current)
  paste(lines, collapse = "\n")
}

#' Generate the stdlib-reference.Rmd overview page.
generate_reference_rmd <- function(vignettes, arl_dir, builtins_by_vignette) {
  out <- character()

  # YAML frontmatter
  out <- c(out, "---")
  out <- c(out, 'title: "Standard Library Overview"')
  out <- c(out, "output: arl::arl_html_vignette")
  out <- c(out, "pkgdown:")
  out <- c(out, "  as_is: true")
  out <- c(out, "vignette: >")
  out <- c(out, "  %\\VignetteIndexEntry{Standard Library Overview}")
  out <- c(out, "  %\\VignetteEngine{knitr::rmarkdown}")
  out <- c(out, "  %\\VignetteEncoding{UTF-8}")
  out <- c(out, "---")
  out <- c(out, "")
  out <- c(out, AUTOGEN_HEADER)
  out <- c(out, "```{r setup, include = FALSE}")
  out <- c(out, 'knitr::opts_chunk$set(collapse = TRUE, comment = "#>")')
  out <- c(out, "arl::register_knitr_engine()")
  out <- c(out, "```")
  out <- c(out, "")

  # Intro paragraph
  out <- c(out, "Arl's standard library has two layers:")
  out <- c(out, "")
  out <- c(out, "1. **Built-in functions** defined in R (`R/engine.R`). These are low-level")
  out <- c(out, "   primitives that need direct access to engine internals — cons-cell")
  out <- c(out, "   operations, the macro expander, the evaluator, promise handling, and")
  out <- c(out, "   documentation helpers. They are always available, even when the stdlib")
  out <- c(out, "   modules are not loaded (`Engine$new(load_stdlib = FALSE)`).")
  out <- c(out, "2. **Stdlib modules** written in Arl (`inst/arl/*.arl`). These provide the")
  out <- c(out, "   bulk of the standard library: list operations, math, strings, control flow,")
  out <- c(out, "   and everything else. Modules are loaded in dependency order (each module")
  out <- c(out, "   declares its dependencies with `(import ...)` and is loaded after the")
  out <- c(out, "   modules it imports).")
  out <- c(out, "")

  # Links to detailed pages
  out <- c(out, "For the full, per-function reference, see the individual stdlib reference pages:")
  out <- c(out, "")
  for (vname in names(vignettes)) {
    vconfig <- vignettes[[vname]]
    out <- c(out, paste0("- [", vconfig$title, "](", vname, ".html)"))
  }
  out <- c(out, "")

  # Importing modules section
  out <- c(out, "## Importing modules")
  out <- c(out, "")
  out <- c(out, "All stdlib modules are loaded automatically by `Engine$new()`. The `import`")
  out <- c(out, "form is useful inside your own modules (where you start with an empty scope)")
  out <- c(out, "and when working with a bare engine (`Engine$new(load_stdlib = FALSE)`):")
  out <- c(out, "")
  out <- c(out, "```{arl, eval=FALSE}")
  out <- c(out, "; Import focused modules")
  out <- c(out, "(import control)   ; when/unless/cond/case/try*")
  out <- c(out, "(import binding)   ; let/let*/letrec")
  out <- c(out, "(import looping)   ; for/loop/recur/until")
  out <- c(out, "(import threading) ; -> and ->>")
  out <- c(out, "(import error)     ; try/catch/finally")
  out <- c(out, "```")
  out <- c(out, "")
  out <- c(out, "From R, you can create an engine with the stdlib already loaded:")
  out <- c(out, "")
  out <- c(out, "```r")
  out <- c(out, "engine <- Engine$new()                   # all stdlib loaded")
  out <- c(out, "bare <- Engine$new(load_stdlib=FALSE)    # builtins only")
  out <- c(out, "```")
  out <- c(out, "")

  # Per-vignette sections with auto-generated function lists
  for (vname in names(vignettes)) {
    vconfig <- vignettes[[vname]]
    # Strip "Standard Library: " prefix for section heading
    heading <- sub("^Standard Library:\\s*", "", vconfig$title)
    out <- c(out, paste0("## [", heading, "](", vname, ".html)"))
    out <- c(out, "")
    if (nchar(vconfig$summary) > 0) {
      out <- c(out, vconfig$summary)
      out <- c(out, "")
    }
    exports <- collect_vignette_exports(
      vconfig, arl_dir, builtins_by_vignette[[vname]]
    )
    if (length(exports) > 0) {
      out <- c(out, format_func_list(exports))
      out <- c(out, "")
    }
  }

  # Built-in functions section
  out <- c(out, "## Built-in functions")
  out <- c(out, "")
  out <- c(out, "The following functions are implemented in R")
  out <- c(out, paste0(
    "([`R/engine.R`](", GITHUB_BASE, "/R/engine.R)) rather than in Arl source"
  ))
  out <- c(out, "modules. They are available even on a bare engine")
  out <- c(out, "(`Engine$new(load_stdlib = FALSE)`).")
  out <- c(out, "")
  out <- c(out, "| Category | Functions |")
  out <- c(out, "|----------|-----------|")
  out <- c(out, "| Cons cells | `pair?`|")
  out <- c(out, "| Macros | `gensym`, `capture`, `macro?`, `macroexpand` |")
  out <- c(out, "| Evaluation | `eval`, `read`, `write`, `r/eval` |")
  out <- c(out, "| Environments | `toplevel-env`, `current-env` |")
  out <- c(out, "| Promises | `promise?`, `force`, `promise-expr` |")
  out <- c(out, "| Documentation | `doc!`, `doc` |")
  out <- c(out, "")
  out <- c(out, "These builtins are documented alongside the stdlib functions they relate to")
  out <- c(out, "in the individual reference pages above.")
  out <- c(out, "")

  # Source files section
  out <- c(out, "## Source files")
  out <- c(out, "")
  out <- c(out, "Built-in functions are defined in")
  out <- c(out, paste0(
    "[`R/engine.R`](", GITHUB_BASE, "/R/engine.R). The"
  ))
  out <- c(out, "Arl stdlib modules are organized by topic in")
  out <- c(out, paste0(
    "[`inst/arl/`](", gsub("/blob/", "/tree/", GITHUB_BASE), "/inst/arl) (each file"
  ))
  out <- c(out, "defines a module). The engine loads these modules in dependency order when")
  out <- c(out, "initializing.")
  out <- c(out, "")
  for (entry in MODULE_SOURCE_FILES) {
    link <- paste0("[`", entry$file, "`](", GITHUB_BASE, "/inst/arl/", entry$file, ")")
    if (!is.null(entry$desc)) {
      out <- c(out, paste0("- ", link, " (", entry$desc, ")"))
    } else {
      out <- c(out, paste0("- ", link))
    }
  }
  out <- c(out, "")
  out <- c(out, "If you're looking for implementation details, these files are the source of")
  out <- c(out, "truth for the stdlib definitions.")

  paste(out, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

#' Generate all stdlib documentation vignettes.
#'
#' @param config_path Path to stdlib-docs.dcf
#' @param arl_dir Path to inst/arl/ directory
#' @param output_dir Path to vignettes/ directory
generate_all <- function(
  config_path = "tools/docs/stdlib-docs.dcf",
  arl_dir = "inst/arl",
  output_dir = "vignettes"
) {
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }
  if (!dir.exists(arl_dir)) {
    stop("Arl source directory not found: ", arl_dir)
  }
  if (!dir.exists(output_dir)) {
    stop("Output directory not found: ", output_dir)
  }

  # Read DCF config (strip comment lines before parsing)
  lines <- readLines(config_path, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines)]
  m <- read.dcf(textConnection(paste(lines, collapse = "\n")))

  vignettes <- list()
  for (i in seq_len(nrow(m))) {
    vname <- m[i, "Name"]
    modules <- trimws(strsplit(m[i, "Modules"], ",")[[1]])
    preamble_val <- m[i, "Preamble"]
    preamble <- if (is.na(preamble_val) || !nzchar(preamble_val)) "" else preamble_val
    summary_val <- m[i, "Summary"]
    summary <- if (is.na(summary_val) || !nzchar(summary_val)) "" else summary_val
    vignettes[[vname]] <- list(
      title   = m[i, "Title"],
      modules = modules,
      preamble = preamble,
      summary = summary
    )
  }

  # Collect all module names needed
  all_modules <- unique(unlist(lapply(vignettes, function(v) v$modules)))
  message("Modules to process: ", paste(all_modules, collapse = ", "))

  # Parse all needed .arl files
  all_parsed <- list()
  for (mod_name in all_modules) {
    arl_file <- file.path(arl_dir, paste0(mod_name, ".arl"))
    if (!file.exists(arl_file)) {
      message("Warning: Module file not found: ", arl_file)
      next
    }
    message("Parsing: ", arl_file)
    all_parsed[[mod_name]] <- .doc_parser$parse_file(arl_file)
  }

  # Check for undocumented exports
  check_undocumented(all_parsed, arl_dir, all_modules)

  # Load R-defined builtin documentation
  builtins_path <- file.path("inst", "builtins-docs.dcf")
  all_builtins <- .doc_parser$load_builtins(builtins_path)
  if (length(all_builtins) > 0) {
    message("Loaded ", length(all_builtins), " R-defined builtin entries from ", builtins_path)
  }
  builtins_by_vignette <- group_builtins_by_vignette(all_builtins)

  # Build global function index for cross-vignette links
  func_index <- build_func_index(vignettes, all_parsed)
  # Add builtins to the func index
  for (fn in all_builtins) {
    func_index[[fn$name]] <- list(
      vignette = fn$vignette,
      slug     = .doc_parser$slugify(fn$name)
    )
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

  # Generate the overview page (stdlib-reference.Rmd)
  message("Generating: stdlib-reference.Rmd")
  ref_rmd <- generate_reference_rmd(vignettes, arl_dir, builtins_by_vignette)
  ref_file <- file.path(output_dir, "stdlib-reference.Rmd")
  writeLines(ref_rmd, ref_file, useBytes = TRUE)
  message("  Wrote: ", ref_file)

  message("Done. Generated ", length(vignettes) + 1L, " vignettes.")
  invisible(NULL)
}

#' Check for exported functions that don't have ;;' annotations.
check_undocumented <- function(all_parsed, arl_dir, modules) {
  for (mod_name in modules) {
    arl_file <- file.path(arl_dir, paste0(mod_name, ".arl"))
    if (!file.exists(arl_file)) next

    exports <- .doc_parser$get_exports(arl_file)
    parsed <- all_parsed[[mod_name]]
    if (is.null(parsed)) next

    documented <- names(parsed$functions)
    undocumented <- setdiff(exports, documented)
    undocumented <- undocumented[!grepl("^__", undocumented)]
    if (length(undocumented) > 0) {
      message("  Warning: Undocumented exports in ", mod_name, ": ",
              paste(undocumented, collapse = ", "))
    }
  }
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------

if (!interactive()) {
  generate_all()
}
