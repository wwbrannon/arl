#!/usr/bin/env Rscript
# generate-lang-docs.R — Auto-generate stdlib reference vignettes from .arl source files.
#
# Usage: Rscript tools/docs/generate-lang-docs.R
#        make lang-docs
#
# Reads tools/docs/lang-docs.dcf for module-to-vignette mapping, parses ;;' annotations
# from inst/arl/*.arl, and writes vignettes/lang-*.Rmd files.

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

# Build a docs engine for runtime introspection of stdlib bindings.
new_docs_engine <- function() {
  if (exists("Engine", mode = "environment", inherits = TRUE)) {
    return(get("Engine", mode = "environment", inherits = TRUE)$new())
  }
  if (requireNamespace("arl", quietly = TRUE) &&
      exists("Engine", where = asNamespace("arl"), mode = "environment", inherits = FALSE)) {
    return(get("Engine", envir = asNamespace("arl"))$new())
  }
  stop(
    "Unable to initialize Arl engine for runtime docs introspection. ",
    "Run via `make lang-docs` or install/load the package first."
  )
}

# Collect runtime arl_doc attributes for exported symbols in each module.
collect_runtime_module_docs <- function(modules, arl_dir) {
  engine <- new_docs_engine()
  base_env <- engine$get_env()
  by_module <- list()

  for (mod_name in modules) {
    arl_file <- file.path(arl_dir, paste0(mod_name, ".arl"))
    if (!file.exists(arl_file)) {
      by_module[[mod_name]] <- list()
      next
    }

    exports <- .doc_parser$get_exports(arl_file)
    if (length(exports) == 0) {
      by_module[[mod_name]] <- list()
      next
    }

    mod_env <- new.env(parent = base_env)
    ok <- TRUE
    tryCatch(
      engine$eval_text(sprintf("(import %s)", mod_name), env = mod_env),
      error = function(e) {
        ok <<- FALSE
        message(
          "Warning: could not import module '", mod_name,
          "' for runtime docs: ", conditionMessage(e)
        )
      }
    )
    if (!ok) {
      by_module[[mod_name]] <- list()
      next
    }

    docs <- list()
    for (sym in exports) {
      if (!exists(sym, envir = mod_env, inherits = FALSE)) next
      obj <- get(sym, envir = mod_env, inherits = FALSE)
      doc <- attr(obj, "arl_doc", exact = TRUE)
      if (!is.null(doc)) docs[[sym]] <- doc
    }
    by_module[[mod_name]] <- docs
  }

  by_module
}

# Overlay runtime arl_doc fields onto parsed annotation entries.
merge_runtime_docs <- function(parsed, runtime_docs) {
  if (is.null(parsed)) parsed <- list(functions = list(), sections = list())
  if (is.null(parsed$functions)) parsed$functions <- list()
  if (length(runtime_docs) == 0) return(parsed)

  for (name in names(runtime_docs)) {
    doc <- runtime_docs[[name]]

    if (is.null(parsed$functions[[name]])) {
      parsed$functions[[name]] <- list(
        name = name,
        description = "",
        signature = "",
        examples = NULL,
        assert = NULL,
        seealso = NULL,
        note = NULL,
        internal = FALSE,
        noeval = FALSE,
        section = NULL,
        section_prose = NULL,
        source_line = NA_integer_
      )
    }

    fn <- parsed$functions[[name]]
    if (!is.null(doc$description)) fn$description <- doc$description
    if (!is.null(doc$signature)) fn$signature <- doc$signature
    if (!is.null(doc$examples)) fn$examples <- doc$examples
    if (!is.null(doc$assert)) fn$assert <- doc$assert
    if (!is.null(doc$seealso)) fn$seealso <- doc$seealso
    if (!is.null(doc$note)) fn$note <- doc$note
    if (!is.null(doc$internal)) fn$internal <- isTRUE(doc$internal)
    if (!is.null(doc$noeval)) fn$noeval <- isTRUE(doc$noeval)

    parsed$functions[[name]] <- fn
  }

  parsed
}

# ---------------------------------------------------------------------------
# Topic vignette generator (jinjar-based)
# ---------------------------------------------------------------------------

#' Build template context for a single topic vignette.
#'
#' @param vignette_name Basename of the vignette (e.g., "lang-math")
#' @param config Config entry with title, modules, preamble
#' @param all_parsed List of parse results from parse_arl_annotations(), keyed by module name
#' @param func_index Global function index for cross-linking
#' @param builtin_funcs R-defined builtin entries for this vignette
#' @return Named list suitable for passing to jinjar::render()
build_topic_context <- function(vignette_name, config, all_parsed,
                                func_index = list(), builtin_funcs = NULL) {
  title <- config$title
  preamble <- config$preamble
  if (is.null(preamble)) preamble <- ""
  preamble <- trimws(preamble)
  if (!nzchar(preamble)) preamble <- FALSE

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
    existing_sec_names <- vapply(all_sections, function(s) s$name, character(1))
    for (bsec in builtin_funcs$sections) {
      if (!bsec$name %in% existing_sec_names) {
        all_sections[[length(all_sections) + 1L]] <- bsec
      }
    }
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

  # Build entries list with section tracking
  entries <- list()
  current_section <- NULL
  emitted_slugs <- character(0)

  for (idx in seq_along(all_funcs)) {
    fn <- all_funcs[[idx]]
    entry <- list()

    # Section header info
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

        entry$section_header <- TRUE
        entry$section_name <- current_section
        entry$section_slug <- section_slug

        # Find section prose
        section_prose <- FALSE
        for (sec in all_sections) {
          if (sec$name == current_section && !is.null(sec$prose) &&
              nchar(trimws(sec$prose)) > 0) {
            section_prose <- trimws(sec$prose)
            break
          }
        }
        entry$section_prose <- section_prose
      } else {
        entry$section_header <- FALSE
      }
    } else {
      entry$section_header <- FALSE
    }

    # Function details
    entry$name <- fn$name
    entry$slug <- .doc_parser$slugify(fn$name)
    entry$description <- if (nchar(fn$description) > 0) fn$description else FALSE
    entry$signature <- if (nchar(fn$signature) > 0) fn$signature else FALSE

    # Examples
    entry$has_examples <- !is.null(fn$examples) && nchar(fn$examples) > 0
    entry$examples <- if (entry$has_examples) fn$examples else ""
    entry$noeval <- isTRUE(fn$noeval)

    # Assert
    entry$has_assert <- !is.null(fn$assert) && nchar(fn$assert) > 0
    entry$assert <- if (entry$has_assert) fn$assert else ""

    # Note
    entry$note <- if (!is.null(fn$note) && nchar(fn$note) > 0) fn$note else FALSE

    # See also (pre-linkified)
    if (!is.null(fn$seealso) && nchar(fn$seealso) > 0) {
      entry$seealso <- linkify_seealso(fn$seealso, func_index, vignette_name)
    } else {
      entry$seealso <- FALSE
    }

    entries[[length(entries) + 1L]] <- entry
  }

  # Standalone sections (sections with no associated functions)
  standalone <- list()
  emitted_sections <- unique(vapply(all_funcs, function(fn) {
    if (is.null(fn$section)) "" else fn$section
  }, character(1)))
  for (sec in all_sections) {
    if (!sec$name %in% emitted_sections) {
      sec_slug <- paste0("section-", gsub("[^a-z0-9]+", "-", tolower(sec$name)))
      sec_slug <- gsub("-+", "-", sec_slug)
      sec_slug <- gsub("^-|-$", "", sec_slug)
      prose <- if (!is.null(sec$prose) && nchar(trimws(sec$prose)) > 0) {
        trimws(sec$prose)
      } else {
        FALSE
      }
      standalone[[length(standalone) + 1L]] <- list(
        name = sec$name, slug = sec_slug, prose = prose
      )
    }
  }

  list(
    title = title,
    preamble = preamble,
    entries = entries,
    standalone_sections = standalone
  )
}

#' Render a topic vignette from a context list using the jinjar template.
#'
#' @param context Named list from build_topic_context()
#' @return Character string with the full Rmd content
render_topic_rmd <- function(context) {
  template_path <- file.path("tools", "docs", "templates", "topic.Rmd.jinja")
  template_text <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  config <- jinjar::jinjar_config(trim_blocks = TRUE, lstrip_blocks = TRUE)
  rendered <- do.call(jinjar::render, c(list(template_text, .config = config), context))
  # Strip one trailing newline to match the format expected by writeLines()
  sub("\n$", "", rendered)
}

# ---------------------------------------------------------------------------
# Overview page generator (lang-reference.Rmd)
# ---------------------------------------------------------------------------

GITHUB_BASE <- "https://github.com/wwbrannon/arl/blob/main"

#' List of .arl module source files in load order, with optional descriptions.
MODULE_SOURCE_FILES <- list(
  list(file = "_r.arl",          desc = "R operator/function aliases, internal"),
  list(file = "core.arl",        desc = NULL),
  list(file = "list.arl",        desc = NULL),
  list(file = "types.arl",       desc = "type predicates, numeric type hierarchy"),
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
  list(file = "math.arl",        desc = NULL),
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
format_func_list <- function(func_names, func_index = list()) {
  items <- vapply(func_names, function(fn) {
    entry <- func_index[[fn]]
    if (!is.null(entry)) {
      paste0("[`", fn, "`](", entry$vignette, ".html#", entry$slug, ")")
    } else {
      paste0("`", fn, "`")
    }
  }, character(1), USE.NAMES = FALSE)
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

#' Turn a vector of function names into linked backtick items.
link_builtins <- function(fns, func_index) {
  items <- vapply(fns, function(fn) {
    entry <- func_index[[fn]]
    if (!is.null(entry)) {
      paste0("[`", fn, "`](", entry$vignette, ".html#", entry$slug, ")")
    } else {
      paste0("`", fn, "`")
    }
  }, character(1), USE.NAMES = FALSE)
  paste(items, collapse = ", ")
}

#' Build template context for the lang-reference.Rmd overview page.
#'
#' @param vignettes Named list of vignette configs
#' @param arl_dir Path to inst/arl/ directory
#' @param builtins_by_vignette Builtins grouped by vignette name
#' @param func_index Global function index for cross-linking
#' @return Named list suitable for passing to jinjar::render()
build_reference_context <- function(vignettes, arl_dir, builtins_by_vignette,
                                    func_index = list()) {
  # Vignette links
  vignette_links <- lapply(names(vignettes), function(vname) {
    list(title = vignettes[[vname]]$title, name = vname)
  })

  # Builtin categories with linked function names
  builtin_categories_raw <- list(
    list(cat = "Cons cells",     fns = c("pair?")),
    list(cat = "Macros",         fns = c("gensym", "capture", "macro?", "macroexpand")),
    list(cat = "Evaluation",     fns = c("eval", "read", "write", "r/eval")),
    list(cat = "Environments",   fns = c("toplevel-env", "current-env")),
    list(cat = "Promises",       fns = c("promise?", "force", "promise-expr")),
    list(cat = "Documentation",  fns = c("doc!", "doc"))
  )

  builtin_categories <- lapply(builtin_categories_raw, function(bc) {
    list(cat = bc$cat, functions = link_builtins(bc$fns, func_index))
  })

  # Per-vignette sections
  vignette_sections <- lapply(names(vignettes), function(vname) {
    vconfig <- vignettes[[vname]]
    heading <- sub("^Standard Library:\\s*", "", vconfig$title)
    exports <- collect_vignette_exports(
      vconfig, arl_dir, builtins_by_vignette[[vname]]
    )
    func_list <- if (length(exports) > 0) format_func_list(exports, func_index) else FALSE

    # Module links
    mod_links <- vapply(vconfig$modules, function(mod) {
      fname <- paste0(mod, ".arl")
      paste0("[`", fname, "`](", GITHUB_BASE, "/inst/arl/", fname, ")")
    }, character(1), USE.NAMES = FALSE)
    has_builtins <- !is.null(builtins_by_vignette[[vname]]) &&
      length(builtins_by_vignette[[vname]]$functions) > 0
    if (has_builtins) mod_links <- c(mod_links, "and builtins")
    modules_line <- paste(mod_links, collapse = ", ")

    list(
      heading = heading,
      name = vname,
      summary = if (nchar(vconfig$summary) > 0) vconfig$summary else FALSE,
      func_list = func_list,
      modules_line = modules_line
    )
  })

  # Source files (pre-formatted strings)
  source_files <- vapply(MODULE_SOURCE_FILES, function(entry) {
    link <- paste0("[`", entry$file, "`](", GITHUB_BASE, "/inst/arl/", entry$file, ")")
    if (!is.null(entry$desc)) {
      paste0("- ", link, " (", entry$desc, ")")
    } else {
      paste0("- ", link)
    }
  }, character(1))

  list(
    github_base = GITHUB_BASE,
    github_tree = gsub("/blob/", "/tree/", GITHUB_BASE),
    vignette_links = vignette_links,
    builtin_categories = builtin_categories,
    vignette_sections = vignette_sections,
    source_files = source_files
  )
}

#' Render the lang-reference.Rmd overview page from a context list.
#'
#' @param context Named list from build_reference_context()
#' @return Character string with the full Rmd content
render_reference_rmd <- function(context) {
  template_path <- file.path("tools", "docs", "templates", "reference.Rmd.jinja")
  template_text <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  config <- jinjar::jinjar_config(trim_blocks = TRUE, lstrip_blocks = TRUE)
  rendered <- do.call(jinjar::render, c(list(template_text, .config = config), context))
  sub("\n$", "", rendered)
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

#' Generate all stdlib documentation vignettes.
#'
#' @param config_path Path to lang-docs.dcf
#' @param arl_dir Path to inst/arl/ directory
#' @param output_dir Path to vignettes/ directory
generate_all <- function(
  config_path = "tools/docs/lang-docs.dcf",
  arl_dir = "inst/arl",
  output_dir = "vignettes"
) {
  if (!requireNamespace("jinjar", quietly = TRUE)) {
    stop("Package 'jinjar' is required for doc generation. ",
         "Install it with: install.packages('jinjar')")
  }

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
    preamble_val <- if ("Preamble" %in% colnames(m)) m[i, "Preamble"] else NA
    preamble <- if (is.na(preamble_val) || !nzchar(preamble_val)) "" else preamble_val
    summary_val <- if ("Summary" %in% colnames(m)) m[i, "Summary"] else NA
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

  # Overlay runtime docs (arl_doc) from imported stdlib modules.
  runtime_docs_by_module <- collect_runtime_module_docs(all_modules, arl_dir)
  for (mod_name in all_modules) {
    all_parsed[[mod_name]] <- merge_runtime_docs(
      all_parsed[[mod_name]],
      runtime_docs_by_module[[mod_name]]
    )
  }

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
    ctx <- build_topic_context(vname, vconfig, all_parsed, func_index,
                               builtin_funcs = builtins_by_vignette[[vname]])
    rmd <- render_topic_rmd(ctx)
    output_file <- file.path(output_dir, paste0(vname, ".Rmd"))
    writeLines(rmd, output_file, useBytes = TRUE)
    message("  Wrote: ", output_file)
  }

  # Generate the overview page (lang-reference.Rmd)
  message("Generating: lang-reference.Rmd")
  ctx <- build_reference_context(vignettes, arl_dir, builtins_by_vignette,
                                 func_index = func_index)
  ref_rmd <- render_reference_rmd(ctx)
  ref_file <- file.path(output_dir, "lang-reference.Rmd")
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
