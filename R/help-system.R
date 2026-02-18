# HelpSystem: Dispatches (help topic) to language special-form docs, macro docstrings,
# env docs, or structured R help.
#
# @field env Env (for default env in help()).
# @field macro_expander MacroExpander (for macro docstrings and usage).
#
#' @keywords internal
#' @noRd
HelpSystem <- R6::R6Class(
  "ArlHelpSystem",
  public = list(
    env = NULL,
    macro_expander = NULL,
    # @description Create help system. Builds specials_help from build_specials_help().
    # @param env Env instance.
    # @param macro_expander MacroExpander instance.
    initialize = function(env, macro_expander) {
      self$env <- env
      self$macro_expander <- macro_expander
      private$specials_help <- private$build_specials_help()
    },
    # @description Show help for topic in the default env (self$env$env).
    # @param topic Symbol or string topic name.
    # @param package Optional package name for R help lookup.
    help = function(topic, package = NULL) {
      self$help_in_env(topic, self$env$env, package = package)
    },
    # @description Show help for topic in the given env. Tries Arl docs first,
    # then falls back to structured R help.
    # @param topic Symbol or string (length 1).
    # @param env Env or R environment.
    # @param package Optional package name for R help lookup.
    help_in_env = function(topic, env, package = NULL) {
      if (is.symbol(topic)) {
        topic <- as.character(topic)
      }
      if (!is.character(topic) || length(topic) != 1 || !nzchar(topic)) {
        stop("help requires a symbol or string")
      }
      if (is.symbol(package)) {
        package <- as.character(package)
      }
      if (!is.null(package) &&
          (!is.character(package) || length(package) != 1 || !nzchar(package))) {
        stop("help package must be a non-empty symbol or string")
      }

      if (inherits(env, "ArlEnv")) {
        env <- env$env
      } else if (is.null(env)) {
        env <- self$env$env
      }
      if (!is.environment(env)) {
        stop("Expected a Env or environment")
      }
      target_env <- env

      fallback_usage <- NULL
      if (is.null(package)) {
        doc <- private$specials_help[[topic]]
        if (!is.null(doc)) {
          private$print_doc(topic, doc)
          return(invisible(NULL))
        }

        macro_symbol <- as.symbol(topic)
        if (self$macro_expander$is_macro(macro_symbol, env = target_env)) {
          macro_fn <- self$macro_expander$get_macro(macro_symbol, env = target_env)
          macro_doc <- attr(macro_fn, "arl_doc", exact = TRUE)
          usage <- private$usage_from_macro(macro_fn, topic)
          if (!is.null(macro_doc)) {
            if (is.character(macro_doc)) {
              macro_doc <- list(description = paste(macro_doc, collapse = "\n"))
            }
            if (is.null(macro_doc$usage) && !is.null(usage)) {
              macro_doc$usage <- usage
            }
            private$print_doc(topic, macro_doc)
            return(invisible(NULL))
          }
          if (!is.null(usage)) {
            private$print_doc(topic, list(usage = usage))
            return(invisible(NULL))
          }
        }

        if (exists(topic, envir = target_env, inherits = TRUE)) {
          binding_env <- private$find_binding_env(topic, target_env)
          obj <- get(topic, envir = target_env, inherits = TRUE)
          obj_doc <- attr(obj, "arl_doc", exact = TRUE)
          usage <- NULL
          if (inherits(obj, "arl_closure")) {
            usage <- private$usage_from_closure(obj, topic)
          } else if (is.function(obj)) {
            usage <- private$usage_from_formals(obj, topic)
          }
          if (!is.null(obj_doc)) {
            if (is.character(obj_doc)) {
              obj_doc <- list(description = paste(obj_doc, collapse = "\n"))
            }
            if (is.null(obj_doc$usage) && !is.null(usage)) {
              obj_doc$usage <- usage
            }
            private$print_doc(topic, obj_doc)
            return(invisible(NULL))
          }
          if (!is.null(usage) && !is.null(binding_env) && identical(binding_env, target_env)) {
            private$print_doc(topic, list(usage = usage))
            return(invisible(NULL))
          }
          if (inherits(obj, "arl_closure") && !is.null(usage)) {
            private$print_doc(topic, list(usage = usage))
            return(invisible(NULL))
          }
          if (!is.null(usage)) {
            fallback_usage <- usage
          }
        }
      }

      r_doc <- private$r_help_doc(topic, package = package)
      if (!is.null(r_doc)) {
        private$print_doc(topic, r_doc)
        return(invisible(NULL))
      }
      if (!is.null(fallback_usage)) {
        private$print_doc(topic, list(usage = fallback_usage))
        return(invisible(NULL))
      }
      private$print_help_not_found(topic, package = package)
      invisible(NULL)
    }
  ),
  private = list(
    specials_help = NULL,
    load_special_form_docs = function() {
      dcf_path <- system.file("reference-docs.dcf", package = "arl")
      if (!nzchar(dcf_path) || !file.exists(dcf_path)) {
        return(list())
      }
      m <- tryCatch(read_dcf_with_comments(dcf_path), error = function(e) NULL)
      if (is.null(m) || nrow(m) == 0) {
        return(list())
      }

      docs <- list()
      for (i in seq_len(nrow(m))) {
        kind <- if ("Kind" %in% colnames(m)) m[i, "Kind"] else NA
        if (is.na(kind) || !nzchar(kind) || tolower(trimws(kind)) != "special-form") {
          next
        }
        topic <- m[i, "Name"]
        if (is.na(topic) || !nzchar(topic)) next

        read_field <- function(field) {
          if (!field %in% colnames(m)) return(NULL)
          val <- m[i, field]
          if (is.na(val) || !nzchar(val)) NULL else val
        }

        doc <- list()
        sig <- read_field("Signature")
        if (!is.null(sig)) doc$usage <- sig
        desc <- read_field("Description")
        if (!is.null(desc)) doc$description <- desc
        examples <- read_field("Examples")
        if (!is.null(examples)) doc$examples <- sub("\\s+$", "", examples)
        seealso <- read_field("Seealso")
        if (!is.null(seealso)) doc$seealso <- seealso
        note <- read_field("Note")
        if (!is.null(note)) doc$note <- note

        docs[[topic]] <- doc
      }
      docs
    },
    build_specials_help = function() {
      fallback <- list(
        quote = list(
          usage = "(quote expr)",
          description = "Return expr without evaluation."
        ),
        quasiquote = list(
          usage = "(quasiquote expr)",
          description = "Template with selective evaluation via unquote."
        ),
        delay = list(
          usage = "(delay expr)",
          description = "Return a promise that evaluates expr when forced."
        ),
        unquote = list(
          usage = "(unquote expr)",
          description = "Evaluate expr inside quasiquote."
        ),
        `unquote-splicing` = list(
          usage = "(unquote-splicing expr)",
          description = "Splice a list into a quasiquoted list."
        ),
        `if` = list(
          usage = "(if test then [else])",
          description = "Evaluate then or else depending on test."
        ),
        lambda = list(
          usage = "(lambda (args...) body...)",
          description = "Create an anonymous function."
        ),
        define = list(
          usage = "(define name value)",
          description = "Bind name to value in the current environment."
        ),
        `set!` = list(
          usage = "(set! name value)",
          description = "Update an existing binding."
        ),
        help = list(
          usage = "(help topic) or (help topic :package pkg)",
          description = "Show help for a topic without evaluating it. Use :package to force R help from a specific package."
        ),
        begin = list(
          usage = "(begin expr...)",
          description = "Evaluate expressions in sequence, returning the last."
        ),
        defmacro = list(
          usage = "(defmacro name (params...) body...)",
          description = "Define a macro."
        ),
        module = list(
          usage = "(module name (export ...) body...)",
          description = "Define a module with explicit exports. Use (export-all) to export all definitions (names starting with _ are excluded as private)."
        ),
        import = list(
          usage = "(import name) or (import name :refer :all) or (import name :as alias)",
          description = "Load a module and bind it as a first-class value. Use :refer to bring exports into scope unqualified, :as to alias. Qualified access via name/sym."
        ),
        macroexpand = list(
          description = "Recursively expand macros in expr."
        ),
        `macroexpand-1` = list(
          description = "Expand a single macro layer in expr."
        ),
        `macroexpand-all` = list(
          description = "Recursively expand macros in expr."
        )
      )
      loaded <- private$load_special_form_docs()
      if (length(loaded) == 0) {
        return(fallback)
      }
      missing <- setdiff(names(fallback), names(loaded))
      if (length(missing) > 0) {
        loaded[missing] <- fallback[missing]
      }
      loaded
    },
    print_doc = function(topic, doc) {
      lines <- c(paste0("Topic: ", topic))
      if (!is.null(doc$usage) && doc$usage != "") {
        usage_lines <- strsplit(doc$usage, "\n", fixed = TRUE)[[1]]
        usage_lines <- usage_lines[nzchar(trimws(usage_lines))]
        if (length(usage_lines) <= 1) {
          lines <- c(lines, paste0("Usage: ", usage_lines))
        } else {
          lines <- c(lines, "Usage:")
          lines <- c(lines, paste0("  ", usage_lines))
        }
      }
      if (!is.null(doc$description) && doc$description != "") {
        desc_lines <- strsplit(doc$description, "\n", fixed = TRUE)[[1]]
        desc_lines <- desc_lines[nzchar(trimws(desc_lines))]
        if (length(desc_lines) <= 1) {
          lines <- c(lines, paste0("Description: ", desc_lines))
        } else {
          lines <- c(lines, "Description:")
          lines <- c(lines, paste0("  ", desc_lines))
        }
      }
      if (!is.null(doc$arguments) && nchar(doc$arguments) > 0) {
        lines <- c(lines, "", "Arguments:")
        arg_lines <- strsplit(doc$arguments, "\n", fixed = TRUE)[[1]]
        arg_lines <- arg_lines[nzchar(trimws(arg_lines))]
        lines <- c(lines, paste0("  ", arg_lines))
      }
      if (!is.null(doc$examples) && nchar(doc$examples) > 0) {
        lines <- c(lines, "", "Examples:")
        example_lines <- strsplit(doc$examples, "\n", fixed = TRUE)[[1]]
        example_lines <- example_lines[nzchar(trimws(example_lines))]
        for (el in example_lines) {
          lines <- c(lines, paste0("  ", el))
        }
      }
      if (!is.null(doc$note) && nchar(doc$note) > 0) {
        lines <- c(lines, "", paste0("Note: ", doc$note))
      }
      if (!is.null(doc$seealso) && nchar(doc$seealso) > 0) {
        lines <- c(lines, "", paste0("See also: ", doc$seealso))
      }
      cat(paste(lines, collapse = "\n"), "\n")
    },
    print_help_not_found = function(topic, package = NULL) {
      if (is.null(package)) {
        cat(paste0("No help found for topic: ", topic), "\n")
      } else {
        cat(paste0("No help found for topic: ", topic, " in package: ", package), "\n")
      }
    },
    find_binding_env = function(name, env) {
      current <- env
      empty <- emptyenv()
      while (is.environment(current) && !identical(current, empty)) {
        if (exists(name, envir = current, inherits = FALSE)) {
          return(current)
        }
        current <- parent.env(current)
      }
      NULL
    },
    trim_empty_lines = function(lines) {
      if (length(lines) == 0) {
        return(character(0))
      }
      keep <- nzchar(trimws(lines))
      if (!any(keep)) {
        return(character(0))
      }
      first <- which(keep)[1]
      last <- utils::tail(which(keep), 1)
      lines[first:last]
    },
    normalize_overstrike = function(lines) {
      vapply(lines, function(line) {
        chars <- strsplit(line, "", fixed = TRUE)[[1]]
        if (length(chars) == 0) {
          return("")
        }
        out <- character(0)
        for (ch in chars) {
          if (identical(ch, "\b")) {
            if (length(out) > 0) {
              out <- out[-length(out)]
            }
          } else {
            out <- c(out, ch)
          }
        }
        paste(out, collapse = "")
      }, character(1))
    },
    package_from_help_path = function(path) {
      pkg_dir <- basename(dirname(dirname(path)))
      if (!nzchar(pkg_dir) || identical(pkg_dir, "help")) {
        return(NA_character_)
      }
      pkg_dir
    },
    parse_help_text_sections = function(lines) {
      sections <- list()
      current <- "preamble"
      sections[[current]] <- character(0)
      for (line in lines) {
        t <- trimws(line)
        if (grepl("^[A-Za-z][A-Za-z ]+:$", t)) {
          current <- tolower(sub(":$", "", t))
          if (is.null(sections[[current]])) {
            sections[[current]] <- character(0)
          }
        } else {
          sections[[current]] <- c(sections[[current]], line)
        }
      }
      sections
    },
    collapse_section = function(sections, name) {
      section <- sections[[name]]
      if (is.null(section)) {
        return(NULL)
      }
      section <- private$trim_empty_lines(section)
      if (length(section) == 0) {
        return(NULL)
      }
      paste(section, collapse = "\n")
    },
    render_r_help_text = function(help_path) {
      get_help_file <- get(".getHelpFile", envir = asNamespace("utils"), inherits = FALSE)
      rd <- get_help_file(help_path)
      utils::capture.output(tools::Rd2txt(rd, options = list(underline_titles = FALSE)))
    },
    r_help_doc = function(topic, package = NULL) {
      help_args <- list(
        topic = topic,
        package = package,
        help_type = "text",
        try.all.packages = is.null(package)
      )
      help_obj <- suppressWarnings(do.call(utils::help, help_args))
      if (length(help_obj) == 0) {
        return(NULL)
      }
      help_paths <- unclass(help_obj)
      help_paths <- as.character(help_paths)
      if (length(help_paths) == 0) {
        return(NULL)
      }
      selected_path <- help_paths[[1]]
      rendered <- tryCatch(
        suppressWarnings(private$render_r_help_text(selected_path)),
        error = function(e) {
          suppressWarnings(utils::capture.output(base::print(help_obj)))
        }
      )
      rendered <- private$trim_empty_lines(rendered)
      if (length(rendered) == 0) {
        return(NULL)
      }
      sections <- private$parse_help_text_sections(rendered)
      doc <- list(
        usage = private$collapse_section(sections, "usage"),
        description = private$collapse_section(sections, "description"),
        arguments = private$collapse_section(sections, "arguments"),
        seealso = private$collapse_section(sections, "see also"),
        examples = private$collapse_section(sections, "examples")
      )
      notes <- character(0)
      selected_pkg <- private$package_from_help_path(selected_path)
      if (!is.na(selected_pkg)) {
        notes <- c(notes, paste0("R package: ", selected_pkg))
      }
      all_pkgs <- unique(stats::na.omit(vapply(help_paths, private$package_from_help_path, character(1))))
      if (length(all_pkgs) > 1) {
        other_pkgs <- setdiff(all_pkgs, selected_pkg)
        if (length(other_pkgs) > 0) {
          notes <- c(notes, paste0("Also found in packages: ", paste(other_pkgs, collapse = ", ")))
        }
      }
      if (length(notes) > 0) {
        doc$note <- paste(notes, collapse = "\n")
      }
      has_content <- any(vapply(doc[c("usage", "description", "arguments", "seealso", "examples", "note")], function(x) {
        !is.null(x) && nzchar(x)
      }, logical(1)))
      if (!has_content) {
        return(NULL)
      }
      doc
    },
    format_default = function(expr) {
      if (identical(expr, quote(expr = ))) {
        return(NULL)
      }
      if (inherits(expr, "arl_missing_default")) {
        return(NULL)
      }
      paste(deparse(expr, width.cutoff = 500), collapse = " ")
    },
    usage_from_closure = function(fn, topic) {
      info <- attr(fn, "arl_closure", exact = TRUE)
      if (is.null(info)) {
        return(NULL)
      }
      args <- character(0)
      if (length(info$params) > 0) {
        param_specs <- info$param_specs
        if (is.null(param_specs)) {
          param_specs <- lapply(info$params, function(name) {
            list(type = "name", formal = name, display = name)
          })
        }
        for (spec in param_specs) {
          name <- spec$formal
          display <- spec$display
          if (is.null(display) || !nzchar(display)) {
            display <- name
          }
          default_expr <- info$defaults[[name]][[1]]
          default_text <- private$format_default(default_expr)
          if (is.null(default_text)) {
            args <- c(args, display)
          } else {
            args <- c(args, paste0("(", display, " ", default_text, ")"))
          }
        }
      }
      rest_spec <- info$rest_param_spec
      if (!is.null(rest_spec)) {
        rest_display <- rest_spec$display
        if (is.null(rest_display) || !nzchar(rest_display)) {
          rest_display <- rest_spec$name
        }
        args <- c(args, ".", rest_display)
      } else if (!is.null(info$rest_param)) {
        args <- c(args, ".", info$rest_param)
      }
      paste0("(", topic, if (length(args) > 0) paste0(" ", paste(args, collapse = " ")) else "", ")")
    },
    usage_from_formals = function(fn, topic) {
      signature <- args(fn)
      if (is.null(signature)) {
        return(paste0("(", topic, ")"))
      }
      args_text <- paste(deparse(signature), collapse = " ")
      args_text <- sub("\\)\\s*NULL\\s*$", ")", args_text)
      args_text <- gsub(",\\s*", " ", args_text)
      args_text <- gsub("\\s+", " ", args_text)
      args_text <- sub("^function\\s*\\(", "", args_text)
      args_text <- sub("\\)$", "", args_text)
      args_text <- trimws(args_text)
      paste0("(", topic, if (nzchar(args_text)) paste0(" ", args_text) else "", ")")
    },
    usage_from_macro = function(fn, topic) {
      info <- attr(fn, "arl_macro", exact = TRUE)
      if (is.null(info) || is.null(info$params)) {
        return(paste0("(", topic, ")"))
      }
      params <- info$params
      rest <- info$rest_param
      parts <- if (length(params) > 0) paste(params, collapse = " ") else ""
      if (!is.null(rest)) {
        parts <- if (nzchar(parts)) paste0(parts, " . ", rest) else paste0(". ", rest)
      }
      paste0("(", topic, if (nzchar(parts)) paste0(" ", parts) else "", ")")
    }
  )
)
