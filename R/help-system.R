# Help system for Rye
#'
#' @keywords internal
#' @noRd
HelpSystem <- R6::R6Class(
  "HelpSystem",
  public = list(
    env = NULL,
    macro_expander = NULL,
    initialize = function(env, macro_expander) {
      self$env <- env
      self$macro_expander <- macro_expander
      private$specials_help <- private$build_specials_help()
    },
    help = function(topic) {
      self$help_in_env(topic, self$env$env)
    },
    help_in_env = function(topic, env) {
      if (!is.character(topic) || length(topic) != 1) {
        stop("help requires a symbol or string")
      }

      if (r6_isinstance(env, "RyeEnv")) {
        env <- env$env
      } else if (is.null(env)) {
        env <- self$env$env
      }
      if (!is.environment(env)) {
        stop("Expected a RyeEnv or environment")
      }
      target_env <- env

      doc <- private$specials_help[[topic]]
      if (!is.null(doc)) {
        private$print_doc(topic, doc)
        return(invisible(NULL))
      }

      macro_symbol <- as.symbol(topic)
      if (self$macro_expander$is_macro(macro_symbol, env = target_env)) {
        macro_fn <- self$macro_expander$get_macro(macro_symbol, env = target_env)
        macro_doc <- attr(macro_fn, "rye_doc", exact = TRUE)
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
        obj <- get(topic, envir = target_env, inherits = TRUE)
        obj_doc <- attr(obj, "rye_doc", exact = TRUE)
        usage <- NULL
        if (inherits(obj, "rye_closure")) {
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
        if (!is.null(usage)) {
          private$print_doc(topic, list(usage = usage))
          return(invisible(NULL))
        }
      }

      utils::help(topic, help_type = "text")
      invisible(NULL)
    }
  ),
  private = list(
    specials_help = NULL,
    build_specials_help = function() {
      list(
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
          usage = "(help topic)",
          description = "Show help for a topic without evaluating it."
        ),
        begin = list(
          usage = "(begin expr...)",
          description = "Evaluate expressions in sequence, returning the last."
        ),
        load = list(
          usage = "(load \"path\")",
          description = "Load and evaluate a Rye source file in the current environment (source-like). Definitions and imports in the file are visible in the caller."
        ),
        run = list(
          usage = "(run \"path\")",
          description = "Run a Rye source file in an isolated child environment. Definitions and imports in the file are not visible in the caller."
        ),
        defmacro = list(
          usage = "(defmacro name (params...) body...)",
          description = "Define a macro."
        ),
        module = list(
          usage = "(module name (export ...) body...)",
          description = "Define a module with explicit exports. Use (export-all) to export all definitions."
        ),
        import = list(
          usage = "(import name)",
          description = "Load a module and attach its exports."
        ),
        `::` = list(
          usage = "(:: pkg name)",
          description = "Access an exported object from an R package."
        ),
        `:::` = list(
          usage = "(::: pkg name)",
          description = "Access a non-exported object from an R package."
        ),
        `~` = list(
          usage = "(~ lhs rhs)",
          description = "Build an R formula without evaluating arguments."
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
    },
    print_doc = function(topic, doc) {
      lines <- c(paste0("Topic: ", topic))
      if (!is.null(doc$usage) && doc$usage != "") {
        lines <- c(lines, paste0("Usage: ", doc$usage))
      }
      if (!is.null(doc$description) && doc$description != "") {
        lines <- c(lines, paste0("Description: ", doc$description))
      }
      cat(paste(lines, collapse = "\n"), "\n")
    },
    format_default = function(expr) {
      if (identical(expr, quote(expr = ))) {
        return(NULL)
      }
      if (inherits(expr, "rye_missing_default")) {
        return(NULL)
      }
      paste(deparse(expr, width.cutoff = 500), collapse = " ")
    },
    usage_from_closure = function(fn, topic) {
      info <- attr(fn, "rye_closure", exact = TRUE)
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
      info <- attr(fn, "rye_macro", exact = TRUE)
      if (is.null(info) || is.null(info$params)) {
        return(paste0("(", topic, ")"))
      }
      params <- info$params
      paste0("(", topic, if (length(params) > 0) paste0(" ", paste(params, collapse = " ")) else "", ")")
    }
  )
)
