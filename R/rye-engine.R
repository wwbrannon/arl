#' Core Rye engine
#'
#' Provides class-based organization for tokenization, parsing, macro expansion,
#' evaluation, and environment management.
#'
#' @importFrom R6 R6Class
#' @field tokenizer Tokenizer instance used to lex source text.
#' @field parser Parser instance used to read expressions.
#' @field macro_expander Macro expander for Rye macros.
#' @field evaluator Evaluator used for expression execution.
#' @field help_system Help system for Rye topics.
#' @field env RyeEnv backing the engine.
#' @field source_tracker Source tracker used for error context.
#' @param env Optional environment or RyeEnv used as the engine base.
#' @param source Character string containing Rye source.
#' @param source_name Optional source name for error reporting.
#' @param tokens Token list produced by the tokenizer.
#' @param expr Rye expression (symbol/call/atomic value).
#' @param exprs List of Rye expressions to evaluate.
#' @param text Character string of Rye code to read/eval.
#' @param path File path to load.
#' @param preserve_src Logical; keep source metadata when macroexpanding.
#' @param topic Help topic as a single string.
#' @examples
#' engine <- RyeEngine$new()
#' engine$eval_text("(+ 1 2 3)")
#' @export
RyeEngine <- R6::R6Class(
  "RyeEngine",
  public = list(
    tokenizer = NULL,
    parser = NULL,
    macro_expander = NULL,
    evaluator = NULL,
    help_system = NULL,
    env = NULL,
    source_tracker = NULL,
    #' @description
    #' Initialize engine components and base environment.
    #' @param env Optional existing environment to use. If NULL, creates a new environment.
    #' @param parent Optional parent environment for the new environment. Only used if env is NULL.
    #'   Defaults to baseenv(). Cannot be specified together with env.
    initialize = function(env = NULL, parent = NULL) {
      self$env <- RyeEnv$new(env = env, parent = parent)
      self$source_tracker <- SourceTracker$new()
      self$tokenizer <- Tokenizer$new()
      self$parser <- Parser$new(self$source_tracker)

      # Create shared evaluation context
      context <- EvalContext$new(self$env, self$source_tracker)

      # Create components with context
      self$macro_expander <- MacroExpander$new(context)
      self$evaluator <- Evaluator$new(
        context,
        load_file_fn = function(path, env) self$load_file_in_env(path, env),
        help_fn = function(topic, env) self$help_in_env(topic, env)
      )

      # Link components in context
      context$evaluator <- self$evaluator
      context$macro_expander <- self$macro_expander

      self$help_system <- HelpSystem$new(self$env, self$macro_expander)

      self$initialize_environment()
    },
    #' @description
    #' Tokenize and parse source into expressions.
    read = function(source, source_name = NULL) {
      tokens <- self$tokenizer$tokenize(source)
      self$parser$parse(tokens, source_name = source_name)
    },
    #' @description
    #' Tokenize source into Rye tokens.
    tokenize = function(source) {
      self$tokenizer$tokenize(source)
    },
    #' @description
    #' Parse tokens into expressions.
    parse = function(tokens, source_name = NULL) {
      self$parser$parse(tokens, source_name = source_name)
    },
    #' @description
    #' Evaluate a single expression.
    eval = function(expr) {
      self$evaluator$eval(expr)
    },
    #' @description
    #' Evaluate a single expression in an explicit environment.
    eval_in_env = function(expr, env) {
      target_env <- private$resolve_env_arg(env)
      self$evaluator$eval_in_env(expr, target_env)
    },
    #' @description
    #' Evaluate expressions in order.
    eval_seq = function(exprs) {
      self$evaluator$eval_seq(exprs)
    },
    #' @description
    #' Evaluate expressions in order in an explicit environment.
    eval_seq_in_env = function(exprs, env) {
      target_env <- private$resolve_env_arg(env)
      self$evaluator$eval_seq_in_env(exprs, target_env)
    },
    #' @description
    #' Evaluate expressions with source tracking.
    eval_exprs = function(exprs) {
      self$eval_exprs_in_env(exprs, self$env$env)
    },
    #' @description
    #' Evaluate expressions with source tracking in an explicit environment.
    eval_exprs_in_env = function(exprs, env) {
      target_env <- private$resolve_env_arg(env)
      self$source_tracker$with_error_context(function() {
        self$evaluator$eval_seq_in_env(exprs, target_env)
      })
    },
    #' @description
    #' Read and evaluate text.
    eval_text = function(text, source_name = "<eval>") {
      exprs <- self$read(text, source_name = source_name)
      self$eval_exprs(exprs)
    },
    #' @description
    #' Read and evaluate text in an explicit environment.
    eval_text_in_env = function(text, env, source_name = "<eval>") {
      exprs <- self$read(text, source_name = source_name)
      self$eval_exprs_in_env(exprs, env)
    },
    #' @description
    #' Populate standard bindings
    initialize_environment = function() {
      env <- self$env$env

      rye_env_registry(env, ".rye_module_registry", create = TRUE)
      rye_env_registry(env, ".rye_macros", create = TRUE)

      env$apply <- function(fn, args) {
        args <- rye_as_list(args)
        if (length(args) > 2 &&
              (identical(fn, base::`+`) || identical(fn, base::`*`) ||
                 identical(fn, base::`-`) || identical(fn, base::`/`))) {
          return(Reduce(fn, args))
        }
        self$evaluator$do_call(fn, args)
      }
      attr(env$apply, "rye_doc") <- list(
        description = "Apply fn to the elements of lst as arguments."
      )

      env$gensym <- function(prefix = "G") {
        self$macro_expander$gensym(prefix = prefix)
      }

      env$capture <- function(symbol, expr) {
        self$macro_expander$capture(symbol, expr)
      }

      env$`macro?` <- function(x) {
        is.symbol(x) && self$macro_expander$is_macro(x, env = env)
      }

      env$macroexpand <- function(expr, preserve_src = FALSE) {
        self$macro_expander$macroexpand(expr, env = env, preserve_src = preserve_src)
      }

      env$`macroexpand-1` <- function(expr, preserve_src = FALSE) {
        self$macro_expander$macroexpand_1(expr, env = env, preserve_src = preserve_src)
      }

      env$`macroexpand-all` <- env$macroexpand

      env$eval <- function(expr, env = parent.frame()) {
        self$eval_in_env(expr, env)
      }
      attr(env$eval, "rye_doc") <- list(
        description = "Evaluate expr in the current environment."
      )

      env$`current-env` <- function() self$env$current_env()

      env$`promise?` <- function(x) {
        is.environment(x) && inherits(x, "rye_promise")
      }
      env$force <- function(x) {
        if (!is.environment(x) || !inherits(x, "rye_promise")) {
            return(x)
        }
        get(rye_promise_value_key, envir = x, inherits = FALSE)
      }

      attr(env$`promise?`, "rye_doc") <- list(
        description = "Return TRUE if x is a promise."
      )
      attr(env$force, "rye_doc") <- list(
        description = "Force a promise or return x unchanged."
      )

      env$rye_read <- function(source, source_name = NULL) {
        self$read(source, source_name = source_name)
      }
      env$rye_parse <- function(tokens, source_name = NULL) {
        self$parse(tokens, source_name = source_name)
      }
      env$rye_tokenize <- function(source) {
        self$tokenize(source)
      }

      env$rye_set_doc <- function(fn, docstring) {
        # If fn is a primitive, wrap it in a regular function
        if (is.primitive(fn)) {
          prim <- fn
          fn <- function(...) prim(...)
        }
        attr(fn, "rye_doc") <- list(description = docstring)
        fn
      }

      env$rye_get_doc <- function(fn) {
        doc_attr <- attr(fn, "rye_doc", exact = TRUE)
        if (is.null(doc_attr)) NULL else doc_attr$description
      }

      # identical? - R's native equality test with optional numeric type coercion
      #
      # Does structural comparison for value-semantic types (lists, vectors, S3, S4)
      # Does pointer comparison for reference-semantic types (environments, RC, R6, external pointers)
      #
      # Parameters:
      #   a, b: Values to compare
      #   num.type.eq: When TRUE, use == for numeric comparisons (allows 4 == 4L to be TRUE)
      #                When FALSE (default), use identical() which distinguishes types (4 != 4L)
      #   ...: Additional arguments passed to R's identical()
      #
      # Examples:
      #   (identical? 4 4L)              ; FALSE - different types
      #   (identical? 4 4L :num.type.eq #t)  ; TRUE - numeric equality
      env$`identical?` <- function(a, b, num.type.eq = FALSE, ...) {
        if (isTRUE(num.type.eq) && is.numeric(a) && is.numeric(b)) {
          # Use == for numeric comparison with type coercion
          if (length(a) != length(b)) {
            return(FALSE)
          }
          if (length(a) == 1) {
            return(isTRUE(a == b))
          } else {
            return(isTRUE(all(a == b)))
          }
        } else {
          # Use R's identical, passing through any additional arguments
          return(identical(a, b, ...))
        }
      }
      attr(env$`identical?`, "rye_doc") <- list(
        description = "R's native equality test. Structural comparison for value types, pointer comparison for reference types. Optional keyword argument :num.type.eq #t enables numeric type coercion (e.g., 4 == 4L)."
      )

      stdlib_env <- env
      env$`r/call` <- function(fn, args = list(), envir = NULL) {
        if (is.null(envir)) {
          envir <- .GlobalEnv
        }
        fn_name <- if (is.symbol(fn)) {
          as.character(fn)
        } else if (is.character(fn)) {
          fn
        } else {
          stop("r/call requires a symbol or string function name")
        }
        fn_obj <- get(fn_name, envir = envir, inherits = TRUE)
        self$evaluator$do_call(fn_obj, rye_as_list(args))
      }
      attr(env$`r/call`, "rye_doc") <- list(
        description = "Call an R function with optional environment. Searches from .GlobalEnv by default, finding base and loaded package functions."
      )

      env$`r/eval` <- function(expr, env = NULL) {
        if (is.null(env)) {
          env <- self$env$current_env()
        }
        expr <- self$macro_expander$hygiene_unwrap(expr)
        # Unwrap (quote x) so we evaluate x in env; R's eval(quote(x), env) would look up x
        if (is.call(expr) && length(expr) == 2L && identical(expr[[1L]], quote(quote))) {
          expr <- expr[[2L]]
        }
        saved <- list()
        if (is.call(expr) && length(expr) > 0) {
          op <- expr[[1]]
          if (is.symbol(op)) {
            op_name <- as.character(op)
            if (op_name == "while" && exists("while", envir = env, inherits = FALSE)) {
              saved[["while"]] <- get("while", envir = env, inherits = FALSE)
              rm("while", envir = env, inherits = FALSE)
            }
            if (op_name == "for" && exists("for", envir = env, inherits = FALSE)) {
              saved[["for"]] <- get("for", envir = env, inherits = FALSE)
              rm("for", envir = env, inherits = FALSE)
            }
          }
        }
        on.exit({
          if (!is.null(saved[["while"]])) {
            assign("while", saved[["while"]], envir = env)
          }
          if (!is.null(saved[["for"]])) {
            assign("for", saved[["for"]], envir = env)
          }
        }, add = TRUE)
        # R's eval(symbol, envir) can look up in the wrong env when called from
        # do.call; use get() for symbols so lookup is explicitly in env.
        if (is.symbol(expr)) {
          get(as.character(expr), envir = env, inherits = TRUE)
        } else {
          eval(expr, envir = env)
        }
      }
      attr(env$`r/eval`, "rye_no_quote") <- TRUE

      loader_path <- rye_resolve_module_path("_stdlib_loader")
      self$eval_in_env(self$read(sprintf('(load "%s")', loader_path))[[1]], env)

      env
    },
    #' @description
    #' Load and evaluate a Rye source file.
    load_file = function(path) {
      self$load_file_in_env(path, self$env$env)
    },
    #' @description
    #' Load and evaluate a Rye source file in an explicit environment.
    load_file_in_env = function(path, env) {
      if (!is.character(path) || length(path) != 1) {
        stop("load requires a single file path string")
      }
      if (!file.exists(path)) {
        stop(sprintf("File not found: %s", path))
      }
      text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      target_env <- private$resolve_env_arg(env)
      self$source_tracker$with_error_context(function() {
        self$evaluator$eval_seq_in_env(self$read(text, source_name = path), target_env)
      })
    },
    #' @description
    #' Expand macros recursively.
    macroexpand = function(expr, preserve_src = FALSE) {
      self$macro_expander$macroexpand(expr, env = self$env$env, preserve_src = preserve_src)
    },
    #' @description
    #' Expand macros recursively in an explicit environment.
    macroexpand_in_env = function(expr, env, preserve_src = FALSE) {
      target_env <- private$resolve_env_arg(env)
      self$macro_expander$macroexpand(expr, env = target_env, preserve_src = preserve_src)
    },
    #' @description
    #' Expand a single macro layer.
    macroexpand_1 = function(expr, preserve_src = FALSE) {
      self$macro_expander$macroexpand_1(expr, env = self$env$env, preserve_src = preserve_src)
    },
    #' @description
    #' Expand a single macro layer in an explicit environment.
    macroexpand_1_in_env = function(expr, env, preserve_src = FALSE) {
      target_env <- private$resolve_env_arg(env)
      self$macro_expander$macroexpand_1(expr, env = target_env, preserve_src = preserve_src)
    },
    #' @description
    #' Show help for a topic.
    help = function(topic) {
      self$help_system$help(topic)
    },
    #' @description
    #' Show help for a topic in an explicit environment.
    help_in_env = function(topic, env) {
      target_env <- private$resolve_env_arg(env)
      self$help_system$help_in_env(topic, target_env)
    },
    #' @description
    #' Start the Rye REPL using this engine.
    repl = function() {
      RyeREPL$new(engine = self)$run()
    }
  ),
  private = list(
    resolve_env_arg = function(env) {
      if (inherits(env, "RyeEnv")) {
        return(env$env)
      }
      if (is.environment(env)) {
        return(env)
      }
      if (is.null(env)) {
        return(self$env$env)
      }
      stop("Expected a RyeEnv or environment")
    }
  )
)

.rye_engine_state <- new.env(parent = emptyenv())

#' Get the default Rye engine
#'
#' @export
rye_default_engine <- function() {
  engine <- get0("engine", envir = .rye_engine_state, inherits = FALSE)
  if (is.null(engine)) {
    engine <- RyeEngine$new()
    assign("engine", engine, envir = .rye_engine_state)
  }
  engine
}
