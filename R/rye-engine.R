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
    compiler = NULL,
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
        load_file_fn = function(path, env, create_scope = FALSE) self$load_file_in_env(path, env, create_scope),
        help_fn = function(topic, env) self$help_in_env(topic, env)
      )

      # Link components in context
      context$evaluator <- self$evaluator
      context$macro_expander <- self$macro_expander

      self$compiler <- Compiler$new(context)
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
    #' Evaluate a single expression. Tries compiled path first; falls back to interpreter.
    eval = function(expr) {
      private$eval_one_compiled_or_interpret(expr, self$env$env)
    },
    #' @description
    #' Evaluate a single expression in an explicit environment.
    eval_in_env = function(expr, env) {
      target_env <- private$resolve_env_arg(env)
      private$eval_one_compiled_or_interpret(expr, target_env)
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
    #' Tries compiled path first; falls back to interpreter.
    eval_exprs_in_env = function(exprs, env) {
      target_env <- private$resolve_env_arg(env)
      self$source_tracker$with_error_context(function() {
        private$eval_seq_compiled_or_interpret(exprs, target_env)
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

      self$env$get_registry(".rye_module_registry", create = TRUE)
      self$env$get_registry(".rye_macros", create = TRUE)

      # Cons-cell primitives (bound in stdlib env so no globalenv/package lookup)
      env$`__cons` <- function(car, cdr) RyeCons$new(car, cdr)
      env$`pair?` <- function(x) r6_isinstance(x, "RyeCons")
      env$`__cons-as-list` <- function(x) if (r6_isinstance(x, "RyeCons")) x$as_list() else list()
      env$`__cons-parts` <- function(x) if (r6_isinstance(x, "RyeCons")) x$parts() else list(prefix = list(), tail = x)

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

      env$`stdlib-env` <- function() env
      env$`current-env` <- function() self$env$current_env()

      env$`promise?` <- function(x) {
        r6_isinstance(x, "RyePromise")
      }
      env$force <- function(x) {
        if (!r6_isinstance(x, "RyePromise")) {
          return(x)
        }
        x$value()
      }

      env$`promise-expr` <- function(p) {
        if (!r6_isinstance(p, "RyePromise")) {
          stop("promise-expr requires a promise (created with delay)")
        }
        p$get_expr()
      }

      attr(env$`promise?`, "rye_doc") <- list(
        description = "Return TRUE if x is a promise."
      )
      attr(env$force, "rye_doc") <- list(
        description = "Force a promise or return x unchanged."
      )
      attr(env$`promise-expr`, "rye_doc") <- list(
        description = "Extract the unevaluated expression from a promise."
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

      # identical? - Thin wrapper around R's native identical() function
      #
      # Does structural comparison for value-semantic types (lists, vectors, S3, S4)
      # Does pointer comparison for reference-semantic types (environments, RC, R6, external pointers)
      #
      # This is simply R's identical() exposed to Rye with no modifications.
      # For configurable equality with numeric type coercion, use equal? with :strict keyword.
      env$`identical?` <- base::identical
      attr(env$`identical?`, "rye_doc") <- list(
        description = "R's native equality test. Structural comparison for value types, pointer comparison for reference types. This is R's identical() function with no modifications."
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

      self$load_stdlib_into_env(env)

      env
    },
    #' @description
    #' Load all stdlib modules in dependency order into an environment. Each module
    #' is loaded and its exports attached into \code{env}. Used by
    #' \code{initialize_environment()} and by the test helper for a custom env.
    #' @param env Environment to load stdlib into (e.g. \code{self$env$env} or a test env).
    load_stdlib_into_env = function(env) {
      resolved <- private$resolve_env_arg(env)
      stdlib_dir <- system.file("rye", package = "rye")
      if (!dir.exists(stdlib_dir)) {
        stop("stdlib directory not found")
      }
      cache_path <- system.file("rye", "load-order.rds", package = "rye")
      if (nzchar(cache_path) && file.exists(cache_path)) {
        load_order <- readRDS(cache_path)
        if (!is.character(load_order) || length(load_order) == 0L) {
          load_order <- NULL
        }
      } else {
        load_order <- NULL
      }
      if (is.null(load_order)) {
        deps <- FileDeps$new(dir = stdlib_dir)
        load_order <- deps$get_load_order()
      }
      target_rye <- RyeEnv$new(resolved)
      for (name in load_order) {
        if (!target_rye$module_registry$exists(name)) {
          path <- rye_resolve_stdlib_path(name)
          if (is.null(path)) {
            stop("stdlib module not found: ", name)
          }
          self$load_file_in_env(path, resolved, create_scope = FALSE)
        }
        target_rye$module_registry$attach_into(name, resolved)
      }
      invisible(NULL)
    },
    #' @description
    #' Load and evaluate a Rye source file in an isolated scope. The file runs in a
    #' child of the engine's environment, so definitions and imports in the file
    #' are not visible in the engine's main environment or to subsequent code. For
    #' source-like behavior (definitions visible in the engine), use
    #' \code{load_file_in_env(path, env, create_scope = FALSE)}.
    load_file = function(path) {
      self$load_file_in_env(path, self$env$env, create_scope = TRUE)
    },
    #' @description
    #' Load and evaluate a Rye source file in an explicit environment. By default
    #' (\code{create_scope = FALSE}) the file is evaluated in \code{env}, so definitions
    #' and imports in the file are visible in that environment. With
    #' \code{create_scope = TRUE}, the file runs in a child of \code{env} and its
    #' definitions are not visible in \code{env}.
    #' @param create_scope If TRUE, evaluate in a new child of \code{env}; if FALSE, in \code{env}.
    load_file_in_env = function(path, env, create_scope = FALSE) {
      if (!is.character(path) || length(path) != 1) {
        stop("load requires a single file path string")
      }
      resolved <- private$resolve_env_arg(env)
      if (!grepl("[/\\\\]", path)) {
        if (RyeEnv$new(resolved)$module_registry$exists(path)) {
          return(invisible(NULL))
        }
        stdlib_path <- rye_resolve_stdlib_path(path)
        if (!is.null(stdlib_path)) {
          path <- stdlib_path
        }
      }
      if (!file.exists(path)) {
        stop(sprintf("File not found: %s", path))
      }
      text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      target_env <- if (isTRUE(create_scope)) new.env(parent = resolved) else resolved
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
      if (r6_isinstance(env, "RyeEnv")) {
        return(env$env)
      }
      if (is.environment(env)) {
        return(env)
      }
      if (is.null(env)) {
        return(self$env$env)
      }
      stop("Expected a RyeEnv or environment")
    },
    eval_one_compiled_or_interpret = function(expr, env) {
      expanded <- self$macroexpand_in_env(expr, env)
      compiled <- self$compiler$compile(expanded, env)
      if (!is.null(compiled)) {
        return(self$evaluator$eval_compiled(compiled, env))
      }
      self$evaluator$eval_in_env(expanded, env)
    },
    eval_seq_compiled_or_interpret = function(exprs, target_env) {
      expanded <- lapply(exprs, function(e) self$macroexpand_in_env(e, target_env))
      compiled <- self$compiler$compile_seq(expanded, target_env)
      if (!is.null(compiled)) {
        src <- self$source_tracker$src_get(exprs[[1]])
        if (!is.null(src)) {
          self$source_tracker$push(src)
        }
        result <- self$evaluator$eval_compiled(compiled, target_env)
        if (!is.null(src)) {
          self$source_tracker$pop()
        }
        return(result)
      }
      self$evaluator$eval_seq_in_env(expanded, target_env)
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
