#' Core Rye engine
#'
#' Provides class-based organization for tokenization, parsing, macro expansion,
#' compiled evaluation, and environment management.
#'
#' @importFrom R6 R6Class
#' @field tokenizer Tokenizer instance used to lex source text.
#' @field parser Parser instance used to read expressions.
#' @field macro_expander Macro expander for Rye macros.
#' @field compiled_runtime Compiled runtime helper for executing compiled expressions.
#' @field compiler Compiler for AST-to-R translation.
#' @field help_system Help system for Rye topics.
#' @field env RyeEnv backing the engine.
#' @field source_tracker Source tracker used for error context.
#' @field module_cache Module cache for caching compiled modules.
#' @field use_env_cache Logical. If TRUE, enables Option C (environment cache) which is
#'   faster but only safe when dependencies don't change. If FALSE (default), only
#'   Option A (compiled expressions cache) is used, which is always safe. Can be set
#'   via engine initialization parameter or global option `rye.use_env_cache`.
#' @param env Optional environment or RyeEnv used as the engine base.
#' @param parent Optional parent environment for the new environment.
#' @param source Character string containing Rye source.
#' @param source_name Optional source name for error reporting.
#' @param tokens Token list produced by the tokenizer.
#' @param expr Rye expression (symbol/call/atomic value).
#' @param exprs List of Rye expressions to evaluate.
#' @param text Character string of Rye code to read/eval.
#' @param env Optional environment or RyeEnv to evaluate in (defaults to engine env).
#' @param path File path to load.
#' @param create_scope Logical; evaluate file in a child environment when TRUE.
#' @param preserve_src Logical; keep source metadata when macroexpanding.
#' @param topic Help topic as a single string.
#' @param compiled_only Logical; if TRUE, require compiled evaluation.
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
    compiled_runtime = NULL,
    compiler = NULL,
    help_system = NULL,
    env = NULL,
    source_tracker = NULL,
    module_cache = NULL,
    use_env_cache = NULL,

    #' @description
    #' Initialize engine components and base environment.
    #' @param env Optional existing environment to use. If NULL, creates a new environment.
    #' @param parent Optional parent environment for the new environment. Only used if env is NULL.
    #'   Defaults to baseenv(). Cannot be specified together with env.
    #' @param use_env_cache Optional logical. If TRUE, enables Option C (environment cache)
    #'   for 4x faster module loading. Only safe when dependencies don't change. Defaults to
    #'   NULL, which inherits from global option `getOption("rye.use_env_cache", FALSE)`.
    #' @param coverage_tracker Optional RyeCoverageTracker instance to enable coverage tracking
    #'   from the start. If provided, coverage will be tracked during stdlib loading.
    initialize = function(env = NULL, parent = NULL, use_env_cache = NULL, coverage_tracker = NULL) {
      # Priority: explicit parameter > global option > default FALSE
      if (is.null(use_env_cache)) {
        self$use_env_cache <- getOption("rye.use_env_cache", FALSE)
      } else {
        self$use_env_cache <- isTRUE(use_env_cache)
      }

      # Show one-time warning if enabled
      if (self$use_env_cache && !getOption("rye.env_cache_warning_shown", FALSE)) {
        message(
          "Note: Environment cache (Option C) is enabled. ",
          "This provides 4x speedup but is only safe when dependencies don't change. ",
          "Disable with options(rye.use_env_cache = FALSE) if working with changing code."
        )
        options(rye.env_cache_warning_shown = TRUE)
      }

      self$env <- RyeEnv$new(env = env, parent = parent)
      self$source_tracker <- SourceTracker$new()
      self$tokenizer <- Tokenizer$new()
      self$parser <- Parser$new(self$source_tracker)
      self$module_cache <- ModuleCache$new()

      # Create shared evaluation context
      context <- EvalContext$new(self$env, self$source_tracker, self$use_env_cache, coverage_tracker)

      # Create components with context
      self$macro_expander <- MacroExpander$new(context)
      # Link components in context
      context$macro_expander <- self$macro_expander

      self$compiler <- Compiler$new(context)
      # Disable constant folding when coverage is active — folding evaluates
      # via base:: and bypasses Rye function bodies, defeating instrumentation.
      if (!is.null(coverage_tracker)) {
        self$compiler$enable_constant_folding <- FALSE
      }
      context$compiler <- self$compiler

      self$compiled_runtime <- CompiledRuntime$new(
        context,
        load_file_fn = function(path, env, create_scope = FALSE) self$load_file_in_env(path, env, create_scope),
        help_fn = function(topic, env) self$help_in_env(topic, env),
        module_cache = self$module_cache
      )
      context$compiled_runtime <- self$compiled_runtime
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
    #' Evaluate a single expression via compiled evaluation.
    eval = function(expr) {
      private$eval_one_compiled(expr, self$env$env, compiled_only = TRUE)
    },

    #' @description
    #' Evaluate a single expression in an explicit environment.
    eval_in_env = function(expr, env) {
      target_env <- private$resolve_env_arg(env)
      private$eval_one_compiled(expr, target_env, compiled_only = TRUE)
    },

    #' @description
    #' Evaluate expressions in order via compiled evaluation.
    eval_seq = function(exprs, env = NULL) {
      self$eval_exprs(exprs, env = env, compiled_only = TRUE)
    },

    #' @description
    #' Evaluate expressions with source tracking.
    eval_exprs = function(exprs, env = NULL, compiled_only = TRUE) {
      target_env <- private$resolve_env_arg(env)
      self$source_tracker$with_error_context(function() {
        private$eval_seq_compiled(exprs, target_env, compiled_only = compiled_only)
      })
    },

    #' @description
    #' Read and evaluate text.
    eval_text = function(text, env = NULL, source_name = "<eval>", compiled_only = TRUE) {
      exprs <- self$read(text, source_name = source_name)
      # Stash raw text so module_compiled can parse ;;' annotations from strings
      self$compiled_runtime$context$compiler$source_text <- text
      on.exit(self$compiled_runtime$context$compiler$source_text <- NULL)
      self$eval_exprs(exprs, env = env, compiled_only = compiled_only)
    },

    #' @description
    #' Populate standard bindings
    initialize_environment = function() {
      env <- self$env$env

      self$env$get_registry(".rye_module_registry", create = TRUE)
      self$env$get_registry(".rye_macros", create = TRUE)

      #
      # Cons-cell primitives (bound in stdlib env so no globalenv/package lookup)
      #

      env$`__cons` <- function(car, cdr) RyeCons$new(car, cdr)
      env$`__cons-as-list` <- function(x) if (r6_isinstance(x, "RyeCons")) x$as_list() else list()
      env$`__cons-parts` <- function(x) if (r6_isinstance(x, "RyeCons")) x$parts() else list(prefix = list(), tail = x)

      env$`pair?` <- function(x) r6_isinstance(x, "RyeCons")

      #
      # Macro builtins
      #

      # these depend on internal state and mostly just wrap the macro
      # expander's functionality for users to call

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

      #
      # Evaluation and environments
      #

      # these need to either expose the compile functionality or close over the
      # engine environment

      env$eval <- function(expr, env = parent.frame()) {
        self$eval_in_env(expr, env)
      }

      env$read <- function(source) {
        exprs <- self$read(source)
        if (length(exprs) > 0L) exprs[[1L]] else NULL
      }

      env$`stdlib-env` <- function() env
      env$`current-env` <- function() {
        if (exists(".rye_env", envir = parent.frame(), inherits = TRUE)) {
          return(get(".rye_env", envir = parent.frame(), inherits = TRUE))
        }
        self$env$current_env()
      }

      #
      # Promises: delay / force / promise-expr
      #

      # delay is a compiler special form, but these can be defined here

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

      #
      # Documentation helpers
      #

      # these are exposed as the doc! and doc macros/functions

      env$`__set_doc` <- function(fn, docstring) {
        # If fn is a primitive, wrap it in a regular function
        if (is.primitive(fn)) {
          prim <- fn
          fn <- function(...) prim(...)
        }
        attr(fn, "rye_doc") <- list(description = docstring)
        fn
      }

      env$`__get_doc` <- function(fn) {
        doc_attr <- attr(fn, "rye_doc", exact = TRUE)
        if (is.null(doc_attr)) NULL else doc_attr$description
      }

      #
      # Evaluate in the R environment
      #

      env$`r/eval` <- function(expr, env = NULL) {
        if (is.null(env)) {
          # Use caller's environment directly (no .rye_env needed)
          env <- parent.frame()
        }
        expr_expr <- substitute(expr)
        expr_value <- expr
        expr <- if (is.call(expr_value) || is.symbol(expr_value)) expr_value else expr_expr
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

      # Load rye_doc attributes from builtins-docs.dcf (single source of truth
      # shared with the vignette generator)
      private$load_builtin_docs(env)

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
          tryCatch(
            self$load_file_in_env(path, resolved, create_scope = FALSE),
            error = function(e) {
              stop(sprintf("Failed to load stdlib module '%s': %s", name, conditionMessage(e)), call. = FALSE)
            }
          )
        }
        tryCatch(
          target_rye$module_registry$attach_into(name, resolved),
          error = function(e) {
            stop(sprintf("Failed to attach stdlib module '%s': %s", name, conditionMessage(e)), call. = FALSE)
          }
        )
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
      module_registry <- self$env$module_registry
      if (!grepl("[/\\\\]", path)) {
        if (module_registry$exists(path)) {
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

      # Try cache loading (only for module files, not create_scope loads)
      # Skip cache when coverage is enabled — cached expressions lack source info for instrumentation
      coverage_active <- !is.null(self$compiled_runtime$context$coverage_tracker) &&
                         self$compiled_runtime$context$coverage_tracker$enabled
      if (!create_scope && !coverage_active) {
        cache_paths <- self$module_cache$get_paths(path)
        if (!is.null(cache_paths)) {
          target_env <- resolved

          # Try Option C first (fastest - full environment cache)
          # ONLY if use_env_cache is enabled
          if (isTRUE(self$use_env_cache) && file.exists(cache_paths$env_cache)) {
            cache_data <- self$module_cache$load_env(cache_paths$env_cache, target_env, path)
            if (!is.null(cache_data)) {
              # Register the cached module
              module_env <- cache_data$module_env
              module_name <- cache_data$module_name
              exports <- cache_data$exports

              module_registry$register(module_name, module_env, exports)

              # Also register by absolute path
              absolute_path <- rye_normalize_path_absolute(path)
              module_registry$alias(absolute_path, module_name)

              return(invisible(NULL))
            }
          }

          # Fallback to Option A (compiled expressions cache)
          if (file.exists(cache_paths$code_cache)) {
            cache_data <- self$module_cache$load_code(cache_paths$code_cache, path)
            if (!is.null(cache_data)) {
              # Recreate module environment (like module_compiled does)
              module_name <- cache_data$module_name
              exports <- cache_data$exports
              export_all <- cache_data$export_all

              module_env <- new.env(parent = target_env)
              assign(".rye_module", TRUE, envir = module_env)
              lockBinding(".rye_module", module_env)

              module_registry$register(module_name, module_env, exports)

              # Register by absolute path
              absolute_path <- rye_normalize_path_absolute(path)
              module_registry$alias(absolute_path, module_name)

              # Install helpers and setup
              self$compiled_runtime$install_helpers(module_env)

              # Evaluate cached compiled expressions in module environment
              if (length(cache_data$compiled_body) == 1L) {
                result <- self$compiled_runtime$eval_compiled(cache_data$compiled_body[[1L]], module_env)
              } else {
                block <- as.call(c(list(quote(`{`)), cache_data$compiled_body))
                result <- self$compiled_runtime$eval_compiled(block, module_env)
              }

              # Handle export_all
              if (export_all) {
                exports <- setdiff(ls(module_env, all.names = TRUE), ".rye_module")
                module_registry$update_exports(module_name, exports)
              }

              return(invisible(result))
            }
          }
        }
      }

      # Cache miss - full load
      text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      target_env <- if (isTRUE(create_scope)) new.env(parent = resolved) else resolved
      self$source_tracker$with_error_context(function() {
        self$eval_seq(self$read(text, source_name = path), env = target_env)
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
    #' Inspect expansion and compilation for debugging. Parse text, expand macros in env,
    #' then compile to R. Returns parsed AST, expanded form, compiled R expression, and
    #' deparsed R code so you can see exactly what a Rye program becomes.
    #' @param text Character; Rye source (single expression or multiple).
    #' @param env Environment or NULL (use engine env). Must have macros/stdlib if needed.
    #' @param source_name Name for parse errors.
    #' @return List with \code{parsed} (first expr), \code{expanded}, \code{compiled} (R expr or NULL), \code{compiled_deparsed} (character, or NULL).
    inspect_compilation = function(text, env = NULL, source_name = "<inspect>") {
      target_env <- private$resolve_env_arg(env)
      exprs <- self$read(text, source_name = source_name)
      if (length(exprs) == 0) {
        return(list(parsed = NULL, expanded = NULL, compiled = NULL, compiled_deparsed = NULL))
      }
      parsed <- exprs[[1]]
      expanded <- self$macroexpand_in_env(parsed, target_env)
      compiled <- tryCatch(
        self$compiler$compile(expanded, target_env, strict = FALSE),
        error = function(e) NULL
      )
      compiled_deparsed <- if (!is.null(compiled)) deparse(compiled) else NULL
      list(parsed = parsed, expanded = expanded, compiled = compiled, compiled_deparsed = compiled_deparsed)
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
    },

    #' @description
    #' Enable coverage tracking.
    #'
    #' Creates a coverage tracker and installs it in the eval context.
    #' Should be called before running code you want to track.
    #'
    #' @return The coverage tracker instance
    enable_coverage = function() {
      if (!requireNamespace("R6", quietly = TRUE)) {
        stop("R6 package required for coverage tracking")
      }

      # Create tracker if needed
      if (is.null(self$compiled_runtime$context$coverage_tracker)) {
        self$compiled_runtime$context$coverage_tracker <- RyeCoverageTracker$new()
      }

      self$compiled_runtime$context$coverage_tracker$set_enabled(TRUE)
      invisible(self$compiled_runtime$context$coverage_tracker)
    },

    #' @description
    #' Disable coverage tracking.
    disable_coverage = function() {
      if (!is.null(self$compiled_runtime$context$coverage_tracker)) {
        self$compiled_runtime$context$coverage_tracker$set_enabled(FALSE)
      }
      invisible(self)
    },

    #' @description
    #' Get coverage data.
    #'
    #' @return Coverage tracker instance, or NULL if not enabled
    get_coverage = function() {
      self$compiled_runtime$context$coverage_tracker
    },

    #' @description
    #' Reset coverage data.
    reset_coverage = function() {
      if (!is.null(self$compiled_runtime$context$coverage_tracker)) {
        self$compiled_runtime$context$coverage_tracker$reset()
      }
      invisible(self)
    }
  ),

  private = list(
    resolve_env_arg = function(env) {
      rye_resolve_env(env, self$env$env)
    },

    load_builtin_docs = function(env) {
      dcf_path <- system.file("builtins-docs.dcf", package = "rye")
      if (!nzchar(dcf_path) || !file.exists(dcf_path)) return(invisible(NULL))
      m <- read_dcf_with_comments(dcf_path)
      doc_fields <- c("Description", "Signature", "Examples", "Seealso", "Note")
      for (i in seq_len(nrow(m))) {
        name <- m[i, "Name"]
        obj <- tryCatch(get(name, envir = env, inherits = FALSE), error = function(e) NULL)
        if (is.null(obj)) next
        doc <- list()
        for (field in doc_fields) {
          val <- m[i, field]
          if (!is.na(val) && nzchar(val)) {
            doc[[tolower(field)]] <- val
          }
        }
        if (length(doc) > 0L) {
          attr(obj, "rye_doc") <- doc
          assign(name, obj, envir = env)
        }
      }
      invisible(NULL)
    },

    eval_one_compiled = function(expr, env, compiled_only = TRUE) {
      expanded <- self$macroexpand_in_env(expr, env, preserve_src = TRUE)
      compiled <- self$compiler$compile(expanded, env, strict = isTRUE(compiled_only))
      if (!is.null(compiled)) {
        result_with_vis <- withVisible(self$compiled_runtime$eval_compiled(compiled, env))
        value <- self$source_tracker$strip_src(result_with_vis$value)
        if (result_with_vis$visible) {
          return(value)
        } else {
          return(invisible(value))
        }
      }
      msg <- self$compiler$last_error
      if (is.null(msg) || !nzchar(msg)) {
        msg <- "Expression could not be compiled"
      } else {
        msg <- paste0("Expression could not be compiled: ", msg)
      }
      stop(msg, call. = FALSE)
    },

    eval_seq_compiled = function(exprs, target_env, compiled_only = TRUE) {
      if (length(exprs) == 0) {
        return(invisible(NULL))
      }
      # Cache R6 method references to avoid repeated self$foo lookups in loop
      compiler <- self$compiler
      source_tracker <- self$source_tracker
      compiled_runtime <- self$compiled_runtime
      coverage_tracker <- compiled_runtime$context$coverage_tracker
      strict <- isTRUE(compiled_only)
      result <- NULL
      result_visible <- FALSE
      for (expr in exprs) {
        # Track top-level expression start line only (don't paint body ranges)
        if (!is.null(coverage_tracker) && coverage_tracker$enabled) {
          src_cov <- source_tracker$src_get(expr)
          if (!is.null(src_cov) && !is.null(src_cov$file) && !is.null(src_cov$start_line)) {
            coverage_tracker$register_coverable(src_cov$file, src_cov$start_line, src_cov$start_line)
            coverage_tracker$track(list(
              file = src_cov$file,
              start_line = src_cov$start_line,
              end_line = src_cov$start_line
            ))
          }
        }
        expanded <- self$macroexpand_in_env(expr, target_env, preserve_src = TRUE)
        compiled <- compiler$compile(expanded, target_env, strict = strict)
        if (is.null(compiled)) {
          msg <- compiler$last_error
          if (is.null(msg) || !nzchar(msg)) {
            msg <- "Expression sequence could not be compiled"
          } else {
            msg <- paste0("Expression sequence could not be compiled: ", msg)
          }
          stop(msg, call. = FALSE)
        }
        src <- source_tracker$src_get(expr)
        if (!is.null(src)) {
          source_tracker$push(src)
        }
        result_with_vis <- withVisible(compiled_runtime$eval_compiled(compiled, target_env))
        if (!is.null(src)) {
          source_tracker$pop()
        }
        result <- result_with_vis$value
        result_visible <- isTRUE(result_with_vis$visible)
      }
      result <- source_tracker$strip_src(result)
      if (isTRUE(result_visible)) {
        return(result)
      }
      invisible(result)
    }
  )
)

#' Get the default Rye engine
#'
#' @export
rye_default_engine <- local({
  engine <- NULL
  function() {
    if (is.null(engine)) {
      engine <<- RyeEngine$new()
    }
    engine
  }
})
