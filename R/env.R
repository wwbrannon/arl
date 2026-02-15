# Env: Wraps an R environment with Arl-specific registries (macros, modules) and
# helpers for define/set!/lookup, module attach, and format_value. Used by the engine
# and compiled runtime.
#
# @field env The underlying R environment.
# @field macro_registry ModuleRegistry for macros (actually macro registry is per-env via .__macros).
# @field module_registry ModuleRegistry instance for loaded modules.
# @field env_stack List of environments pushed for evaluation (e.g. current env during eval).
#
#' @keywords internal
#' @noRd
Env <- R6::R6Class(
  "Env",
  private = list(
    .env = NULL,
    .macro_registry = NULL,
    .module_registry = NULL,
    .env_stack = NULL,       # environment used as hash map for O(1) push/pop
    .env_stack_top = 0L      # integer counter
  ),
  active = list(
    # Read-only access to internal environment
    env = function(value) {
      if (!missing(value)) stop("Cannot reassign env field")
      private$.env
    },
    # Read-only access to macro registry
    macro_registry = function(value) {
      if (!missing(value)) stop("Cannot reassign macro_registry field")
      private$.macro_registry
    },
    # Read-only access to module registry
    module_registry = function(value) {
      if (!missing(value)) stop("Cannot reassign module_registry field")
      private$.module_registry
    },
    # Read-only access to env stack (returns list for backward compatibility)
    env_stack = function(value) {
      if (!missing(value)) stop("Cannot reassign env_stack field")
      top <- private$.env_stack_top
      if (top == 0L) return(list())
      keys <- as.character(seq_len(top))
      mget(keys, envir = private$.env_stack)
    }
  ),
  public = list(
    # @description Create a Env from an existing environment or a new one with optional parent.
    # @param env Optional existing environment. If NULL, a new environment is created.
    # @param parent Optional parent for the new environment when env is NULL. Cannot be used with env.
    initialize = function(env = NULL, parent = NULL) {
      if (!is.null(env) && !is.null(parent)) {
        stop("Cannot specify both 'env' and 'parent' arguments")
      }
      if (is.null(env)) {
        if (is.null(parent)) {
          parent <- baseenv()
        }
        env <- new.env(parent = parent)
      }
      if (!is.environment(env)) {
        stop("Env requires an environment")
      }
      private$.env <- env
      private$.macro_registry <- self$macro_registry_env(env, create = TRUE)
      private$.module_registry <- ModuleRegistry$new(self)
      private$.env_stack <- new.env(parent = emptyenv())
      private$.env_stack_top <- 0L
    },
    # @description Push an environment onto the env stack (e.g. current eval env).
    # @param env Environment to push.
    push_env = function(env) {
      top <- private$.env_stack_top + 1L
      private$.env_stack_top <- top
      private$.env_stack[[as.character(top)]] <- env
      invisible(NULL)
    },
    # @description Pop the most recently pushed environment from the stack.
    pop_env = function() {
      top <- private$.env_stack_top
      if (top > 0L) {
        rm(list = as.character(top), envir = private$.env_stack)
        private$.env_stack_top <- top - 1L
      }
      invisible(NULL)
    },
    # @description Return the top of the env stack, or globalenv() if stack is empty.
    # @return Environment.
    current_env = function() {
      top <- private$.env_stack_top
      if (top > 0L) {
        return(private$.env_stack[[as.character(top)]])
      }
      globalenv()
    },
    # @description Return the raw R environment.
    # @return Environment.
    raw = function() {
      self$env
    },
    # @description Define a binding (create in current env). Used by define.
    # @param name Symbol or character name.
    # @param value Value to assign.
    assign = function(name, value) {
      target <- self$find_define_env(name)
      assign(name, value, envir = target)
      invisible(NULL)
    },
    # @description Update an existing binding. Used by set!. Errors if name is not defined.
    # @param name Symbol or character name.
    # @param value Value to assign.
    assign_existing = function(name, value) {
      target_env <- self$find_existing_env(name)
      assign(name, value, envir = target_env)
      invisible(NULL)
    },
    # @description Get a module's registry entry (env, exports, path) by name.
    # @param name Module name (single string).
    # @return List or NULL.
    resolve_module = function(name) {
      self$module_registry$get(name)
    },
    # @description Attach a module's exports into this Env's environment.
    # @param name Module name (single string).
    # @description Attach a module's exports into this Env's environment.
    # @param name Module name (single string).
    attach_module = function(name) {
      self$module_registry$attach(name)
    },
    # @description Format a value for display (e.g. REPL). Uses format-value from env if bound.
    # @param value Value to format.
    # @return Character string.
    format_value = function(value) {
      formatter <- get0("format-value", envir = self$env, inherits = TRUE)
      if (!is.function(formatter)) {
        return(paste(as.character(value), collapse = " "))
      }
      tryCatch(
        do.call(formatter, list(value), quote = TRUE),
        error = function(e) {
          warning("format-value failed, using fallback: ",
                  conditionMessage(e), call. = FALSE)
          paste(utils::capture.output(print(value)), collapse = "\n")
        }
      )
    },
    # @description Coerce value to a single string (symbol or length-1 character). Errors with message otherwise.
    # @param value Symbol or character.
    # @param message Error message if invalid.
    # @return Character string.
    symbol_or_string = function(value, message) {
      if (is.symbol(value)) {
        return(as.character(value))
      }
      if (is.character(value) && length(value) == 1) {
        return(value)
      }
      stop(message)
    },
    # @description Get (or create) a named registry environment in an env. Used for .__macros and .__module_registry.
    # @param name Registry name (e.g. ".__macros").
    # @param env Target environment or NULL for self$env.
    # @param create If TRUE, create the registry if missing.
    # @return Environment or NULL.
    get_registry = function(name, env = NULL, create = TRUE) {
      target_env <- if (is.null(env)) self$env else env
      registry <- get0(name, envir = target_env, inherits = TRUE)
      if (is.null(registry) && create) {
        registry <- new.env(parent = emptyenv())
        assign(name, registry, envir = target_env)
        lockBinding(name, target_env)
      }
      registry
    },
    # @description Get (or create) the macro registry environment for an env.
    # @param env Target environment or NULL for self$env.
    # @param create If TRUE, create the registry if missing.
    # @return Environment or NULL.
    macro_registry_env = function(env = NULL, create = TRUE) {
      self$get_registry(".__macros", env, create = create)
    },
    # @description Get (or create) the module registry environment for an env.
    # @param env Target environment or NULL for self$env.
    # @param create If TRUE, create the registry if missing.
    # @return Environment or NULL.
    module_registry_env = function(env = NULL, create = TRUE) {
      self$get_registry(".__module_registry", env, create = create)
    },
    # @description Environment where (define name ...) should create a binding (always current env).
    # @param name Unused; for interface consistency.
    # @return Environment.
    find_define_env = function(name) {
      # Define always creates a binding in the current environment,
      # never modifies parent environments (proper lexical scoping)
      self$env
    },
    # @description Environment containing an existing binding for name (for set!). Walks parent chain.
    # @param name Symbol or character name.
    # @return Environment.
    find_existing_env = function(name) {
      if (!exists(name, envir = self$env, inherits = TRUE)) {
        stop(sprintf("set!: variable '%s' is not defined", name))
      }
      target_env <- self$env
      while (!exists(name, envir = target_env, inherits = FALSE)) {
        if (identical(target_env, emptyenv())) {
          stop(sprintf("set!: variable '%s' not found", name))
        }
        target_env <- parent.env(target_env)
      }
      target_env
    },
    # @description Assign value to a pattern (symbol or list/destructuring). Dispatches to assign or destructure_bind.
    # @param pattern Symbol or list pattern.
    # @param value Value (or list of values for destructuring).
    # @param mode "define" or "set".
    # @param context String for error messages.
    assign_pattern = function(pattern, value, mode = c("define", "set"), context = "define") {
      mode <- match.arg(mode)
      if (is.symbol(pattern)) {
        name <- as.character(pattern)
        if (identical(mode, "define")) {
          self$assign(name, value)
        } else {
          self$assign_existing(name, value)
        }
        return(invisible(NULL))
      }
      if (r6_isinstance(pattern, "Cons")) {
        parts <- pattern$parts()
        pattern <- c(parts$prefix, list(as.symbol(".")), list(parts$tail))
        self$destructure_bind(pattern, value, mode = mode)
        return(invisible(NULL))
      }
      if (is.call(pattern) || (is.list(pattern) && is.null(attr(pattern, "class", exact = TRUE)))) {
        self$destructure_bind(pattern, value, mode = mode)
        return(invisible(NULL))
      }
      stop(sprintf("%s requires a symbol or list pattern as the first argument", context))
    },
    # @description Recursively bind pattern to value (list/call with optional . rest). Used by define/set! destructuring.
    # @param pattern Symbol, list, or call (may contain . for rest).
    # @param value Value or list of values.
    # @param mode "define" or "set".
    destructure_bind = function(pattern, value, mode = c("define", "set")) {
      mode <- match.arg(mode)
      bind_symbol <- function(symbol, val) {
        name <- as.character(symbol)
        if (identical(name, ".")) {
          stop("Invalid destructuring pattern: '.'")
        }
        if (identical(mode, "define")) {
          self$assign(name, val)
        } else {
          self$assign_existing(name, val)
        }
        invisible(NULL)
      }
      if (r6_isinstance(pattern, "Cons")) {
        parts <- pattern$parts()
        pattern <- c(parts$prefix, list(as.symbol(".")), list(parts$tail))
      }
      if (is.symbol(pattern)) {
        return(bind_symbol(pattern, value))
      }
      if (is.null(pattern)) {
        value_list <- as.list(value)
        if (length(value_list) != 0) {
          stop(sprintf("Destructuring pattern expects empty list, got %d item(s)", length(value_list)))
        }
        return(invisible(NULL))
      }
      if (is.call(pattern) || (is.list(pattern) && is.null(attr(pattern, "class", exact = TRUE)))) {
        parts <- if (is.call(pattern)) as.list(pattern) else pattern
        dot_idx <- which(vapply(parts, function(x) {
          is.symbol(x) && identical(as.character(x), ".")
        }, logical(1)))
        if (length(dot_idx) > 1) {
          stop("Destructuring pattern can only contain one '.'")
        }
        value_list <- as.list(value)
        if (length(dot_idx) == 1) {
          if (dot_idx == 1 || dot_idx == length(parts)) {
            stop("Destructuring '.' must appear between head and rest")
          }
          if (dot_idx != length(parts) - 1) {
            stop("Destructuring '.' must be followed by a single rest pattern")
          }
          head_patterns <- parts[1:(dot_idx - 1)]
          rest_pattern <- parts[[dot_idx + 1]]
          if (length(value_list) < length(head_patterns)) {
            stop(sprintf(
              "Destructuring pattern expects at least %d item(s), got %d",
              length(head_patterns),
              length(value_list)
            ))
          }
          for (i in seq_along(head_patterns)) {
            self$destructure_bind(head_patterns[[i]], value_list[[i]], mode)
          }
          rest_values <- if (length(value_list) > length(head_patterns)) {
            value_list[(length(head_patterns) + 1):length(value_list)]
          } else {
            list()
          }
          self$destructure_bind(rest_pattern, rest_values, mode)
          return(invisible(NULL))
        }
        if (length(value_list) != length(parts)) {
          stop(sprintf(
            "Destructuring pattern expects %d item(s), got %d",
            length(parts),
            length(value_list)
          ))
        }
        for (i in seq_along(parts)) {
          self$destructure_bind(parts[[i]], value_list[[i]], mode)
        }
        return(invisible(NULL))
      }
      stop("Invalid destructuring pattern")
    }
  )
)
