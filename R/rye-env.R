#' Rye environment wrapper
#'
#' @keywords internal
#' @noRd
RyeEnv <- R6::R6Class(
  "RyeEnv",
  public = list(
    env = NULL,
    macro_registry = NULL,
    module_registry = NULL,
    env_stack = NULL,
    initialize = function(env = NULL) {
      if (is.null(env)) {
        env <- new.env(parent = baseenv())
      }
      if (!is.environment(env)) {
        stop("RyeEnv requires an environment")
      }
      self$env <- env
      self$macro_registry <- self$macro_registry_env(env, create = TRUE)
      self$module_registry <- ModuleRegistry$new(self)
      self$env_stack <- list()
    },
    push_env = function(env) {
      self$env_stack <- c(self$env_stack, list(env))
      invisible(NULL)
    },
    pop_env = function() {
      n <- length(self$env_stack)
      if (n > 0) {
        self$env_stack <- self$env_stack[-n]
      }
      invisible(NULL)
    },
    current_env = function() {
      n <- length(self$env_stack)
      if (n > 0) {
        return(self$env_stack[[n]])
      }
      globalenv()
    },
    raw = function() {
      self$env
    },
    assign = function(name, value) {
      target <- self$find_define_env(name)
      assign(name, value, envir = target)
      invisible(NULL)
    },
    assign_existing = function(name, value) {
      target_env <- self$find_existing_env(name)
      assign(name, value, envir = target_env)
      invisible(NULL)
    },
    resolve_module = function(name) {
      self$module_registry$get(name)
    },
    attach_module = function(name) {
      self$module_registry$attach(name)
    },
    format_value = function(value) {
      formatter <- get0("format-value", envir = self$env, inherits = TRUE)
      if (!is.function(formatter)) {
        return(paste(as.character(value), collapse = " "))
      }
      tryCatch(
        do.call(formatter, list(value)),
        error = function(e) paste(as.character(value), collapse = " ")
      )
    },
    symbol_or_string = function(value, message) {
      if (is.symbol(value)) {
        return(as.character(value))
      }
      if (is.character(value) && length(value) == 1) {
        return(value)
      }
      stop(message)
    },
    macro_registry_env = function(env = NULL, create = TRUE) {
      target_env <- if (is.null(env)) self$env else env
      rye_env_registry(target_env, ".rye_macros", create = create)
    },
    module_registry_env = function(env = NULL, create = TRUE) {
      target_env <- if (is.null(env)) self$env else env
      rye_env_registry(target_env, ".rye_module_registry", create = create)
    },
    find_define_env = function(name) {
      if (isTRUE(get0(".rye_module", envir = self$env, inherits = FALSE))) {
        return(self$env)
      }
      target <- self$env
      repeat {
        if (exists(name, envir = target, inherits = FALSE)) {
          # Found the binding - check if it's locked
          if (bindingIsLocked(name, target)) {
            # Skip locked bindings and create new binding in original env
            break
          }
          return(target)
        }
        parent_env <- parent.env(target)
        # Stop at baseenv or emptyenv - these are the natural boundaries
        if (identical(parent_env, baseenv()) || identical(parent_env, emptyenv())) {
          break
        }
        target <- parent_env
      }
      self$env
    },
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
      if (is.call(pattern) || (is.list(pattern) && is.null(attr(pattern, "class", exact = TRUE)))) {
        self$destructure_bind(pattern, value, mode = mode)
        return(invisible(NULL))
      }
      stop(sprintf("%s requires a symbol or list pattern as the first argument", context))
    },
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
      if (is.symbol(pattern)) {
        return(bind_symbol(pattern, value))
      }
      if (is.null(pattern)) {
        value_list <- rye_as_list(value)
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
        value_list <- rye_as_list(value)
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

