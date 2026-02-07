# Module caching system for Rye
# Implements dual-cache strategy:
# - Option C (.env.rds): Full module environment (fast, requires safety check)
# - Option A (.code.rds): Compiled expressions (safe fallback)

#' Get cache file paths for a source file
#' @param src_file Path to source .rye file
#' @return List with cache_dir, env_cache, code_cache, code_r, file_hash
get_cache_paths <- function(src_file) {
  if (!file.exists(src_file)) {
    return(NULL)
  }

  # Use MD5 for file content hashing (fast and adequate for cache invalidation)
  file_hash <- tools::md5sum(src_file)

  cache_dir <- file.path(dirname(src_file), ".rye_cache")
  base_name <- basename(src_file)

  list(
    cache_dir = cache_dir,
    env_cache = file.path(cache_dir, paste0(base_name, ".", file_hash, ".env.rds")),
    code_cache = file.path(cache_dir, paste0(base_name, ".", file_hash, ".code.rds")),
    code_r = file.path(cache_dir, paste0(base_name, ".", file_hash, ".code.R")),
    file_hash = file_hash
  )
}

#' Check if a cache file is valid
#' @param cache_data Deserialized cache data
#' @param src_file Source file path
#' @return TRUE if valid, FALSE otherwise
is_cache_valid <- function(cache_data, src_file) {
  if (!is.list(cache_data)) return(FALSE)

  # Check version
  current_version <- as.character(utils::packageVersion("rye"))
  if (is.null(cache_data$version) || cache_data$version != current_version) {
    return(FALSE)
  }

  # Check file hash
  paths <- get_cache_paths(src_file)
  if (is.null(paths) || is.null(cache_data$file_hash)) {
    return(FALSE)
  }
  if (cache_data$file_hash != paths$file_hash) {
    return(FALSE)
  }

  TRUE
}

#' Check if a module environment is safe to cache
#' @param module_env Module environment
#' @param engine_env Engine environment
#' @return List with safe (logical) and issues (character vector)
is_safe_to_cache <- function(module_env, engine_env) {
  issues <- character(0)

  for (name in ls(module_env, all.names = TRUE)) {
    # Skip special internal names that are safe
    if (name %in% c(".rye_module", ".rye_env", ".rye_exports")) next

    # Skip compiler-generated intermediate values (safe)
    if (grepl("^\\.__rye_define_value__", name)) next

    # Skip R interop wrappers (safe - they're from other modules)
    if (grepl("^__r", name)) next

    # Skip Rye helper functions installed by runtime (safe)
    if (grepl("^\\.rye_", name)) next

    # Skip macro helpers (safe)
    if (name %in% c("quasiquote", "unquote", "unquote-splicing")) next

    obj <- tryCatch(
      get(name, envir = module_env),
      error = function(e) NULL
    )
    if (is.null(obj)) next

    # Check 1: External pointers (would be invalid after deserialization)
    if (typeof(obj) == "externalptr") {
      issues <- c(issues, paste0("External pointer: ", name))
    }

    # Check 2: Connections (would be invalid)
    if (inherits(obj, "connection")) {
      issues <- c(issues, paste0("Connection: ", name))
    }

    # Check 3: Functions with non-module environment
    if (is.function(obj)) {
      fn_env <- environment(obj)
      # Allow module_env, standard R envs, namespace envs, or other module envs
      is_safe_env <- identical(fn_env, module_env) ||
                     identical(fn_env, baseenv()) ||
                     identical(fn_env, emptyenv()) ||
                     (is.environment(fn_env) && isNamespace(fn_env)) ||
                     (is.environment(fn_env) && isTRUE(get0(".rye_module", envir = fn_env, inherits = FALSE)))

      if (!is_safe_env) {
        # Function captured from elsewhere (might be old engine_env)
        issues <- c(issues, paste0("Captured function: ", name))
      }
    }

    # Check 4: Sub-environments with non-module parent
    if (is.environment(obj) && !identical(obj, module_env)) {
      parent <- parent.env(obj)
      if (!identical(parent, module_env) &&
          !identical(parent, emptyenv()) &&
          !identical(parent, baseenv())) {
        issues <- c(issues, paste0("Sub-environment with non-module parent: ", name))
      }
    }
  }

  list(safe = length(issues) == 0, issues = issues)
}

#' Write Option C cache (full module environment)
#' @param module_name Module name
#' @param module_env Module environment
#' @param exports Export list
#' @param src_file Source file path
#' @param file_hash File hash
write_env_cache <- function(module_name, module_env, exports, src_file, file_hash) {
  paths <- get_cache_paths(src_file)
  if (is.null(paths)) return(FALSE)

  # Create cache directory if needed
  if (!dir.exists(paths$cache_dir)) {
    dir.create(paths$cache_dir, recursive = TRUE)
  }

  # Sever parent link before serialization (use emptyenv for safety)
  old_parent <- parent.env(module_env)
  parent.env(module_env) <- emptyenv()

  tryCatch({
    cache_data <- list(
      version = as.character(utils::packageVersion("rye")),
      file_hash = file_hash,
      module_name = module_name,
      module_env = module_env,
      exports = exports
    )

    saveRDS(cache_data, paths$env_cache, compress = FALSE)

    # Restore parent for current session
    parent.env(module_env) <- old_parent
    TRUE
  }, error = function(e) {
    # Restore parent even on error
    parent.env(module_env) <- old_parent
    warning(sprintf("Failed to write env cache for %s: %s", module_name, conditionMessage(e)))
    FALSE
  })
}

#' Write Option A cache (compiled expressions)
#' @param module_name Module name
#' @param compiled_body List of compiled R expressions
#' @param exports Export list
#' @param export_all Export all flag
#' @param src_file Source file path
#' @param file_hash File hash
write_code_cache <- function(module_name, compiled_body, exports, export_all, src_file, file_hash) {
  paths <- get_cache_paths(src_file)
  if (is.null(paths)) return(FALSE)

  # Create cache directory if needed
  if (!dir.exists(paths$cache_dir)) {
    dir.create(paths$cache_dir, recursive = TRUE)
  }

  tryCatch({
    cache_data <- list(
      version = as.character(utils::packageVersion("rye")),
      file_hash = file_hash,
      module_name = module_name,
      exports = exports,
      export_all = export_all,
      compiled_body = compiled_body
    )

    saveRDS(cache_data, paths$code_cache, compress = FALSE)

    # Also write human-readable .code.R file for inspection
    tryCatch({
      r_code <- c(
        paste0("# Compiled code for module: ", module_name),
        paste0("# Source: ", basename(src_file)),
        paste0("# Hash: ", file_hash),
        paste0("# Rye version: ", utils::packageVersion("rye")),
        paste0("# Exports: ", paste(exports, collapse = ", ")),
        paste0("# Export all: ", export_all),
        "",
        "# === Compiled Body Expressions ==="
      )

      for (i in seq_along(compiled_body)) {
        expr <- compiled_body[[i]]
        r_code <- c(
          r_code,
          "",
          paste0("# --- Expression ", i, " ---"),
          deparse(expr, width.cutoff = 80)
        )
      }

      writeLines(r_code, paths$code_r)
    }, error = function(e) {
      # Non-fatal - .code.R is just for inspection
      warning(sprintf("Failed to write .code.R for %s: %s", module_name, conditionMessage(e)))
    })

    TRUE
  }, error = function(e) {
    warning(sprintf("Failed to write code cache for %s: %s", module_name, conditionMessage(e)))
    FALSE
  })
}

#' Load Option C cache (full module environment)
#' @param cache_file Path to .env.rds cache file
#' @param engine_env Engine environment to relink as parent
#' @param src_file Source file for validation
#' @return Cache data (module_env, module_name, exports) or NULL if invalid
load_env_cache <- function(cache_file, engine_env, src_file) {
  if (!file.exists(cache_file)) {
    return(NULL)
  }

  cache_data <- tryCatch(
    readRDS(cache_file),
    error = function(e) {
      warning(sprintf("Failed to read env cache %s: %s", cache_file, conditionMessage(e)))
      NULL
    }
  )

  if (is.null(cache_data)) {
    return(NULL)
  }

  # Validate cache
  if (!is_cache_valid(cache_data, src_file)) {
    # Invalid cache, delete all related cache files
    paths <- get_cache_paths(src_file)
    if (!is.null(paths)) {
      unlink(paths$env_cache)
      unlink(paths$code_cache)
      unlink(paths$code_r)
    }
    return(NULL)
  }

  module_env <- cache_data$module_env

  # Relink to current engine environment
  parent.env(module_env) <- engine_env

  # Return full cache data so caller can register module
  cache_data
}

#' Load Option A cache (compiled expressions)
#' @param cache_file Path to .code.rds cache file
#' @param src_file Source file for validation
#' @return Cache data or NULL if invalid
load_code_cache <- function(cache_file, src_file) {
  if (!file.exists(cache_file)) {
    return(NULL)
  }

  cache_data <- tryCatch(
    readRDS(cache_file),
    error = function(e) {
      warning(sprintf("Failed to read code cache %s: %s", cache_file, conditionMessage(e)))
      NULL
    }
  )

  if (is.null(cache_data)) {
    return(NULL)
  }

  # Validate cache
  if (!is_cache_valid(cache_data, src_file)) {
    # Invalid cache, delete all related cache files
    paths <- get_cache_paths(src_file)
    if (!is.null(paths)) {
      unlink(paths$env_cache)
      unlink(paths$code_cache)
      unlink(paths$code_r)
    }
    return(NULL)
  }

  cache_data
}
