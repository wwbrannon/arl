# Module caching system for Arl
# Caches compiled expressions (.code.rds) for faster module loading.

#' @title ModuleCache
#' @description R6 class for managing module caching
#' @keywords internal
#' @noRd
ModuleCache <- R6::R6Class(
  "ModuleCache",
  public = list(
    #' @description Initialize module cache
    initialize = function() {
      # Stateless for now - could add cache statistics in future
    },

    #' @description Get cache file paths for a source file
    #' @param src_file Path to source .arl file
    #' @return List with cache_dir, code_cache, code_r, file_hash
    get_paths = function(src_file) {
      if (!file.exists(src_file)) {
        return(NULL)
      }

      # Use MD5 for file content hashing (fast and adequate for cache invalidation)
      file_hash <- tools::md5sum(src_file)

      cache_dir <- file.path(dirname(src_file), paste0(".", .pkg_name, "_cache"))
      base_name <- basename(src_file)

      list(
        cache_dir = cache_dir,
        code_cache = file.path(cache_dir, paste0(base_name, ".", file_hash, ".code.rds")),
        code_r = file.path(cache_dir, paste0(base_name, ".", file_hash, ".code.R")),
        file_hash = file_hash
      )
    },

    #' @description Write expr cache (compiled expressions)
    #' @param module_name Module name
    #' @param compiled_body List of compiled R expressions
    #' @param exports Export list
    #' @param export_all Export all flag
    #' @param src_file Source file path
    #' @param file_hash File hash
    write_code = function(module_name, compiled_body, exports, export_all, src_file, file_hash, coverage = FALSE) {
      paths <- self$get_paths(src_file)
      if (is.null(paths)) return(FALSE)

      # Create cache directory if needed
      if (!dir.exists(paths$cache_dir)) {
        dir.create(paths$cache_dir, recursive = TRUE)
      }

      tryCatch({
        # Deflate resolved refs (replace tagged closures with symbolic placeholders)
        compiled_body <- lapply(compiled_body, deflate_resolved_refs)

        cache_data <- list(
          version = as.character(utils::packageVersion("arl")),
          file_hash = file_hash,
          coverage = coverage,
          default_packages = sort(getOption("defaultPackages", character(0))),
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
            paste0("# Arl version: ", utils::packageVersion("arl")),
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
    },

    #' @description Load expr cache (compiled expressions)
    #' @param cache_file Path to .code.rds cache file
    #' @param src_file Source file for validation
    #' @return Cache data or NULL if invalid
    load_code = function(cache_file, src_file, coverage = FALSE) {
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
      if (!private$is_valid(cache_data, src_file, coverage = coverage)) {
        # Invalid cache, delete all related cache files
        paths <- self$get_paths(src_file)
        if (!is.null(paths)) {
          unlink(paths$code_cache)
          unlink(paths$code_r)
        }
        return(NULL)
      }

      cache_data
    }
  ),

  private = list(
    #' @description Check if a cache file is valid
    #' @param cache_data Deserialized cache data
    #' @param src_file Source file path
    #' @return TRUE if valid, FALSE otherwise
    is_valid = function(cache_data, src_file, coverage = FALSE) {
      if (!is.list(cache_data)) return(FALSE)

      # Check version
      current_version <- as.character(utils::packageVersion("arl"))
      if (is.null(cache_data$version) || cache_data$version != current_version) {
        return(FALSE)
      }

      # Check file hash
      paths <- self$get_paths(src_file)
      if (is.null(paths) || is.null(cache_data$file_hash)) {
        return(FALSE)
      }
      if (cache_data$file_hash != paths$file_hash) {
        return(FALSE)
      }

      # Check defaultPackages (affects which symbols are resolved vs left bare)
      current_pkgs <- sort(getOption("defaultPackages", character(0)))
      if (!is.null(cache_data$default_packages) &&
          !identical(cache_data$default_packages, current_pkgs)) {
        return(FALSE)
      }

      # Reject caches compiled with a different coverage state — coverage-
      # instrumented code contains .__coverage_track calls that don't exist
      # when coverage is off, and non-instrumented code lacks the tracking
      # calls that coverage needs.  The field must be present (not NULL);
      # caches from before this field was added are invalidated.
      if (is.null(cache_data$coverage) || !identical(cache_data$coverage, coverage)) {
        return(FALSE)
      }

      TRUE
    }
  )
)

# Walk compiled expression tree, replace tagged closures with symbolic placeholders
# so they can be serialized without capturing entire environments.
deflate_resolved_refs <- function(expr) {
  if (is.function(expr) && !is.null(attr(expr, "arl_resolved_from"))) {
    info <- attr(expr, "arl_resolved_from")
    return(as.call(list(
      as.symbol(".__resolve_ref"),
      info$module_name,
      info$source_symbol
    )))
  }
  if (is.call(expr)) {
    return(as.call(lapply(as.list(expr), deflate_resolved_refs)))
  }
  if (is.list(expr) && is.null(attr(expr, "class", exact = TRUE))) {
    return(lapply(expr, deflate_resolved_refs))
  }
  expr
}

# Walk compiled expression tree, replace .__resolve_ref placeholders with
# actual values looked up from the module registry.
inflate_resolved_refs <- function(expr, registry_env) {
  if (is.call(expr) && length(expr) == 3L && is.symbol(expr[[1]]) &&
      identical(as.character(expr[[1]]), ".__resolve_ref")) {
    mod_name <- expr[[2]]
    sym_name <- expr[[3]]
    if (is.character(mod_name) && is.character(sym_name) &&
        exists(mod_name, envir = registry_env, inherits = FALSE)) {
      entry <- get(mod_name, envir = registry_env, inherits = FALSE)
      if (exists(sym_name, envir = entry$env, inherits = TRUE)) {
        val <- get(sym_name, envir = entry$env, inherits = TRUE)
        if (is.function(val)) {
          attr(val, "arl_resolved_from") <- list(
            module_name = mod_name, source_symbol = sym_name
          )
        }
        return(val)
      }
    }
    return(expr)  # module not loaded yet, keep placeholder
  }
  if (is.call(expr)) {
    return(as.call(lapply(as.list(expr), inflate_resolved_refs, registry_env)))
  }
  if (is.list(expr) && is.null(attr(expr, "class", exact = TRUE))) {
    return(lapply(expr, inflate_resolved_refs, registry_env))
  }
  expr
}
