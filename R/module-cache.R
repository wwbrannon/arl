# Module caching system for Arl
# Caches compiled expressions (.code.rds) for faster module loading.

#' @title ModuleCache
#' @description R6 class for managing module caching
#' @keywords internal
#' @noRd
ModuleCache <- R6::R6Class(
  "ArlModuleCache",
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
      if (is_in_library_tree(src_file)) {
        src_dir <- normalizePath(dirname(src_file), mustWork = FALSE, winslash = "/")
        dir_hash <- md5_string(src_dir)
        safe_dir <- paste0(basename(src_dir), "-", substr(dir_hash, 1, 12))
        cache_dir <- file.path(tools::R_user_dir(.pkg_name, "cache"), "modules", safe_dir)
      }
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
    write_code = function(module_name, compiled_body, exports, export_all,
                          re_export, src_file, file_hash, coverage = FALSE,
                          cache_paths = NULL, compiler_flags = NULL) {
      paths <- if (!is.null(cache_paths)) cache_paths else self$get_paths(src_file)
      if (is.null(paths)) return(FALSE)

      # Create cache directory if needed
      if (!dir.exists(paths$cache_dir)) {
        dir.create(paths$cache_dir, recursive = TRUE)
      }

      # Clean up stale cache files for the same source before writing new ones
      private$cleanup_stale_cache(paths$cache_dir, basename(src_file), file_hash)

      tryCatch({
        # Deflate resolved refs (replace tagged closures with symbolic placeholders)
        compiled_body <- lapply(compiled_body, private$deflate_resolved_refs)

        cache_data <- list(
          version = as.character(utils::packageVersion("arl")),
          file_hash = file_hash,
          coverage = coverage,
          default_packages = sort(getOption("defaultPackages", character(0))),
          compiler_flags = compiler_flags,
          module_name = module_name,
          exports = exports,
          export_all = export_all,
          re_export = re_export,
          compiled_body = compiled_body
        )

        saveRDS(cache_data, paths$code_cache, compress = FALSE)

        # Write human-readable .code.R file for inspection (debug only)
        if (isTRUE(.pkg_option("debug_cache", FALSE))) {
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
        }

        TRUE
      }, error = function(e) {
        cd <- paths$cache_dir
        if (!(cd %in% private$.warned_dirs)) {
          warning(sprintf("Failed to write code cache in %s: %s", cd, conditionMessage(e)))
          private$.warned_dirs <- c(private$.warned_dirs, cd)
        }
        FALSE
      })
    },

    #' @description Load expr cache (compiled expressions)
    #' @param cache_file Path to .code.rds cache file
    #' @param src_file Source file for validation
    #' @return Cache data or NULL if invalid
    load_code = function(cache_file, src_file, coverage = FALSE, file_hash = NULL, compiler_flags = NULL) {
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

      # Validate cache (pass file_hash to avoid recomputing MD5)
      if (!private$is_valid(cache_data, src_file, coverage = coverage,
                           file_hash = file_hash,
                           compiler_flags = compiler_flags)) {
        # Invalid cache, delete related cache files (use dirname to avoid get_paths)
        unlink(cache_file)
        code_r <- sub("\\.code\\.rds$", ".code.R", cache_file)
        if (file.exists(code_r)) unlink(code_r)
        return(NULL)
      }

      cache_data
    }
  ),

  private = list(
    .warned_dirs = character(0),
    #' @description Walk compiled expression tree, replace tagged closures with
    #' symbolic placeholders so they can be serialized without capturing entire
    #' environments.
    #' @param expr A compiled R expression (call, list, or atomic).
    #' @return The deflated expression.
    deflate_resolved_refs = function(expr) {
      if (is.function(expr) && !is.null(attr(expr, "arl_resolved_from"))) {
        info <- attr(expr, "arl_resolved_from")
        return(as.call(list(
          as.symbol(".__resolve_ref"),
          info$module_name,
          info$source_symbol
        )))
      }
      if (is.call(expr)) {
        return(as.call(lapply(as.list(expr), private$deflate_resolved_refs)))
      }
      if (is.list(expr) && is.null(attr(expr, "class", exact = TRUE))) {
        return(lapply(expr, private$deflate_resolved_refs))
      }
      expr
    },
    #' @description Check if a cache file is valid
    #' @param cache_data Deserialized cache data
    #' @param src_file Source file path
    #' @return TRUE if valid, FALSE otherwise
    is_valid = function(cache_data, src_file, coverage = FALSE, file_hash = NULL, compiler_flags = NULL) {
      if (!is.list(cache_data)) return(FALSE)

      # Check version
      current_version <- as.character(utils::packageVersion("arl"))
      if (is.null(cache_data$version) || cache_data$version != current_version) {
        return(FALSE)
      }

      # Check file hash (use provided hash to avoid recomputing MD5)
      if (is.null(file_hash)) {
        file_hash <- tools::md5sum(src_file)
      }
      if (is.null(cache_data$file_hash)) {
        return(FALSE)
      }
      if (cache_data$file_hash != file_hash) {
        return(FALSE)
      }

      # Check defaultPackages (affects which symbols are resolved vs left bare).
      # The field must be present (not NULL); caches from before this field
      # was added are invalidated.
      if (is.null(cache_data$default_packages)) {
        return(FALSE)
      }
      current_pkgs <- sort(getOption("defaultPackages", character(0)))
      if (!identical(cache_data$default_packages, current_pkgs)) {
        return(FALSE)
      }

      # Check compiler flags — different optimization settings produce different
      # compiled output. The field must be present (not NULL); caches from
      # before this field was added are invalidated.
      if (is.null(cache_data$compiler_flags)) {
        return(FALSE)
      }
      if (!is.null(compiler_flags) && !identical(cache_data$compiler_flags, compiler_flags)) {
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
    },
    #' @description Clean up stale cache files for a source file
    #' @param cache_dir Cache directory path
    #' @param src_basename Source file basename (e.g., "module.arl")
    #' @param current_hash The current hash being written
    cleanup_stale_cache = function(cache_dir, src_basename, current_hash) {
      if (!dir.exists(cache_dir)) return(invisible(NULL))
      # Pattern: <basename>.<hash>.code.rds and .code.R
      # Match all cache files for this source file
      pattern <- paste0("^", gsub("\\.", "\\\\.", src_basename), "\\.[a-f0-9]+\\.code\\.(rds|R)$")
      existing <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
      # Keep files matching the current hash
      current_pattern <- paste0(src_basename, ".", current_hash, ".code.")
      stale <- existing[!grepl(current_pattern, basename(existing), fixed = TRUE)]
      if (length(stale) > 0) {
        unlink(stale)
      }
      invisible(NULL)
    }
  )
)

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
