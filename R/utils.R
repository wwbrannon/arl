# Package identity -- change this single value when renaming the package.
# Used to derive option names (e.g. "arl.disable_tco") and filesystem
# paths (e.g. ".arl_cache", "~/.arl_history").
.pkg_name <- "arl"

# Convert an option name to its env var form: "disable_tco" -> "ARL_DISABLE_TCO"
.option_to_envvar <- function(name) {
  toupper(paste0(.pkg_name, "_", name))
}

# Coerce a string env var value to match the type of `default`.
.coerce_env_value <- function(val, default) {
  if (is.logical(default)) {
    return(tolower(val) %in% c("1", "true", "yes"))
  }
  val
}

# Get a package-namespaced option with env var fallback.
# Precedence: R option > env var > default.
# Env var fallback is only checked when `default` is non-NULL, which
# naturally excludes test hooks (they use NULL default).
.pkg_option <- function(name, default = NULL) {
  opt_name <- paste0(.pkg_name, ".", name)
  opt_val <- getOption(opt_name)
  if (!is.null(opt_val)) return(opt_val)

  if (!is.null(default)) {
    env_name <- .option_to_envvar(name)
    env_val <- Sys.getenv(env_name, unset = NA)
    if (!is.na(env_val) && nzchar(env_val)) {
      return(.coerce_env_value(env_val, default))
    }
  }

  default
}

# Set a package-namespaced option: .set_pkg_option("repl_quiet", TRUE)
# is equivalent to options(arl.repl_quiet = TRUE).
.set_pkg_option <- function(name, value) {
  opt <- list(value)
  names(opt) <- paste0(.pkg_name, ".", name)
  do.call("options", opt)
}

# Create a new environment tagged as Arl-owned.
# Drop-in replacement for new.env(); unlock_binding() requires this tag.
arl_new_env <- function(hash = TRUE, parent = emptyenv(), size = 29L) {
  env <- new.env(hash = hash, parent = parent, size = size)
  attr(env, ".arl_owned") <- TRUE
  env
}

# Unlock a binding, guarding against environments we didn't create.
# All Arl-managed environments are tagged via arl_new_env().
unlock_binding <- function(sym, env) {
  if (!isTRUE(attr(env, ".arl_owned", exact = TRUE))) {
    stop("unlock_binding: refusing to unlock binding '", sym,
         "' in non-Arl environment", call. = FALSE)
  }
  do.call("unlockBinding", list(sym, env))
}

resolve_stdlib_path <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    return(NULL)
  }
  dir_path <- system.file(.pkg_name, package = .pkg_name)
  if (identical(dir_path, "")) {
    return(NULL)
  }
  candidates <- c(
    file.path(dir_path, name),
    file.path(dir_path, paste0(name, ".arl"))
  )
  for (path in candidates) {
    if (file.exists(path)) {
      return(path)
    }
  }
  NULL
}

# Resolve an env argument to a raw R environment.
# Accepts Env (extracts $env), environment (pass-through), or NULL (uses fallback_env).
resolve_env <- function(env, fallback_env) {
  if (inherits(env, "ArlEnv")) {
    return(env$env)
  }
  if (is.environment(env)) {
    return(env)
  }
  if (is.null(env)) {
    return(fallback_env)
  }
  stop("Expected a Env or environment")
}

# Check whether a source expression should have its coverage narrowed to just
# the start line. This applies to forms whose sub-expressions are instrumented
# separately:
#   - if: branches tracked by wrap_branch_coverage
#   - define/defmacro wrapping a lambda: body tracked inside lambda
should_narrow_coverage <- function(src_expr) {
  if (!is.call(src_expr) || length(src_expr) < 3 || !is.symbol(src_expr[[1]])) {
    return(FALSE)
  }
  head_name <- as.character(src_expr[[1]])
  if (identical(head_name, "if")) {
    return(TRUE)
  }
  if (head_name %in% c("define", "defmacro") && length(src_expr) >= 3) {
    val <- src_expr[[3]]
    if (is.call(val) && length(val) >= 3 && is.symbol(val[[1]]) &&
        identical(as.character(val[[1]]), "lambda")) {
      return(TRUE)
    }
  }
  FALSE
}

# Normalize a file path to absolute form for consistent registry keys.
# Relative paths are resolved relative to getwd(). Uses forward slashes.
# Read a DCF file, stripping comment lines (starting with #) before parsing.
read_dcf_with_comments <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines)]
  read.dcf(textConnection(paste(lines, collapse = "\n")))
}

normalize_path_absolute <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    return(path)
  }
  if (file.exists(path)) {
    # File exists: resolve symlinks with mustWork = TRUE for stronger guarantees
    normalized <- tryCatch(
      normalizePath(path, mustWork = TRUE, winslash = "/"),
      error = function(e) normalizePath(path, mustWork = FALSE, winslash = "/")
    )
  } else {
    # File doesn't exist: normalize the parent directory (which likely exists)
    # and reattach basename. This resolves macOS /var -> /private/var symlinks
    # even for temp files that haven't been written yet.
    parent <- dirname(path)
    base <- basename(path)
    norm_parent <- tryCatch(
      normalizePath(parent, mustWork = FALSE, winslash = "/"),
      error = function(e) parent
    )
    normalized <- file.path(norm_parent, base)
  }
  if (is.na(normalized) || !nzchar(normalized)) {
    return(path)
  }
  normalized
}
