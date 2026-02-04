#!/usr/bin/env Rscript
# stdlib-deps.R: Extract module/import/export from inst/rye/*.rye, build dependency
# graph, topological sort for load order, and optional undeclared-deps check.
# Usage: Rscript tools/stdlib-deps.R [--check-undeclared] [inst/rye]
# Default dir: inst/rye relative to package root (R's system.file or pkg path).

# Strip semicolon comments (; to EOL), preserve strings
strip_comments <- function(text) {
  out <- character()
  for (line in strsplit(text, "\n")[[1]]) {
    in_str <- FALSE
    quote_char <- ""
    i <- 1
    n <- nchar(line)
    while (i <= n) {
      c <- substr(line, i, i)
      if (in_str) {
        if (c == quote_char && substr(line, i - 1L, i - 1L) != "\\")
          in_str <- FALSE
        i <- i + 1L
        next
      }
      if (c %in% c('"', "'")) {
        in_str <- TRUE
        quote_char <- c
        i <- i + 1L
        next
      }
      if (c == ";") {
        line <- substr(line, 1L, i - 1L)
        break
      }
      i <- i + 1L
    }
    out <- c(out, line)
  }
  paste(out, collapse = "\n")
}

# Find matching closing paren; start is position of opening (
match_paren <- function(text, start) {
  depth <- 1L
  i <- start + 1L
  n <- nchar(text)
  in_str <- FALSE
  quote_char <- ""
  while (i <= n && depth > 0) {
    c <- substr(text, i, i)
    if (in_str) {
      if (c == quote_char && (i == 1L || substr(text, i - 1L, i - 1L) != "\\"))
        in_str <- FALSE
      i <- i + 1L
      next
    }
    if (c %in% c('"', "'")) {
      in_str <- TRUE
      quote_char <- c
      i <- i + 1L
      next
    }
    if (c == "(") depth <- depth + 1L
    else if (c == ")") depth <- depth - 1L
    i <- i + 1L
  }
  if (depth != 0) return(NA_integer_)
  i - 1L
}

# Extract first (module NAME (export ...)) from text; return list(name=, exports=character())
extract_module <- function(text) {
  re <- gregexpr("\\(module\\s+", text, perl = TRUE)[[1]]
  if (re[1] == -1L) return(NULL)
  start <- as.integer(re[1])
  end <- start + attr(re, "match.length") - 1L
  # NAME is next token (symbol or string)
  rest <- substr(text, end + 1L, nchar(text))
  rest <- sub("^[ \t\n\r]+", "", rest)
  if (substr(rest, 1L, 1L) == '"') {
    q <- regexpr('"[^"]*"', rest)
    name <- substr(rest, 2L, attr(q, "match.length") - 1L)
    rest <- substr(rest, attr(q, "match.length") + 1L, nchar(rest))
  } else {
    m <- regexpr("^[a-zA-Z0-9_.-]+", rest)
    if (m[1] == -1L) return(NULL)
    name <- substr(rest, 1L, attr(m, "match.length"))
    rest <- substr(rest, attr(m, "match.length") + 1L, nchar(rest))
  }
  rest <- sub("^[ \t\n\r]+", "", rest)
  if (substr(rest, 1L, 1L) != "(") return(list(name = name, exports = character()))
  # (export sym ...)
  exp_start <- regexpr("\\(export\\s+", rest, perl = TRUE)[[1]]
  if (exp_start == -1L) return(list(name = name, exports = character()))
  exp_start <- exp_start + attr(regexpr("\\(export\\s+", rest, perl = TRUE), "match.length") - 1L
  exp_open <- as.integer(regexpr("\\(", rest)[[1]])
  exp_close <- match_paren(rest, exp_open)
  if (is.na(exp_close)) return(list(name = name, exports = character()))
  exp_body <- substr(rest, exp_start + 1L, exp_close - 1L)
  # Tokenize export body: symbols and strings
  exp_body <- gsub("[ \t\n\r]+", " ", exp_body)
  syms <- character()
  while (nchar(trimws(exp_body)) > 0) {
    exp_body <- trimws(exp_body)
    if (substr(exp_body, 1L, 1L) == '"') {
      q <- regexpr('"[^"]*"', exp_body)
      syms <- c(syms, substr(exp_body, 2L, attr(q, "match.length") - 1L))
      exp_body <- substr(exp_body, attr(q, "match.length") + 1L, nchar(exp_body))
    } else {
      m <- regexpr("^[a-zA-Z0-9_.?-]+", exp_body)
      if (m[1] == -1L) break
      syms <- c(syms, substr(exp_body, 1L, attr(m, "match.length")))
      exp_body <- substr(exp_body, attr(m, "match.length") + 1L, nchar(exp_body))
    }
  }
  list(name = name, exports = syms)
}

# Extract all (import X) from text; return character vector of module names
extract_imports <- function(text) {
  imports <- character()
  pos <- 1L
  n <- nchar(text)
  while (pos <= n) {
    re <- regexpr("\\(import\\s+", substr(text, pos, n), perl = TRUE)[[1]]
    if (re == -1L) break
    start <- pos + re - 1L
    after_open <- start + attr(regexpr("\\(import\\s+", substr(text, pos, n), perl = TRUE), "match.length")
    rest <- substr(text, after_open, n)
    rest <- sub("^[ \t\n\r]+", "", rest)
    if (substr(rest, 1L, 1L) == '"') {
      q <- regexpr('"[^"]*"', rest)
      imp <- substr(rest, 2L, attr(q, "match.length") - 1L)
    } else {
      m <- regexpr("^[a-zA-Z0-9_.-]+", rest)
      if (m[1] == -1L) { pos <- after_open; next }
      imp <- substr(rest, 1L, attr(m, "match.length"))
    }
    imports <- c(imports, imp)
    pos <- after_open + 1L
  }
  unique(imports)
}

# Topological sort: for edges A -> B (A depends on B), return order with B before A.
# edges: list of (from, to) meaning "from depends on to" (from imports to).
# Load order = deps first, so we output order with every dependency before its dependents.
topsort_deps <- function(vertices, edges) {
  adj <- list()
  for (v in vertices) adj[[v]] <- character()
  for (e in edges) {
    from <- e$from
    to <- e$to
    if (!to %in% adj[[from]]) adj[[from]] <- c(adj[[from]], to)
  }
  # in_degree[v] = number of modules v imports (must be loaded before v)
  in_degree <- setNames(rep(0L, length(vertices)), vertices)
  for (v in vertices)
    in_degree[[v]] <- length(adj[[v]])
  # who_depends_on[u] = nodes that import u (so when we add u, we can decrement their in_degree)
  who_depends_on <- list()
  for (v in vertices) who_depends_on[[v]] <- character()
  for (v in vertices)
    for (u in adj[[v]])
      who_depends_on[[u]] <- c(who_depends_on[[u]], v)
  queue <- vertices[in_degree == 0]
  order <- character()
  while (length(queue) > 0) {
    u <- queue[1]
    queue <- queue[-1]
    order <- c(order, u)
    for (v in who_depends_on[[u]]) {
      in_degree[[v]] <- in_degree[[v]] - 1L
      if (in_degree[[v]] == 0L) queue <- c(queue, v)
    }
  }
  if (length(order) != length(vertices))
    stop("Cycle detected in dependency graph")
  order
}

# Build dependency graph: edge A -> B if A (import B)
build_graph <- function(modules) {
  vertices <- names(modules)
  edges <- list()
  for (nm in vertices) {
    imps <- modules[[nm]]$imports
    for (imp in imps)
      if (imp %in% vertices)
        edges[[length(edges) + 1L]] <- list(from = nm, to = imp)
  }
  list(vertices = vertices, edges = edges)
}

# Scan body for symbols that are exports of other modules (undeclared deps).
# Body = full file text; we only look for bare symbols that match known exports.
# Simple heuristic: split on non-symbol chars and check each token.
find_undeclared_deps <- function(module_name, body_text, all_exports, declared_imports) {
  # Get symbols that are exported by some other module
  other_exports <- list()
  for (nm in names(all_exports))
    if (nm != module_name && !nm %in% declared_imports)
      for (sym in all_exports[[nm]])
        other_exports[[sym]] <- c(other_exports[[sym]], nm)
  # Tokenize body: words that are valid Rye symbols (contain ?!- etc.)
  tokens <- unique(
    regmatches(body_text, gregexpr("[a-zA-Z0-9_.?-]+", body_text))[[1]]
  )
  reported <- list()
  for (sym in tokens) {
    if (sym %in% c("lambda", "define", "defmacro", "if", "begin", "quote", "set!", "import", "export", "module", "list", "not", "and", "or")) next
    if (!sym %in% names(other_exports)) next
    from_modules <- other_exports[[sym]]
    for (k in from_modules)
      reported[[paste0(module_name, " uses ", k, " but does not import ", k)]] <- TRUE
  }
  names(reported)
}

main <- function(stdlib_dir = NULL, check_undeclared = FALSE) {
  if (is.null(stdlib_dir)) {
    pkg_root <- Sys.getenv("RYE_PKG_ROOT", getwd())
    stdlib_dir <- file.path(pkg_root, "inst", "rye")
  }
  if (!dir.exists(stdlib_dir))
    stop("Directory not found: ", stdlib_dir)
  rye_files <- list.files(stdlib_dir, pattern = "\\.rye$", full.names = TRUE)
  rye_files <- rye_files[basename(rye_files) != "_stdlib_loader.rye"]
  modules <- list()
  all_exports <- list()
  for (f in rye_files) {
    text <- strip_comments(paste(readLines(f, warn = FALSE), collapse = "\n"))
    mod <- extract_module(text)
    if (is.null(mod)) next
    imports <- extract_imports(text)
    modules[[mod$name]] <- list(
      name = mod$name,
      exports = mod$exports,
      imports = imports,
      file = basename(f)
    )
    all_exports[[mod$name]] <- mod$exports
  }
  g <- build_graph(modules)
  load_order <- topsort_deps(g$vertices, g$edges)
  cat("Load order (topological sort):\n")
  cat(paste(load_order, collapse = " "), "\n")
  if (check_undeclared) {
    cat("\nUndeclared dependency check:\n")
    for (nm in names(modules)) {
      f <- file.path(stdlib_dir, modules[[nm]]$file)
      body_text <- strip_comments(paste(readLines(f, warn = FALSE), collapse = "\n"))
      undeclared <- find_undeclared_deps(nm, body_text, all_exports, modules[[nm]]$imports)
      for (msg in undeclared) cat("  ", msg, "\n", sep = "")
    }
  }
  invisible(list(modules = modules, load_order = load_order, graph = g))
}

# Run main only when executed via Rscript (not when sourced by tests)
run_as_script <- length(commandArgs(trailingOnly = FALSE)) > 1 &&
  any(grepl("stdlib-deps\\.R", commandArgs(trailingOnly = FALSE), fixed = TRUE))
if (run_as_script) {
  args <- commandArgs(trailingOnly = TRUE)
  check_undeclared <- "--check-undeclared" %in% args
  args <- args[args != "--check-undeclared"]
  stdlib_dir <- if (length(args) > 0) args[1] else NULL
  main(stdlib_dir = stdlib_dir, check_undeclared = check_undeclared)
}
