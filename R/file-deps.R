# FileDeps: static analysis of a directory of .arl files for module dependencies and load order.
# Uses topsort for topological sort. Generic over dir/pattern/exclude; no reference to engine or evaluator.

#' @importFrom R6 R6Class
#' @keywords internal
#' @noRd
FileDeps <- R6::R6Class(
  "FileDeps",
  public = list(
    #' @field dir Path to directory containing .arl module files.
    dir = NULL,
    #' @field modules List of module info (name, exports, imports, file) keyed by name.
    modules = NULL,
    #' @field graph List with vertices (character) and edges (list of list(from=, to=)).
    graph = NULL,
    #' @field load_order Character vector of module names in dependency order.
    load_order = NULL,

    #' @description Create a FileDeps instance by scanning a directory.
    #' @param dir Path to directory containing files matching \code{pattern}.
    #' @param pattern Regex for file names (default \code{"\\.arl$"}).
    #' @param exclude Character vector of basenames to skip (default none).
    initialize = function(dir, pattern = "\\.arl$", exclude = character()) {
      if (!dir.exists(dir)) {
        stop("Directory not found: ", dir)
      }
      self$dir <- dir
      all_files <- list.files(dir, pattern = pattern, full.names = TRUE)
      if (length(exclude) > 0L) {
        all_files <- all_files[!basename(all_files) %in% exclude]
      }
      modules <- list()
      all_exports <- list()
      for (f in all_files) {
        text <- private$strip_comments(paste(readLines(f, warn = FALSE), collapse = "\n"))
        mod <- private$extract_module(text)
        if (is.null(mod)) next
        imports <- private$extract_imports(text)
        modules[[mod$name]] <- list(
          name = mod$name,
          exports = mod$exports,
          imports = imports,
          file = basename(f)
        )
        all_exports[[mod$name]] <- mod$exports
      }
      self$modules <- modules
      g <- private$build_graph(modules)
      self$graph <- g
      self$load_order <- topsort(g$vertices, g$edges)
      invisible(self)
    },

    #' @description Get module names in topological (dependency) order.
    #' @return Character vector.
    get_load_order = function() {
      self$load_order
    },

    #' @description Get the full modules list (name, exports, imports, file).
    #' @return Named list of module info.
    get_modules = function() {
      self$modules
    },

    #' @description Get the dependency graph (vertices and edges).
    #' @return List with \code{vertices} (character) and \code{edges} (list).
    get_graph = function() {
      self$graph
    }
  ),
  private = list(
    strip_comments = function(text) {
      out <- character()
      for (line in strsplit(text, "\n")[[1]]) {
        in_str <- FALSE
        quote_char <- ""
        i <- 1L
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
    },
    match_paren = function(text, start) {
      depth <- 1L
      i <- start + 1L
      n <- nchar(text)
      in_str <- FALSE
      quote_char <- ""
      while (i <= n && depth > 0L) {
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
      if (depth != 0L) return(NA_integer_)
      i - 1L
    },
    extract_module = function(text) {
      re <- gregexpr("\\(module\\s+", text, perl = TRUE)[[1]]
      if (re[1] == -1L) return(NULL)
      start <- as.integer(re[1])
      end <- start + attr(re, "match.length") - 1L
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
      exp_start <- regexpr("\\(export\\s+", rest, perl = TRUE)[[1]]
      if (exp_start == -1L) return(list(name = name, exports = character()))
      exp_start <- exp_start + attr(regexpr("\\(export\\s+", rest, perl = TRUE), "match.length") - 1L
      exp_open <- as.integer(regexpr("\\(", rest)[[1]])
      exp_close <- private$match_paren(rest, exp_open)
      if (is.na(exp_close)) return(list(name = name, exports = character()))
      exp_body <- substr(rest, exp_start + 1L, exp_close - 1L)
      exp_body <- gsub("[ \t\n\r]+", " ", exp_body)
      syms <- character()
      while (nchar(trimws(exp_body)) > 0L) {
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
    },
    extract_imports = function(text) {
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
    },
    build_graph = function(modules) {
      vertices <- names(modules)
      edges <- list()
      for (nm in vertices) {
        imps <- modules[[nm]]$imports
        for (imp in imps)
          if (imp %in% vertices)
            edges[[length(edges) + 1L]] <- list(from = nm, to = imp)
      }
      list(vertices = vertices, edges = edges)
    },
    strip_strings = function(text) {
      # Replace string literal contents with spaces so tokenization ignores them.
      n <- nchar(text)
      if (n == 0L) return(text)
      result <- character(n)
      in_str <- FALSE
      quote_char <- ""
      for (i in seq_len(n)) {
        c <- substr(text, i, i)
        if (in_str) {
          if (c == quote_char && (i == 1L || substr(text, i - 1L, i - 1L) != "\\"))
            in_str <- FALSE
          result[i] <- " "
          next
        }
        if (c %in% c('"', "'")) {
          in_str <- TRUE
          quote_char <- c
          result[i] <- " "
          next
        }
        result[i] <- c
      }
      paste(result, collapse = "")
    },
    find_undeclared_deps = function(module_name, body_text, all_exports, declared_imports) {
      other_exports <- list()
      for (nm in names(all_exports))
        if (nm != module_name && !nm %in% declared_imports)
          for (sym in all_exports[[nm]])
            other_exports[[sym]] <- c(other_exports[[sym]], nm)
      body_no_strings <- private$strip_strings(body_text)
      tokens <- unique(
        regmatches(body_no_strings, gregexpr("[a-zA-Z0-9_.?-]+", body_no_strings))[[1]]
      )
      reported <- character()
      for (sym in tokens) {
        builtins <- c("lambda", "define", "defmacro", "if", "begin", "quote",
          "set!", "import", "export", "module", "list", "not", "and", "or")
        if (sym %in% builtins) next
        if (!sym %in% names(other_exports)) next
        from_modules <- other_exports[[sym]]
        for (k in from_modules)
          reported <- c(reported, paste0(module_name, " uses ", k, " but does not import ", k))
      }
      unique(reported)
    }
  )
)
