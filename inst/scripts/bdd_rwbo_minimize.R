# Exact Boolean formula synthesis with Rcudd and Rwbo.
# Source for solve_rwbo(); run with
# Rscript for the eight-term example. See the adjacent README.md.
# In expression strings, ^ means Boolean XOR and ! means NOT (Rcudd syntax).

rwbo_example_expression <- function() {
  paste("(a & c & e)", "(a & c & f)", "(a & d & e)", "(a & d & f)",
        "(b & c & e)", "(b & c & f)", "(b & d & e)", "(b & d & f)", sep = " | ")
}

.rwbo_integer <- function(value, name, minimum = 0L) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value != floor(value) || value < minimum ||
      value > .Machine$integer.max - 1) {
    stop("'", name, "' must be a whole number >= ", minimum, ".", call. = FALSE)
  }
  as.integer(value)
}

.rwbo_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install the '", package, "' package first.", call. = FALSE)
  }
}

solve_rwbo <- function(problem, time_limit = 60L) {
  if (!inherits(problem, "rwbo_formula_problem")) stop("Use a problem returned by Rcudd::bdd_to_wcnf().")
  time_limit <- .rwbo_integer(time_limit, "time_limit")
  if (!is.null(problem$shortcut)) return(Rcudd::interpret_rwbo_output(problem, ""))
  .rwbo_require("Rwbo")
  output <- Rwbo::run_open_wbo(
    args = c("-algorithm=1", "-print-model", "-verbosity=0", problem$wcnf_file),
    timeout = time_limit
  )
  Rcudd::interpret_rwbo_output(problem, output)
}

rwbo_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- list(expr = rwbo_example_expression(), output_dir = "rwbo-output",
                  max_operators = 8L, max_variables = 10L, time_limit = 60L)
  prepare_only <- FALSE
  for (arg in args) {
    if (arg == "--prepare-only") {
      prepare_only <- TRUE
    } else if (arg == "--help") {
      cat("Usage: Rscript bdd_rwbo_minimize.R [--expr=FORMULA] [--output-dir=DIR]\n",
          "  [--max-operators=8] [--max-variables=10] [--time-limit=60] [--prepare-only]\n",
          "Operators: & (AND), | (OR), ^ (Boolean XOR), ! (NOT).\n", sep = "")
      return(invisible(NULL))
    } else {
      match <- regexec("^--(expr|output-dir|max-operators|max-variables|time-limit)=(.*)$", arg)
      pieces <- regmatches(arg, match)[[1L]]
      if (!length(pieces)) stop("Unknown argument: ", arg)
      key <- gsub("-", "_", pieces[[2L]], fixed = TRUE)
      options[[key]] <- if (key %in% c("expr", "output_dir")) pieces[[3L]] else {
        suppressWarnings(as.numeric(pieces[[3L]]))
      }
    }
  }
  if (!dir.exists(options$output_dir)) {
    if (!dir.create(options$output_dir, recursive = TRUE)) stop("Cannot create output directory.")
  }
  .rwbo_require("Rcudd")
  problem <- Rcudd::bdd_to_wcnf(options$expr, file.path(options$output_dir, "formula.wcnf"),
                          options$max_operators, options$max_variables)
  saveRDS(problem, file.path(options$output_dir, "problem.rds"))
  cat("Input expression: ", problem$expression, "\n",
      "BDD support: ", paste(problem$variables, collapse = ", "), "\n",
      "Truth-table rows: ", length(problem$target), "\n",
      "WCNF: ", problem$wcnf_file, " (", problem$nvars, " variables, ",
      problem$nclauses, " clauses)\n", sep = "")
  if (prepare_only) return(invisible(problem))
  result <- solve_rwbo(problem, options$time_limit)
  saveRDS(result, file.path(options$output_dir, "result.rds"))
  writeLines(result$solver_output, file.path(options$output_dir, "solver.log"))
  cat("Status: ", result$status, "\nExpression: ", result$expression,
      "\nOperators: ", result$operator_count, "\nCost: ", result$cost, "\nBDD equivalent: ", result$verified,
      "\nMinimum within operator bound proven: ", result$optimal, "\n", sep = "")
  if (!result$verified) {
    stop("No verified formula returned. UNSAT means no formula within --max-operators; ",
         "UNKNOWN may require a longer --time-limit.", call. = FALSE)
  }
  if (!result$optimal) warning("A verified formula was found, but optimality is unproven.")
  invisible(result)
}

if (sys.nframe() == 0L) rwbo_main()
