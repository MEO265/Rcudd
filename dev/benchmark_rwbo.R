# Run from the Rcudd checkout with this branch installed:
#   Rscript --vanilla dev/benchmark_rwbo.R
# Or source this file and call benchmark_rwbo(sizes = c(8L, 9L), repeats = 3L).
# Inputs contain only &&, || and !; each pair represents an XOR in expanded form.
rwbo_pair_example <- function(n) {
  stopifnot(length(n) == 1L, n >= 2L, n <= 20L, n == floor(n))
  parts <- vapply(seq_len(n %/% 2L), function(j) {
    a <- letters[[2L * j - 1L]]
    b <- letters[[2L * j]]
    sprintf("((%s && !%s) || (!%s && %s))", a, b, a, b)
  }, character(1L))
  if (n %% 2L) parts <- c(parts, letters[[n]])
  paste(parts, collapse = " && ")
}

benchmark_rwbo <- function(sizes = c(4L, 6L, 8L, 9L, 10L),
                           repeats = 3L, time_limit = 10L) {
  stopifnot(repeats >= 1L, repeats == floor(repeats),
            time_limit >= 1L, time_limit == floor(time_limit))
  if (!requireNamespace("Rcudd", quietly = TRUE) ||
      !requireNamespace("Rwbo", quietly = TRUE)) {
    stop("Install this Rcudd branch and the fixed Rwbo package first.")
  }

  # Time the solver call inside the public interface, in this R process only.
  clock <- function() unname(proc.time()[["elapsed"]])
  timing <- new.env(parent = emptyenv())
  timing$start <- timing$seconds <- NA_real_
  ns <- asNamespace("Rcudd")
  invisible(trace(".rwbo_run_solver",
    tracer = function() timing$start <- clock(),
    exit = function() timing$seconds <- clock() - timing$start,
    where = ns, print = FALSE))
  on.exit(invisible(untrace(".rwbo_run_solver", where = ns)), add = TRUE)

  rows <- list()
  for (n in sizes) {
    expr <- rwbo_pair_example(n)
    # n essential variables need at least n-1 binary operators. This example
    # attains that bound with pairwise XORs followed by ANDs (plus an odd leaf).
    bound <- max(8L, n - 1L)
    inputs <- do.call(expand.grid,
      stats::setNames(rep(list(c(FALSE, TRUE)), n), letters[seq_len(n)]))
    expected <- Reduce(`&`, lapply(seq_len(n %/% 2L), function(j)
      inputs[[2L * j - 1L]] != inputs[[2L * j]]))
    if (n %% 2L) expected <- expected & inputs[[n]]
    for (run in seq_len(repeats)) {
      timing$start <- timing$seconds <- NA_real_
      bdd_seconds <- unname(system.time(
        Rcudd::bdd_restrict_from_expressions(expr, use_rwbo = 0L)
      )[["elapsed"]])
      total <- unname(system.time({
        result <- Rcudd::bdd_restrict_from_expressions(expr, use_rwbo = Inf,
          rwbo_control = list(max_operators = bound, time_limit = time_limit))$chain[[1L]]
      })[["elapsed"]])
      opt <- result$optimization
      parsed <- str2lang(result$reduced_expression)
      actual <- vapply(seq_len(nrow(inputs)), function(i)
        eval(parsed, as.list(inputs[i, , drop = FALSE])), logical(1L))
      stopifnot(identical(actual, expected))
      if (opt$applied) stopifnot(opt$verified, opt$operator_count_after >= n - 1L)
      if (opt$optimal) stopifnot(opt$operator_count_after == n - 1L,
                                opt$cost_after == 2L * (n - 1L))
      # Exhaustive validation above is deliberately outside the measured call.
      row <- data.frame(variables = n, xor_pairs = n %/% 2L, run = run,
        max_operators = bound, time_limit = time_limit, bdd_seconds = bdd_seconds,
        total_seconds = total, solver_seconds = timing$seconds,
        before = opt$operator_count_before, after = opt$operator_count_after,
        cost_before = opt$cost_before, cost_after = opt$cost_after,
        status = opt$status, verified = opt$verified, optimal = opt$optimal,
        expression = if (opt$applied) result$reduced_expression else NA_character_)
      rows[[length(rows) + 1L]] <- row
      print(row[c("variables", "run", "max_operators", "total_seconds",
                  "solver_seconds", "after", "cost_after", "status", "optimal")], row.names = FALSE)
      flush.console()
    }
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  invisible(result)
}

if (sys.nframe() == 0L) benchmark_rwbo()
