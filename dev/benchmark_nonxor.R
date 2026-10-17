# Non-XOR comparison, including globally optimal input formulas.
# Rscript --vanilla dev/benchmark_nonxor.R UNIT_LIBRARY WEIGHTED_LIBRARY OUTPUT_DIR
nonxor_cases <- function(sizes = c(4L, 6L, 8L, 9L)) {
  cases <- list()
  add <- function(name, family, n, expression, bound, optimal, operators = NULL, cost = NULL) {
    cases[[length(cases) + 1L]] <<- list(name = paste0(name, n), family = family,
      n = n, expression = expression, bound = as.integer(bound), input_optimal = optimal,
      expected_operators = operators, expected_cost = cost)
  }
  for (n in sizes) {
    vars <- letters[seq_len(n)]
    and <- paste(vars, collapse = " && ")
    add("and", "AND", n, and, 8L, TRUE, n - 1L, 2L * (n - 1L))
    add("or", "OR", n, paste(vars, collapse = " || "), 8L, TRUE, n - 1L, 2L * (n - 1L))
    add("nand", "NAND", n, paste0("!(", and, ")"), max(8L, n), TRUE, n, 2L * n - 1L)
    pairs <- lapply(seq_len(n %/% 2L), function(j) vars[c(2L*j - 1L, 2L*j)])
    groups <- vapply(pairs, function(pair) paste0("(", paste(pair, collapse = " || "), ")"), "")
    if (n %% 2L) groups <- c(groups, vars[[n]])
    add("cnf", "AND of OR pairs", n, paste(groups, collapse = " && "),
        8L, TRUE, n - 1L, 2L * (n - 1L))
    choices <- do.call(expand.grid, rep(list(1:2), length(pairs)))
    terms <- vapply(seq_len(nrow(choices)), function(row) {
      selected <- vapply(seq_along(pairs), function(j) pairs[[j]][[choices[row, j]]], "")
      if (n %% 2L) selected <- c(selected, vars[[n]])
      paste0("(", paste(selected, collapse = " && "), ")")
    }, "")
    add("dnf", "Expanded OR-pair product", n, paste(terms, collapse = " || "),
        8L, FALSE, n - 1L, 2L * (n - 1L))
    threshold <- "((a && b) || ((a || b) && (c || d)) || (c && d))"
    if (n > 4L) threshold <- paste(c(threshold, vars[5:n]), collapse = " && ")
    add("threshold", "At least two of a:b:c:d, AND remaining variables", n,
        threshold, max(8L, n + 3L), NA)
  }
  cases
}

benchmark_nonxor <- function(unit_library, weighted_library, output_dir,
                             repeats = 3L, time_limit = 10L) {
  source("dev/compare_rwbo_costs.R", local = TRUE)
  cases <- nonxor_cases()
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  descriptions <- do.call(rbind, lapply(cases, function(case) data.frame(
    case = case$name, family = case$family, variables = case$n,
    max_operators = case$bound, input_optimal = case$input_optimal,
    expression = case$expression)))
  write.csv(descriptions, file.path(output_dir, "cases.csv"), row.names = FALSE)
  results <- list()
  cat("AUTOMATIC: one paired run per input, including redundant DNF.\n")
  flush.console()
  results$automatic <- compare_rwbo_costs(unit_library, weighted_library,
    file.path(output_dir, "automatic"), repeats = 1L, time_limit = time_limit,
    extended_time_limit = NULL, cases = cases[!vapply(cases, function(x) x$family ==
      "At least two of a:b:c:d, AND remaining variables", logical(1L))],
    route = "automatic", matching_slots = FALSE)
  cat("DIRECT OPTIMAL: identical bounds, three paired repetitions per input.\n")
  flush.console()
  results$direct <- compare_rwbo_costs(unit_library, weighted_library,
    file.path(output_dir, "direct"), repeats = repeats, time_limit = time_limit,
    extended_time_limit = NULL,
    cases = cases[vapply(cases, function(x) isTRUE(x$input_optimal), logical(1L))],
    route = "direct")
  cat("DIRECT THRESHOLD: one paired run per input, feasible witness included.\n")
  flush.console()
  results$threshold <- compare_rwbo_costs(unit_library, weighted_library,
    file.path(output_dir, "threshold"), repeats = 1L, time_limit = time_limit,
    extended_time_limit = NULL,
    cases = cases[vapply(cases, function(x) is.na(x$input_optimal), logical(1L))],
    route = "direct")
  saveRDS(results, file.path(output_dir, "all-results.rds"))
  invisible(results)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L)
    stop("Usage: Rscript dev/benchmark_nonxor.R UNIT_LIBRARY WEIGHTED_LIBRARY OUTPUT_DIR")
  benchmark_nonxor(args[[1L]], args[[2L]], args[[3L]])
}
