# Compare identical weighted costs before/after the redundant binary bound.
# Rscript --vanilla dev/benchmark_binary_bound.R BEFORE_LIBRARY AFTER_LIBRARY OUTPUT_DIR AFTER_COMMIT
benchmark_binary_bound <- function(before_library, after_library, output_dir,
                                    after_commit, repeats = 3L, time_limit = 10L) {
  source("dev/compare_rwbo_costs.R", local = TRUE)
  source("dev/benchmark_nonxor.R", local = TRUE)
  source("dev/benchmark_rwbo.R", local = TRUE)
  known <- nonxor_cases()
  get_case <- function(name) {
    found <- Filter(function(x) x$name == name, known)
    stopifnot(length(found) == 1L)
    found[[1L]]
  }
  xor_case <- function(n) list(name = paste0("xor", n), n = n, bound = 8L,
    expression = rwbo_pair_example(n), input_optimal = FALSE,
    expected_operators = n - 1L, expected_cost = 2L * (n - 1L))
  direct <- c(lapply(c(4L, 6L, 8L, 9L), xor_case),
    lapply(c("and8", "or8", "cnf6", "cnf8", "nand6", "nand9"), get_case))
  automatic <- c(lapply(c("and8", "or8", "nand9", "cnf6", "cnf8", "dnf8"), get_case),
    lapply(c(8L, 9L), xor_case))
  threshold <- lapply(c("threshold4", "threshold8"), get_case)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  all_cases <- c(direct, automatic, threshold)
  descriptions <- do.call(rbind, lapply(all_cases, function(case) data.frame(
    case = case$name, variables = case$n, max_operators = case$bound,
    input_optimal = case$input_optimal, expression = case$expression)))
  write.csv(unique(descriptions), file.path(output_dir, "cases.csv"), row.names = FALSE)
  run <- function(cases, suite, n, route) compare_rwbo_costs(before_library, after_library,
    file.path(output_dir, suite), repeats = n, time_limit = time_limit,
    extended_time_limit = NULL, cases = cases, route = route,
    matching_slots = TRUE, models = c("weighted", "bounded"),
    source_commits = c("9465ada", after_commit))
  cat("DIRECT: three paired runs, unchanged costs/bounds, XOR and non-XOR.\n")
  flush.console()
  results <- list(direct = run(direct, "direct", repeats, "direct"))
  cat("AUTOMATIC: skipped cases and actual public interface behavior.\n")
  flush.console()
  results$automatic <- run(automatic, "automatic", 1L, "automatic")
  cat("THRESHOLD: one exploratory paired run per case.\n")
  flush.console()
  results$threshold <- run(threshold, "threshold", 1L, "direct")
  saveRDS(results, file.path(output_dir, "all-results.rds"))
  invisible(results)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 4L)
    stop("Usage: Rscript dev/benchmark_binary_bound.R BEFORE_LIBRARY AFTER_LIBRARY OUTPUT_DIR AFTER_COMMIT")
  benchmark_binary_bound(args[[1L]], args[[2L]], args[[3L]], args[[4L]])
}
