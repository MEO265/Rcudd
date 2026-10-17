# Compare the old and new costs through the same public Rcudd interface.
# Rscript --vanilla dev/compare_rwbo_costs.R UNIT_LIBRARY WEIGHTED_LIBRARY OUTPUT_DIR
# Libraries must contain commits 654c526 (unit) and 9465ada (weighted), respectively.

.rwbo_compare_worker <- function(job_file, result_file) {
  job <- readRDS(job_file)
  .libPaths(c(job$library, .libPaths()))
  stopifnot(requireNamespace("Rcudd", quietly = TRUE),
            requireNamespace("Rwbo", quietly = TRUE))
  ns <- asNamespace("Rcudd")
  package_path <- normalizePath(find.package("Rcudd"), winslash = "/")
  stopifnot(identical(package_path,
    normalizePath(file.path(job$library, "Rcudd"), winslash = "/")))

  # Verify the installed cost model and warm both native libraries before timing.
  probe_path <- tempfile(fileext = ".wcnf")
  on.exit(unlink(probe_path), add = TRUE)
  probe <- Rcudd::bdd_to_wcnf("a ^ b", probe_path, max_operators = 1L)
  header <- as.integer(strsplit(readLines(probe_path, n = 1L), " ")[[1L]][3:5])
  expected <- if (job$model == "unit") c(25L, 131L, 2L) else c(25L, 132L, 3L)
  stopifnot(identical(header, expected))
  probe_output <- Rwbo::run_open_wbo(
    c("-algorithm=1", "-print-model", "-verbosity=0", probe_path), timeout = 2L)
  probe_result <- Rcudd::interpret_rwbo_output(probe, probe_output)
  stopifnot(probe_result$verified, probe_result$optimal,
            probe_result$operator_count == 1L)
  if (job$model == "weighted") stopifnot(probe_result$cost == 2L)

  clock <- function() unname(proc.time()[["elapsed"]])
  timing <- new.env(parent = emptyenv())
  timing$solver_seconds <- NA_real_
  timing$header <- NULL
  original <- get(".rwbo_run_solver", ns)
  instrumented <- function(path, timeout) {
    # Only read the header; neither copy nor change the generated problem.
    timing$header <- readLines(path, n = 1L)
    timing$timeout <- timeout
    started <- clock()
    on.exit(timing$solver_seconds <- clock() - started)
    original(path, timeout)
  }
  unlockBinding(".rwbo_run_solver", ns)
  assign(".rwbo_run_solver", instrumented, envir = ns)
  lockBinding(".rwbo_run_solver", ns)
  on.exit({
    unlockBinding(".rwbo_run_solver", ns)
    assign(".rwbo_run_solver", original, envir = ns)
    lockBinding(".rwbo_run_solver", ns)
  }, add = TRUE)

  baseline <- NULL
  bdd_seconds <- unname(system.time({
    baseline <- Rcudd::bdd_restrict_from_expressions(job$expression, use_rwbo = 0L)$chain[[1L]]
  })[["elapsed"]])
  entry <- NULL
  total_seconds <- unname(system.time({
    entry <- Rcudd::bdd_restrict_from_expressions(job$expression, use_rwbo = Inf,
      rwbo_control = list(max_operators = job$max_operators,
                          max_clauses = 2000000L, time_limit = job$time_limit))$chain[[1L]]
  })[["elapsed"]])

  # Exhaustive validation and all bookkeeping are outside the measured interval.
  parsed <- str2lang(entry$reduced_expression)
  actual <- vapply(seq_len(nrow(job$inputs)), function(i)
    eval(parsed, as.list(job$inputs[i, , drop = FALSE])), logical(1L))
  stopifnot(identical(actual, job$target), length(timing$header) == 1L)
  observed_header <- as.integer(strsplit(timing$header, " ")[[1L]][3:5])
  # nvars = K^2 + K * (4 + 2*(n+2) + 3*2^n); recover the actual slot count.
  linear <- 4 + 2 * (job$variables + 2) + 3 * 2^job$variables
  slots <- as.integer(round((sqrt(linear^2 + 4 * observed_header[[1L]]) - linear) / 2))
  stopifnot(observed_header[[1L]] == slots^2 + linear * slots,
            slots == job$max_operators, timing$timeout == job$time_limit)
  opt <- entry$optimization
  stopifnot(opt$status %in% c("improved", "not_improved", "time_limit"))
  if (opt$optimal) stopifnot(opt$verified,
    opt$operator_count_after == job$variables - 1L)
  if (opt$optimal && job$model == "weighted")
    stopifnot(opt$cost_after == 2L * (job$variables - 1L))
  dll <- list.files(file.path(package_path, "libs"), "Rcudd[.]dll$",
                    recursive = TRUE, full.names = TRUE)
  row <- data.frame(phase = job$phase, case = job$case, model = job$model,
    run = job$run, position = job$position, variables = job$variables,
    max_operators = slots, time_limit = job$time_limit,
    total_seconds = total_seconds, solver_seconds = timing$solver_seconds,
    bdd_seconds = bdd_seconds, status = opt$status, verified = opt$verified,
    optimal = opt$optimal, operator_count = opt$operator_count_after,
    nvars = observed_header[[1L]], nclauses = observed_header[[2L]],
    baseline_cpp = baseline$reduced_cpp_expression,
    expression = entry$reduced_expression,
    source_commit = job$commit, rcudd_path = package_path,
    rcudd_dll_md5 = unname(tools::md5sum(dll)[[1L]]),
    r_version = as.character(getRversion()),
    rwbo_version = as.character(utils::packageVersion("Rwbo")))
  saveRDS(list(row = row, entry = entry, probe_header = header), result_file)
  invisible(row)
}

compare_rwbo_costs <- function(unit_library, weighted_library,
                               output_dir = ".rwbo-test/cost-comparison",
                               repeats = 5L, time_limit = 10L,
                               extended_time_limit = 30L,
                               script = "dev/compare_rwbo_costs.R") {
  stopifnot(repeats >= 1L, repeats == floor(repeats), time_limit >= 1L,
            time_limit == floor(time_limit), extended_time_limit >= time_limit)
  script <- normalizePath(script, winslash = "/", mustWork = TRUE)
  libraries <- c(unit = normalizePath(unit_library, winslash = "/", mustWork = TRUE),
    weighted = normalizePath(weighted_library, winslash = "/", mustWork = TRUE))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  source(file.path(dirname(script), "benchmark_rwbo.R"), local = TRUE)
  cases <- list(list(name = "parity3", n = 3L, bound = 4L,
    expression = paste("(a && !b && !c)", "(!a && b && !c)",
                       "(!a && !b && c)", "(a && b && c)", sep = " || ")))
  for (n in c(4L, 6L, 8L, 9L)) cases[[length(cases) + 1L]] <- list(
    name = paste0("pairs", n), n = n, bound = 8L, expression = rwbo_pair_example(n))
  for (i in seq_along(cases)) {
    case <- cases[[i]]
    case$inputs <- do.call(expand.grid,
      stats::setNames(rep(list(c(FALSE, TRUE)), case$n), letters[seq_len(case$n)]))
    parsed <- str2lang(case$expression)
    case$target <- vapply(seq_len(nrow(case$inputs)), function(j)
      eval(parsed, as.list(case$inputs[j, , drop = FALSE])), logical(1L))
    cases[[i]] <- case
  }
  commits <- c(unit = "654c526", weighted = "9465ada")
  schedule <- list()
  add_pair <- function(case, phase, run, limit, case_index) {
    models <- if ((run + case_index) %% 2L) c("weighted", "unit") else c("unit", "weighted")
    for (position in seq_along(models)) {
      model <- models[[position]]
      schedule[[length(schedule) + 1L]] <<- list(phase = phase, case = case$name,
        model = model, run = run, position = position, library = libraries[[model]],
        commit = commits[[model]], expression = case$expression, variables = case$n,
        inputs = case$inputs, target = case$target, max_operators = case$bound,
        time_limit = as.integer(limit))
    }
  }
  # Alternate AB/BA for each case; reverse case order in every second round.
  for (run in seq_len(repeats)) {
    order <- if (run %% 2L) seq_along(cases) else rev(seq_along(cases))
    for (i in order) add_pair(cases[[i]], "primary", run, time_limit, i)
  }
  for (i in 4:5) add_pair(cases[[i]], "extended", 1L, extended_time_limit, i)
  saveRDS(schedule, file.path(output_dir, "schedule.rds"))
  rows <- list()
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  for (i in seq_along(schedule)) {
    job <- schedule[[i]]
    stem <- sprintf("%03d-%s-%s-%s", i, job$phase, job$case, job$model)
    job_file <- file.path(output_dir, paste0(stem, "-job.rds"))
    result_file <- file.path(output_dir, paste0(stem, "-result.rds"))
    log_file <- file.path(output_dir, paste0(stem, ".log"))
    saveRDS(job, job_file)
    status <- system2(rscript, c("--vanilla", shQuote(script), "--worker",
                                shQuote(job_file), shQuote(result_file)),
                      stdout = log_file, stderr = log_file)
    if (status != 0L || !file.exists(result_file)) {
      cat(readLines(log_file), sep = "\n")
      stop("Comparison worker failed: ", stem)
    }
    rows[[length(rows) + 1L]] <- readRDS(result_file)$row
    row <- rows[[length(rows)]]
    write.csv(do.call(rbind, rows), file.path(output_dir, "runs.csv"), row.names = FALSE)
    cat(sprintf("%02d/%02d %-8s %-7s %-8s run%d total=%6.2fs solver=%6.2fs %s\n",
      i, length(schedule), job$phase, job$case, job$model, job$run,
      row$total_seconds, row$solver_seconds, row$status))
    flush.console()
  }
  result <- do.call(rbind, rows)
  for (phase in unique(result$phase)) for (case in unique(result$case)) {
    group <- result[result$phase == phase & result$case == case, , drop = FALSE]
    for (run in unique(group$run)) {
      pair <- group[group$run == run, , drop = FALSE]
      if (!nrow(pair)) next
      a <- pair[pair$model == "unit", , drop = FALSE]
      b <- pair[pair$model == "weighted", , drop = FALSE]
      stopifnot(nrow(a) == 1L, nrow(b) == 1L,
        a$nvars == b$nvars, a$max_operators == b$max_operators,
        b$nclauses - a$nclauses == a$max_operators,
        a$baseline_cpp == b$baseline_cpp, a$rwbo_version == b$rwbo_version)
    }
  }
  groups <- split(result, interaction(result$phase, result$case, result$model, drop = TRUE))
  summary <- do.call(rbind, lapply(groups, function(group) data.frame(
    phase = group$phase[[1L]], case = group$case[[1L]], model = group$model[[1L]],
    variables = group$variables[[1L]], max_operators = group$max_operators[[1L]],
    time_limit = group$time_limit[[1L]], runs = nrow(group),
    median_total = stats::median(group$total_seconds),
    min_total = min(group$total_seconds), max_total = max(group$total_seconds),
    median_solver = stats::median(group$solver_seconds),
    optimal = sum(group$optimal), timeouts = sum(group$status == "time_limit"))))
  rownames(summary) <- NULL
  write.csv(summary, file.path(output_dir, "summary.csv"), row.names = FALSE)
  print(summary, row.names = FALSE)
  invisible(list(runs = result, summary = summary))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 3L && args[[1L]] == "--worker") {
    .rwbo_compare_worker(args[[2L]], args[[3L]])
  } else if (length(args) == 3L) {
    compare_rwbo_costs(args[[1L]], args[[2L]], args[[3L]])
  } else stop("Usage: Rscript dev/compare_rwbo_costs.R UNIT_LIBRARY WEIGHTED_LIBRARY OUTPUT_DIR")
}
