# Compare operator costs or equivalent encodings in isolated R processes.
# Rscript --vanilla dev/compare_rwbo_costs.R UNIT_LIBRARY WEIGHTED_LIBRARY OUTPUT_DIR
# CLI defaults: commits 654c526 (unit) and 9465ada (weighted).
# For weighted before/after comparisons see benchmark_binary_bound.R.

# Exact extra counters/clauses for the unrestricted binary lower-bound encoder.
.rwbo_bound_counts <- function(n, k, model) {
  r <- max(0L, n - 1L)
  if (model != "bounded" || r == 0L) return(c(auxiliary = 0, clauses = 0))
  if (k == r) return(c(auxiliary = 0, clauses = k))
  if (r == 1L) return(c(auxiliary = 0, clauses = 1))
  d <- k - r + 1L
  c(auxiliary = r * d, clauses = (2L * r - 1L) * d + 1L)
}

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
  expected <- switch(job$model, unit = c(25L, 131L, 2L),
    weighted = c(25L, 132L, 3L), bounded = c(25L, 133L, 3L))
  stopifnot(identical(header, expected))
  probe_output <- Rwbo::run_open_wbo(
    c("-algorithm=1", "-print-model", "-verbosity=0", probe_path), timeout = 2L)
  probe_result <- Rcudd::interpret_rwbo_output(probe, probe_output)
  stopifnot(probe_result$verified, probe_result$optimal,
            probe_result$operator_count == 1L)
  if (job$model != "unit") stopifnot(probe_result$cost == 2L)
  if (job$model == "bounded") stopifnot(probe$binary_lower_bound == 1L)

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

  route <- if (is.null(job$route)) "automatic" else job$route
  cpp_input <- gsub("||", "|", gsub("&&", "&", job$expression, fixed = TRUE), fixed = TRUE)
  count_ops <- function(expr) lengths(regmatches(expr, gregexpr("[!&|^]", expr)))
  weighted_cost <- function(expr) {
    ops <- regmatches(expr, gregexpr("[!&|^]", expr))[[1L]]
    sum(ifelse(ops == "!", 1L, 2L))
  }
  baseline <- NULL
  bdd_seconds <- unname(system.time({
    baseline <- if (route == "automatic") {
      Rcudd::bdd_restrict_from_expressions(job$expression, use_rwbo = 0L)$chain[[1L]]
    } else Rcudd::bdd_literals(cpp_input)
  })[["elapsed"]])
  entry <- problem <- output <- NULL
  if (route == "automatic") {
    total_seconds <- unname(system.time({
      entry <- Rcudd::bdd_restrict_from_expressions(job$expression, use_rwbo = Inf,
        rwbo_control = list(max_operators = job$max_operators,
                            max_clauses = 2000000L, time_limit = job$time_limit))$chain[[1L]]
    })[["elapsed"]])
    opt <- entry$optimization
    candidate_cpp <- entry$reduced_cpp_expression
    expression <- entry$reduced_expression
    baseline_cpp <- baseline$reduced_cpp_expression
    parsed <- str2lang(expression)
    actual <- vapply(seq_len(nrow(job$inputs)), function(i)
      eval(parsed, as.list(job$inputs[i, , drop = FALSE])), logical(1L))
    stopifnot(identical(actual, job$target))
  } else {
    problem_path <- tempfile(fileext = ".wcnf")
    on.exit(unlink(problem_path), add = TRUE)
    result <- NULL
    total_seconds <- unname(system.time({
      problem <- Rcudd::bdd_to_wcnf(cpp_input, problem_path,
        max_operators = job$max_operators, max_clauses = 2000000L)
      output <- tryCatch(instrumented(problem_path, job$time_limit), error = identity)
      if (!inherits(output, "error")) result <- Rcudd::interpret_rwbo_output(problem, output)
    })[["elapsed"]])
    stopifnot(identical(problem$variables, letters[seq_len(job$variables)]),
              identical(problem$target, job$target))
    if (inherits(output, "error")) {
      if (!grepl("timed out|timeout", conditionMessage(output), ignore.case = TRUE)) stop(output)
      opt <- list(status = "time_limit", verified = FALSE, optimal = FALSE,
                  operator_count_after = count_ops(cpp_input))
      candidate_cpp <- cpp_input
    } else {
      stopifnot(result$verified)
      opt <- list(status = if (result$optimal) "optimal" else "candidate",
        verified = result$verified, optimal = result$optimal,
        operator_count_after = result$operator_count)
      candidate_cpp <- result$expression
      # Independent BDD reconstruction after timing; K=0 keeps this check tiny.
      verify_path <- tempfile(fileext = ".wcnf")
      on.exit(unlink(verify_path), add = TRUE)
      independent <- Rcudd::bdd_to_wcnf(candidate_cpp, verify_path, max_operators = 0L)
      stopifnot(identical(independent$variables, problem$variables),
                identical(independent$target, job$target))
    }
    expression <- candidate_cpp
    baseline_cpp <- baseline$reduced_expression
    entry <- result
  }

  slots <- NA_integer_
  observed_header <- rep(NA_integer_, 3L)
  if (length(timing$header)) {
    observed_header <- as.integer(strsplit(timing$header, " ")[[1L]][3:5])
    linear <- 4 + 2 * (job$variables + 2) + 3 * 2^job$variables
    possible <- seq_len(job$max_operators)
    sizes <- vapply(possible, function(k) k^2 + linear * k +
      .rwbo_bound_counts(job$variables, k, job$model)[["auxiliary"]], numeric(1L))
    slots <- possible[sizes == observed_header[[1L]]]
    stopifnot(length(slots) == 1L, slots <= job$max_operators,
              timing$timeout == job$time_limit)
    if (route == "direct" || isTRUE(job$matching_slots)) stopifnot(slots == job$max_operators)
  } else stopifnot(route == "automatic", opt$status %in% c("minimal", "not_candidate"))
  expected_ops <- job$expected_operators
  if (is.null(expected_ops) && is.null(job$route)) expected_ops <- job$variables - 1L
  if (opt$optimal && !is.null(expected_ops))
    stopifnot(opt$verified, opt$operator_count_after == expected_ops)
  if (opt$optimal && job$model != "unit" && !is.null(job$expected_cost))
    stopifnot(weighted_cost(candidate_cpp) == job$expected_cost)
  stopifnot(opt$status %in% c("improved", "not_improved", "time_limit",
                             "minimal", "not_candidate", "optimal", "candidate"))
  dll <- list.files(file.path(package_path, "libs"), "Rcudd[.]dll$",
                    recursive = TRUE, full.names = TRUE)
  row <- data.frame(phase = job$phase, case = job$case, model = job$model,
    route = route, input_optimal = if (is.null(job$input_optimal)) NA else job$input_optimal,
    binary_lower_bound = if (job$model == "bounded") job$variables - 1L else 0L,
    binary_auxiliaries = if (is.na(slots)) NA_real_ else
      .rwbo_bound_counts(job$variables, slots, job$model)[["auxiliary"]],
    input_operator_count = count_ops(cpp_input),
    configured_max_operators = job$max_operators,
    solver_called = length(timing$header) == 1L,
    xor_count = lengths(regmatches(candidate_cpp, gregexpr("^", candidate_cpp, fixed = TRUE))),
    weighted_cost = weighted_cost(candidate_cpp),
    run = job$run, position = job$position, variables = job$variables,
    max_operators = slots, time_limit = job$time_limit,
    total_seconds = total_seconds, solver_seconds = timing$solver_seconds,
    bdd_seconds = bdd_seconds, status = opt$status, verified = opt$verified,
    optimal = opt$optimal, operator_count = opt$operator_count_after,
    nvars = observed_header[[1L]], nclauses = observed_header[[2L]],
    baseline_cpp = baseline_cpp,
    expression = expression,
    source_commit = job$commit, rcudd_path = package_path,
    rcudd_dll_md5 = unname(tools::md5sum(dll)[[1L]]),
    r_version = as.character(getRversion()),
    rwbo_version = as.character(utils::packageVersion("Rwbo")))
  saveRDS(list(row = row, entry = entry, probe_header = header, problem = problem,
               output = output), result_file)
  invisible(row)
}

compare_rwbo_costs <- function(unit_library, weighted_library,
                               output_dir = ".rwbo-test/cost-comparison",
                               repeats = 5L, time_limit = 10L,
                               extended_time_limit = 30L,
                               script = "dev/compare_rwbo_costs.R", cases = NULL,
                               route = "automatic", matching_slots = TRUE,
                               models = c("unit", "weighted"),
                               source_commits = c("654c526", "9465ada")) {
  stopifnot(repeats >= 1L, repeats == floor(repeats), time_limit >= 1L,
            time_limit == floor(time_limit), (is.null(extended_time_limit) || extended_time_limit >= time_limit),
            route %in% c("automatic", "direct"), length(models) == 2L,
            !anyDuplicated(models), all(models %in% c("unit", "weighted", "bounded")),
            length(source_commits) == 2L)
  script <- normalizePath(script, winslash = "/", mustWork = TRUE)
  libraries <- stats::setNames(c(
    normalizePath(unit_library, winslash = "/", mustWork = TRUE),
    normalizePath(weighted_library, winslash = "/", mustWork = TRUE)), models)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  source(file.path(dirname(script), "benchmark_rwbo.R"), local = TRUE)
  default_cases <- is.null(cases)
  if (default_cases) {
  cases <- list(list(name = "parity3", n = 3L, bound = 4L,
    expression = paste("(a && !b && !c)", "(!a && b && !c)",
                       "(!a && !b && c)", "(a && b && c)", sep = " || ")))
  for (n in c(4L, 6L, 8L, 9L)) cases[[length(cases) + 1L]] <- list(
    name = paste0("pairs", n), n = n, bound = 8L, expression = rwbo_pair_example(n))
  }
  for (i in seq_along(cases)) {
    case <- cases[[i]]
    if (default_cases) {
      case$expected_operators <- case$n - 1L
      case$expected_cost <- 2L * (case$n - 1L)
    }
    case$inputs <- do.call(expand.grid,
      stats::setNames(rep(list(c(FALSE, TRUE)), case$n), letters[seq_len(case$n)]))
    parsed <- str2lang(case$expression)
    case$target <- vapply(seq_len(nrow(case$inputs)), function(j)
      eval(parsed, as.list(case$inputs[j, , drop = FALSE])), logical(1L))
    cases[[i]] <- case
  }
  commits <- stats::setNames(source_commits, models)
  schedule <- list()
  add_pair <- function(case, phase, run, limit, case_index) {
    pair_models <- if ((run + case_index) %% 2L) rev(models) else models
    for (position in seq_along(pair_models)) {
      model <- pair_models[[position]]
      schedule[[length(schedule) + 1L]] <<- list(phase = phase, case = case$name,
        model = model, run = run, position = position, library = libraries[[model]],
        commit = commits[[model]], expression = case$expression, variables = case$n,
        inputs = case$inputs, target = case$target, max_operators = case$bound,
        time_limit = as.integer(limit), route = route, matching_slots = matching_slots,
        input_optimal = if (is.null(case$input_optimal)) NA else case$input_optimal,
        expected_operators = case$expected_operators,
        expected_cost = case$expected_cost)
    }
  }
  # Alternate AB/BA for each case; reverse case order in every second round.
  for (run in seq_len(repeats)) {
    order <- if (run %% 2L) seq_along(cases) else rev(seq_along(cases))
    for (i in order) add_pair(cases[[i]], "primary", run, time_limit, i)
  }
  if (!is.null(extended_time_limit))
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
      a <- pair[pair$model == models[[1L]], , drop = FALSE]
      b <- pair[pair$model == models[[2L]], , drop = FALSE]
      stopifnot(nrow(a) == 1L, nrow(b) == 1L,
        a$baseline_cpp == b$baseline_cpp, a$rwbo_version == b$rwbo_version,
        a$solver_called == b$solver_called)
      if (all(models %in% c("weighted", "bounded")) && a$optimal && b$optimal)
        stopifnot(a$weighted_cost == b$weighted_cost)
      if (a$solver_called && (matching_slots || route == "direct"))
        stopifnot(a$max_operators == b$max_operators)
      if (a$solver_called && !is.na(a$max_operators) && a$max_operators == b$max_operators) {
        k <- a$max_operators
        extra_a <- .rwbo_bound_counts(a$variables, k, a$model)
        extra_b <- .rwbo_bound_counts(b$variables, k, b$model)
        soft_a <- k * if (a$model == "unit") 1L else 2L
        soft_b <- k * if (b$model == "unit") 1L else 2L
        stopifnot(b$nvars - a$nvars == extra_b[["auxiliary"]] - extra_a[["auxiliary"]],
          b$nclauses - a$nclauses == soft_b - soft_a + extra_b[["clauses"]] - extra_a[["clauses"]])
      }
    }
  }
  groups <- split(result, interaction(result$phase, result$case, result$model, drop = TRUE))
  summary <- do.call(rbind, lapply(groups, function(group) data.frame(
    phase = group$phase[[1L]], case = group$case[[1L]], model = group$model[[1L]],
    variables = group$variables[[1L]], max_operators = group$max_operators[[1L]],
    time_limit = group$time_limit[[1L]], runs = nrow(group),
    solver_calls = sum(group$solver_called),
    statuses = paste(sort(unique(group$status)), collapse = "/"),
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
