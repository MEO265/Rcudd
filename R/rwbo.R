# Shared controls and lifecycle for optional native-prepared MaxSAT problems.
.rwbo_scalar <- function(value, name, minimum = 0, infinite = FALSE) {
  valid <- typeof(value) %in% c("integer", "double") && !is.object(value) && is.null(dim(value)) &&
    length(value) == 1L && !is.na(value) && value >= minimum &&
    ((is.finite(value) && value == floor(value)) ||
       (infinite && value == Inf))
  if (!valid) {
    suffix <- if (infinite) " or Inf" else ""
    stop("'", name, "' must be a numeric whole number >= ", minimum,
         suffix, "; logical values are not accepted.", call. = FALSE)
  }
  value
}

.rwbo_elapsed <- function() unname(proc.time()[["elapsed"]])

.rwbo_available <- function() requireNamespace("Rwbo", quietly = TRUE)

.rwbo_create_directory <- function(path) dir.create(path, showWarnings = FALSE)

.rwbo_setup <- function(use_rwbo, rwbo_control) {
  limit <- .rwbo_scalar(use_rwbo, "use_rwbo", infinite = TRUE)
  if (!is.list(rwbo_control) || is.object(rwbo_control)) {
    stop("'rwbo_control' must be a named list.", call. = FALSE)
  }
  defaults <- list(max_operators = 8L, max_clauses = 2000000L, time_limit = 5L)
  keys <- names(rwbo_control)
  if (length(rwbo_control) &&
      (is.null(keys) || anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys) ||
       any(!keys %in% names(defaults)))) {
    stop("'rwbo_control' accepts unique names: max_operators, max_clauses, time_limit.",
         call. = FALSE)
  }
  for (key in keys) defaults[key] <- rwbo_control[key]
  for (key in names(defaults)) {
    minimum <- if (key == "max_operators") 0L else 1L
    value <- .rwbo_scalar(defaults[[key]], paste0("rwbo_control$", key), minimum)
    maximum <- .Machine$integer.max - if (key == "max_operators") 2L else 1L
    if (value > maximum) {
      stop("'rwbo_control$", key, "' exceeds the supported integer range.",
           call. = FALSE)
    }
    defaults[[key]] <- as.integer(value)
  }
  state <- list(start = .rwbo_elapsed(), time_limit = defaults$time_limit,
                directory = NULL, config = NULL)
  if (limit == 0) return(state)

  directory <- tempfile("rcudd-rwbo-")
  # An unavailable directory becomes a native resource-limit fallback.
  tryCatch(.rwbo_create_directory(directory), error = function(error) FALSE)
  # From here the public wrapper owns cleanup, including native/solver failures.
  state$directory <- directory
  available <- tryCatch(.rwbo_available(), error = function(error) FALSE)
  state$config <- c(list(max_variables = as.integer(min(limit, 20)),
                        directory = directory, available = available), defaults)
  state
}

.rwbo_operator_count <- function(expr) {
  hits <- gregexpr("[!&|^]", expr, perl = TRUE)[[1L]]
  if (hits[[1L]] == -1L) 0L else length(hits)
}

.rwbo_operator_cost <- function(expr) {
  operators <- regmatches(expr, gregexpr("[!&|^]", expr, perl = TRUE))[[1L]]
  # Use doubles for arithmetic: a long formula's weighted cost can exceed an R integer.
  sum(ifelse(operators == "!", 1, 2))
}

.rwbo_diagnostic <- function(status, before, after = before, applied = FALSE,
                             verified = FALSE, optimal = FALSE,
                             solver_status = NA_character_, message = NA_character_,
                             cost_before, cost_after = cost_before) {
  list(status = status, applied = applied,
       operator_count_before = as.integer(before), operator_count_after = as.integer(after),
       cost_before = as.numeric(cost_before), cost_after = as.numeric(cost_after),
       verified = verified, optimal = optimal, solver_status = solver_status,
       message = .rwbo_message(message))
}

.rwbo_message <- function(message) {
  if (is.null(message) || !length(message) || is.na(message[[1L]])) return(NA_character_)
  lines <- strsplit(message[[1L]], "\n", fixed = TRUE)[[1L]]
  lines <- lines[nzchar(lines)]
  if (!length(lines)) NA_character_ else substr(lines[[1L]], 1L, 500L)
}

.rwbo_run_solver <- function(path, timeout) {
  Rwbo::run_open_wbo(
    args = c("-algorithm=1", "-print-model", "-verbosity=0", path), timeout = timeout
  )
}

.rwbo_finish <- function(entry, state) {
  prepared <- attr(entry, "rwbo", exact = TRUE)
  attr(entry, "rwbo") <- NULL
  if (is.null(prepared)) return(entry)

  before <- prepared$operator_count
  if (is.null(before)) before <- .rwbo_operator_count(entry$reduced_expression)
  cost_before <- prepared$cost
  if (is.null(cost_before)) cost_before <- .rwbo_operator_cost(entry$reduced_expression)
  message <- prepared$message
  entry$optimization <- .rwbo_diagnostic(
    prepared$status, before, verified = isTRUE(prepared$optimal),
    optimal = isTRUE(prepared$optimal), message = message, cost_before = cost_before
  )
  if (!identical(prepared$status, "prepared")) return(entry)

  output <- ""
  if (is.null(prepared$problem$shortcut)) {
    remaining <- state$time_limit - (.rwbo_elapsed() - state$start)
    if (remaining <= 0) {
      entry$optimization$status <- "time_limit"
      entry$optimization$message <- "The shared RWBO time budget was exhausted."
      return(entry)
    }
    # Rwbo accepts whole seconds; rounding can exceed the shared budget by <1s.
    output <- tryCatch(.rwbo_run_solver(prepared$problem$wcnf_file, as.integer(ceiling(remaining))),
                       error = identity)
    if (inherits(output, "error")) {
      timed_out <- grepl("timeout|timed? out|time limit|elapsed time", conditionMessage(output),
                         ignore.case = TRUE) ||
        .rwbo_elapsed() - state$start >= state$time_limit
      entry$optimization$status <- if (timed_out) "time_limit" else "solver_error"
      entry$optimization$message <- .rwbo_message(conditionMessage(output))
      return(entry)
    }
  }
  result <- tryCatch(interpret_rwbo_output(prepared$problem, output), error = identity)
  if (inherits(result, "error")) {
    entry$optimization$status <- "invalid_output"
    entry$optimization$message <- .rwbo_message(conditionMessage(result))
    return(entry)
  }
  entry$optimization$solver_status <- result$status
  if (!isTRUE(result$verified)) {
    entry$optimization$status <- "no_solution"
    return(entry)
  }
  count <- result$operator_count
  cost <- result$cost
  valid_number <- function(value) {
    typeof(value) %in% c("integer", "double") && !is.object(value) && is.null(dim(value)) &&
      length(value) == 1L && !is.na(value) && is.finite(value) && value >= 0 &&
      value == floor(value)
  }
  if (!valid_number(count) || count > .Machine$integer.max || !valid_number(cost) ||
      !is.character(result$expression) || length(result$expression) != 1L ||
      is.na(result$expression) || !nzchar(result$expression) ||
      count != .rwbo_operator_count(result$expression) ||
      cost != .rwbo_operator_cost(result$expression)) {
    entry$optimization$status <- "invalid_output"
    entry$optimization$message <- "The verified result has invalid formula metadata."
    return(entry)
  }
  entry$optimization$verified <- TRUE
  entry$optimization$optimal <- isTRUE(result$optimal) && cost <= cost_before
  entry$optimization$status <- "not_improved"
  if (cost >= cost_before) return(entry)

  candidate <- tryCatch(bdd_literals(result$expression), error = identity)
  if (inherits(candidate, "error")) {
    entry$optimization$status <- "invalid_output"
    entry$optimization$verified <- FALSE
    entry$optimization$optimal <- FALSE
    entry$optimization$message <- .rwbo_message(conditionMessage(candidate))
    return(entry)
  }
  entry$reduced_expression <- result$expression
  entry$constant <- candidate$constant
  entry$variables <- candidate$variables
  entry$optimization <- .rwbo_diagnostic(
    "improved", before, count, applied = TRUE, verified = TRUE,
    optimal = isTRUE(result$optimal), solver_status = result$status,
    cost_before = cost_before, cost_after = cost
  )
  entry
}
