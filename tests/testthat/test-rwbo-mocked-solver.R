rwbo_mock_responses <- function(problems, outputs, .env = parent.frame()) {
  stopifnot(length(problems) == length(outputs))
  expected_hashes <- vapply(problems, function(problem) {
    return(unname(tools::md5sum(problem$wcnf_file)))
  }, character(1L))
  force(outputs)
  calls <- new.env(parent = emptyenv())
  calls$paths <- character()
  calls$timeouts <- integer()
  local_mocked_bindings(
    .rwbo_available = function() {
      return(TRUE)
    },
    .rwbo_run_solver = function(path, timeout) {
      index <- length(calls$paths) + 1L
      if (index > length(outputs)) {
        stop("Unexpected solver call.", call. = FALSE)
      }
      expect_identical(unname(tools::md5sum(path)), expected_hashes[[index]])
      expect_type(timeout, "integer")
      expect_gt(timeout, 0L)
      calls$paths <- c(calls$paths, path)
      calls$timeouts <- c(calls$timeouts, timeout)
      return(outputs[[index]])
    },
    .package = "Rcudd",
    .env = .env
  )
  return(calls)
}

rwbo_boolean_rows <- function(code, inputs) {
  parsed <- if (is.character(code)) str2lang(code) else code
  return(vapply(seq_len(nrow(inputs)), function(row) {
    return(eval(parsed, as.list(inputs[row, , drop = FALSE])))
  }, logical(1L)))
}

rwbo_slow_cases <- function() {
  pair_terms <- vapply(seq_len(4L), function(index) {
    left <- letters[[2L * index - 1L]]
    right <- letters[[2L * index]]
    return(paste0("((", left, " & !", right, ") | (!", left, " & ", right, "))"))
  }, character(1L))
  expanded <- paste(pair_terms, collapse = " & ")
  return(list(
    list(
      input = expanded,
      formula = quote((a ^ b) & (c ^ d) & (e ^ f) & (g ^ h)),
      operators = 7L
    ),
    list(
      input = paste(expanded, "i", sep = " & "),
      formula = quote((a ^ b) & (c ^ d) & (e ^ f) & (g ^ h) & i),
      operators = 8L
    ),
    list(
      input = "(a | b) & (c | d) & (e | f) & (g | h)",
      formula = quote((a | b) & (c | d) & (e | f) & (g | h)),
      operators = 7L
    )
  ))
}

test_that("slow eight- and nine-variable cases use raw solver witnesses in both cost modes", {
  problem_file <- tempfile(fileext = ".wcnf")
  check_file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(c(problem_file, check_file)))

  # Each witness has n-1 binary operators, attaining the essential-support
  # lower bound in both objectives. OPTIMUM FOUND is therefore justified here.
  for (case in rwbo_slow_cases()) {
    expect_false(grepl("^", case$input, fixed = TRUE))
    for (preference in c(TRUE, FALSE)) {
      problem <- bdd_to_wcnf(
        case$input, problem_file, max_operators = 8L, prefer_negations = preference
      )
      witness <- rwbo_model_witness(problem, case$formula)
      stdout <- rwbo_witness_output(witness, status = "OPTIMUM FOUND")
      stdout_rows <- strsplit(stdout, "\n", fixed = TRUE)[[1L]]
      expect_gt(nchar(tail(stdout_rows, 1L)), 8192L)
      calls <- rwbo_mock_responses(list(problem), list(stdout))

      actual <- bdd_literals(
        case$input, use_rwbo = Inf,
        rwbo_control = list(max_operators = 8L, prefer_negations = preference, time_limit = 30L)
      )

      expect_identical(witness$operator_count, case$operators)
      expect_true(actual$optimization$applied)
      expect_true(actual$optimization$verified)
      expect_true(actual$optimization$optimal)
      expect_identical(actual$optimization$solver_status, "OPTIMUM FOUND")
      expect_identical(actual$optimization$operator_count_after, case$operators)
      expect_identical(
        actual$optimization$cost_after,
        as.numeric(case$operators * if (preference) 2L else 1L)
      )
      expect_lt(actual$optimization$cost_after, actual$optimization$cost_before)
      # Reconstruct a separate BDD after decoding. With K=0 this comparison
      # does not repeat synthesis or search for a minimum.
      independent <- bdd_to_wcnf(actual$reduced_expression, check_file, max_operators = 0L)
      expect_identical(independent$variables, problem$variables)
      expect_identical(independent$target, problem$target)
      expect_length(calls$paths, 1L)
      expect_false(file.exists(calls$paths))
      expect_null(attr(actual, "rwbo", exact = TRUE))
    }
  }
})

test_that("a feasible threshold witness is adopted without claiming a minimum", {
  problem_file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(problem_file))
  input <- "(a & b) | ((a | b) & (c | d)) | (c & d)"
  formula <- quote((a & b) | ((a | b) & (c | d)) | (c & d))

  # This threshold example exceeded the benchmark's solver budget. Its known
  # formula is only a feasible witness; the test does not assume optimality.
  for (preference in c(TRUE, FALSE)) {
    problem <- bdd_to_wcnf(input, problem_file, max_operators = 8L, prefer_negations = preference)
    witness <- rwbo_model_witness(problem, formula)
    stdout <- rwbo_witness_output(witness, chunk_size = 17L)
    calls <- rwbo_mock_responses(list(problem), list(stdout))
    actual <- bdd_literals(
      input, use_rwbo = Inf,
      rwbo_control = list(max_operators = 8L, prefer_negations = preference, time_limit = 30L)
    )
    expect_identical(witness$operator_count, 7L)
    expect_identical(actual$optimization$status, "improved")
    expect_true(actual$optimization$verified)
    expect_false(actual$optimization$optimal)
    expect_identical(actual$optimization$solver_status, "SATISFIABLE")
    expect_identical(actual$optimization$cost_after, as.numeric(witness$cost))
    expect_identical(actual$optimization$operator_count_after, witness$operator_count)
    expect_length(calls$paths, 1L)
    expect_false(file.exists(calls$paths))
  }
})

test_that("raw mocked models preserve branch contexts and restore context-only variables", {
  problem_files <- c(tempfile(fileext = ".wcnf"), tempfile(fileext = ".wcnf"))
  on.exit(unlink(problem_files))
  conditions <- list(quote(xor(a, b)), quote((a && !c) || (!a && c)), TRUE)
  converted <- expressions_to_cpp_logic(conditions[[1L]], conditions[[2L]], TRUE)
  care <- paste0("!(", converted$expressions[[1L]], ")")
  inputs <- expand.grid(a = c(FALSE, TRUE), b = c(FALSE, TRUE), c = c(FALSE, TRUE))
  context <- !xor(inputs$a, inputs$b)

  for (preference in c(TRUE, FALSE)) {
    first <- bdd_to_wcnf(
      converted$expressions[[1L]], problem_files[[1L]],
      max_operators = 1L, prefer_negations = preference
    )
    second <- bdd_to_wcnf(
      converted$expressions[[2L]], problem_files[[2L]], max_operators = 1L,
      care = care, prefer_negations = preference
    )
    # In the second branch p1==p2, so p2^p3 is equivalent to p1^p3 only
    # under care. Each XOR needs one binary operator, also in this context.
    outputs <- list(
      rwbo_witness_output(rwbo_model_witness(first, quote(p1 ^ p2)), status = "OPTIMUM FOUND"),
      rwbo_witness_output(rwbo_model_witness(second, quote(p2 ^ p3)), status = "OPTIMUM FOUND")
    )
    calls <- rwbo_mock_responses(list(first, second), outputs)
    actual <- bdd_restrict_from_expressions(
      conditions, use_rwbo = Inf,
      rwbo_control = list(max_operators = 1L, prefer_negations = preference, time_limit = 30L)
    )$chain

    expect_true(actual[[1L]]$optimization$applied)
    expect_true(actual[[2L]]$optimization$applied)
    expect_true(actual[[2L]]$optimization$verified)
    expect_true(actual[[2L]]$optimization$optimal)
    expect_identical(actual[[2L]]$initial_variables, c("p1", "p3"))
    expect_identical(actual[[2L]]$variables, c("p2", "p3"))
    candidate <- rwbo_boolean_rows(actual[[2L]]$reduced_expression, inputs)
    original <- rwbo_boolean_rows(conditions[[2L]], inputs)
    expect_identical(candidate[context], original[context])
    expect_true(any(candidate[!context] != original[!context]))
    expect_true(actual[[3L]]$context_reachable)
    expect_true(actual[[3L]]$constant)
    expect_identical(actual[[3L]]$reduced_expression, "TRUE")
    expect_length(calls$paths, 2L)
    expect_false(any(file.exists(calls$paths)))
  }
})

test_that("invalid long solver answers retain the verified BDD fallback", {
  problem_file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(problem_file))
  case <- rwbo_slow_cases()[[1L]]
  baseline <- bdd_literals(case$input)

  for (preference in c(TRUE, FALSE)) {
    problem <- bdd_to_wcnf(
      case$input, problem_file, max_operators = 8L, prefer_negations = preference
    )
    witness <- rwbo_model_witness(problem, case$formula)
    valid_stdout <- rwbo_witness_output(witness, status = "OPTIMUM FOUND")
    wrong_cost <- witness
    wrong_cost$cost <- wrong_cost$cost + 1L
    wrong_logic <- rwbo_model_witness(problem, quote(a | b | c | d | e | f | g | h))
    wrong_counter <- witness
    counter_id <- tail(problem$binary_auxiliary, 1L)
    wrong_counter$model[[counter_id]] <- -wrong_counter$model[[counter_id]]

    # Reproduce a truncated physical v-line, keeping only complete tokens.
    stdout_rows <- strsplit(valid_stdout, "\n", fixed = TRUE)[[1L]]
    truncated_model <- sub(" [^ ]*$", " 0", substr(stdout_rows[[3L]], 1L, 8192L))
    truncated_stdout <- paste(c(stdout_rows[1:2], truncated_model), collapse = "\n")
    outputs <- list(
      rwbo_witness_output(wrong_cost, status = "OPTIMUM FOUND"),
      truncated_stdout,
      rwbo_witness_output(wrong_logic, status = "OPTIMUM FOUND"),
      rwbo_witness_output(wrong_counter, status = "OPTIMUM FOUND")
    )
    expected_messages <- c("cost", "incomplete", "hard clause", "hard clause")

    for (index in seq_along(outputs)) {
      calls <- rwbo_mock_responses(list(problem), outputs[index])
      actual <- bdd_literals(
        case$input, use_rwbo = Inf,
        rwbo_control = list(max_operators = 8L, prefer_negations = preference, time_limit = 30L)
      )
      expect_identical(actual$reduced_expression, baseline$reduced_expression)
      expect_identical(actual$optimization$status, "invalid_output")
      expect_false(actual$optimization$applied)
      expect_false(actual$optimization$verified)
      expect_false(actual$optimization$optimal)
      expect_identical(actual$optimization$cost_after, actual$optimization$cost_before)
      expect_match(actual$optimization$message, expected_messages[[index]])
      expect_length(calls$paths, 1L)
      expect_false(file.exists(calls$paths))
    }
  }
})
