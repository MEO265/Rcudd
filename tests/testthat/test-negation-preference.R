negation_internal <- function(name) getFromNamespace(name, "Rcudd")

negation_xor_model <- function(problem) {
  gate <- problem$gates[[1L]]
  model <- -seq_len(problem$nvars)
  positive <- c(gate$kind[["xor"]], gate$left[[3L]], gate$right[[4L]],
                gate$left_value[problem$inputs$a], gate$right_value[problem$inputs$b],
                gate$out[problem$target])
  model[positive] <- positive
  cost <- if (problem$prefer_negations) 2L else 1L
  paste("s OPTIMUM FOUND", paste("o", cost),
        paste("v", paste(model, collapse = " "), "0"), sep = "\n")
}

negation_test_entry <- function(expression, prefer_negations) {
  count <- negation_internal(".rwbo_operator_count")(expression)
  cost <- negation_internal(".rwbo_operator_cost")(expression, prefer_negations)
  entry <- list(constant = NA, variables = "a", initial_variables = "a",
                reduced_expression = expression)
  attr(entry, "rwbo") <- list(
    status = "prepared", operator_count = count, cost = cost,
    optimal = FALSE, problem = list(wcnf_file = "mock.wcnf", prefer_negations = prefer_negations)
  )
  entry
}

test_that("negation preference is a strict logical scalar in every public entry point", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  invalid <- list(NULL, logical(), NA, NA_real_, 0L, 1L, 0, 1, Inf, "TRUE",
                  c(TRUE, FALSE), matrix(TRUE), structure(TRUE, class = "flag"))
  for (value in invalid) {
    info <- paste(deparse(value), collapse = " ")
    writeLines("sentinel", file)
    expect_error(bdd_to_wcnf("a ^ b", file, 1L, prefer_negations = value),
                 "prefer_negations", info = info)
    expect_identical(readLines(file), "sentinel")
    for (enabled in c(0L, 2L)) {
      control <- list(prefer_negations = value)
      expect_error(bdd_literals("a", use_rwbo = enabled, rwbo_control = control),
                   "prefer_negations", info = info)
      expect_error(bdd_restrict_chain("a", use_rwbo = enabled, rwbo_control = control),
                   "prefer_negations", info = info)
      expect_error(bdd_restrict_from_expressions(quote(a), use_rwbo = enabled,
                                                 rwbo_control = control),
                   "prefer_negations", info = info)
    }
  }
  expect_error(bdd_literals("a", rwbo_control = list(prefer_negations = TRUE,
                                                     prefer_negations = FALSE)), "unique|control")
  for (flag in c(TRUE, FALSE)) {
    expect_identical(bdd_to_wcnf("a", file, prefer_negations = flag)$prefer_negations, flag)
    state <- negation_internal(".rwbo_setup")(0L, list(prefer_negations = flag))
    expect_identical(state$prefer_negations, flag)
    named_flag <- stats::setNames(flag, "setting")
    expect_identical(bdd_to_wcnf("a", file, prefer_negations = named_flag)$prefer_negations, flag)
    named_state <- negation_internal(".rwbo_setup")(0L, list(prefer_negations = named_flag))
    expect_identical(named_state$prefer_negations, flag)
  }
})

test_that("both cost modes retain gate IDs and exact WCNF resource checks", {
  files <- c(tempfile(fileext = ".wcnf"), tempfile(fileext = ".wcnf"))
  on.exit(unlink(files))
  weighted <- bdd_to_wcnf("a ^ b", files[[1L]], max_operators = 1L)
  uniform <- bdd_to_wcnf("a ^ b", files[[2L]], max_operators = 1L,
                         prefer_negations = FALSE)
  expect_true(weighted$prefer_negations)
  expect_false(uniform$prefer_negations)
  expect_identical(weighted$gates, uniform$gates)
  expect_identical(weighted$binary_auxiliary, uniform$binary_auxiliary)
  expect_identical(weighted$binary_lower_bound, uniform$binary_lower_bound)
  expect_identical(weighted$nvars, uniform$nvars)
  expect_identical(readLines(files[[1L]])[[1L]], "p wcnf 25 133 3")
  expect_identical(readLines(files[[2L]])[[1L]], "p wcnf 25 132 2")
  expect_match(weighted$objective, "weighted|! = 1")
  expect_match(uniform$objective, "operator|uniform")

  for (flag in c(TRUE, FALSE)) {
    problem <- bdd_to_wcnf("a & b & c", files[[1L]], max_operators = 4L,
                            prefer_negations = flag)
    expect_identical(problem$binary_lower_bound, 2L)
    expect_length(problem$binary_auxiliary, 6L)
    expect_identical(problem$nclauses, length(readLines(files[[1L]])) - 1L)
    writeLines("sentinel", files[[1L]])
    expect_error(bdd_to_wcnf("a & b & c", files[[1L]], max_operators = 4L,
                             max_clauses = problem$nclauses - 1L,
                             prefer_negations = flag), "Clause limit")
    expect_identical(readLines(files[[1L]]), "sentinel")
    exact <- bdd_to_wcnf("a & b & c", files[[1L]], max_operators = 4L,
                          max_clauses = problem$nclauses, prefer_negations = flag)
    expect_identical(exact$nclauses, problem$nclauses)
    shortcut <- bdd_to_wcnf("a", files[[1L]], prefer_negations = flag)
    expect_identical(readLines(files[[1L]]), c("p wcnf 1 2 2", "2 1 0", "1 1 0"))
    expect_identical(interpret_rwbo_output(shortcut, "")$cost, 0L)
  }
})

test_that("cost mode is bound to complete solver models and saved problem metadata", {
  file <- tempfile(fileext = ".wcnf")
  on.exit(unlink(file))
  for (flag in c(TRUE, FALSE)) {
    problem <- bdd_to_wcnf("a ^ b", file, max_operators = 1L,
                            prefer_negations = flag)
    output <- negation_xor_model(problem)
    result <- interpret_rwbo_output(problem, output)
    expect_true(result$verified)
    expect_true(result$optimal)
    expect_identical(result$expression, "a ^ b")
    expect_identical(result$operator_count, 1L)
    expect_identical(result$cost, if (flag) 2L else 1L)
    named <- problem; named$prefer_negations <- stats::setNames(flag, "setting")
    expect_identical(interpret_rwbo_output(named, output), result)
    other_cost <- if (flag) 1L else 2L
    wrong_cost <- sub("o [12]", paste("o", other_cost), output)
    expect_error(interpret_rwbo_output(problem, wrong_cost), "cost does not match")
    bad <- problem; bad$prefer_negations <- !flag
    expect_error(interpret_rwbo_output(bad, output), "count|match|prefer_negations")
    for (value in list(NULL, NA, 0L, 1, "FALSE", c(TRUE, FALSE), matrix(FALSE),
                        structure(FALSE, class = "flag"))) {
      bad <- problem; bad["prefer_negations"] <- list(value)
      expect_error(interpret_rwbo_output(bad, output), "prefer_negations")
    }
  }
})

test_that("automatic preparation uses the selected costs and corresponding slot cap", {
  local_mocked_bindings(.rwbo_available = function() TRUE, .package = "Rcudd")
  expression <- "(a & !b) | (!a & b)"
  caps <- integer()
  for (flag in c(TRUE, FALSE)) {
    state <- negation_internal(".rwbo_setup")(2L, list(max_operators = 8L,
                                                       prefer_negations = flag))
    on.exit(unlink(state$directory, recursive = TRUE), add = TRUE)
    expect_identical(state$config$prefer_negations, flag)
    literal <- .Call(negation_internal("c_bdd_remaining_literals"), expression, state$config)
    chain <- .Call(negation_internal("c_bdd_restrict_chain"), expression, character(), state$config)[[1L]]
    for (entry in list(literal, chain)) {
      prepared <- attr(entry, "rwbo", exact = TRUE)
      expect_identical(prepared$status, "prepared")
      expect_identical(prepared$problem$prefer_negations, flag)
      expect_equal(prepared$cost, if (flag) 9 else 6)
      expect_equal(prepared$problem$max_operators, min(8L, prepared$cost - 1L))
      expect_identical(prepared$problem$binary_lower_bound, 1L)
    }
    caps <- c(caps, attr(literal, "rwbo")$problem$max_operators)
  }
  expect_identical(caps, c(8L, 5L))
})

test_that("all automatic public paths pass preference into their exported objective", {
  seen <- character()
  local_mocked_bindings(
    .rwbo_available = function() TRUE,
    .rwbo_run_solver = function(path, timeout) {
      # Decode with the real interpreter after this mocked UNKNOWN response.
      seen <<- c(seen, readLines(path, n = 1L))
      "s UNKNOWN"
    }, .package = "Rcudd"
  )
  expression <- "(a & !b) | (!a & b)"
  for (flag in c(TRUE, FALSE)) {
    control <- list(prefer_negations = flag)
    entries <- list(
      bdd_literals(expression, use_rwbo = 2L, rwbo_control = control),
      bdd_restrict_chain(expression, use_rwbo = 2L, rwbo_control = control)[[1L]],
      bdd_restrict_from_expressions(quote((a && !b) || (!a && b)), use_rwbo = 2L,
                                     rwbo_control = control)$chain[[1L]]
    )
    expect_length(seen, if (flag) 3L else 6L)
    for (entry in entries) {
      expect_identical(entry$optimization$status, "no_solution")
      expect_identical(entry$optimization$cost_before, if (flag) 9 else 6)
      expect_identical(entry$optimization$cost_after, if (flag) 9 else 6)
    }
    expect_true(all(grepl(if (flag) " 17$" else " 6$", tail(seen, 3L))))
  }
})

test_that("disabled and unavailable diagnostics use the requested cost convention", {
  local_mocked_bindings(
    .rwbo_available = function() FALSE,
    .rwbo_run_solver = function(...) stop("unexpected solver call"), .package = "Rcudd"
  )
  for (flag in c(TRUE, FALSE)) {
    control <- list(prefer_negations = flag)
    disabled <- bdd_restrict_from_expressions(quote(a && !b), use_rwbo = 0L,
                                               rwbo_control = control)$chain[[1L]]
    expect_identical(disabled$optimization$status, "disabled")
    expect_identical(disabled$optimization$operator_count_before, 3L)
    expect_identical(disabled$optimization$cost_before, if (flag) 4 else 3)
    expect_identical(disabled$optimization$cost_after, if (flag) 4 else 3)
    named <- bdd_restrict_from_expressions(quote(a && !b), use_rwbo = 0L,
                 rwbo_control = list(prefer_negations = stats::setNames(flag, "setting")))$chain[[1L]]
    expect_identical(named$optimization, disabled$optimization)
    for (entry in list(
      bdd_literals("(a & !b) | (!a & b)", use_rwbo = 2L, rwbo_control = control),
      bdd_restrict_chain("(a & !b) | (!a & b)", use_rwbo = 2L,
                          rwbo_control = control)[[1L]],
      bdd_restrict_from_expressions(quote((a && !b) || (!a && b)), use_rwbo = 2L,
                                     rwbo_control = control)$chain[[1L]]
    )) {
      expect_identical(entry$optimization$status, "package_unavailable")
      expect_identical(entry$optimization$cost_before, if (flag) 9 else 6)
    }
  }
})

test_that("adoption changes only according to the selected relative operator costs", {
  decoded <- NULL
  local_mocked_bindings(
    .rwbo_elapsed = function() 1,
    .rwbo_run_solver = function(...) "mock output",
    interpret_rwbo_output = function(...) decoded, .package = "Rcudd"
  )
  finish <- negation_internal(".rwbo_finish")
  # Both formulas equal !a. Five NOTs use more operators but cost less under
  # the default preference; the uniform objective makes the opposite choice.
  mixed <- "!(a & (a | (a & a)))"
  nots <- "!!!!!a"
  for (flag in c(TRUE, FALSE)) {
    state <- list(start = 0, time_limit = 5L,
                  prefer_negations = stats::setNames(flag, "setting"))
    decoded <- list(expression = nots, operator_count = 5L, cost = 5L,
                     optimal = FALSE, verified = TRUE, status = "SATISFIABLE")
    entry <- negation_test_entry(mixed, flag)
    # Without native cached cost, the R fallback must honor the same flag.
    attr(entry, "rwbo")$cost <- NULL
    result <- finish(entry, state)
    expect_identical(result$optimization$applied, flag)
    expect_identical(result$reduced_expression, if (flag) nots else mixed)
    expect_identical(result$optimization$cost_before, if (flag) 7 else 4)
    expect_identical(result$optimization$cost_after, if (flag) 5 else 4)
    decoded <- list(expression = mixed, operator_count = 4L, cost = if (flag) 7L else 4L,
                     optimal = FALSE, verified = TRUE, status = "SATISFIABLE")
    reverse <- finish(negation_test_entry(nots, flag), state)
    expect_identical(reverse$optimization$applied, !flag)
    expect_identical(reverse$reduced_expression, if (flag) nots else mixed)
    expect_false(reverse$optimization$optimal)
  }
})
