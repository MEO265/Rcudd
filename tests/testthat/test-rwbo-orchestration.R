rwbo_internal <- function(name) getFromNamespace(name, "Rcudd")

rwbo_prepared_entry <- function(expression = "(a & !b) | (!a & b)") {
  entry <- list(constant = NA, variables = c("a", "b"),
                initial_variables = c("a", "b", "z"),
                reduced_expression = expression,
                context_reachable = TRUE, reachable = TRUE)
  attr(entry, "rwbo") <- list(
    status = "prepared", message = "",
    operator_count = rwbo_internal(".rwbo_operator_count")(expression),
    cost = rwbo_internal(".rwbo_operator_cost")(expression),
    optimal = FALSE, problem = list(wcnf_file = "unused.wcnf")
  )
  entry
}

rwbo_decoded <- function(expression = "a ^ b", optimal = TRUE, verified = TRUE,
                          cost = rwbo_internal(".rwbo_operator_cost")(expression),
                          operator_count = rwbo_internal(".rwbo_operator_count")(expression),
                          status = "OPTIMUM FOUND") {
  list(expression = expression, operator_count = operator_count, cost = cost, optimal = optimal,
       verified = verified, status = status)
}

test_that("use_rwbo and independent resource controls reject ambiguous values", {
  bad <- list(TRUE, FALSE, NA_real_, NaN, -1, -Inf, 1.5, numeric(), c(1, 2),
              "2", 1 + 0i, structure(2, class = "odd"), matrix(2))
  for (value in bad) {
    expect_error(bdd_literals("a", use_rwbo = value), "use_rwbo")
    expect_error(bdd_restrict_chain("a", use_rwbo = value), "use_rwbo")
  }
  for (controls in list(TRUE, c(time_limit = 2), list(2), list(extra = 2), list(time_limit = NULL),
                         list(time_limit = 2, time_limit = 3))) {
    expect_error(bdd_literals("a", rwbo_control = controls), "rwbo_control")
  }
  for (value in list(TRUE, NA_real_, Inf, -1, 0.5, numeric(), 2^31)) {
    expect_error(bdd_literals("a", rwbo_control = list(max_operators = value)), "max_operators")
  }
  for (key in c("max_clauses", "time_limit")) {
    expect_error(bdd_literals("a", rwbo_control = stats::setNames(list(0), key)), key)
  }
  for (key in c("max_operators", "max_clauses", "time_limit")) {
    expect_error(bdd_literals("a", rwbo_control = stats::setNames(list(.Machine$integer.max), key)), key)
  }
  expect_error(bdd_literals("a", rwbo_control = list(max_operators = .Machine$integer.max - 1)),
               "max_operators")
  expect_identical(names(bdd_literals("a", use_rwbo = 0)),
                   c("constant", "variables", "initial_variables", "reduced_expression"))
  state <- rwbo_internal(".rwbo_setup")(Inf, list())
  on.exit(unlink(state$directory, recursive = TRUE))
  expect_identical(state$config$max_variables, 20L)
})

test_that("disabled optimization never loads or calls Rwbo", {
  local_mocked_bindings(
    .rwbo_available = function() stop("unexpected package lookup"),
    .rwbo_run_solver = function(...) stop("unexpected solver call"), .package = "Rcudd"
  )
  expect_identical(bdd_literals("a & a")$reduced_expression, "a")
  expect_length(bdd_restrict_chain(c("a", "b")), 2L)
})

test_that("native candidate filters and missing package preserve the baseline", {
  local_mocked_bindings(.rwbo_available = function() FALSE,
                        .rwbo_run_solver = function(...) stop("unexpected solver call"),
                        .package = "Rcudd")
  expr <- "(a & !b) | (!a & b)"
  baseline <- bdd_literals(expr)
  missing <- bdd_literals(expr, use_rwbo = 2L)
  expect_identical(missing$reduced_expression, baseline$reduced_expression)
  expect_identical(missing$optimization$status, "package_unavailable")
  expect_false(missing$optimization$applied)
  expect_false(missing$optimization$verified)
  expect_null(attr(missing, "rwbo", exact = TRUE))
  minimal <- bdd_literals("a & b", use_rwbo = 2L)$optimization
  expect_identical(minimal$status, "minimal")
  expect_true(minimal$verified)
  expect_true(minimal$optimal)
})

test_that("native zero-cost shortcuts need neither solver nor remaining budget", {
  local_mocked_bindings(
    .rwbo_elapsed = function() stop("unexpected budget check"),
    .rwbo_run_solver = function(...) stop("unexpected solver call"),
    interpret_rwbo_output = function(problem, output) {
      expect_identical(output, "")
      rwbo_decoded(cost = 0L, expression = "a")
    }, .package = "Rcudd"
  )
  entry <- rwbo_prepared_entry()
  attr(entry, "rwbo")$problem$shortcut <- "a"
  # Re-parsing the candidate also starts a disabled wrapper's harmless timer.
  local_mocked_bindings(bdd_literals = function(...) list(constant = NA, variables = "a"),
                        .package = "Rcudd")
  result <- rwbo_internal(".rwbo_finish")(entry, list(start = 0, time_limit = 1L))
  expect_identical(result$reduced_expression, "a")
  expect_true(result$optimization$applied)
  expect_true(result$optimization$verified)
  expect_true(result$optimization$optimal)
})

test_that("only verified strictly cheaper candidates replace formulas", {
  local_mocked_bindings(.rwbo_elapsed = function() 1,
                        .rwbo_run_solver = function(...) "stdout",
                        interpret_rwbo_output = function(...) rwbo_decoded(),
                        .package = "Rcudd")
  entry <- rwbo_prepared_entry()
  result <- rwbo_internal(".rwbo_finish")(entry, list(start = 0, time_limit = 5L))
  expect_identical(result$reduced_expression, "a ^ b")
  expect_identical(result$variables, c("a", "b"))
  expect_identical(result$initial_variables, c("a", "b", "z"))
  expect_true(result$context_reachable)
  expect_true(result$reachable)
  expect_identical(result$optimization$status, "improved")
  expect_identical(result$optimization$operator_count_before, 5L)
  expect_identical(result$optimization$operator_count_after, 1L)
  expect_identical(result$optimization$cost_before, 8)
  expect_identical(result$optimization$cost_after, 2)
  expect_true(result$optimization$verified)
  expect_true(result$optimization$optimal)
  expect_null(attr(result, "rwbo", exact = TRUE))
})

test_that("adoption compares weighted costs even when the operator count grows", {
  # Both expressions denote !a. The mocked candidate isolates the adoption rule;
  # it deliberately makes no claim to be a minimum.
  entry <- rwbo_prepared_entry("!(a & (a | (a & a)))")
  decoded <- rwbo_decoded(expression = "!!!!!a", optimal = FALSE, status = "SATISFIABLE")
  local_mocked_bindings(.rwbo_elapsed = function() 1,
                        .rwbo_run_solver = function(...) "stdout",
                        interpret_rwbo_output = function(...) decoded,
                        .package = "Rcudd")
  result <- rwbo_internal(".rwbo_finish")(entry, list(start = 0, time_limit = 5L))
  expect_identical(result$reduced_expression, "!!!!!a")
  expect_identical(result$optimization$operator_count_before, 4L)
  expect_identical(result$optimization$operator_count_after, 5L)
  expect_identical(result$optimization$cost_before, 7)
  expect_identical(result$optimization$cost_after, 5)
  expect_true(result$optimization$applied)
  expect_false(result$optimization$optimal)
})

test_that("equal or higher weighted costs retain the baseline", {
  entry <- rwbo_prepared_entry("!!!a")
  decoded <- rwbo_decoded(expression = "!(a & a)")
  local_mocked_bindings(.rwbo_elapsed = function() 1,
                        .rwbo_run_solver = function(...) "stdout",
                        interpret_rwbo_output = function(...) decoded,
                        .package = "Rcudd")
  finish <- rwbo_internal(".rwbo_finish")
  state <- list(start = 0, time_limit = 5L)
  equal <- finish(entry, state)
  expect_identical(equal$reduced_expression, "!!!a")
  expect_identical(equal$optimization$status, "not_improved")
  expect_identical(equal$optimization$operator_count_after, 3L)
  expect_identical(equal$optimization$cost_after, 3)
  expect_true(equal$optimization$optimal)
  # This equivalent candidate has fewer operators, but higher cost.
  entry <- rwbo_prepared_entry("!!!!!a")
  decoded <- rwbo_decoded(expression = "!(a & (a | (a & a)))")
  higher <- finish(entry, state)
  expect_identical(higher$reduced_expression, "!!!!!a")
  expect_false(higher$optimization$applied)
  expect_false(higher$optimization$optimal)
  expect_identical(higher$optimization$operator_count_after, 5L)
  expect_identical(higher$optimization$cost_after, 5)
})

test_that("malformed decoded cost and count metadata are recoverable", {
  decoded <- rwbo_decoded()
  local_mocked_bindings(.rwbo_elapsed = function() 1,
                        .rwbo_run_solver = function(...) "stdout",
                        interpret_rwbo_output = function(...) decoded,
                        .package = "Rcudd")
  finish <- rwbo_internal(".rwbo_finish")
  state <- list(start = 0, time_limit = 5L)
  for (field in c("cost", "operator_count")) {
    for (value in list(NULL, NA_real_, NaN, Inf, -1, 0.5, TRUE, "2", c(1, 2),
                       structure(2, class = "odd"), matrix(2), 2^32)) {
      decoded <- rwbo_decoded()
      decoded[field] <- list(value)
      result <- finish(rwbo_prepared_entry(), state)
      expect_identical(result$optimization$status, "invalid_output", info = field)
      expect_false(result$optimization$applied)
      expect_false(result$optimization$verified)
      expect_identical(result$optimization$cost_before, 8)
      expect_identical(result$optimization$cost_after, 8)
    }
  }
  # Plausible scalar metadata must also match the decoded formula.
  for (values in list(c(cost = 1L, operator_count = 1L),
                      c(cost = 2L, operator_count = 2L))) {
    decoded <- rwbo_decoded(cost = values[["cost"]], operator_count = values[["operator_count"]])
    expect_identical(finish(rwbo_prepared_entry(), state)$optimization$status, "invalid_output")
  }
})

test_that("verified candidates and bounded minimum proofs remain distinct", {
  decoded <- rwbo_decoded(expression = "(a & !b) | (!a & b)",
                          optimal = FALSE, status = "SATISFIABLE")
  local_mocked_bindings(.rwbo_elapsed = function() 1,
                        .rwbo_run_solver = function(...) "stdout",
                        interpret_rwbo_output = function(...) decoded,
                        .package = "Rcudd")
  finish <- rwbo_internal(".rwbo_finish")
  state <- list(start = 0, time_limit = 5L)
  result <- finish(rwbo_prepared_entry(), state)
  expect_identical(result$optimization$status, "not_improved")
  expect_false(result$optimization$applied)
  expect_true(result$optimization$verified)
  expect_false(result$optimization$optimal)
  decoded$optimal <- TRUE
  expect_true(finish(rwbo_prepared_entry(), state)$optimization$optimal)
  decoded <- rwbo_decoded(expression = "((a & !b) | (!a & b)) & true")
  expect_false(finish(rwbo_prepared_entry(), state)$optimization$optimal)
  decoded <- rwbo_decoded(optimal = FALSE, status = "SATISFIABLE")
  improved <- finish(rwbo_prepared_entry(), state)
  expect_true(improved$optimization$applied)
  expect_false(improved$optimization$optimal)
  decoded <- rwbo_decoded(verified = FALSE, optimal = FALSE, status = "UNKNOWN")
  no_model <- finish(rwbo_prepared_entry(), state)
  expect_identical(no_model$optimization$status, "no_solution")
  expect_identical(no_model$optimization$solver_status, "UNKNOWN")
  expect_false(no_model$optimization$verified)
})

test_that("chain jobs share the budget and never send timeout zero", {
  now <- c(2.2, 5.1)
  timeout_seen <- integer()
  local_mocked_bindings(
    .rwbo_elapsed = function() { value <- now[[1L]]; now <<- now[-1L]; value },
    .rwbo_run_solver = function(path, timeout) { timeout_seen <<- c(timeout_seen, timeout); "stdout" },
    interpret_rwbo_output = function(...) rwbo_decoded(expression = "(a & !b) | (!a & b)"),
    .package = "Rcudd"
  )
  finish <- rwbo_internal(".rwbo_finish")
  state <- list(start = 0, time_limit = 5L)
  first <- finish(rwbo_prepared_entry(), state)
  second <- finish(rwbo_prepared_entry(), state)
  expect_identical(timeout_seen, 3L)
  expect_identical(first$optimization$status, "not_improved")
  expect_identical(second$optimization$status, "time_limit")
  expect_false(second$optimization$applied)
})

test_that("a one-second budget still allows a bounded solver call", {
  timeout_seen <- NULL
  local_mocked_bindings(
    .rwbo_elapsed = function() 0.2,
    .rwbo_run_solver = function(path, timeout) { timeout_seen <<- timeout; "stdout" },
    interpret_rwbo_output = function(...) rwbo_decoded(expression = "(a & !b) | (!a & b)"),
    .package = "Rcudd"
  )
  rwbo_internal(".rwbo_finish")(rwbo_prepared_entry(), list(start = 0, time_limit = 1L))
  expect_identical(timeout_seen, 1L)
})

test_that("solver failures, timeouts and invalid output preserve baseline diagnostics", {
  failure <- "failed"
  local_mocked_bindings(.rwbo_elapsed = function() 1,
                        .rwbo_run_solver = function(...) stop(failure), .package = "Rcudd")
  finish <- rwbo_internal(".rwbo_finish")
  state <- list(start = 0, time_limit = 5L)
  entry <- rwbo_prepared_entry()
  result <- finish(entry, state)
  expect_identical(result$reduced_expression, entry$reduced_expression)
  expect_identical(result$optimization$status, "solver_error")
  failure <- "solver timeout"
  expect_identical(finish(entry, state)$optimization$status, "time_limit")
  failure <- paste0(strrep("a", 1000), "\n", strrep("b", 1000))
  expect_equal(nchar(finish(entry, state)$optimization$message), 500L)
})

test_that("invalid models are a recoverable diagnostic", {
  local_mocked_bindings(.rwbo_elapsed = function() 1,
                        .rwbo_run_solver = function(...) "broken output",
                        interpret_rwbo_output = function(...) stop("incomplete model"),
                        .package = "Rcudd")
  result <- rwbo_internal(".rwbo_finish")(rwbo_prepared_entry(), list(start = 0, time_limit = 5L))
  expect_identical(result$optimization$status, "invalid_output")
  expect_false(result$optimization$verified)
  expect_false(result$optimization$optimal)
  expect_match(result$optimization$message, "incomplete model")
})

test_that("temporary files are removed after native and solver failures", {
  before <- list.files(tempdir(), pattern = "^rcudd-rwbo-", full.names = TRUE)
  local_mocked_bindings(.rwbo_available = function() TRUE, .package = "Rcudd")
  expect_error(bdd_literals("a &", use_rwbo = 2L), "Unexpected end")
  expect_identical(list.files(tempdir(), pattern = "^rcudd-rwbo-", full.names = TRUE), before)
  seen <- NULL
  local_mocked_bindings(.rwbo_run_solver = function(path, timeout) {
    seen <<- path
    expect_true(file.exists(path))
    stop("test solver failed")
  }, .package = "Rcudd")
  result <- bdd_literals("(a & !b) | (!a & b)", use_rwbo = 2L)
  expect_identical(result$optimization$status, "solver_error")
  expect_false(is.null(seen))
  expect_false(dir.exists(dirname(seen)))
  expect_identical(list.files(tempdir(), pattern = "^rcudd-rwbo-", full.names = TRUE), before)
})

test_that("temporary directory creation failure retains the BDD result", {
  local_mocked_bindings(.rwbo_available = function() TRUE,
                        .rwbo_create_directory = function(path) FALSE,
                        .rwbo_run_solver = function(...) stop("unexpected solver call"),
                        .package = "Rcudd")
  expr <- "(a & !b) | (!a & b)"
  baseline <- bdd_literals(expr)
  result <- bdd_literals(expr, use_rwbo = 2L)
  expect_identical(result$reduced_expression, baseline$reduced_expression)
  expect_identical(result$optimization$status, "resource_limit")
  expect_false(result$optimization$applied)
})


test_that("native search bounds retain equally numerous cheaper candidates", {
  local_mocked_bindings(.rwbo_available = function() TRUE, .package = "Rcudd")
  state <- rwbo_internal(".rwbo_setup")(2L, list(max_operators = 8L))
  on.exit(unlink(state$directory, recursive = TRUE))
  expression <- "(a & !b) | (!a & b)"
  entry <- .Call(rwbo_internal("c_bdd_remaining_literals"), expression, state$config)
  prepared <- attr(entry, "rwbo", exact = TRUE)
  expect_identical(prepared$status, "prepared")
  expect_equal(prepared$cost, rwbo_internal(".rwbo_operator_cost")(entry$reduced_expression))
  expect_equal(prepared$problem$max_operators, min(8L, prepared$cost - 1L))
  expect_gte(prepared$problem$max_operators, prepared$operator_count)
})
