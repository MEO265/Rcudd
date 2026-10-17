lintr_eval_boolean <- function(expr, inputs) {
  vapply(seq_len(nrow(inputs)), function(i) {
    eval(str2lang(expr), as.list(inputs[i, , drop = FALSE]))
  }, logical(1L))
}

lintr_expect_optimization <- function(entry) {
  expect_named(entry$optimization, c(
    "status", "applied", "operator_count_before", "operator_count_after",
    "cost_before", "cost_after",
    "verified", "optimal", "solver_status", "message"
  ), ignore.order = TRUE)
  for (field in c("status", "solver_status", "message")) {
    expect_type(entry$optimization[[field]], "character")
    expect_length(entry$optimization[[field]], 1L)
  }
  for (field in c("applied", "verified", "optimal")) {
    expect_type(entry$optimization[[field]], "logical")
    expect_length(entry$optimization[[field]], 1L)
    expect_false(is.na(entry$optimization[[field]]), info = field)
  }
  for (field in c("operator_count_before", "operator_count_after", "cost_before", "cost_after")) {
    expect_length(entry$optimization[[field]], 1L)
    expect_true(is.numeric(entry$optimization[[field]]), info = field)
    expect_gte(entry$optimization[[field]], 0, label = field)
  }
}

test_that("one R expression uses the same interface as a branch chain", {
  expr <- quote((alpha && beta) || (alpha && !beta))
  result <- bdd_restrict_from_expressions(list(expr))

  expect_named(result, c("expressions", "placeholders", "order_variables", "chain"))
  expect_length(result$chain, 1L)
  branch <- result$chain[[1L]]
  expect_identical(branch$reduced_expression, "alpha")
  expect_identical(branch$reduced_cpp_expression, "p1")
  expect_identical(branch$variables, "p1")
  expect_identical(branch$initial_variables, c("p1", "p2"))
  expect_true(branch$context_reachable)
  expect_true(branch$reachable)
  expect_false(branch$optimization$applied)
  lintr_expect_optimization(branch)

  explicit <- bdd_restrict_from_expressions(list(expr), use_rwbo = 0L)
  expect_identical(explicit, result)
})

test_that("common quoted inputs normalize to the same nonempty chain", {
  expected <- bdd_restrict_from_expressions(list(quote(a && b)))
  for (input in list(quote(a && b), expression(a && b), "a && b")) {
    expect_identical(bdd_restrict_from_expressions(input), expected)
  }
  for (constant in c(FALSE, TRUE)) {
    expect_identical(bdd_restrict_from_expressions(constant),
                     bdd_restrict_from_expressions(list(constant)))
  }
  chain <- bdd_restrict_from_expressions(list(quote(a), quote(b), TRUE))
  expect_identical(bdd_restrict_from_expressions(c("a", "b", "TRUE")), chain)
  expect_identical(bdd_restrict_from_expressions(expression(a, b, TRUE)), chain)
  for (empty in list(list(), expression(), character())) {
    expect_error(bdd_restrict_from_expressions(empty), "exprs|empty|least one")
  }
})

test_that("disabled diagnostics count Boolean costs before R reconstruction", {
  result <- bdd_restrict_from_expressions(quote(!flag && count > 1), use_rwbo = 0L)
  branch <- result$chain[[1L]]
  expect_identical(branch$optimization$status, "disabled")
  expect_identical(branch$optimization$operator_count_before, 3L)
  expect_identical(branch$optimization$operator_count_after, 3L)
  expect_identical(branch$optimization$cost_before, 4)
  expect_identical(branch$optimization$cost_after, 4)
  lintr_expect_optimization(branch)
})

test_that("reachability distinguishes false conditions and exhausted contexts", {
  result <- bdd_restrict_from_expressions(
    list(FALSE, quote(a), quote(a), TRUE, quote(b)), use_rwbo = 0L
  )
  expect_identical(
    vapply(result$chain, `[[`, logical(1L), "context_reachable"),
    c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    vapply(result$chain, `[[`, logical(1L), "reachable"),
    c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )
  expect_identical(
    vapply(result$chain, `[[`, character(1L), "reduced_expression"),
    c("FALSE", "a", "FALSE", "TRUE", "FALSE")
  )
  invisible(lapply(result$chain, lintr_expect_optimization))
})

test_that("readable replacements preserve original branch contexts", {
  exprs <- list(quote(a || b), quote(a || c), quote(!b && d), TRUE)
  result <- bdd_restrict_from_expressions(exprs, use_rwbo = 0L)
  inputs <- expand.grid(a = c(FALSE, TRUE), b = c(FALSE, TRUE),
                        c = c(FALSE, TRUE), d = c(FALSE, TRUE))
  context <- rep(TRUE, nrow(inputs))
  for (i in seq_along(exprs)) {
    original <- vapply(seq_len(nrow(inputs)), function(j) {
      eval(exprs[[i]], as.list(inputs[j, , drop = FALSE]))
    }, logical(1L))
    branch <- result$chain[[i]]
    candidate <- lintr_eval_boolean(branch$reduced_expression, inputs)
    expect_identical(candidate[context], original[context], info = paste("branch", i))
    expect_identical(branch$context_reachable, any(context))
    expect_identical(branch$reachable, any(context & original))
    expect_identical(branch$variables,
                     bdd_literals(branch$reduced_cpp_expression)$variables)
    context <- context & !original
  }
  expect_identical(result$chain[[2L]]$reduced_expression, "c")
})

test_that("comparison chains return R conditions including a final else", {
  result <- bdd_restrict_from_expressions(
    list(quote(a > b), quote(a >= b), TRUE), use_rwbo = 0
  )
  expect_identical(result$chain[[2L]]$reduced_expression, "a == b")
  expect_true(result$chain[[3L]]$context_reachable)
  expect_true(result$chain[[3L]]$reachable)
  expect_true(result$chain[[3L]]$constant)
  inputs <- expand.grid(a = -1:1, b = -1:1)
  context <- rep(TRUE, nrow(inputs))
  originals <- c("a > b", "a >= b", "TRUE")
  for (i in seq_along(originals)) {
    original <- lintr_eval_boolean(originals[[i]], inputs)
    candidate <- lintr_eval_boolean(result$chain[[i]]$reduced_expression, inputs)
    expect_identical(candidate[context], original[context])
    expect_false(grepl("[LEG]_n[0-9]", result$chain[[i]]$reduced_expression))
    context <- context & !original
  }

  exhausted <- bdd_restrict_from_expressions(list(quote(a > b), quote(a <= b), TRUE))
  expect_false(exhausted$chain[[3L]]$context_reachable)
  expect_false(exhausted$chain[[3L]]$reachable)
})

test_that("conversion never evaluates or mutates the supplied expressions", {
  evaluated <- FALSE
  tripwire <- function() {
    evaluated <<- TRUE
    stop("input expression was evaluated")
  }
  exprs <- list(quote(tripwire() && value), quote(!tripwire()), TRUE)
  original <- unserialize(serialize(exprs, NULL))
  result <- bdd_restrict_from_expressions(exprs, use_rwbo = 0L)
  expect_false(evaluated)
  expect_identical(exprs, original)
  expect_true(any(grepl("tripwire()", result$placeholders, fixed = TRUE)))
  expect_length(result$chain, length(exprs))
})

test_that("reserved and non-syntactic symbols remain variable lookups", {
  bindings <- list(FALSE, TRUE, FALSE)
  names(bindings) <- c("TRUE", "FALSE", "not a syntactic name")
  environment <- list2env(bindings, parent = baseenv())
  for (name in names(bindings)) {
    original <- as.name(name)
    branch <- bdd_restrict_from_expressions(original)$chain[[1L]]
    expect_identical(branch$constant, NA)
    expect_identical(branch$variables, "p1")
    expect_identical(str2lang(branch$reduced_expression), original)
    expect_identical(eval(str2lang(branch$reduced_expression), environment),
                     eval(original, environment))
  }
})

test_that("non-symbol call heads are preserved as opaque expressions", {
  expressions <- list(
    quote(base::isTRUE(value)),
    quote((function(x) x)(value)),
    quote((function() stop("this expression must remain unevaluated"))())
  )
  for (expression in expressions) {
    result <- bdd_restrict_from_expressions(expression)
    branch <- result$chain[[1L]]
    expect_identical(branch$variables, "p1")
    expect_identical(deparse1(str2lang(branch$reduced_expression), backtick = TRUE),
                     deparse1(expression, backtick = TRUE))
    expect_length(result$placeholders, 1L)
  }
})

test_that("long atom and comparison operands remain single parseable fragments", {
  arguments <- lapply(seq_len(30L), function(i) as.name(paste0("argument_", i)))
  atom <- as.call(c(list(as.name("opaque_function")), arguments))
  expect_gt(length(deparse(atom)), 1L)
  result <- bdd_restrict_from_expressions(atom)
  expect_length(result$placeholders, 1L)
  expect_identical(deparse1(str2lang(result$chain[[1L]]$reduced_expression)),
                   deparse1(atom))

  comparison <- call("<", atom, quote(other_operand))
  compared <- bdd_restrict_from_expressions(comparison)
  expect_length(compared$order_variables, 2L)
  expect_true(deparse1(atom, backtick = TRUE) %in% unname(compared$order_variables))
  expect_identical(deparse1(str2lang(compared$chain[[1L]]$reduced_expression)),
                   deparse1(comparison))
})

test_that("RWBO variable limits accept only numeric nonnegative whole scalars", {
  invalid <- list(TRUE, FALSE, NA, NA_real_, NaN, -1, -Inf, 1.5,
                  numeric(), c(0, 1), "3", list(3))
  for (limit in invalid) {
    expect_error(
      bdd_restrict_from_expressions(list(quote(a)), use_rwbo = limit),
      "use_rwbo", info = paste(deparse(limit), collapse = " ")
    )
  }
  # Constants require no optional solver even when the variable filter is removed.
  for (limit in list(0L, 0, 1L, 1, Inf)) {
    result <- bdd_restrict_from_expressions(list(TRUE), use_rwbo = limit)
    expect_true(result$chain[[1L]]$constant)
    expect_false(result$chain[[1L]]$optimization$applied)
  }
})

test_that("solver controls reject ambiguous values before doing any work", {
  invalid <- list(
    TRUE, 1, list(unknown = 1), list(time_limit = -1),
    list(time_limit = NA_real_), list(time_limit = Inf),
    list(max_operators = TRUE), list(max_operators = 1.5),
    list(max_clauses = 0), list(max_clauses = Inf)
  )
  for (control in invalid) {
    expect_error(
      bdd_restrict_from_expressions(list(TRUE), rwbo_control = control),
      "rwbo_control|time_limit|max_operators|max_clauses|control"
    )
  }
})
