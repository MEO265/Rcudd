# Run after installing Rcudd, with the fixed optional Rwbo package available:
# Rscript --vanilla inst/scripts/test_lintr_rwbo.R
library(Rcudd)
if (!requireNamespace("Rwbo", quietly = TRUE)) {
  stop("Install Rwbo before running the real-solver interface checks.")
}

truth_inputs <- function(variables) {
  inputs <- rep(list(c(FALSE, TRUE)), length(variables))
  names(inputs) <- variables
  do.call(expand.grid, inputs)
}

as_dnf <- function(inputs, target) {
  terms <- apply(inputs[target, , drop = FALSE], 1L, function(row) {
    literals <- ifelse(row, names(inputs), paste0("!", names(inputs)))
    paste0("(", paste(literals, collapse = " && "), ")")
  })
  paste(terms, collapse = " || ")
}

evaluate_rows <- function(expression, inputs) {
  parsed <- str2lang(expression)
  vapply(seq_len(nrow(inputs)), function(i) {
    eval(parsed, as.list(inputs[i, , drop = FALSE]))
  }, logical(1L))
}

count_xor <- function(expression) {
  count <- function(node) {
    if (!is.call(node)) return(0L)
    as.integer(identical(node[[1L]], as.name("xor"))) +
      sum(vapply(as.list(node)[-1L], count, integer(1L)))
  }
  count(str2lang(expression))
}

check_solver_case <- function(variables, target, minimum) {
  inputs <- truth_inputs(variables)
  expected <- target(inputs)
  expression <- as_dnf(inputs, expected)
  stopifnot(!grepl("xor", expression, fixed = TRUE))
  result <- bdd_restrict_from_expressions(
    list(expression), use_rwbo = Inf,
    rwbo_control = list(time_limit = 30L, max_operators = minimum)
  )$chain[[1L]]
  stopifnot(
    identical(evaluate_rows(result$reduced_expression, inputs), expected),
    isTRUE(result$optimization$applied),
    isTRUE(result$optimization$verified),
    isTRUE(result$optimization$optimal),
    result$optimization$operator_count_after == minimum,
    result$optimization$cost_after == 2L * minimum,
    result$optimization$cost_after < result$optimization$cost_before,
    count_xor(result$reduced_expression) == 2L,
    identical(result$variables, bdd_literals(result$reduced_cpp_expression)$variables)
  )
  cat(length(variables), "variables:", result$reduced_expression, "\n")
  invisible(list(expression = expression, inputs = inputs, target = expected))
}

parity <- check_solver_case(
  c("a", "b", "c"),
  function(inputs) rowSums(inputs) %% 2L == 1L,
  2L
)
check_solver_case(
  c("a", "b", "c", "d"),
  function(inputs) (inputs$a != inputs$b) & (inputs$c != inputs$d),
  3L
)

# A failed resource-bounded search must leave the valid BDD formula available.
fallback <- bdd_restrict_from_expressions(
  list(parity$expression), use_rwbo = Inf,
  rwbo_control = list(time_limit = 30L, max_operators = 0L)
)$chain[[1L]]
stopifnot(
  !fallback$optimization$applied,
  identical(evaluate_rows(fallback$reduced_expression, parity$inputs), parity$target)
)

# When the first equality test is false, a and b differ. Therefore the second
# condition !a can use the cheaper positive leaf b, although the two formulas
# differ outside that branch's context. The original input support is retained.
contextual <- bdd_restrict_from_expressions(
  list(quote((a && b) || (!a && !b)), quote(!a), TRUE),
  use_rwbo = Inf, rwbo_control = list(time_limit = 30L, max_operators = 0L)
)$chain
proxy <- contextual[[2L]]
stopifnot(
  identical(proxy$reduced_expression, "b"),
  identical(proxy$reduced_cpp_expression, "p2"),
  identical(proxy$initial_variables, "p1"),
  identical(proxy$variables, "p2"),
  proxy$optimization$operator_count_before == 1L,
  proxy$optimization$operator_count_after == 0L,
  proxy$optimization$cost_before == 1L,
  proxy$optimization$cost_after == 0L,
  isTRUE(proxy$optimization$applied),
  isTRUE(proxy$optimization$verified),
  isTRUE(proxy$optimization$optimal),
  all(vapply(contextual, function(entry) {
    is.null(attr(entry, "rwbo", exact = TRUE)) &&
      !any(c("problem", "wcnf_file", "solver_output") %in% names(entry))
  }, logical(1L)))
)
context_inputs <- truth_inputs(c("a", "b"))
care <- context_inputs$a != context_inputs$b
stopifnot(identical(
  evaluate_rows(proxy$reduced_expression, context_inputs)[care],
  (!context_inputs$a)[care]
))

chain <- bdd_restrict_from_expressions(
  list(quote(a > b), quote(a >= b), TRUE), use_rwbo = 6L,
  rwbo_control = list(time_limit = 30L, max_operators = 3L)
)$chain
stopifnot(
  identical(chain[[2L]]$reduced_expression, "a == b"),
  isTRUE(chain[[3L]]$context_reachable),
  isTRUE(chain[[3L]]$reachable),
  isTRUE(chain[[3L]]$constant)
)
cat("Rcudd lintr interface: real-solver checks passed.\n")
