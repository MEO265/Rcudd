#' Export exact Boolean formula synthesis as weighted CNF
#'
#' Builds a BDD once and delegates to the native exporter, which evaluates the
#' existing CUDD graph directly. It writes a weighted CNF problem minimizing the
#' cost of a formula tree: unary NOT (`!`) costs one; binary AND (`&`), OR (`|`)
#' and XOR (`^`) cost two each. This function exports the problem; it does not
#' run Rwbo.
#'
#' @param expr Single Boolean expression in Rcudd syntax. Here `^` means XOR.
#' @param wcnf_file Output filename, overwritten on success.
#' @param max_operators Maximum number of operator slots (default eight).
#' @param max_variables Maximum essential BDD variables (default ten; hard cap 20).
#' @param max_clauses Maximum WCNF clauses including soft clauses (default two million).
#' @param care Boolean context in Rcudd syntax. The candidate must match `expr`
#'   only where this expression is true (default `"true"`).
#'
#' @details
#' The encoding has `2^n` rows for the union of expression and context variables,
#' even when the BDD is
#' compact. Exact clause and SAT-variable bounds are checked before large
#' allocations or file writes. Clauses are streamed to disk. Each active
#' non-root gate has exactly one consumer, so shared subexpressions incur their
#' complete printed cost. The bound limits operator count, independently of
#' cost. A proved MaxSAT optimum is optimal within this bound. It is also global
#' when its cost is at most `max_operators + 1`: every strictly cheaper formula
#' then fits, since each operator costs at least one. Equivalence is restricted
#' to `care`. UNSAT only excludes formulas within the bound.
#'
#' Constants and positive variables have zero-cost shortcuts. A tiny WCNF still
#' includes a soft clause for compatibility with Open-WBO 0.1.2. A bound below
#' the essential-support lower bound writes a contradictory WCNF.
#'
#' Native callers with an existing `BDD` use `rcudd::write_bdd_wcnf()` declared
#' in `inst/include/rcudd/bdd_wcnf.hpp`; no parsing or string conversion is needed.
#' See the installed `scripts/README.md` for an example.
#'
#' @return An `rwbo_formula_problem` list with the original expression,
#' available `variables`, their zero-based CUDD `variable_indices`, logical
#' `inputs` data frame, `target` and `care` (empty means all rows), search bounds, `shortcut`, objective,
#' lower bound, `gates` SAT-id metadata, `nvars`, `nclauses` and normalized
#' `wcnf_file`. Input column one changes fastest. `bdd_expression` is `NULL`
#' to avoid rendering a potentially very large expression.
#'
#' @examples
#' path <- tempfile(fileext = ".wcnf")
#' problem <- bdd_to_wcnf("(a & !b) | (!a & b)", path, max_operators = 2L)
#' problem[c("variables", "nvars", "nclauses")]
#' unlink(path)
#'
#' @seealso `vignette("rwbo-wcnf", package = "Rcudd")` for the file format,
#'   SAT variables and operator clauses.
#'
#' @export
bdd_to_wcnf <- function(expr, wcnf_file = "formula.wcnf", max_operators = 8L,
                        max_variables = 10L, max_clauses = 2000000L, care = "true") {
  result <- .Call(c_bdd_wcnf_expression, expr, path.expand(wcnf_file),
                  max_operators, max_variables, max_clauses, care)
  result$wcnf_file <- normalizePath(result$wcnf_file, winslash = "/", mustWork = TRUE)
  result
}
