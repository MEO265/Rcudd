#' Export exact Boolean formula synthesis as weighted CNF
#'
#' Builds a BDD once and delegates to the native exporter, which evaluates the
#' existing CUDD graph directly. It writes a weighted CNF problem minimizing the
#' number of binary AND (`&`), OR (`|`), XOR (`^`) and unary NOT (`!`) operators
#' in a formula tree. This function exports the problem; it does not run Rwbo.
#'
#' @param expr Single Boolean expression in Rcudd syntax. Here `^` means XOR.
#' @param wcnf_file Output filename, overwritten on success.
#' @param max_operators Maximum number of operator slots (default eight).
#' @param max_variables Maximum essential BDD variables (default ten; hard cap 20).
#' @param max_clauses Maximum WCNF clauses including soft clauses (default two million).
#'
#' @details
#' The encoding has `2^n` rows for `n` essential variables even when the BDD is
#' compact. Exact clause and SAT-variable bounds are checked before large
#' allocations or file writes. Clauses are streamed to disk. Each active
#' non-root gate has exactly one consumer, so shared subexpressions incur their
#' complete printed cost. Once an optimum within the bound is proved by a
#' MaxSAT solver, every cheaper formula also fits: the optimum is global for
#' this operator set and cost. UNSAT only excludes formulas within the bound.
#'
#' Constants and positive variables have zero-cost shortcuts. A tiny WCNF still
#' includes a soft clause for compatibility with Open-WBO 0.1.2. A bound below
#' the essential-support lower bound writes a contradictory WCNF.
#'
#' Native callers with an existing `BDD` use `rcudd::write_bdd_wcnf()` declared
#' in `inst/include/rcudd/bdd_wcnf.hpp`; no parsing or string conversion is needed.
#' See the installed `scripts/README.md` for a concrete C++ example and the
#' `prepare_rwbo()`, `solve_rwbo()` and `read_rwbo_result()` helpers.
#'
#' @return An `rwbo_formula_problem` list with the original expression,
#' essential `variables`, their zero-based CUDD `variable_indices`, logical
#' `inputs` data frame and `target`, search bounds, `shortcut`, objective,
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
#' @export
bdd_to_wcnf <- function(expr, wcnf_file = "formula.wcnf", max_operators = 8L,
                        max_variables = 10L, max_clauses = 2000000L) {
  result <- .Call(c_bdd_wcnf_expression, expr, path.expand(wcnf_file),
                  max_operators, max_variables, max_clauses)
  result$wcnf_file <- normalizePath(result$wcnf_file, winslash = "/", mustWork = TRUE)
  result
}