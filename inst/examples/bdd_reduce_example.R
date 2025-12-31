# Beispiel: großen BDD bauen, darstellen, mit Reduce reduzieren, erneut darstellen

library(Rcudd)

manager <- CuddManager()

vars <- lapply(1:10, function(i) cudd_bdd_var(manager))
names(vars) <- paste0("x", seq_along(vars))

clauses <- list(
  vars$x1 * vars$x2,
  vars$x3 + !vars$x4,
  (vars$x5 * !vars$x6) + (vars$x7 * vars$x8),
  vars$x9 ^ vars$x10,
  (!vars$x1 * vars$x3) + (vars$x2 * !vars$x4)
)

large_bdd <- Reduce(`+`, clauses)

cat("Großer BDD (vor Reduce):\n")
cudd_bdd_print_debug(large_bdd, nvars = 10L, verbosity = 2L)

restriction <- vars$x1 * !vars$x3 * vars$x5
reduced_bdd <- cudd_bdd_restrict(large_bdd, restriction)

cat("Reduzierter BDD (nach Reduce):\n")
cudd_bdd_print_debug(reduced_bdd, nvars = 10L, verbosity = 2L)
