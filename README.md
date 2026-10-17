# Rcudd

[![R-CMD-check](https://github.com/MEO265/Rcudd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MEO265/Rcudd/actions/workflows/R-CMD-check.yaml)

Rcudd simplifies Boolean expressions and conditional chains with the bundled
[CUDD library](https://github.com/cuddorg/cudd).

```r
Rcudd::bdd_restrict_from_expressions(quote((x && y) || (x && !y)))$chain[[1]]$reduced_expression
# "x"

# if / else if / else
result <- Rcudd::bdd_restrict_from_expressions(
  list(quote(a > b), quote(a >= b), TRUE), use_rwbo = 6L
)
result$chain[[2]]$reduced_expression
# "a == b"
```

`bdd_restrict_from_expressions()` is the single entry point needed by lintr.
Each branch reports a readable replacement, context and branch reachability,
and an optimization diagnostic. `TRUE` as the final input represents `else`.

`use_rwbo = 0L` (default) uses BDD reduction alone. A positive whole number
enables optional Rwbo synthesis up to that many Boolean variables, including
context variables; `Inf` removes this filter. Logical flags are rejected.
Independent operator, clause and time limits remain; missing Rwbo or a failed
search retains the BDD result. See [solver examples](inst/scripts/README.md).

For Boolean strings use `bdd_literals()` or `bdd_restrict_chain()`. Manual
solver workflows use `bdd_to_wcnf()` and `interpret_rwbo_output()`.
`cpp_logic_to_r()` restores either one Boolean string or a vector of strings.

BDD reduction does not guarantee a shortest formula. Conversion uses classical
Boolean logic and a total ordering for comparisons. Callers such as lintr decide
whether these assumptions hold for the original R code, including missing
values, scalar conditions and side effects.

CUDD was developed at the University of Colorado and is distributed under its
BSD license. Rcudd does not expose CUDD manager objects as R objects.
