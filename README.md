# Rcudd

[![R-CMD-check](https://github.com/MEO265/Rcudd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MEO265/Rcudd/actions/workflows/R-CMD-check.yaml)

Rcudd simplifies Boolean expressions and conditional chains with the bundled
[CUDD library](https://github.com/cuddorg/cudd).

```r
Rcudd::bdd_literals("(x & y) | (x & !y)")$reduced_expression
# "x"

Rcudd::bdd_restrict_from_expressions(list(quote(a > b), quote(a >= b)))$chain
```

- `bdd_literals()`: remaining variables, constants and a factored formula.
- `bdd_restrict_chain()`: Boolean strings under constraints and earlier false branches.
- `bdd_restrict_from_expressions()`: quoted R expressions, shared placeholders,
  ordering constraints and readable R results.
- `bdd_to_wcnf()` and `interpret_rwbo_output()`: bounded formula synthesis
  with AND, OR, XOR and NOT; see [solver examples](inst/scripts/README.md).

BDD reduction does not guarantee a shortest formula. Conversion uses classical
Boolean logic and a total ordering for comparisons. Callers such as lintr decide
whether these assumptions hold for the original R code, including missing
values, scalar conditions and side effects.

CUDD was developed at the University of Colorado and is distributed under its
BSD license. Rcudd does not expose CUDD manager objects as R objects.