# Formula synthesis with Rcudd and Rwbo

## Automatic use, including lintr callers

Use one entry point for a single R condition or a complete if/else-if/else chain:

```r
Rcudd::bdd_restrict_from_expressions(
  "(a && !b) || (!a && b)", use_rwbo = 6L
)$chain[[1]]$reduced_expression
# "xor(a, b)" (operand order may differ)

result <- Rcudd::bdd_restrict_from_expressions(
  list(quote(a > b), quote(a >= b), TRUE),
  use_rwbo = 6L, rwbo_control = list(time_limit = 5L)
)
result$chain[[2]]$reduced_expression # "a == b"
result$chain[[2]]$optimization
```

`use_rwbo` accepts numeric `0` (disabled), a positive whole variable limit, or
`Inf`. Logical flags are rejected. Context variables count towards the limit;
comparison operands can introduce several Boolean variables. Defaults also
limit synthesis to eight operators, two million clauses and a shared five-second
budget. The native cap of 20 variables still applies with `Inf`.
The budget is checked between jobs; an active export and validation finish,
and solver timeouts are rounded up to whole seconds.

Only verified, strictly cheaper formulas replace BDD results. The diagnostic
distinguishes improvements, directly proven minima, skipped searches and
failures. `verified` and `optimal` distinguish a valid formula from a proven
minimum within the operator bound. `cost_before` and `cost_after` use NOT cost
one and binary operator cost two; operator counts remain separate. Missing Rwbo,
timeouts and invalid models retain the BDD result. Temporary files are managed internally.

`context_reachable` means the preceding conditions can all be false;
`reachable` additionally requires the current condition to be true. A final
`TRUE` represents else. Inputs are never evaluated. Callers decide whether
classical Boolean logic and total-order comparisons apply to their R code.

## Manual export and solver execution

For the WCNF layout, SAT IDs and operator clauses, see
`vignette("rwbo-wcnf", package = "Rcudd")` (German).

Install Rcudd and a version of Rwbo whose `run_open_wbo()` preserves complete
stdout lines and accepts an elapsed `timeout`. Rcudd strings use `&`, `|`, `^`
(Boolean XOR), and `!`. NOT costs one; each binary operator costs two.
Repeated subexpressions are counted each time they appear in the formula tree.
`max_operators` limits the number of operator slots, independently of cost.

```r
source("inst/scripts/bdd_rwbo_minimize.R")
problem <- Rcudd::bdd_to_wcnf(rwbo_example_expression(), "formula.wcnf", 5L)
result <- solve_rwbo(problem, time_limit = 60L)
result[c("expression", "operator_count", "cost", "optimal", "verified")]
```

The eight-term example reduces to `(a | b) & (c | d) & (e | f)`: five operators,
cost ten. Factor order may differ. To run it from a terminal:

```text
Rscript inst/scripts/bdd_rwbo_minimize.R --max-operators=5
Rscript inst/scripts/bdd_rwbo_minimize.R --prepare-only --output-dir=rwbo-output
Rscript inst/scripts/bdd_rwbo_minimize.R "--expr=!(x & y) | (z ^ x)"
```

The script saves `formula.wcnf`, `problem.rds`, `result.rds`, and `solver.log`.
`--prepare-only` writes the first two without Rwbo. Sourcing it runs nothing.
For separate solver execution, keep the WCNF file available for validation:

```r
output <- Rwbo::run_open_wbo(
  c("-algorithm=1", "-print-model", problem$wcnf_file), timeout = 60L
)
result <- Rcudd::interpret_rwbo_output(problem, output)
```

`verified` means the decoded formula passed model, WCNF, and equivalence checks.
`optimal` additionally requires a proven minimum within `max_operators` (or a
direct zero-cost BDD shortcut). With unequal operator costs, this need not be a
global minimum. A sufficient global guarantee is `cost <= max_operators + 1`,
since every strictly cheaper formula then fits the bound. UNSAT also excludes
only formulas within the operator bound. See
`?Rcudd::bdd_to_wcnf` and `?Rcudd::interpret_rwbo_output` for the full contract.
The manual example runner propagates timeouts and solver errors.

## Native use

The exporter can consume an existing CUDD root directly:

```cpp
#include "cuddObj.hh"
#include "rcudd/bdd_wcnf.hpp"

Cudd manager;
BDD a = manager.bddVar(0), b = manager.bddVar(1);
rcudd::WcnfOptions options;
options.max_operators = 3;
auto problem = rcudd::write_bdd_wcnf(!(a ^ b), {"a", "b"}, "formula.wcnf", options);
// After running the solver, with its complete stdout in solver_output:
auto result = rcudd::interpret_rwbo_output(problem, solver_output);
```

Names are indexed by original CUDD index, even after variable reordering. Keep
the manager alive and idle during export. Interpretation only needs the metadata
and WCNF. The installed header `include/rcudd/bdd_wcnf.hpp` documents metadata and
limits. External native builds must link the implementations in
`src/bdd_wcnf.cpp` and `src/rwbo_output.cpp` with the same CUDD build and C++ ABI;
this is not a portable cross-DLL BDD interface.

## Checks

```text
Rscript inst/scripts/test_bdd_rwbo_minimize.R
Rscript inst/scripts/test_lintr_rwbo.R
```

The integration checks use real Rwbo, including all 16 two-variable functions,
the eight-term example, and inputs without XOR that reduce to two XORs.
The regular testthat suite exercises export and interpretation without Rwbo.
`tests/native/test_bdd_wcnf.cpp` independently checks existing roots, reordered
variables and complemented edges against CUDD's `Eval()`. Compile it with the
two implementation files above and the bundled CUDD archive. Its optional output
fixture can be passed to the integration script through `RCUDD_NATIVE_FIXTURE`.
