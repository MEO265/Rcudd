# Exact formula minimization with Rcudd and Rwbo

This standalone script constructs a ROBDD once with `Rcudd` and passes the existing
CUDD root to a native C++ exporter for a weighted CNF (WCNF) synthesis
problem for `Rwbo`. AND (`&`), OR (`|`), Boolean XOR (`^`), and NOT (`!`) each cost
one operator. In these strings `^` follows Rcudd syntax; R itself uses `^` for
exponentiation.

## Run the example

Install this checkout of Rcudd (including its new `bdd_to_wcnf()` function)
and install Rwbo. From the worktree root:

```r
install.packages("Rwbo")
# In a terminal: R CMD INSTALL .
source("inst/scripts/bdd_rwbo_minimize.R")
formel <- rwbo_example_expression()
problem <- prepare_rwbo(formel, "formula.wcnf", max_operators = 8L)
result <- solve_rwbo(problem, time_limit = 60L)
result[c("expression", "operator_count", "optimal", "verified")]
```

The result is equivalent to `(a | b) & (c | d) & (e | f)`, with five binary
operators. Operand and factor order may differ. The input has 23 operators.

The CLI has the same example built in:

```text
Rscript inst/scripts/bdd_rwbo_minimize.R
Rscript inst/scripts/bdd_rwbo_minimize.R --max-operators=5 --time-limit=120
Rscript inst/scripts/bdd_rwbo_minimize.R --prepare-only --output-dir=rwbo-output
Rscript inst/scripts/bdd_rwbo_minimize.R "--expr=!(x & y) | (z ^ x)"
```

It writes `formula.wcnf`, `problem.rds`, `result.rds`, and `solver.log` into
`rwbo-output`. `--prepare-only` only writes the first two and requires no Rwbo.
Sourcing the script does not run the example or write files.

To invoke the solver separately and decode its model:

```r
problem <- readRDS("rwbo-output/problem.rds")
output <- Rwbo::run_open_wbo(c("-algorithm=1", "-print-model", problem$wcnf_file))
result <- read_rwbo_result(problem, output)
```

If moving the WCNF to another location, update `problem$wcnf_file` accordingly.

## What is minimized and what is proven

Every `&`, `|`, `^`, and `!` costs one. Variables, Boolean constants, and
parentheses cost zero. Operators `&`, `|`, and `^` are binary: a chain of three
operands costs two. The encoding describes a formula tree, so repeating a
subexpression incurs its full cost each time. It does not count shared gates as
one operator while printing them repeatedly.

Hard clauses choose one operator per used slot, choose its inputs, give it the
right semantics on every BDD assignment, and force the root to match the target.
Every used non-root slot has exactly one consumer. Unused slots form a prefix;
this only fixes a topological numbering. Binary input order removes commutative
symmetry. One soft unit clause per slot charges one for using it. The hard
weight is `max_operators + 1`, greater than all soft costs combined.

`optimal = TRUE` means Open-WBO reported `OPTIMUM FOUND` and the decoded model
passed both WCNF validation and an independent BDD equivalence check. Zero-cost
constants and positive variables are proven directly from the BDD. Once an
optimum is found within the bound, it is a **global minimum for this cost and
operator set**: every cheaper formula also fits within that same bound. The
bound therefore limits the search size without weakening a successful proof.
Only essential BDD variables are needed; substituting constants for irrelevant
variables cannot increase a formula's cost.

`UNSATISFIABLE` means no formula fits `max_operators`; increase that bound.
`SATISFIABLE` can return a verified candidate with `optimal = FALSE`.
`UNKNOWN`, or a status without a model, does not claim a usable result.

## Limits and tests

This is exact synthesis for small functions. The BDD determines the support and
truth table; the encoding then uses **2^n rows**, even when the BDD is compact.
Defaults are eight operator slots, ten essential variables, two million clauses,
and a 60-second elapsed-time limit. Limits are explicit and can be adjusted through
function arguments. `time_limit = 0L` removes the time limit. The limit uses
elapsed time: Windows Open-WBO does not support its own CPU-limit flag. Runtime can
grow sharply with either variables or operator slots.

`solve_rwbo()` calls the public `Rwbo::run_open_wbo()` function with its elapsed
`timeout`. Use the corrected Rwbo implementation that preserves complete stdout
lines and supports `timeout`; an older released wrapper may not meet that
contract. Rcudd does not launch the solver binary itself or repair split output.
Timeouts and solver errors propagate from Rwbo to this explicit example runner.

Run the standalone integration checks with both packages installed:

```text
Rscript inst/scripts/test_bdd_rwbo_minimize.R
```

The checks cover the eight-term example, XOR and NOT, all 16 two-variable
functions against independently enumerated minimum costs, bounded UNSAT,
discarded BDD variables, unusual variable names, and incomplete solver results.
Two additional inputs contain only AND, OR, and NOT: the odd-parity DNF over
three variables must synthesize `a ^ b ^ c` (two operators), and the four-term
DNF for `(a ^ b) & (c ^ d)` must synthesize two XORs and one AND (three operators).

## Reuse the native exporter with an existing BDD

`prepare_rwbo()` is now a thin wrapper around the exported R function
`Rcudd::bdd_to_wcnf()`. The R function parses the original expression once and
passes its root to **the same** C++ implementation available to native callers:

```cpp
#include "cuddObj.hh"
#include "rcudd/bdd_wcnf.hpp" // add inst/include to the include search path

Cudd manager(5, 0);
BDD b = manager.bddVar(1);
BDD a = manager.bddVar(3);
BDD root = !(a ^ b); // or any already existing BDD from this manager

// Vector positions are original CUDD indices, NOT current levels or a sorted
// support. Unused index positions may contain arbitrary strings or be empty.
std::vector<std::string> names_by_index = {"", "b", "", "a", ""};
rcudd::WcnfOptions options;
options.max_operators = 3;
options.max_variables = 10;
options.max_clauses = 2000000;
rcudd::WcnfProblem problem = rcudd::write_bdd_wcnf(
    root, names_by_index, "formula.wcnf", options);
// problem.variables == {"a", "b"}
// problem.variable_indices == {3, 1}
// problem.target == {true, false, false, true}
// root and manager remain valid and owned by the caller.
```

The declaration is in `inst/include/rcudd/bdd_wcnf.hpp` (installed under
`include/rcudd/bdd_wcnf.hpp`), implemented in `src/bdd_wcnf.cpp`. Native code
inside Rcudd can call it directly. Other native builds must compile/link this
implementation against the **same CUDD build and C++ ABI as the supplied BDD**;
this header does not define a portable cross-DLL BDD interface.

The core walks the existing CUDD nodes directly, accumulating complemented-edge
bits. It does not render/reparse an expression or build/restrict a BDD per row.
It uses stable CUDD indices, so support holes and manager reordering work.
Keep the root/manager alive, and do not concurrently use/reorder that manager.
Essential names must be unique parser-compatible tokens: no whitespace or
`! & | ^ ( )`, and not `true`/`false` (case insensitive). Valid unusual tokens
such as `KEEP.OUT.ATTRS`, `long_name.1` and UTF-8 names are preserved.

`WcnfProblem` returns the support names and original indices, truth vector,
1-based SAT ids for all gate kinds/selectors/row values, clause/variable counts,
operator bound, lower bound, optional zero-cost shortcut and output path.
Input column `j` (zero based) at row `r` is `(r >> j) & 1`. Source slots are
`false`, `true`, essential variables in metadata order, then preceding gates.
Kinds are `unused`, `and`, `or`, `xor`, `not` in that order. There is no shared
subexpression discount. Metadata from the R entry point additionally contains
`inputs` as a logical data frame and the original `expression`; its
`bdd_expression` is `NULL`, avoiding potentially enormous BDD string rendering.

Exact counts (including soft clauses) and integer overflow are checked before
truth-table/gate allocation or file writes. Clauses are streamed to disk.
The output file is replaced on success; I/O failures may leave a partial file.
The exponential limit still applies: defaults are 10 essential variables,
8 operator slots and 2,000,000 clauses; a hard cap of 20 essential variables
also applies if the configurable variable limit is raised.

## Native regression harness

`tests/native/test_bdd_wcnf.cpp` constructs its BDDs directly, checks truth rows
against CUDD's independent `BDD::Eval()`, calls `ShuffleHeap()` on the existing
manager, checks complemented roots/internal edges, retains working roots after
export, and exercises resource/name errors. It emits a reordered-root fixture
which the integration script can compare with the R entry point and then solve
with the real Rwbo binary.

For the Windows toolchain used here, run from the worktree root in PowerShell:

```powershell
$env:PATH = "C:\rtools45\x86_64-w64-mingw32.static.posix\bin;C:\rtools45\usr\bin;C:\Program Files\R\R-4.5.3\bin\x64;" + $env:PATH
g++ -std=c++17 -O2 -Wall -Isrc/cudd_pack/cplusplus -Isrc/cudd_pack/cudd -Iinst/include tests/native/test_bdd_wcnf.cpp src/bdd_wcnf.cpp src/rwbo_output.cpp src/cudd_pack/cudd/.libs/libcudd.a -lm -lpthread -lws2_32 -lpsapi "-LC:/Program Files/R/R-4.5.3/bin/x64" -lR -o .rwbo-test/test_bdd_wcnf.exe
./.rwbo-test/test_bdd_wcnf.exe .rwbo-test
$env:RCUDD_NATIVE_FIXTURE = "$PWD/.rwbo-test/native-reordered.wcnf"
Rscript inst/scripts/test_bdd_rwbo_minimize.R
```

Build/install this checkout first so the bundled CUDD archive and native R
entry point exist. Adapt the R/toolchain paths and local library settings for
your machine. The package's regular testthat suite tests metadata, UTF-8
shortcuts, constants, parser errors and resource checks without needing Rwbo.
## Interpret Rwbo output in C++ or R

The same public header exposes the matching native interpreter:

```cpp
// Use the WcnfProblem returned by write_bdd_wcnf() above.
// solver_output is the complete stdout from Rwbo/Open-WBO, not a file path.
rcudd::WcnfSolution result = rcudd::interpret_rwbo_output(problem, solver_output);
if (result.verified) {
    std::cout << result.expression << " (" << result.operator_count << ")\n";
}
// result.optimal distinguishes a proven minimum from a SATISFIABLE candidate.
```

Its implementation is `src/rwbo_output.cpp`; include it when linking the native
harness or another build of the exporter. `WcnfSolution` contains
`has_expression`, `expression`, `operator_count` (-1 when absent), `status`,
`verified`, and `optimal`. It validates metadata (including SAT ids and
shortcuts), the complete signed model, every WCNF hard clause and objective,
formula-tree structure, selected-wire/operator semantics, and equality to the
BDD-derived target on all rows. Root/manager objects are no longer needed for
this step. Keep the generated WCNF at `problem.wcnf_file` for model checking.

The R API is available without sourcing the standalone script:

```r
problem <- Rcudd::bdd_to_wcnf("!(a & b)", "formula.wcnf", max_operators = 3L)
output <- Rwbo::run_open_wbo(c("-algorithm=1", "-print-model", problem$wcnf_file))
result <- Rcudd::interpret_rwbo_output(problem, output)
```

`read_rwbo_result()` delegates to this R function. The R wrapper additionally
verifies the decoded expression against the original input through one BDD
equivalence check. Invalid metadata/models/objectives/clauses raise errors;
UNSAT, UNKNOWN or absent models return an unverified result with no expression.
Only a validated `OPTIMUM FOUND` result proves optimality, with directly proven
zero-cost BDD shortcuts as the exception. Both native and R paths expect complete
physical output lines from the corrected Rwbo wrapper. Tests include complete
and truncated models, conflicting and out-of-range assignments, malformed
objective/metadata/WCNF data, and a complete long solver model with 6,400 SAT
variables.

## Next integration step

This branch provides the BDD/R-expression conversion, native WCNF exporter,
native result interpreter, and an explicit solver example. Automatic Rwbo calls
inside the literal/chain APIs and lintr integration remain a separate step.

The agreed interface direction is an optional numeric `use_rwbo` limit: zero
disables optimization, a positive whole number limits the relevant variable
count, and `Inf` removes that user-level filter. Logical `TRUE` is not accepted.
Clause/operator limits, an elapsed-time budget, and candidate selection remain
independent safeguards. Chains also need a context-aware encoding and proof
under that context before this can be enabled transparently for lintr.
