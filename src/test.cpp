#include <vector>
#include <string>
#include <R.h>
#include <Rinternals.h>
#include "rcudd.h"
#include "cuddObj.hh"

extern "C" SEXP run_cudd_example(SEXP x_in, SEXP y_in) {
    int xv = INTEGER(x_in)[0];
    int yv = INTEGER(y_in)[0];

    Cudd mgr;

    BDD x = mgr.bddVar(); // index 0
    BDD y = mgr.bddVar(); // index 1

    BDD f = (x & y) | (!x & !y);  // XNOR

    int inputs[2] = {xv, yv};

    BDD res = f.Eval(inputs);  // BDD, nicht int

    int val = res.IsOne() ? 1 : (res.IsZero() ? 0 : NA_INTEGER);

    SEXP out = PROTECT(ScalarInteger(val));
    UNPROTECT(1);
    return out;
}