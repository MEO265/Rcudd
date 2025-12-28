#include "rcudd.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"c_cudd_new", (DL_FUNC) &c_cudd_new, 0},
    {"c_bdd_remaining_literals", (DL_FUNC) &bdd_remaining_literals, 1},
    {"c_bdd_restrict_chain", (DL_FUNC) &bdd_restrict_chain, 2},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_Rcudd(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
