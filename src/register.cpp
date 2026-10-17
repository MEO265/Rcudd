#include "rcudd.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"c_interpret_rwbo_output", (DL_FUNC) &interpret_rwbo_expression, 2},
    {"c_bdd_wcnf_expression", (DL_FUNC) &bdd_wcnf_expression, 6},
    {"c_bdd_remaining_literals", (DL_FUNC) &bdd_remaining_literals, 2},
    {"c_bdd_restrict_chain", (DL_FUNC) &bdd_restrict_chain, 3},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_Rcudd(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
