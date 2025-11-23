#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "rcudd.h"

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"c_run_cudd_example", (DL_FUNC) &run_cudd_example, 2},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_Rcudd(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
