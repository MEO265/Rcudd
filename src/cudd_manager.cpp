#include "rcudd.h"
#include <R.h>
#include <Rinternals.h>
#include "cuddObj.hh"

static void cudd_manager_finalizer(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        return;
    }
    void *addr = R_ExternalPtrAddr(ptr);
    if (addr != nullptr) {
        delete static_cast<Cudd *>(addr);
        R_ClearExternalPtr(ptr);
    }
}

extern "C" SEXP c_cudd_new() {
    try {
        Cudd *mgr = new Cudd();
        SEXP ptr = PROTECT(R_MakeExternalPtr(mgr, R_NilValue, R_NilValue));
        R_RegisterCFinalizerEx(ptr, cudd_manager_finalizer, TRUE);
        UNPROTECT(1);
        return ptr;
    } catch (const std::exception &ex) {
        Rf_error("Failed to create CUDD manager: %s", ex.what());
    }

    return R_NilValue;
}
