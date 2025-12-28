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

static Cudd *cudd_manager_from_ptr(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected an external pointer for a CUDD manager.");
    }
    void *addr = R_ExternalPtrAddr(ptr);
    if (addr == nullptr) {
        Rf_error("CUDD manager pointer is NULL.");
    }
    return static_cast<Cudd *>(addr);
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

extern "C" SEXP c_cudd_read_size(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadSize());
}

extern "C" SEXP c_cudd_read_cache_slots(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadCacheSlots()));
}

extern "C" SEXP c_cudd_read_cache_used_slots(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(mgr->ReadCacheUsedSlots());
}

extern "C" SEXP c_cudd_read_cache_lookups(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(mgr->ReadCacheLookUps());
}

extern "C" SEXP c_cudd_read_cache_hits(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(mgr->ReadCacheHits());
}

extern "C" SEXP c_cudd_read_min_hit(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadMinHit()));
}

extern "C" SEXP c_cudd_read_loose_up_to(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadLooseUpTo()));
}

extern "C" SEXP c_cudd_read_node_count(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadNodeCount()));
}

extern "C" SEXP c_cudd_read_zdd_size(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadZddSize());
}

extern "C" SEXP c_cudd_read_reorderings(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadReorderings()));
}
