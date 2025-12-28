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

static void bdd_finalizer(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        return;
    }
    void *addr = R_ExternalPtrAddr(ptr);
    if (addr != nullptr) {
        delete static_cast<BDD *>(addr);
        R_ClearExternalPtr(ptr);
    }
}

static void add_finalizer(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        return;
    }
    void *addr = R_ExternalPtrAddr(ptr);
    if (addr != nullptr) {
        delete static_cast<ADD *>(addr);
        R_ClearExternalPtr(ptr);
    }
}

static void zdd_finalizer(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        return;
    }
    void *addr = R_ExternalPtrAddr(ptr);
    if (addr != nullptr) {
        delete static_cast<ZDD *>(addr);
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

static BDD *bdd_from_ptr(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected an external pointer for a CUDD BDD.");
    }
    void *addr = R_ExternalPtrAddr(ptr);
    if (addr == nullptr) {
        Rf_error("CUDD BDD pointer is NULL.");
    }
    return static_cast<BDD *>(addr);
}

static ADD *add_from_ptr(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected an external pointer for a CUDD ADD.");
    }
    void *addr = R_ExternalPtrAddr(ptr);
    if (addr == nullptr) {
        Rf_error("CUDD ADD pointer is NULL.");
    }
    return static_cast<ADD *>(addr);
}

static ZDD *zdd_from_ptr(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Expected an external pointer for a CUDD ZDD.");
    }
    void *addr = R_ExternalPtrAddr(ptr);
    if (addr == nullptr) {
        Rf_error("CUDD ZDD pointer is NULL.");
    }
    return static_cast<ZDD *>(addr);
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

extern "C" SEXP c_cudd_bdd_one(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    BDD *bdd = new BDD(mgr->bddOne());
    SEXP ptr = PROTECT(R_MakeExternalPtr(bdd, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_zero(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    BDD *bdd = new BDD(mgr->bddZero());
    SEXP ptr = PROTECT(R_MakeExternalPtr(bdd, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_var(SEXP mgr_ptr, SEXP index) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    BDD *bdd = nullptr;

    if (Rf_isNull(index)) {
        bdd = new BDD(mgr->bddVar());
    } else {
        if (!Rf_isNumeric(index) || Rf_length(index) != 1) {
            Rf_error("'index' must be a single numeric value.");
        }
        int idx = Rf_asInteger(index);
        if (idx == NA_INTEGER || idx < 0) {
            Rf_error("'index' must be a non-negative integer.");
        }
        bdd = new BDD(mgr->bddVar(idx));
    }

    SEXP ptr = PROTECT(R_MakeExternalPtr(bdd, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_add_one(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    ADD *add = new ADD(mgr->addOne());
    SEXP ptr = PROTECT(R_MakeExternalPtr(add, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, add_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_add_zero(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    ADD *add = new ADD(mgr->addZero());
    SEXP ptr = PROTECT(R_MakeExternalPtr(add, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, add_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_add_var(SEXP mgr_ptr, SEXP index) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    ADD *add = nullptr;

    if (Rf_isNull(index)) {
        add = new ADD(mgr->addVar());
    } else {
        if (!Rf_isNumeric(index) || Rf_length(index) != 1) {
            Rf_error("'index' must be a single numeric value.");
        }
        int idx = Rf_asInteger(index);
        if (idx == NA_INTEGER || idx < 0) {
            Rf_error("'index' must be a non-negative integer.");
        }
        add = new ADD(mgr->addVar(idx));
    }

    SEXP ptr = PROTECT(R_MakeExternalPtr(add, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, add_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_zdd_one(SEXP mgr_ptr, SEXP index) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (!Rf_isNumeric(index) || Rf_length(index) != 1) {
        Rf_error("'index' must be a single numeric value.");
    }
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER || idx < 0) {
        Rf_error("'index' must be a non-negative integer.");
    }
    ZDD *zdd = new ZDD(mgr->zddOne(idx));
    SEXP ptr = PROTECT(R_MakeExternalPtr(zdd, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, zdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_zdd_zero(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    ZDD *zdd = new ZDD(mgr->zddZero());
    SEXP ptr = PROTECT(R_MakeExternalPtr(zdd, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, zdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_zdd_var(SEXP mgr_ptr, SEXP index) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx;
    if (Rf_isNull(index)) {
        idx = mgr->ReadZddSize();
    } else {
        if (!Rf_isNumeric(index) || Rf_length(index) != 1) {
            Rf_error("'index' must be a single numeric value.");
        }
        idx = Rf_asInteger(index);
        if (idx == NA_INTEGER || idx < 0) {
            Rf_error("'index' must be a non-negative integer.");
        }
    }

    ZDD *zdd = new ZDD(mgr->zddVar(idx));
    SEXP ptr = PROTECT(R_MakeExternalPtr(zdd, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, zdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_not(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *result = new BDD(!(*bdd));
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_and(SEXP lhs_ptr, SEXP rhs_ptr) {
    BDD *lhs = bdd_from_ptr(lhs_ptr);
    BDD *rhs = bdd_from_ptr(rhs_ptr);
    BDD *result = new BDD((*lhs) * (*rhs));
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_or(SEXP lhs_ptr, SEXP rhs_ptr) {
    BDD *lhs = bdd_from_ptr(lhs_ptr);
    BDD *rhs = bdd_from_ptr(rhs_ptr);
    BDD *result = new BDD((*lhs) + (*rhs));
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_xor(SEXP lhs_ptr, SEXP rhs_ptr) {
    BDD *lhs = bdd_from_ptr(lhs_ptr);
    BDD *rhs = bdd_from_ptr(rhs_ptr);
    BDD *result = new BDD((*lhs) ^ (*rhs));
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_add_times(SEXP lhs_ptr, SEXP rhs_ptr) {
    ADD *lhs = add_from_ptr(lhs_ptr);
    ADD *rhs = add_from_ptr(rhs_ptr);
    ADD *result = new ADD((*lhs) * (*rhs));
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, add_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_add_plus(SEXP lhs_ptr, SEXP rhs_ptr) {
    ADD *lhs = add_from_ptr(lhs_ptr);
    ADD *rhs = add_from_ptr(rhs_ptr);
    ADD *result = new ADD((*lhs) + (*rhs));
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, add_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_zdd_intersect(SEXP lhs_ptr, SEXP rhs_ptr) {
    ZDD *lhs = zdd_from_ptr(lhs_ptr);
    ZDD *rhs = zdd_from_ptr(rhs_ptr);
    ZDD *result = new ZDD((*lhs) * (*rhs));
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, zdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_zdd_union(SEXP lhs_ptr, SEXP rhs_ptr) {
    ZDD *lhs = zdd_from_ptr(lhs_ptr);
    ZDD *rhs = zdd_from_ptr(rhs_ptr);
    ZDD *result = new ZDD((*lhs) + (*rhs));
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, zdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_to_add(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    ADD *result = new ADD(bdd->Add());
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, add_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_add_to_bdd(SEXP add_ptr) {
    ADD *add = add_from_ptr(add_ptr);
    BDD *result = new BDD(add->BddPattern());
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_to_zdd(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    ZDD *result = new ZDD(bdd->PortToZdd());
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, zdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_zdd_to_bdd(SEXP zdd_ptr) {
    ZDD *zdd = zdd_from_ptr(zdd_ptr);
    BDD *result = new BDD(zdd->PortToBdd());
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_epd_print_minterm(SEXP bdd_ptr, SEXP nvars) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    if (!Rf_isNumeric(nvars) || Rf_length(nvars) != 1) {
        Rf_error("'nvars' must be a single numeric value.");
    }
    int vars = Rf_asInteger(nvars);
    if (vars == NA_INTEGER || vars < 0) {
        Rf_error("'nvars' must be a non-negative integer.");
    }
    bdd->EpdPrintMinterm(vars, nullptr);
    return R_NilValue;
}

extern "C" SEXP c_cudd_add_epd_print_minterm(SEXP add_ptr, SEXP nvars) {
    ADD *add = add_from_ptr(add_ptr);
    if (!Rf_isNumeric(nvars) || Rf_length(nvars) != 1) {
        Rf_error("'nvars' must be a single numeric value.");
    }
    int vars = Rf_asInteger(nvars);
    if (vars == NA_INTEGER || vars < 0) {
        Rf_error("'nvars' must be a non-negative integer.");
    }
    add->EpdPrintMinterm(vars, nullptr);
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_print_minterm(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    bdd->PrintMinterm();
    return R_NilValue;
}

extern "C" SEXP c_cudd_add_print_minterm(SEXP add_ptr) {
    ADD *add = add_from_ptr(add_ptr);
    add->PrintMinterm();
    return R_NilValue;
}

extern "C" SEXP c_cudd_zdd_print_minterm(SEXP zdd_ptr) {
    ZDD *zdd = zdd_from_ptr(zdd_ptr);
    zdd->PrintMinterm();
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_print_debug(SEXP bdd_ptr, SEXP nvars, SEXP verbosity) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars;
    if (Rf_isNull(nvars)) {
        vars = Cudd_ReadSize(bdd->manager());
    } else {
        if (!Rf_isNumeric(nvars) || Rf_length(nvars) != 1) {
            Rf_error("'nvars' must be a single numeric value.");
        }
        vars = Rf_asInteger(nvars);
        if (vars == NA_INTEGER || vars < 0) {
            Rf_error("'nvars' must be a non-negative integer.");
        }
    }

    int verb = 2;
    if (!Rf_isNull(verbosity)) {
        if (!Rf_isNumeric(verbosity) || Rf_length(verbosity) != 1) {
            Rf_error("'verbosity' must be a single numeric value.");
        }
        verb = Rf_asInteger(verbosity);
        if (verb == NA_INTEGER || verb < 0) {
            Rf_error("'verbosity' must be a non-negative integer.");
        }
    }

    Cudd_PrintDebug(bdd->manager(), bdd->getNode(), vars, verb);
    return R_NilValue;
}
