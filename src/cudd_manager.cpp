#include "rcudd.h"
#include <R.h>
#include <Rinternals.h>
#include <cstdio>
#include <string>
#include <vector>
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

extern "C" SEXP c_cudd_set_min_hit(SEXP mgr_ptr, SEXP hr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(hr);
    if (value == NA_INTEGER || value < 0) {
        Rf_error("'hr' must be a non-negative integer.");
    }
    mgr->SetMinHit(static_cast<unsigned int>(value));
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_loose_up_to(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadLooseUpTo()));
}

extern "C" SEXP c_cudd_set_loose_up_to(SEXP mgr_ptr, SEXP lut) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(lut);
    if (value == NA_INTEGER || value < 0) {
        Rf_error("'lut' must be a non-negative integer.");
    }
    mgr->SetLooseUpTo(static_cast<unsigned int>(value));
    return R_NilValue;
}

extern "C" SEXP c_cudd_make_verbose(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->makeVerbose();
    return R_NilValue;
}

extern "C" SEXP c_cudd_make_terse(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->makeTerse();
    return R_NilValue;
}

extern "C" SEXP c_cudd_is_verbose(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarLogical(mgr->isVerbose());
}

extern "C" SEXP c_cudd_info(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->info();
    return R_NilValue;
}

extern "C" SEXP c_cudd_push_variable_name(SEXP mgr_ptr, SEXP name) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (!Rf_isString(name) || Rf_length(name) != 1) {
        Rf_error("'name' must be a single string.");
    }
    mgr->pushVariableName(CHAR(STRING_ELT(name, 0)));
    return R_NilValue;
}

extern "C" SEXP c_cudd_clear_variable_names(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->clearVariableNames();
    return R_NilValue;
}

extern "C" SEXP c_cudd_get_variable_name(SEXP mgr_ptr, SEXP index) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER || idx < 0) {
        Rf_error("'index' must be a non-negative integer.");
    }
    std::string name = mgr->getVariableName(static_cast<size_t>(idx));
    return Rf_mkString(name.c_str());
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

extern "C" SEXP c_cudd_read_start_time(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadStartTime()));
}

extern "C" SEXP c_cudd_read_elapsed_time(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadElapsedTime()));
}

extern "C" SEXP c_cudd_set_start_time(SEXP mgr_ptr, SEXP st) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (!Rf_isNumeric(st) || Rf_length(st) != 1) {
        Rf_error("'st' must be a single numeric value.");
    }
    unsigned long value = static_cast<unsigned long>(Rf_asReal(st));
    mgr->SetStartTime(value);
    return R_NilValue;
}

extern "C" SEXP c_cudd_reset_start_time(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->ResetStartTime();
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_time_limit(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadTimeLimit()));
}

extern "C" SEXP c_cudd_set_time_limit(SEXP mgr_ptr, SEXP tl) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (!Rf_isNumeric(tl) || Rf_length(tl) != 1) {
        Rf_error("'tl' must be a single numeric value.");
    }
    unsigned long value = static_cast<unsigned long>(Rf_asReal(tl));
    unsigned long result = mgr->SetTimeLimit(value);
    return Rf_ScalarReal(static_cast<double>(result));
}

extern "C" SEXP c_cudd_update_time_limit(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->UpdateTimeLimit();
    return R_NilValue;
}

extern "C" SEXP c_cudd_increase_time_limit(SEXP mgr_ptr, SEXP inc) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (!Rf_isNumeric(inc) || Rf_length(inc) != 1) {
        Rf_error("'inc' must be a single numeric value.");
    }
    unsigned long value = static_cast<unsigned long>(Rf_asReal(inc));
    mgr->IncreaseTimeLimit(value);
    return R_NilValue;
}

extern "C" SEXP c_cudd_unset_time_limit(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->UnsetTimeLimit();
    return R_NilValue;
}

extern "C" SEXP c_cudd_time_limited(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarLogical(mgr->TimeLimited());
}

extern "C" SEXP c_cudd_autodyn_enable(SEXP mgr_ptr, SEXP method) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (Rf_isNull(method)) {
        mgr->AutodynEnable();
    } else {
        int value = Rf_asInteger(method);
        if (value == NA_INTEGER) {
            Rf_error("'method' must be a valid integer.");
        }
        mgr->AutodynEnable(static_cast<Cudd_ReorderingType>(value));
    }
    return R_NilValue;
}

extern "C" SEXP c_cudd_autodyn_disable(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->AutodynDisable();
    return R_NilValue;
}

extern "C" SEXP c_cudd_reordering_status(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    Cudd_ReorderingType method;
    bool enabled = mgr->ReorderingStatus(&method);
    SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, Rf_mkChar("enabled"));
    SET_STRING_ELT(names, 1, Rf_mkChar("method"));
    Rf_setAttrib(out, R_NamesSymbol, names);
    SET_VECTOR_ELT(out, 0, Rf_ScalarLogical(enabled));
    SET_VECTOR_ELT(out, 1, Rf_ScalarInteger(static_cast<int>(method)));
    UNPROTECT(2);
    return out;
}

extern "C" SEXP c_cudd_autodyn_enable_zdd(SEXP mgr_ptr, SEXP method) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (Rf_isNull(method)) {
        mgr->AutodynEnableZdd();
    } else {
        int value = Rf_asInteger(method);
        if (value == NA_INTEGER) {
            Rf_error("'method' must be a valid integer.");
        }
        mgr->AutodynEnableZdd(static_cast<Cudd_ReorderingType>(value));
    }
    return R_NilValue;
}

extern "C" SEXP c_cudd_autodyn_disable_zdd(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->AutodynDisableZdd();
    return R_NilValue;
}

extern "C" SEXP c_cudd_reordering_status_zdd(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    Cudd_ReorderingType method;
    bool enabled = mgr->ReorderingStatusZdd(&method);
    SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, Rf_mkChar("enabled"));
    SET_STRING_ELT(names, 1, Rf_mkChar("method"));
    Rf_setAttrib(out, R_NamesSymbol, names);
    SET_VECTOR_ELT(out, 0, Rf_ScalarLogical(enabled));
    SET_VECTOR_ELT(out, 1, Rf_ScalarInteger(static_cast<int>(method)));
    UNPROTECT(2);
    return out;
}

extern "C" SEXP c_cudd_zdd_realignment_enabled(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarLogical(mgr->zddRealignmentEnabled());
}

extern "C" SEXP c_cudd_zdd_realign_enable(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->zddRealignEnable();
    return R_NilValue;
}

extern "C" SEXP c_cudd_zdd_realign_disable(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->zddRealignDisable();
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_realignment_enabled(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarLogical(mgr->bddRealignmentEnabled());
}

extern "C" SEXP c_cudd_bdd_realign_enable(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->bddRealignEnable();
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_realign_disable(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->bddRealignDisable();
    return R_NilValue;
}

extern "C" SEXP c_cudd_background(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    ADD *add = new ADD(mgr->background());
    SEXP ptr = PROTECT(R_MakeExternalPtr(add, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, add_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_set_background(SEXP mgr_ptr, SEXP add_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    ADD *add = add_from_ptr(add_ptr);
    mgr->SetBackground(*add);
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_max_cache(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadMaxCache()));
}

extern "C" SEXP c_cudd_read_max_cache_hard(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadMaxCacheHard()));
}

extern "C" SEXP c_cudd_set_max_cache_hard(SEXP mgr_ptr, SEXP mc) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(mc);
    if (value == NA_INTEGER || value < 0) {
        Rf_error("'mc' must be a non-negative integer.");
    }
    mgr->SetMaxCacheHard(static_cast<unsigned int>(value));
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_slots(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadSlots()));
}

extern "C" SEXP c_cudd_read_keys(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadKeys()));
}

extern "C" SEXP c_cudd_read_dead(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadDead()));
}

extern "C" SEXP c_cudd_read_min_dead(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadMinDead()));
}

extern "C" SEXP c_cudd_read_max_reorderings(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadMaxReorderings()));
}

extern "C" SEXP c_cudd_set_max_reorderings(SEXP mgr_ptr, SEXP mr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(mr);
    if (value == NA_INTEGER || value < 0) {
        Rf_error("'mr' must be a non-negative integer.");
    }
    mgr->SetMaxReorderings(static_cast<unsigned int>(value));
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_reordering_time(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadReorderingTime()));
}

extern "C" SEXP c_cudd_read_garbage_collections(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadGarbageCollections());
}

extern "C" SEXP c_cudd_read_garbage_collection_time(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadGarbageCollectionTime()));
}

extern "C" SEXP c_cudd_read_sift_max_var(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadSiftMaxVar());
}

extern "C" SEXP c_cudd_set_sift_max_var(SEXP mgr_ptr, SEXP smv) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(smv);
    if (value == NA_INTEGER) {
        Rf_error("'smv' must be a valid integer.");
    }
    mgr->SetSiftMaxVar(value);
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_sift_max_swap(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadSiftMaxSwap());
}

extern "C" SEXP c_cudd_set_sift_max_swap(SEXP mgr_ptr, SEXP sms) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(sms);
    if (value == NA_INTEGER) {
        Rf_error("'sms' must be a valid integer.");
    }
    mgr->SetSiftMaxSwap(value);
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_max_growth(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(mgr->ReadMaxGrowth());
}

extern "C" SEXP c_cudd_set_max_growth(SEXP mgr_ptr, SEXP mg) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (!Rf_isNumeric(mg) || Rf_length(mg) != 1) {
        Rf_error("'mg' must be a single numeric value.");
    }
    mgr->SetMaxGrowth(Rf_asReal(mg));
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_perm(SEXP mgr_ptr, SEXP i) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx = Rf_asInteger(i);
    if (idx == NA_INTEGER) {
        Rf_error("'i' must be a valid integer.");
    }
    return Rf_ScalarInteger(mgr->ReadPerm(idx));
}

extern "C" SEXP c_cudd_read_perm_zdd(SEXP mgr_ptr, SEXP i) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx = Rf_asInteger(i);
    if (idx == NA_INTEGER) {
        Rf_error("'i' must be a valid integer.");
    }
    return Rf_ScalarInteger(mgr->ReadPermZdd(idx));
}

extern "C" SEXP c_cudd_read_inv_perm(SEXP mgr_ptr, SEXP i) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx = Rf_asInteger(i);
    if (idx == NA_INTEGER) {
        Rf_error("'i' must be a valid integer.");
    }
    return Rf_ScalarInteger(mgr->ReadInvPerm(idx));
}

extern "C" SEXP c_cudd_read_inv_perm_zdd(SEXP mgr_ptr, SEXP i) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx = Rf_asInteger(i);
    if (idx == NA_INTEGER) {
        Rf_error("'i' must be a valid integer.");
    }
    return Rf_ScalarInteger(mgr->ReadInvPermZdd(idx));
}

extern "C" SEXP c_cudd_read_vars(SEXP mgr_ptr, SEXP i) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx = Rf_asInteger(i);
    if (idx == NA_INTEGER) {
        Rf_error("'i' must be a valid integer.");
    }
    BDD *bdd = new BDD(mgr->ReadVars(idx));
    SEXP ptr = PROTECT(R_MakeExternalPtr(bdd, R_NilValue, mgr_ptr));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_read_epsilon(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(mgr->ReadEpsilon());
}

extern "C" SEXP c_cudd_set_epsilon(SEXP mgr_ptr, SEXP ep) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (!Rf_isNumeric(ep) || Rf_length(ep) != 1) {
        Rf_error("'ep' must be a single numeric value.");
    }
    mgr->SetEpsilon(Rf_asReal(ep));
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_groupcheck(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadGroupcheck()));
}

extern "C" SEXP c_cudd_set_groupcheck(SEXP mgr_ptr, SEXP gc) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(gc);
    if (value == NA_INTEGER) {
        Rf_error("'gc' must be a valid integer.");
    }
    mgr->SetGroupcheck(static_cast<Cudd_AggregationType>(value));
    return R_NilValue;
}

extern "C" SEXP c_cudd_garbage_collection_enabled(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarLogical(mgr->GarbageCollectionEnabled());
}

extern "C" SEXP c_cudd_enable_garbage_collection(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->EnableGarbageCollection();
    return R_NilValue;
}

extern "C" SEXP c_cudd_disable_garbage_collection(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->DisableGarbageCollection();
    return R_NilValue;
}

extern "C" SEXP c_cudd_dead_are_counted(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarLogical(mgr->DeadAreCounted());
}

extern "C" SEXP c_cudd_turn_on_count_dead(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->TurnOnCountDead();
    return R_NilValue;
}

extern "C" SEXP c_cudd_turn_off_count_dead(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->TurnOffCountDead();
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_recomb(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadRecomb());
}

extern "C" SEXP c_cudd_set_recomb(SEXP mgr_ptr, SEXP recomb) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(recomb);
    if (value == NA_INTEGER) {
        Rf_error("'recomb' must be a valid integer.");
    }
    mgr->SetRecomb(value);
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_symmviolation(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadSymmviolation());
}

extern "C" SEXP c_cudd_set_symmviolation(SEXP mgr_ptr, SEXP symm) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(symm);
    if (value == NA_INTEGER) {
        Rf_error("'symm' must be a valid integer.");
    }
    mgr->SetSymmviolation(value);
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_arcviolation(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadArcviolation());
}

extern "C" SEXP c_cudd_set_arcviolation(SEXP mgr_ptr, SEXP arc) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(arc);
    if (value == NA_INTEGER) {
        Rf_error("'arc' must be a valid integer.");
    }
    mgr->SetArcviolation(value);
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_population_size(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadPopulationSize());
}

extern "C" SEXP c_cudd_set_population_size(SEXP mgr_ptr, SEXP pop) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(pop);
    if (value == NA_INTEGER) {
        Rf_error("'pop' must be a valid integer.");
    }
    mgr->SetPopulationSize(value);
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_number_xovers(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadNumberXovers());
}

extern "C" SEXP c_cudd_set_number_xovers(SEXP mgr_ptr, SEXP xovers) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(xovers);
    if (value == NA_INTEGER) {
        Rf_error("'xovers' must be a valid integer.");
    }
    mgr->SetNumberXovers(value);
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_order_randomization(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadOrderRandomization()));
}

extern "C" SEXP c_cudd_set_order_randomization(SEXP mgr_ptr, SEXP factor) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(factor);
    if (value == NA_INTEGER) {
        Rf_error("'factor' must be a valid integer.");
    }
    mgr->SetOrderRandomization(static_cast<unsigned int>(value));
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_memory_in_use(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadMemoryInUse()));
}

extern "C" SEXP c_cudd_read_peak_node_count(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadPeakNodeCount()));
}

extern "C" SEXP c_cudd_read_node_count_current(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadNodeCount()));
}

extern "C" SEXP c_cudd_read_node_count_zdd(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->zddReadNodeCount()));
}

extern "C" SEXP c_cudd_enable_reordering_reporting(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->EnableReorderingReporting();
    return R_NilValue;
}

extern "C" SEXP c_cudd_disable_reordering_reporting(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->DisableReorderingReporting();
    return R_NilValue;
}

extern "C" SEXP c_cudd_reordering_reporting(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarLogical(mgr->ReorderingReporting());
}

extern "C" SEXP c_cudd_read_error_code(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(mgr->ReadErrorCode());
}

extern "C" SEXP c_cudd_clear_error_code(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    mgr->ClearErrorCode();
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_next_reordering(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadNextReordering()));
}

extern "C" SEXP c_cudd_set_next_reordering(SEXP mgr_ptr, SEXP nr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(nr);
    if (value == NA_INTEGER) {
        Rf_error("'nr' must be a valid integer.");
    }
    mgr->SetNextReordering(static_cast<unsigned int>(value));
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_swap_steps(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(mgr->ReadSwapSteps());
}

extern "C" SEXP c_cudd_read_max_live(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarInteger(static_cast<int>(mgr->ReadMaxLive()));
}

extern "C" SEXP c_cudd_set_max_live(SEXP mgr_ptr, SEXP max_live) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int value = Rf_asInteger(max_live);
    if (value == NA_INTEGER) {
        Rf_error("'max_live' must be a valid integer.");
    }
    mgr->SetMaxLive(static_cast<unsigned int>(value));
    return R_NilValue;
}

extern "C" SEXP c_cudd_read_max_memory(SEXP mgr_ptr) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return Rf_ScalarReal(static_cast<double>(mgr->ReadMaxMemory()));
}

extern "C" SEXP c_cudd_set_max_memory(SEXP mgr_ptr, SEXP max_mem) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    if (!Rf_isNumeric(max_mem) || Rf_length(max_mem) != 1) {
        Rf_error("'max_mem' must be a single numeric value.");
    }
    size_t value = static_cast<size_t>(Rf_asReal(max_mem));
    size_t result = mgr->SetMaxMemory(value);
    return Rf_ScalarReal(static_cast<double>(result));
}

extern "C" SEXP c_cudd_bdd_bind_var(SEXP mgr_ptr, SEXP index) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER) {
        Rf_error("'index' must be a valid integer.");
    }
    int result = mgr->bddBindVar(idx);
    return Rf_ScalarInteger(result);
}

extern "C" SEXP c_cudd_bdd_unbind_var(SEXP mgr_ptr, SEXP index) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER) {
        Rf_error("'index' must be a valid integer.");
    }
    int result = mgr->bddUnbindVar(idx);
    return Rf_ScalarInteger(result);
}

extern "C" SEXP c_cudd_bdd_var_is_bound(SEXP mgr_ptr, SEXP index) {
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER) {
        Rf_error("'index' must be a valid integer.");
    }
    return Rf_ScalarLogical(mgr->bddVarIsBound(idx));
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

extern "C" SEXP c_cudd_bdd_dump_dot(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    FILE *fp = tmpfile();
    if (fp == nullptr) {
        Rf_error("Failed to create temporary file for DOT output.");
    }

    DdManager *mgr = bdd->manager();
    DdNode *node = bdd->getNode();
    DdNode *nodes[1] = { node };
    int result = Cudd_DumpDot(mgr, 1, nodes, nullptr, nullptr, fp);
    if (result != 1) {
        fclose(fp);
        Rf_error("Failed to dump DOT representation.");
    }

    if (fseek(fp, 0, SEEK_END) != 0) {
        fclose(fp);
        Rf_error("Failed to seek DOT output.");
    }
    long size = ftell(fp);
    if (size < 0) {
        fclose(fp);
        Rf_error("Failed to read DOT output size.");
    }
    if (fseek(fp, 0, SEEK_SET) != 0) {
        fclose(fp);
        Rf_error("Failed to rewind DOT output.");
    }

    std::string buffer(static_cast<size_t>(size), '\0');
    size_t read_bytes = fread(buffer.data(), 1, buffer.size(), fp);
    fclose(fp);

    buffer.resize(read_bytes);

    SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(out, 0, Rf_mkCharLen(buffer.data(), static_cast<int>(buffer.size())));
    UNPROTECT(1);
    return out;
}
