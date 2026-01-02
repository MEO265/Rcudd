#include "rcudd.h"
#include <R.h>
#include <Rinternals.h>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <unordered_map>
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

static SEXP bdd_to_xptr(const BDD &bdd) {
    BDD *result = new BDD(bdd);
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

static SEXP zdd_to_xptr(const ZDD &zdd) {
    ZDD *result = new ZDD(zdd);
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, zdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

static std::vector<BDD> bdd_vector_from_list(SEXP list, const char *name) {
    if (!Rf_isNewList(list)) {
        Rf_error("'%s' must be a list of CuddBDD objects.", name);
    }
    R_xlen_t size = Rf_xlength(list);
    std::vector<BDD> result;
    result.reserve(static_cast<size_t>(size));
    for (R_xlen_t i = 0; i < size; ++i) {
        SEXP item = VECTOR_ELT(list, i);
        BDD *bdd = bdd_from_ptr(item);
        result.push_back(*bdd);
    }
    return result;
}

static SEXP bdd_list_from_vector(const std::vector<BDD> &values) {
    R_xlen_t size = static_cast<R_xlen_t>(values.size());
    SEXP list = PROTECT(Rf_allocVector(VECSXP, size));
    for (R_xlen_t i = 0; i < size; ++i) {
        SEXP ptr = bdd_to_xptr(values[static_cast<size_t>(i)]);
        SET_VECTOR_ELT(list, i, ptr);
    }
    UNPROTECT(1);
    return list;
}

static std::vector<int> int_vector_from_sexp(SEXP vec, const char *name) {
    if (!Rf_isInteger(vec)) {
        Rf_error("'%s' must be an integer vector.", name);
    }
    R_xlen_t size = Rf_xlength(vec);
    std::vector<int> result;
    result.reserve(static_cast<size_t>(size));
    for (R_xlen_t i = 0; i < size; ++i) {
        int value = INTEGER(vec)[i];
        if (value == NA_INTEGER) {
            Rf_error("'%s' must not contain NA.", name);
        }
        result.push_back(value);
    }
    return result;
}

static SEXP int_vector_to_sexp(const std::vector<unsigned int> &values) {
    R_xlen_t size = static_cast<R_xlen_t>(values.size());
    SEXP vec = PROTECT(Rf_allocVector(INTSXP, size));
    for (R_xlen_t i = 0; i < size; ++i) {
        INTEGER(vec)[i] = static_cast<int>(values[static_cast<size_t>(i)]);
    }
    UNPROTECT(1);
    return vec;
}

static std::vector<double> double_vector_from_sexp(SEXP vec, const char *name) {
    if (!Rf_isReal(vec)) {
        Rf_error("'%s' must be a numeric vector.", name);
    }
    R_xlen_t size = Rf_xlength(vec);
    std::vector<double> result;
    result.reserve(static_cast<size_t>(size));
    for (R_xlen_t i = 0; i < size; ++i) {
        double value = REAL(vec)[i];
        if (ISNA(value)) {
            Rf_error("'%s' must not contain NA.", name);
        }
        result.push_back(value);
    }
    return result;
}

static std::string bdd_ite_formula(DdManager *mgr, DdNode *node, std::unordered_map<DdNode *, std::string> &memo) {
    auto make_not = [](const std::string &expr) {
        return "not(" + expr + ")";
    };
    auto make_and = [](const std::string &lhs, const std::string &rhs) {
        return "and(" + lhs + ", " + rhs + ")";
    };
    auto make_or = [](const std::string &lhs, const std::string &rhs) {
        return "or(" + lhs + ", " + rhs + ")";
    };
    auto make_xor = [](const std::string &lhs, const std::string &rhs) {
        return "xor(" + lhs + ", " + rhs + ")";
    };
    auto is_const = [mgr](DdNode *candidate, bool &value) {
        DdNode *regular = Cudd_Regular(candidate);
        if (!Cudd_IsConstant(regular)) {
            return false;
        }
        bool is_one = regular == Cudd_ReadOne(mgr);
        if (Cudd_IsComplement(candidate)) {
            is_one = !is_one;
        }
        value = is_one;
        return true;
    };
    bool complemented = Cudd_IsComplement(node);
    DdNode *regular = Cudd_Regular(node);
    if (Cudd_IsConstant(regular)) {
        bool is_one = regular == Cudd_ReadOne(mgr);
        if (complemented) {
            is_one = !is_one;
        }
        return is_one ? "TRUE" : "FALSE";
    }
    if (complemented) {
        return make_not(bdd_ite_formula(mgr, regular, memo));
    }
    auto found = memo.find(regular);
    if (found != memo.end()) {
        return found->second;
    }
    int index = static_cast<int>(Cudd_NodeReadIndex(regular));
    std::string var = "x" + std::to_string(index);
    DdNode *t = Cudd_T(regular);
    DdNode *e = Cudd_E(regular);
    bool t_comp = Cudd_IsComplement(t);
    bool e_comp = Cudd_IsComplement(e);
    DdNode *t_reg = Cudd_Regular(t);
    DdNode *e_reg = Cudd_Regular(e);
    bool t_const = false;
    bool e_const = false;
    bool t_value = false;
    bool e_value = false;
    t_const = is_const(t, t_value);
    e_const = is_const(e, e_value);
    std::string formula;
    if (t_reg == e_reg && t_comp != e_comp) {
        if (t_comp && !e_comp) {
            std::string sub = bdd_ite_formula(mgr, e, memo);
            formula = make_xor(var, sub);
        } else {
            std::string sub = bdd_ite_formula(mgr, t, memo);
            formula = make_not(make_xor(var, sub));
        }
    } else if (t_const && e_const) {
        if (t_value == e_value) {
            formula = t_value ? "TRUE" : "FALSE";
        } else if (t_value) {
            formula = var;
        } else {
            formula = make_not(var);
        }
    } else if (t_const && t_value) {
        std::string e_formula = bdd_ite_formula(mgr, e, memo);
        formula = make_or(var, e_formula);
    } else if (t_const && !t_value) {
        std::string e_formula = bdd_ite_formula(mgr, e, memo);
        formula = make_and(make_not(var), e_formula);
    } else if (e_const && !e_value) {
        std::string t_formula = bdd_ite_formula(mgr, t, memo);
        formula = make_and(var, t_formula);
    } else if (e_const && e_value) {
        std::string t_formula = bdd_ite_formula(mgr, t, memo);
        formula = make_or(make_not(var), t_formula);
    } else {
        std::string t_formula = bdd_ite_formula(mgr, t, memo);
        std::string e_formula = bdd_ite_formula(mgr, e, memo);
        formula = make_or(make_and(var, t_formula), make_and(make_not(var), e_formula));
    }
    memo.emplace(regular, formula);
    return formula;
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

extern "C" SEXP c_cudd_bdd_restrict(SEXP bdd_ptr, SEXP constraint_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *constraint = bdd_from_ptr(constraint_ptr);
    BDD *result = new BDD(bdd->Restrict(*constraint));
    SEXP ptr = PROTECT(R_MakeExternalPtr(result, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, bdd_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

extern "C" SEXP c_cudd_bdd_print(SEXP bdd_ptr, SEXP nvars, SEXP verbosity) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(nvars);
    int verb = Rf_asInteger(verbosity);
    if (vars == NA_INTEGER || vars < 0 || verb == NA_INTEGER) {
        Rf_error("'nvars' must be non-negative and 'verbosity' must be an integer.");
    }
    bdd->print(vars, verb);
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_summary(SEXP bdd_ptr, SEXP nvars, SEXP mode) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(nvars);
    int summary_mode = Rf_asInteger(mode);
    if (vars == NA_INTEGER || vars < 0 || summary_mode == NA_INTEGER) {
        Rf_error("'nvars' must be non-negative and 'mode' must be an integer.");
    }
    bdd->summary(vars, summary_mode);
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_ite_formula(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::unordered_map<DdNode *, std::string> memo;
    std::string formula = bdd_ite_formula(bdd->manager(), bdd->getNode(), memo);
    return Rf_mkString(formula.c_str());
}

extern "C" SEXP c_cudd_bdd_apa_print_minterm(SEXP bdd_ptr, SEXP nvars) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(nvars);
    if (vars == NA_INTEGER || vars < 0) {
        Rf_error("'nvars' must be a non-negative integer.");
    }
    bdd->ApaPrintMinterm(vars);
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_apa_print_minterm_exp(SEXP bdd_ptr, SEXP nvars, SEXP precision) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(nvars);
    int prec = Rf_asInteger(precision);
    if (vars == NA_INTEGER || vars < 0 || prec == NA_INTEGER) {
        Rf_error("'nvars' must be non-negative and 'precision' must be an integer.");
    }
    bdd->ApaPrintMintermExp(vars, prec);
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_ldbl_count_minterm(SEXP bdd_ptr, SEXP nvars) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(nvars);
    if (vars == NA_INTEGER || vars < 0) {
        Rf_error("'nvars' must be a non-negative integer.");
    }
    long double count = bdd->LdblCountMinterm(vars);
    return Rf_ScalarReal(static_cast<double>(count));
}

extern "C" SEXP c_cudd_bdd_shortest_path(SEXP bdd_ptr, SEXP weight) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int size = Cudd_ReadSize(bdd->manager());
    std::vector<int> weight_vec;
    int *weight_ptr = nullptr;
    if (!Rf_isNull(weight)) {
        weight_vec = int_vector_from_sexp(weight, "weight");
        if (static_cast<int>(weight_vec.size()) != size) {
            Rf_error("'weight' must have length %d.", size);
        }
        weight_ptr = weight_vec.data();
    }
    std::vector<int> support(static_cast<size_t>(size), 0);
    int length = 0;
    BDD result = bdd->ShortestPath(weight_ptr, support.data(), &length);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(result));
    SEXP support_vec = PROTECT(Rf_allocVector(INTSXP, size));
    for (int i = 0; i < size; ++i) {
        INTEGER(support_vec)[i] = support[static_cast<size_t>(i)];
    }
    SET_VECTOR_ELT(output, 1, support_vec);
    SET_VECTOR_ELT(output, 2, Rf_ScalarInteger(length));
    UNPROTECT(2);
    return output;
}

extern "C" SEXP c_cudd_bdd_largest_cube(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int length = 0;
    BDD result = bdd->LargestCube(&length);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(result));
    SET_VECTOR_ELT(output, 1, Rf_ScalarInteger(length));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_shortest_length(SEXP bdd_ptr, SEXP weight) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int size = Cudd_ReadSize(bdd->manager());
    std::vector<int> weight_vec;
    int *weight_ptr = nullptr;
    if (!Rf_isNull(weight)) {
        weight_vec = int_vector_from_sexp(weight, "weight");
        if (static_cast<int>(weight_vec.size()) != size) {
            Rf_error("'weight' must have length %d.", size);
        }
        weight_ptr = weight_vec.data();
    }
    int result = bdd->ShortestLength(weight_ptr);
    return Rf_ScalarInteger(result);
}

extern "C" SEXP c_cudd_bdd_equiv_dc(SEXP bdd_ptr, SEXP g_ptr, SEXP d_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *g = bdd_from_ptr(g_ptr);
    BDD *d = bdd_from_ptr(d_ptr);
    return Rf_ScalarLogical(bdd->EquivDC(*g, *d));
}

extern "C" SEXP c_cudd_bdd_cof_minterm(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int size = Cudd_ReadSize(bdd->manager());
    double *values = bdd->CofMinterm();
    if (values == nullptr) {
        Rf_error("Failed to compute cofactor minterms.");
    }
    SEXP output = PROTECT(Rf_allocVector(REALSXP, size + 1));
    for (int i = 0; i < size + 1; ++i) {
        REAL(output)[i] = values[i];
    }
    free(values);
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_is_one(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return Rf_ScalarLogical(bdd->IsOne());
}

extern "C" SEXP c_cudd_bdd_is_cube(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return Rf_ScalarLogical(bdd->IsCube());
}

extern "C" SEXP c_cudd_bdd_find_essential(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return bdd_to_xptr(bdd->FindEssential());
}

extern "C" SEXP c_cudd_bdd_print_two_literal_clauses(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    bdd->PrintTwoLiteralClauses();
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_count_minterm(SEXP bdd_ptr, SEXP nvars) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(nvars);
    if (vars == NA_INTEGER || vars < 0) {
        Rf_error("'nvars' must be a non-negative integer.");
    }
    return Rf_ScalarReal(bdd->CountMinterm(vars));
}

extern "C" SEXP c_cudd_bdd_count_path(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return Rf_ScalarReal(bdd->CountPath());
}

extern "C" SEXP c_cudd_bdd_support(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return bdd_to_xptr(bdd->Support());
}

extern "C" SEXP c_cudd_bdd_support_size(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return Rf_ScalarInteger(bdd->SupportSize());
}

extern "C" SEXP c_cudd_bdd_support_indices(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<unsigned int> indices = bdd->SupportIndices();
    return int_vector_to_sexp(indices);
}

extern "C" SEXP c_cudd_bdd_classify_support(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    BDD common;
    BDD only_f;
    BDD only_g;
    bdd->ClassifySupport(*other, &common, &only_f, &only_g);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(common));
    SET_VECTOR_ELT(output, 1, bdd_to_xptr(only_f));
    SET_VECTOR_ELT(output, 2, bdd_to_xptr(only_g));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_count_leaves(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return Rf_ScalarInteger(bdd->CountLeaves());
}

extern "C" SEXP c_cudd_bdd_density(SEXP bdd_ptr, SEXP nvars) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(nvars);
    if (vars == NA_INTEGER || vars < 0) {
        Rf_error("'nvars' must be a non-negative integer.");
    }
    return Rf_ScalarReal(bdd->Density(vars));
}

extern "C" SEXP c_cudd_bdd_under_approx(SEXP bdd_ptr, SEXP num_vars, SEXP threshold, SEXP safe, SEXP quality) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    bool safe_flag = Rf_asLogical(safe);
    double qual = Rf_asReal(quality);
    return bdd_to_xptr(bdd->UnderApprox(vars, thresh, safe_flag, qual));
}

extern "C" SEXP c_cudd_bdd_over_approx(SEXP bdd_ptr, SEXP num_vars, SEXP threshold, SEXP safe, SEXP quality) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    bool safe_flag = Rf_asLogical(safe);
    double qual = Rf_asReal(quality);
    return bdd_to_xptr(bdd->OverApprox(vars, thresh, safe_flag, qual));
}

extern "C" SEXP c_cudd_bdd_remap_under_approx(SEXP bdd_ptr, SEXP num_vars, SEXP threshold, SEXP quality) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    double qual = Rf_asReal(quality);
    return bdd_to_xptr(bdd->RemapUnderApprox(vars, thresh, qual));
}

extern "C" SEXP c_cudd_bdd_remap_over_approx(SEXP bdd_ptr, SEXP num_vars, SEXP threshold, SEXP quality) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    double qual = Rf_asReal(quality);
    return bdd_to_xptr(bdd->RemapOverApprox(vars, thresh, qual));
}

extern "C" SEXP c_cudd_bdd_biased_under_approx(SEXP bdd_ptr, SEXP bias_ptr, SEXP num_vars, SEXP threshold, SEXP quality1, SEXP quality0) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *bias = bdd_from_ptr(bias_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    double qual1 = Rf_asReal(quality1);
    double qual0 = Rf_asReal(quality0);
    return bdd_to_xptr(bdd->BiasedUnderApprox(*bias, vars, thresh, qual1, qual0));
}

extern "C" SEXP c_cudd_bdd_biased_over_approx(SEXP bdd_ptr, SEXP bias_ptr, SEXP num_vars, SEXP threshold, SEXP quality1, SEXP quality0) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *bias = bdd_from_ptr(bias_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    double qual1 = Rf_asReal(quality1);
    double qual0 = Rf_asReal(quality0);
    return bdd_to_xptr(bdd->BiasedOverApprox(*bias, vars, thresh, qual1, qual0));
}

extern "C" SEXP c_cudd_bdd_clipping_and(SEXP bdd_ptr, SEXP other_ptr, SEXP max_depth, SEXP direction) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    int depth = Rf_asInteger(max_depth);
    int dir = Rf_asInteger(direction);
    if (depth == NA_INTEGER || dir == NA_INTEGER) {
        Rf_error("'max_depth' and 'direction' must be integers.");
    }
    return bdd_to_xptr(bdd->ClippingAnd(*other, depth, dir));
}

extern "C" SEXP c_cudd_bdd_clipping_and_abstract(SEXP bdd_ptr, SEXP other_ptr, SEXP cube_ptr, SEXP max_depth, SEXP direction) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    BDD *cube = bdd_from_ptr(cube_ptr);
    int depth = Rf_asInteger(max_depth);
    int dir = Rf_asInteger(direction);
    if (depth == NA_INTEGER || dir == NA_INTEGER) {
        Rf_error("'max_depth' and 'direction' must be integers.");
    }
    return bdd_to_xptr(bdd->ClippingAndAbstract(*other, *cube, depth, dir));
}

extern "C" SEXP c_cudd_bdd_var_are_symmetric(SEXP bdd_ptr, SEXP index1, SEXP index2) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int idx1 = Rf_asInteger(index1);
    int idx2 = Rf_asInteger(index2);
    if (idx1 == NA_INTEGER || idx1 < 0 || idx2 == NA_INTEGER || idx2 < 0) {
        Rf_error("'index1' and 'index2' must be non-negative integers.");
    }
    return Rf_ScalarLogical(bdd->VarAreSymmetric(idx1, idx2));
}

extern "C" SEXP c_cudd_bdd_adj_permute_x(SEXP bdd_ptr, SEXP x_list) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<BDD> x = bdd_vector_from_list(x_list, "x");
    return bdd_to_xptr(bdd->AdjPermuteX(x));
}

extern "C" SEXP c_cudd_bdd_is_var_essential(SEXP bdd_ptr, SEXP index, SEXP phase) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int idx = Rf_asInteger(index);
    int phase_val = Rf_asInteger(phase);
    if (idx == NA_INTEGER || idx < 0 || phase_val == NA_INTEGER) {
        Rf_error("'index' must be non-negative and 'phase' must be an integer.");
    }
    return Rf_ScalarLogical(bdd->IsVarEssential(idx, phase_val));
}

extern "C" SEXP c_cudd_bdd_np_and(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->NPAnd(*other));
}

extern "C" SEXP c_cudd_bdd_constrain_decomp(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<BDD> parts = bdd->ConstrainDecomp();
    return bdd_list_from_vector(parts);
}

extern "C" SEXP c_cudd_bdd_char_to_vect(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<BDD> parts = bdd->CharToVect();
    return bdd_list_from_vector(parts);
}

extern "C" SEXP c_cudd_bdd_leq_unless(SEXP bdd_ptr, SEXP g_ptr, SEXP d_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *g = bdd_from_ptr(g_ptr);
    BDD *d = bdd_from_ptr(d_ptr);
    return Rf_ScalarLogical(bdd->LeqUnless(*g, *d));
}

extern "C" SEXP c_cudd_bdd_maximally_expand(SEXP bdd_ptr, SEXP ub_ptr, SEXP f_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *ub = bdd_from_ptr(ub_ptr);
    BDD *f = bdd_from_ptr(f_ptr);
    return bdd_to_xptr(bdd->MaximallyExpand(*ub, *f));
}

extern "C" SEXP c_cudd_bdd_largest_prime_unate(SEXP bdd_ptr, SEXP phases_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *phases = bdd_from_ptr(phases_ptr);
    return bdd_to_xptr(bdd->LargestPrimeUnate(*phases));
}

extern "C" SEXP c_cudd_bdd_solve_eqn(SEXP bdd_ptr, SEXP y_ptr, SEXP n) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *y = bdd_from_ptr(y_ptr);
    int count = Rf_asInteger(n);
    if (count == NA_INTEGER || count < 0) {
        Rf_error("'n' must be a non-negative integer.");
    }
    std::vector<BDD> g;
    int *y_index = nullptr;
    BDD result = bdd->SolveEqn(*y, g, &y_index, count);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(result));
    SET_VECTOR_ELT(output, 1, bdd_list_from_vector(g));
    SEXP y_index_vec = PROTECT(Rf_allocVector(INTSXP, count));
    for (int i = 0; i < count; ++i) {
        INTEGER(y_index_vec)[i] = y_index[i];
    }
    SET_VECTOR_ELT(output, 2, y_index_vec);
    if (y_index != nullptr) {
        free(y_index);
    }
    UNPROTECT(2);
    return output;
}

extern "C" SEXP c_cudd_bdd_verify_sol(SEXP bdd_ptr, SEXP g_list, SEXP y_index) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<BDD> g = bdd_vector_from_list(g_list, "g");
    std::vector<int> y_index_vec = int_vector_from_sexp(y_index, "y_index");
    BDD result = bdd->VerifySol(g, y_index_vec.data());
    return bdd_to_xptr(result);
}

extern "C" SEXP c_cudd_bdd_split_set(SEXP bdd_ptr, SEXP x_vars, SEXP m) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<BDD> vars = bdd_vector_from_list(x_vars, "x_vars");
    double value = Rf_asReal(m);
    return bdd_to_xptr(bdd->SplitSet(vars, value));
}

extern "C" SEXP c_cudd_bdd_estimate_cofactor(SEXP bdd_ptr, SEXP index, SEXP phase) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int idx = Rf_asInteger(index);
    int phase_val = Rf_asInteger(phase);
    if (idx == NA_INTEGER || idx < 0 || phase_val == NA_INTEGER) {
        Rf_error("'index' must be non-negative and 'phase' must be an integer.");
    }
    return Rf_ScalarInteger(bdd->EstimateCofactor(idx, phase_val));
}

extern "C" SEXP c_cudd_bdd_estimate_cofactor_simple(SEXP bdd_ptr, SEXP index) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER || idx < 0) {
        Rf_error("'index' must be a non-negative integer.");
    }
    return Rf_ScalarInteger(bdd->EstimateCofactorSimple(idx));
}

extern "C" SEXP c_cudd_bdd_zdd_isop(SEXP bdd_ptr, SEXP upper_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *upper = bdd_from_ptr(upper_ptr);
    ZDD *zdd = new ZDD();
    BDD result = bdd->zddIsop(*upper, zdd);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(result));
    SEXP zdd_ptr = PROTECT(R_MakeExternalPtr(zdd, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(zdd_ptr, zdd_finalizer, TRUE);
    SET_VECTOR_ELT(output, 1, zdd_ptr);
    UNPROTECT(2);
    return output;
}

extern "C" SEXP c_cudd_bdd_transfer(SEXP bdd_ptr, SEXP mgr_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    Cudd *mgr = cudd_manager_from_ptr(mgr_ptr);
    return bdd_to_xptr(bdd->Transfer(*mgr));
}

extern "C" SEXP c_cudd_bdd_is_zero(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return Rf_ScalarLogical(bdd->IsZero());
}

extern "C" SEXP c_cudd_bdd_is_var(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return Rf_ScalarLogical(bdd->IsVar());
}

extern "C" SEXP c_cudd_bdd_leq(SEXP lhs_ptr, SEXP rhs_ptr) {
    BDD *lhs = bdd_from_ptr(lhs_ptr);
    BDD *rhs = bdd_from_ptr(rhs_ptr);
    return Rf_ScalarLogical(lhs->Leq(*rhs));
}

extern "C" SEXP c_cudd_bdd_and_abstract(SEXP bdd_ptr, SEXP other_ptr, SEXP cube_ptr, SEXP limit) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    BDD *cube = bdd_from_ptr(cube_ptr);
    unsigned int lim = static_cast<unsigned int>(Rf_asInteger(limit));
    return bdd_to_xptr(bdd->AndAbstract(*other, *cube, lim));
}

extern "C" SEXP c_cudd_bdd_exist_abstract(SEXP bdd_ptr, SEXP cube_ptr, SEXP limit) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *cube = bdd_from_ptr(cube_ptr);
    unsigned int lim = static_cast<unsigned int>(Rf_asInteger(limit));
    return bdd_to_xptr(bdd->ExistAbstract(*cube, lim));
}

extern "C" SEXP c_cudd_bdd_univ_abstract(SEXP bdd_ptr, SEXP cube_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *cube = bdd_from_ptr(cube_ptr);
    return bdd_to_xptr(bdd->UnivAbstract(*cube));
}

extern "C" SEXP c_cudd_bdd_xor_exist_abstract(SEXP bdd_ptr, SEXP other_ptr, SEXP cube_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    BDD *cube = bdd_from_ptr(cube_ptr);
    return bdd_to_xptr(bdd->XorExistAbstract(*other, *cube));
}

extern "C" SEXP c_cudd_bdd_boolean_diff(SEXP bdd_ptr, SEXP index) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER || idx < 0) {
        Rf_error("'index' must be a non-negative integer.");
    }
    return bdd_to_xptr(bdd->BooleanDiff(idx));
}

extern "C" SEXP c_cudd_bdd_var_is_dependent(SEXP bdd_ptr, SEXP var_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *var = bdd_from_ptr(var_ptr);
    return Rf_ScalarLogical(bdd->VarIsDependent(*var));
}

extern "C" SEXP c_cudd_bdd_correlation(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return Rf_ScalarReal(bdd->Correlation(*other));
}

extern "C" SEXP c_cudd_bdd_correlation_weights(SEXP bdd_ptr, SEXP other_ptr, SEXP prob) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    std::vector<double> prob_vec = double_vector_from_sexp(prob, "prob");
    int size = Cudd_ReadSize(bdd->manager());
    if (static_cast<int>(prob_vec.size()) != size) {
        Rf_error("'prob' must have length %d.", size);
    }
    double corr = bdd->CorrelationWeights(*other, prob_vec.data());
    return Rf_ScalarReal(corr);
}

extern "C" SEXP c_cudd_bdd_xor_method(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->Xor(*other));
}

extern "C" SEXP c_cudd_bdd_ite(SEXP bdd_ptr, SEXP g_ptr, SEXP h_ptr, SEXP limit) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *g = bdd_from_ptr(g_ptr);
    BDD *h = bdd_from_ptr(h_ptr);
    unsigned int lim = static_cast<unsigned int>(Rf_asInteger(limit));
    return bdd_to_xptr(bdd->Ite(*g, *h, lim));
}

extern "C" SEXP c_cudd_bdd_ite_constant(SEXP bdd_ptr, SEXP g_ptr, SEXP h_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *g = bdd_from_ptr(g_ptr);
    BDD *h = bdd_from_ptr(h_ptr);
    return bdd_to_xptr(bdd->IteConstant(*g, *h));
}

extern "C" SEXP c_cudd_bdd_intersect(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->Intersect(*other));
}

extern "C" SEXP c_cudd_bdd_and_limit(SEXP bdd_ptr, SEXP other_ptr, SEXP limit) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    unsigned int lim = static_cast<unsigned int>(Rf_asInteger(limit));
    return bdd_to_xptr(bdd->And(*other, lim));
}

extern "C" SEXP c_cudd_bdd_or_limit(SEXP bdd_ptr, SEXP other_ptr, SEXP limit) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    unsigned int lim = static_cast<unsigned int>(Rf_asInteger(limit));
    return bdd_to_xptr(bdd->Or(*other, lim));
}

extern "C" SEXP c_cudd_bdd_nand(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->Nand(*other));
}

extern "C" SEXP c_cudd_bdd_nor(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->Nor(*other));
}

extern "C" SEXP c_cudd_bdd_xnor(SEXP bdd_ptr, SEXP other_ptr, SEXP limit) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    unsigned int lim = static_cast<unsigned int>(Rf_asInteger(limit));
    return bdd_to_xptr(bdd->Xnor(*other, lim));
}

extern "C" SEXP c_cudd_bdd_cofactor(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->Cofactor(*other));
}

extern "C" SEXP c_cudd_bdd_constrain(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->Constrain(*other));
}

extern "C" SEXP c_cudd_bdd_compose(SEXP bdd_ptr, SEXP other_ptr, SEXP index) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER || idx < 0) {
        Rf_error("'index' must be a non-negative integer.");
    }
    return bdd_to_xptr(bdd->Compose(*other, idx));
}

extern "C" SEXP c_cudd_bdd_permute(SEXP bdd_ptr, SEXP permut) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<int> perm = int_vector_from_sexp(permut, "permut");
    return bdd_to_xptr(bdd->Permute(perm.data()));
}

extern "C" SEXP c_cudd_bdd_swap_variables(SEXP bdd_ptr, SEXP x_list, SEXP y_list) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<BDD> x = bdd_vector_from_list(x_list, "x");
    std::vector<BDD> y = bdd_vector_from_list(y_list, "y");
    return bdd_to_xptr(bdd->SwapVariables(x, y));
}

extern "C" SEXP c_cudd_bdd_vector_compose(SEXP bdd_ptr, SEXP vector_list) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<BDD> vec = bdd_vector_from_list(vector_list, "vector");
    return bdd_to_xptr(bdd->VectorCompose(vec));
}

extern "C" SEXP c_cudd_bdd_approx_conj_decomp(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD g;
    BDD h;
    bdd->ApproxConjDecomp(&g, &h);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(g));
    SET_VECTOR_ELT(output, 1, bdd_to_xptr(h));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_approx_disj_decomp(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD g;
    BDD h;
    bdd->ApproxDisjDecomp(&g, &h);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(g));
    SET_VECTOR_ELT(output, 1, bdd_to_xptr(h));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_iter_conj_decomp(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD g;
    BDD h;
    bdd->IterConjDecomp(&g, &h);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(g));
    SET_VECTOR_ELT(output, 1, bdd_to_xptr(h));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_iter_disj_decomp(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD g;
    BDD h;
    bdd->IterDisjDecomp(&g, &h);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(g));
    SET_VECTOR_ELT(output, 1, bdd_to_xptr(h));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_gen_conj_decomp(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD g;
    BDD h;
    bdd->GenConjDecomp(&g, &h);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(g));
    SET_VECTOR_ELT(output, 1, bdd_to_xptr(h));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_gen_disj_decomp(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD g;
    BDD h;
    bdd->GenDisjDecomp(&g, &h);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(g));
    SET_VECTOR_ELT(output, 1, bdd_to_xptr(h));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_var_conj_decomp(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD g;
    BDD h;
    bdd->VarConjDecomp(&g, &h);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(g));
    SET_VECTOR_ELT(output, 1, bdd_to_xptr(h));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_var_disj_decomp(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD g;
    BDD h;
    bdd->VarDisjDecomp(&g, &h);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, bdd_to_xptr(g));
    SET_VECTOR_ELT(output, 1, bdd_to_xptr(h));
    UNPROTECT(1);
    return output;
}

extern "C" SEXP c_cudd_bdd_li_compaction(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->LICompaction(*other));
}

extern "C" SEXP c_cudd_bdd_squeeze(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->Squeeze(*other));
}

extern "C" SEXP c_cudd_bdd_interpolate(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->Interpolate(*other));
}

extern "C" SEXP c_cudd_bdd_minimize(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->Minimize(*other));
}

extern "C" SEXP c_cudd_bdd_subset_compress(SEXP bdd_ptr, SEXP nvars, SEXP threshold) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(nvars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'nvars' must be non-negative and 'threshold' must be an integer.");
    }
    return bdd_to_xptr(bdd->SubsetCompress(vars, thresh));
}

extern "C" SEXP c_cudd_bdd_superset_compress(SEXP bdd_ptr, SEXP nvars, SEXP threshold) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(nvars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'nvars' must be non-negative and 'threshold' must be an integer.");
    }
    return bdd_to_xptr(bdd->SupersetCompress(vars, thresh));
}

extern "C" SEXP c_cudd_bdd_literal_set_intersection(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->LiteralSetIntersection(*other));
}

extern "C" SEXP c_cudd_bdd_c_projection(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->CProjection(*other));
}

extern "C" SEXP c_cudd_bdd_min_hamming_dist(SEXP bdd_ptr, SEXP minterm, SEXP upper_bound) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<int> minterm_vec = int_vector_from_sexp(minterm, "minterm");
    int bound = Rf_asInteger(upper_bound);
    if (bound == NA_INTEGER) {
        Rf_error("'upper_bound' must be an integer.");
    }
    int result = bdd->MinHammingDist(minterm_vec.data(), bound);
    SEXP output = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(output, 0, Rf_ScalarInteger(result));
    SEXP minterm_out = PROTECT(Rf_allocVector(INTSXP, minterm_vec.size()));
    for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(minterm_vec.size()); ++i) {
        INTEGER(minterm_out)[i] = minterm_vec[static_cast<size_t>(i)];
    }
    SET_VECTOR_ELT(output, 1, minterm_out);
    UNPROTECT(2);
    return output;
}

extern "C" SEXP c_cudd_bdd_eval(SEXP bdd_ptr, SEXP inputs) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<int> values = int_vector_from_sexp(inputs, "inputs");
    return bdd_to_xptr(bdd->Eval(values.data()));
}

extern "C" SEXP c_cudd_bdd_decreasing(SEXP bdd_ptr, SEXP index) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER || idx < 0) {
        Rf_error("'index' must be a non-negative integer.");
    }
    return bdd_to_xptr(bdd->Decreasing(idx));
}

extern "C" SEXP c_cudd_bdd_increasing(SEXP bdd_ptr, SEXP index) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int idx = Rf_asInteger(index);
    if (idx == NA_INTEGER || idx < 0) {
        Rf_error("'index' must be a non-negative integer.");
    }
    return bdd_to_xptr(bdd->Increasing(idx));
}

extern "C" SEXP c_cudd_bdd_make_prime(SEXP bdd_ptr, SEXP other_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *other = bdd_from_ptr(other_ptr);
    return bdd_to_xptr(bdd->MakePrime(*other));
}

extern "C" SEXP c_cudd_bdd_subset_heavy_branch(SEXP bdd_ptr, SEXP num_vars, SEXP threshold) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    return bdd_to_xptr(bdd->SubsetHeavyBranch(vars, thresh));
}

extern "C" SEXP c_cudd_bdd_superset_heavy_branch(SEXP bdd_ptr, SEXP num_vars, SEXP threshold) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    return bdd_to_xptr(bdd->SupersetHeavyBranch(vars, thresh));
}

extern "C" SEXP c_cudd_bdd_subset_short_paths(SEXP bdd_ptr, SEXP num_vars, SEXP threshold, SEXP hardlimit) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    bool hard = Rf_asLogical(hardlimit);
    return bdd_to_xptr(bdd->SubsetShortPaths(vars, thresh, hard));
}

extern "C" SEXP c_cudd_bdd_superset_short_paths(SEXP bdd_ptr, SEXP num_vars, SEXP threshold, SEXP hardlimit) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int vars = Rf_asInteger(num_vars);
    int thresh = Rf_asInteger(threshold);
    if (vars == NA_INTEGER || vars < 0 || thresh == NA_INTEGER) {
        Rf_error("'num_vars' must be non-negative and 'threshold' must be an integer.");
    }
    bool hard = Rf_asLogical(hardlimit);
    return bdd_to_xptr(bdd->SupersetShortPaths(vars, thresh, hard));
}

extern "C" SEXP c_cudd_bdd_print_cover(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    bdd->PrintCover();
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_print_cover_with_cube(SEXP bdd_ptr, SEXP cube_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *cube = bdd_from_ptr(cube_ptr);
    bdd->PrintCover(*cube);
    return R_NilValue;
}

extern "C" SEXP c_cudd_bdd_pick_one_cube(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    int size = Cudd_ReadSize(bdd->manager());
    std::vector<char> buffer(static_cast<size_t>(size) + 1, '\0');
    bdd->PickOneCube(buffer.data());
    return Rf_mkString(buffer.data());
}

extern "C" SEXP c_cudd_bdd_pick_one_minterm(SEXP bdd_ptr, SEXP vars_list) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::vector<BDD> vars = bdd_vector_from_list(vars_list, "vars");
    return bdd_to_xptr(bdd->PickOneMinterm(vars));
}

extern "C" SEXP c_cudd_bdd_isop(SEXP bdd_ptr, SEXP upper_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    BDD *upper = bdd_from_ptr(upper_ptr);
    return bdd_to_xptr(bdd->Isop(*upper));
}

extern "C" SEXP c_cudd_bdd_port_to_zdd(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    return zdd_to_xptr(bdd->PortToZdd());
}

extern "C" SEXP c_cudd_bdd_factored_form_string(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    std::string output = bdd->FactoredFormString();
    return Rf_mkString(output.c_str());
}

extern "C" SEXP c_cudd_bdd_print_factored_form(SEXP bdd_ptr) {
    BDD *bdd = bdd_from_ptr(bdd_ptr);
    bdd->PrintFactoredForm();
    return R_NilValue;
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

extern "C" SEXP c_cudd_bdd_truth_table(SEXP bdd_ptr, SEXP nvars) {
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

    if (vars > 30) {
        Rf_error("'nvars' is too large to build a truth table.");
    }

    R_xlen_t rows = static_cast<R_xlen_t>(1ULL << vars);
    int cols = vars + 1;
    SEXP table = PROTECT(Rf_allocMatrix(INTSXP, rows, cols));
    int *data = INTEGER(table);
    std::vector<int> inputs(static_cast<size_t>(vars), 0);

    for (R_xlen_t row = 0; row < rows; ++row) {
        for (int var = 0; var < vars; ++var) {
            int value = static_cast<int>((row >> var) & 1ULL);
            inputs[static_cast<size_t>(var)] = value;
            data[static_cast<R_xlen_t>(var) * rows + row] = value;
        }
        BDD result = bdd->Eval(inputs.data());
        data[static_cast<R_xlen_t>(vars) * rows + row] = result.IsOne() ? 1 : 0;
    }

    UNPROTECT(1);
    return table;
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
