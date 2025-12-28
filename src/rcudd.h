#ifndef RCUDD
#define RCUDD

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#include <Rinternals.h>

extern "C" SEXP bdd_remaining_literals(SEXP expr_in);
extern "C" SEXP bdd_restrict_chain(SEXP exprs_in, SEXP additional_constraints_in);
extern "C" SEXP c_cudd_new();
extern "C" SEXP c_cudd_read_size(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_cache_slots(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_cache_used_slots(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_cache_lookups(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_cache_hits(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_min_hit(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_loose_up_to(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_node_count(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_zdd_size(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_reorderings(SEXP mgr_ptr);

#ifdef __cplusplus
#include <string>
#include <unordered_map>
#include <vector>
#include "cuddObj.hh"

namespace rcudd {
BDD parse_bdd_expression(const std::string &expr,
                         Cudd &mgr,
                         std::unordered_map<std::string, unsigned int> &var_idx,
                         std::vector<std::string> &idx_to_name,
                         std::vector<std::string> *encountered_vars = nullptr);
}
#endif

#endif
