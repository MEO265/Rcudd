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
extern "C" SEXP c_cudd_bdd_one(SEXP mgr_ptr);
extern "C" SEXP c_cudd_bdd_zero(SEXP mgr_ptr);
extern "C" SEXP c_cudd_bdd_var(SEXP mgr_ptr, SEXP index);
extern "C" SEXP c_cudd_add_one(SEXP mgr_ptr);
extern "C" SEXP c_cudd_add_zero(SEXP mgr_ptr);
extern "C" SEXP c_cudd_add_var(SEXP mgr_ptr, SEXP index);
extern "C" SEXP c_cudd_zdd_one(SEXP mgr_ptr, SEXP index);
extern "C" SEXP c_cudd_zdd_zero(SEXP mgr_ptr);
extern "C" SEXP c_cudd_zdd_var(SEXP mgr_ptr, SEXP index);
extern "C" SEXP c_cudd_bdd_not(SEXP bdd_ptr);
extern "C" SEXP c_cudd_bdd_and(SEXP lhs_ptr, SEXP rhs_ptr);
extern "C" SEXP c_cudd_bdd_or(SEXP lhs_ptr, SEXP rhs_ptr);
extern "C" SEXP c_cudd_bdd_xor(SEXP lhs_ptr, SEXP rhs_ptr);
extern "C" SEXP c_cudd_add_times(SEXP lhs_ptr, SEXP rhs_ptr);
extern "C" SEXP c_cudd_add_plus(SEXP lhs_ptr, SEXP rhs_ptr);
extern "C" SEXP c_cudd_zdd_intersect(SEXP lhs_ptr, SEXP rhs_ptr);
extern "C" SEXP c_cudd_zdd_union(SEXP lhs_ptr, SEXP rhs_ptr);
extern "C" SEXP c_cudd_bdd_to_add(SEXP bdd_ptr);
extern "C" SEXP c_cudd_add_to_bdd(SEXP add_ptr);
extern "C" SEXP c_cudd_bdd_to_zdd(SEXP bdd_ptr);
extern "C" SEXP c_cudd_zdd_to_bdd(SEXP zdd_ptr);

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
