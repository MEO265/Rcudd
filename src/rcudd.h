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
extern "C" SEXP c_cudd_set_min_hit(SEXP mgr_ptr, SEXP hr);
extern "C" SEXP c_cudd_read_loose_up_to(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_loose_up_to(SEXP mgr_ptr, SEXP lut);
extern "C" SEXP c_cudd_make_verbose(SEXP mgr_ptr);
extern "C" SEXP c_cudd_make_terse(SEXP mgr_ptr);
extern "C" SEXP c_cudd_is_verbose(SEXP mgr_ptr);
extern "C" SEXP c_cudd_info(SEXP mgr_ptr);
extern "C" SEXP c_cudd_push_variable_name(SEXP mgr_ptr, SEXP name);
extern "C" SEXP c_cudd_clear_variable_names(SEXP mgr_ptr);
extern "C" SEXP c_cudd_get_variable_name(SEXP mgr_ptr, SEXP index);
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
extern "C" SEXP c_cudd_bdd_epd_print_minterm(SEXP bdd_ptr, SEXP nvars);
extern "C" SEXP c_cudd_add_epd_print_minterm(SEXP add_ptr, SEXP nvars);
extern "C" SEXP c_cudd_bdd_print_minterm(SEXP bdd_ptr);
extern "C" SEXP c_cudd_add_print_minterm(SEXP add_ptr);
extern "C" SEXP c_cudd_zdd_print_minterm(SEXP zdd_ptr);
extern "C" SEXP c_cudd_bdd_print_debug(SEXP bdd_ptr, SEXP nvars, SEXP verbosity);
extern "C" SEXP c_cudd_bdd_dump_dot(SEXP bdd_ptr);
extern "C" SEXP c_cudd_read_start_time(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_elapsed_time(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_start_time(SEXP mgr_ptr, SEXP st);
extern "C" SEXP c_cudd_reset_start_time(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_time_limit(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_time_limit(SEXP mgr_ptr, SEXP tl);
extern "C" SEXP c_cudd_update_time_limit(SEXP mgr_ptr);
extern "C" SEXP c_cudd_increase_time_limit(SEXP mgr_ptr, SEXP inc);
extern "C" SEXP c_cudd_unset_time_limit(SEXP mgr_ptr);
extern "C" SEXP c_cudd_time_limited(SEXP mgr_ptr);
extern "C" SEXP c_cudd_autodyn_enable(SEXP mgr_ptr, SEXP method);
extern "C" SEXP c_cudd_autodyn_disable(SEXP mgr_ptr);
extern "C" SEXP c_cudd_reordering_status(SEXP mgr_ptr);
extern "C" SEXP c_cudd_autodyn_enable_zdd(SEXP mgr_ptr, SEXP method);
extern "C" SEXP c_cudd_autodyn_disable_zdd(SEXP mgr_ptr);
extern "C" SEXP c_cudd_reordering_status_zdd(SEXP mgr_ptr);
extern "C" SEXP c_cudd_zdd_realignment_enabled(SEXP mgr_ptr);
extern "C" SEXP c_cudd_zdd_realign_enable(SEXP mgr_ptr);
extern "C" SEXP c_cudd_zdd_realign_disable(SEXP mgr_ptr);
extern "C" SEXP c_cudd_bdd_realignment_enabled(SEXP mgr_ptr);
extern "C" SEXP c_cudd_bdd_realign_enable(SEXP mgr_ptr);
extern "C" SEXP c_cudd_bdd_realign_disable(SEXP mgr_ptr);
extern "C" SEXP c_cudd_background(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_background(SEXP mgr_ptr, SEXP add_ptr);
extern "C" SEXP c_cudd_read_max_cache(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_max_cache_hard(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_max_cache_hard(SEXP mgr_ptr, SEXP mc);
extern "C" SEXP c_cudd_read_slots(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_keys(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_dead(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_min_dead(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_max_reorderings(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_max_reorderings(SEXP mgr_ptr, SEXP mr);
extern "C" SEXP c_cudd_read_reordering_time(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_garbage_collections(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_garbage_collection_time(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_sift_max_var(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_sift_max_var(SEXP mgr_ptr, SEXP smv);
extern "C" SEXP c_cudd_read_sift_max_swap(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_sift_max_swap(SEXP mgr_ptr, SEXP sms);
extern "C" SEXP c_cudd_read_max_growth(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_max_growth(SEXP mgr_ptr, SEXP mg);
extern "C" SEXP c_cudd_read_perm(SEXP mgr_ptr, SEXP i);
extern "C" SEXP c_cudd_read_perm_zdd(SEXP mgr_ptr, SEXP i);
extern "C" SEXP c_cudd_read_inv_perm(SEXP mgr_ptr, SEXP i);
extern "C" SEXP c_cudd_read_inv_perm_zdd(SEXP mgr_ptr, SEXP i);
extern "C" SEXP c_cudd_read_vars(SEXP mgr_ptr, SEXP i);
extern "C" SEXP c_cudd_read_epsilon(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_epsilon(SEXP mgr_ptr, SEXP ep);
extern "C" SEXP c_cudd_read_groupcheck(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_groupcheck(SEXP mgr_ptr, SEXP gc);
extern "C" SEXP c_cudd_garbage_collection_enabled(SEXP mgr_ptr);
extern "C" SEXP c_cudd_enable_garbage_collection(SEXP mgr_ptr);
extern "C" SEXP c_cudd_disable_garbage_collection(SEXP mgr_ptr);
extern "C" SEXP c_cudd_dead_are_counted(SEXP mgr_ptr);
extern "C" SEXP c_cudd_turn_on_count_dead(SEXP mgr_ptr);
extern "C" SEXP c_cudd_turn_off_count_dead(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_recomb(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_recomb(SEXP mgr_ptr, SEXP recomb);
extern "C" SEXP c_cudd_read_symmviolation(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_symmviolation(SEXP mgr_ptr, SEXP symm);
extern "C" SEXP c_cudd_read_arcviolation(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_arcviolation(SEXP mgr_ptr, SEXP arc);
extern "C" SEXP c_cudd_read_population_size(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_population_size(SEXP mgr_ptr, SEXP pop);
extern "C" SEXP c_cudd_read_number_xovers(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_number_xovers(SEXP mgr_ptr, SEXP xovers);
extern "C" SEXP c_cudd_read_order_randomization(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_order_randomization(SEXP mgr_ptr, SEXP factor);
extern "C" SEXP c_cudd_read_memory_in_use(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_peak_node_count(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_node_count_current(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_node_count_zdd(SEXP mgr_ptr);
extern "C" SEXP c_cudd_enable_reordering_reporting(SEXP mgr_ptr);
extern "C" SEXP c_cudd_disable_reordering_reporting(SEXP mgr_ptr);
extern "C" SEXP c_cudd_reordering_reporting(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_error_code(SEXP mgr_ptr);
extern "C" SEXP c_cudd_clear_error_code(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_next_reordering(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_next_reordering(SEXP mgr_ptr, SEXP nr);
extern "C" SEXP c_cudd_read_swap_steps(SEXP mgr_ptr);
extern "C" SEXP c_cudd_read_max_live(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_max_live(SEXP mgr_ptr, SEXP max_live);
extern "C" SEXP c_cudd_read_max_memory(SEXP mgr_ptr);
extern "C" SEXP c_cudd_set_max_memory(SEXP mgr_ptr, SEXP max_mem);
extern "C" SEXP c_cudd_bdd_bind_var(SEXP mgr_ptr, SEXP index);
extern "C" SEXP c_cudd_bdd_unbind_var(SEXP mgr_ptr, SEXP index);
extern "C" SEXP c_cudd_bdd_var_is_bound(SEXP mgr_ptr, SEXP index);

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
