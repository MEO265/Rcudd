#include "rcudd.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"c_cudd_new", (DL_FUNC) &c_cudd_new, 0},
    {"c_cudd_read_size", (DL_FUNC) &c_cudd_read_size, 1},
    {"c_cudd_read_cache_slots", (DL_FUNC) &c_cudd_read_cache_slots, 1},
    {"c_cudd_read_cache_used_slots", (DL_FUNC) &c_cudd_read_cache_used_slots, 1},
    {"c_cudd_read_cache_lookups", (DL_FUNC) &c_cudd_read_cache_lookups, 1},
    {"c_cudd_read_cache_hits", (DL_FUNC) &c_cudd_read_cache_hits, 1},
    {"c_cudd_read_min_hit", (DL_FUNC) &c_cudd_read_min_hit, 1},
    {"c_cudd_read_loose_up_to", (DL_FUNC) &c_cudd_read_loose_up_to, 1},
    {"c_cudd_read_node_count", (DL_FUNC) &c_cudd_read_node_count, 1},
    {"c_cudd_read_zdd_size", (DL_FUNC) &c_cudd_read_zdd_size, 1},
    {"c_cudd_read_reorderings", (DL_FUNC) &c_cudd_read_reorderings, 1},
    {"c_cudd_bdd_one", (DL_FUNC) &c_cudd_bdd_one, 1},
    {"c_cudd_bdd_zero", (DL_FUNC) &c_cudd_bdd_zero, 1},
    {"c_cudd_bdd_var", (DL_FUNC) &c_cudd_bdd_var, 2},
    {"c_cudd_add_one", (DL_FUNC) &c_cudd_add_one, 1},
    {"c_cudd_add_zero", (DL_FUNC) &c_cudd_add_zero, 1},
    {"c_cudd_add_var", (DL_FUNC) &c_cudd_add_var, 2},
    {"c_cudd_zdd_one", (DL_FUNC) &c_cudd_zdd_one, 2},
    {"c_cudd_zdd_zero", (DL_FUNC) &c_cudd_zdd_zero, 1},
    {"c_cudd_zdd_var", (DL_FUNC) &c_cudd_zdd_var, 2},
    {"c_cudd_bdd_not", (DL_FUNC) &c_cudd_bdd_not, 1},
    {"c_cudd_bdd_and", (DL_FUNC) &c_cudd_bdd_and, 2},
    {"c_cudd_bdd_or", (DL_FUNC) &c_cudd_bdd_or, 2},
    {"c_cudd_bdd_xor", (DL_FUNC) &c_cudd_bdd_xor, 2},
    {"c_cudd_add_times", (DL_FUNC) &c_cudd_add_times, 2},
    {"c_cudd_add_plus", (DL_FUNC) &c_cudd_add_plus, 2},
    {"c_cudd_zdd_intersect", (DL_FUNC) &c_cudd_zdd_intersect, 2},
    {"c_cudd_zdd_union", (DL_FUNC) &c_cudd_zdd_union, 2},
    {"c_cudd_bdd_to_add", (DL_FUNC) &c_cudd_bdd_to_add, 1},
    {"c_cudd_add_to_bdd", (DL_FUNC) &c_cudd_add_to_bdd, 1},
    {"c_cudd_bdd_to_zdd", (DL_FUNC) &c_cudd_bdd_to_zdd, 1},
    {"c_cudd_zdd_to_bdd", (DL_FUNC) &c_cudd_zdd_to_bdd, 1},
    {"c_cudd_bdd_epd_print_minterm", (DL_FUNC) &c_cudd_bdd_epd_print_minterm, 2},
    {"c_cudd_add_epd_print_minterm", (DL_FUNC) &c_cudd_add_epd_print_minterm, 2},
    {"c_cudd_bdd_print_minterm", (DL_FUNC) &c_cudd_bdd_print_minterm, 1},
    {"c_cudd_add_print_minterm", (DL_FUNC) &c_cudd_add_print_minterm, 1},
    {"c_cudd_zdd_print_minterm", (DL_FUNC) &c_cudd_zdd_print_minterm, 1},
    {"c_bdd_remaining_literals", (DL_FUNC) &bdd_remaining_literals, 1},
    {"c_bdd_restrict_chain", (DL_FUNC) &bdd_restrict_chain, 2},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_Rcudd(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, FALSE);
}
