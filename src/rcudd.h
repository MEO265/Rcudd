#ifndef RCUDD
#define RCUDD

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#include <Rinternals.h>

extern "C" SEXP bdd_remaining_literals(SEXP expr_in);
extern "C" SEXP bdd_restrict_chain(SEXP exprs_in, SEXP additional_constraints_in);

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
