#ifndef RCUDD
#define RCUDD

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#include <Rinternals.h>

extern "C" SEXP bdd_remaining_literals(SEXP expr_in, SEXP config);
extern "C" SEXP bdd_wcnf_expression(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern "C" SEXP interpret_rwbo_expression(SEXP, SEXP);
extern "C" SEXP bdd_restrict_chain(SEXP exprs_in, SEXP additional_constraints_in, SEXP config);

#ifdef __cplusplus
#include <string>
#include <unordered_map>
#include <vector>
#include "cuddObj.hh"

namespace rcudd {
struct WcnfProblem;
SEXP as_r_problem(const WcnfProblem &, SEXP expression, SEXP care_expression);
BDD parse_bdd_expression(const std::string &expr,
                         Cudd &mgr,
                         std::unordered_map<std::string, unsigned int> &var_idx,
                         std::vector<std::string> &idx_to_name,
                         std::vector<std::string> *encountered_vars = nullptr);
std::string bdd_expression(const BDD &bdd,
                           const std::vector<std::string> &idx_to_name);
SEXP bdd_summary(const BDD &root, const std::vector<std::string> &names_by_index,
                 std::vector<std::string> initial_names);
}
#endif

#endif
