#include "rcudd.h"
#include <algorithm>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>
#include <R.h>
#include <Rinternals.h>
#include "cuddObj.hh"

extern "C" SEXP bdd_restrict_chain(SEXP exprs_in, SEXP additional_constraints_in) {
    if (!Rf_isString(exprs_in)) {
        Rf_error("'exprs' must be a character vector.");
    }

    if (!Rf_isString(additional_constraints_in)) {
        Rf_error("'additional_constraints' must be a character vector.");
    }

    R_xlen_t n = Rf_xlength(exprs_in);
    if (n == 0) {
        Rf_error("At least one expression must be provided.");
    }

    R_xlen_t n_constraints = Rf_xlength(additional_constraints_in);

    try {
        std::unordered_map<std::string, unsigned int> var_idx;
        std::vector<std::string> idx_to_name;
        Cudd mgr;

        std::vector<BDD> bdds;
        bdds.reserve(static_cast<std::size_t>(n));

        std::vector<std::vector<std::string>> initial_names;
        initial_names.reserve(static_cast<std::size_t>(n));

        for (R_xlen_t i = 0; i < n; ++i) {
            const char *expr_c = CHAR(STRING_ELT(exprs_in, i));
            std::string expr(expr_c ? expr_c : "");
            std::vector<std::string> encountered;
            bdds.push_back(
                rcudd::parse_bdd_expression(expr, mgr, var_idx, idx_to_name, &encountered));

            std::sort(encountered.begin(), encountered.end());
            encountered.erase(std::unique(encountered.begin(), encountered.end()),
                               encountered.end());
            initial_names.push_back(std::move(encountered));
        }

        BDD shared_constraints = mgr.bddOne();
        for (R_xlen_t i = 0; i < n_constraints; ++i) {
            const char *expr_c = CHAR(STRING_ELT(additional_constraints_in, i));
            std::string expr(expr_c ? expr_c : "");
            shared_constraints = shared_constraints &
                rcudd::parse_bdd_expression(expr, mgr, var_idx, idx_to_name);
        }

        SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
        BDD previous_constraints = mgr.bddOne();

        for (R_xlen_t i = 0; i < n; ++i) {
            const BDD &original = bdds[static_cast<std::size_t>(i)];
            BDD context = previous_constraints & shared_constraints;
            BDD result = original.Restrict(context);

            SEXP entry = PROTECT(rcudd::bdd_summary(result, idx_to_name, initial_names[static_cast<std::size_t>(i)]));
            SET_VECTOR_ELT(out, i, entry);
            UNPROTECT(1); // entry
            previous_constraints = previous_constraints & !bdds[static_cast<std::size_t>(i)];
        }

        UNPROTECT(1);
        return out;
    } catch (const std::exception &ex) {
        Rf_error("Failed while building or restricting the ROBDD: %s", ex.what());
    }

    return R_NilValue;
}
