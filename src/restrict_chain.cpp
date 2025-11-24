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

        std::vector<BDD> constraint_bdds;
        constraint_bdds.reserve(static_cast<std::size_t>(n_constraints));
        for (R_xlen_t i = 0; i < n_constraints; ++i) {
            const char *expr_c = CHAR(STRING_ELT(additional_constraints_in, i));
            std::string expr(expr_c ? expr_c : "");
            constraint_bdds.push_back(
                rcudd::parse_bdd_expression(expr, mgr, var_idx, idx_to_name));
        }

        auto bdd_summary = [&](const BDD &bdd, const std::vector<std::string> &initial_vars) {
            SEXP entry = PROTECT(Rf_allocVector(VECSXP, 3));
            SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
            SET_STRING_ELT(names, 0, Rf_mkChar("constant"));
            SET_STRING_ELT(names, 1, Rf_mkChar("variables"));
            SET_STRING_ELT(names, 2, Rf_mkChar("initial_variables"));
            Rf_setAttrib(entry, R_NamesSymbol, names);

            SEXP constant = PROTECT(Rf_allocVector(LGLSXP, 1));
            int const_val = NA_LOGICAL;
            if (bdd.IsOne()) {
                const_val = 1;
            } else if (bdd.IsZero()) {
                const_val = 0;
            }
            LOGICAL(constant)[0] = const_val;
            SET_VECTOR_ELT(entry, 0, constant);

            auto support = bdd.SupportIndices();
            std::vector<std::string> names_out;
            names_out.reserve(support.size());
            for (unsigned int idx : support) {
                if (idx < idx_to_name.size()) {
                    names_out.push_back(idx_to_name[idx]);
                }
            }
            std::sort(names_out.begin(), names_out.end());

            SEXP vars = PROTECT(Rf_allocVector(STRSXP, names_out.size()));
            for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(names_out.size()); ++i) {
                SET_STRING_ELT(vars, i, Rf_mkChar(names_out[i].c_str()));
            }
            SET_VECTOR_ELT(entry, 1, vars);

            SEXP initial = PROTECT(Rf_allocVector(STRSXP, initial_vars.size()));
            for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(initial_vars.size()); ++i) {
                SET_STRING_ELT(initial, i, Rf_mkChar(initial_vars[i].c_str()));
            }
            SET_VECTOR_ELT(entry, 2, initial);

            UNPROTECT(4);
            return entry;
        };

        SEXP out = PROTECT(Rf_allocVector(VECSXP, n));

        for (R_xlen_t i = 0; i < n; ++i) {
            BDD result = bdds[static_cast<std::size_t>(i)];
            if (i > 0) {
                // Apply negated prior BDDs so each later expression excludes
                // solutions that were already covered earlier in the chain.
                for (std::size_t j = static_cast<std::size_t>(i); j-- > 0;) {
                    result = result.Restrict(!bdds[j]);
                }
            }

            for (const auto &constraint : constraint_bdds) {
                result = result.Restrict(constraint);
            }

            SEXP entry = bdd_summary(result, initial_names[static_cast<std::size_t>(i)]);
            SET_VECTOR_ELT(out, i, entry);
            UNPROTECT(1); // entry
        }

        UNPROTECT(1);
        return out;
    } catch (const std::exception &ex) {
        Rf_error("Failed while building or restricting the ROBDD: %s", ex.what());
    }

    return R_NilValue;
}
