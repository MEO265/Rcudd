#include "rcudd.h"
#include <algorithm>

namespace rcudd {
SEXP bdd_summary(const BDD &root, const std::vector<std::string> &names_by_index,
                 std::vector<std::string> initial_names) {
    std::string expression = bdd_expression(root, names_by_index);
    std::vector<std::string> support_names;
    for (unsigned int index : root.SupportIndices()) {
        support_names.push_back(names_by_index.at(index));
    }
    std::sort(support_names.begin(), support_names.end());
    std::sort(initial_names.begin(), initial_names.end());
    initial_names.erase(std::unique(initial_names.begin(), initial_names.end()),
                        initial_names.end());

    SEXP result = PROTECT(Rf_allocVector(VECSXP, 4));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 4));
    const char *fields[] = {"constant", "variables", "initial_variables", "reduced_expression"};
    for (int i = 0; i < 4; ++i) SET_STRING_ELT(names, i, Rf_mkChar(fields[i]));
    Rf_setAttrib(result, R_NamesSymbol, names);
    SET_VECTOR_ELT(result, 0, Rf_ScalarLogical(root.IsOne() ? TRUE : root.IsZero() ? FALSE : NA_LOGICAL));
    SEXP variables = PROTECT(Rf_allocVector(STRSXP, support_names.size()));
    for (std::size_t i = 0; i < support_names.size(); ++i)
        SET_STRING_ELT(variables, i, Rf_mkCharCE(support_names[i].c_str(), CE_UTF8));
    SET_VECTOR_ELT(result, 1, variables);
    SEXP initial = PROTECT(Rf_allocVector(STRSXP, initial_names.size()));
    for (std::size_t i = 0; i < initial_names.size(); ++i)
        SET_STRING_ELT(initial, i, Rf_mkCharCE(initial_names[i].c_str(), CE_UTF8));
    SET_VECTOR_ELT(result, 2, initial);
    SET_VECTOR_ELT(result, 3, Rf_ScalarString(Rf_mkCharCE(expression.c_str(), CE_UTF8)));
    UNPROTECT(4);
    return result;
}
}
