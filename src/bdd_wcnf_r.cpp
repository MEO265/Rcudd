#include "rcudd.h"
#include "../inst/include/rcudd/bdd_wcnf.hpp"
#include <cmath>
#include <climits>
#include <cstdint>
#include <stdexcept>

namespace {
std::string scalar_string(SEXP value, const char *name) {
    if (TYPEOF(value) != STRSXP || XLENGTH(value) != 1 || STRING_ELT(value, 0) == NA_STRING)
        throw std::invalid_argument(std::string("'") + name + "' must be a single non-missing string.");
    std::string result(Rf_translateCharUTF8(STRING_ELT(value, 0)));
    if (result.empty()) throw std::invalid_argument(std::string("'") + name + "' must not be empty.");
    return result;
}
unsigned int scalar_bound(SEXP value, const char *name, unsigned int minimum) {
    if ((TYPEOF(value) != INTSXP && TYPEOF(value) != REALSXP) || XLENGTH(value) != 1)
        throw std::invalid_argument(std::string("'") + name + "' must be a whole number.");
    const double number = Rf_asReal(value);
    if (!std::isfinite(number) || number < minimum || number != std::floor(number) || number > INT_MAX - 1)
        throw std::invalid_argument(std::string("'") + name + "' must be a whole number within integer limits.");
    return static_cast<unsigned int>(number);
}
SEXP strings(const std::vector<std::string> &values) {
    SEXP out = PROTECT(Rf_allocVector(STRSXP, values.size()));
    for (std::size_t i = 0; i < values.size(); ++i)
        SET_STRING_ELT(out, i, Rf_mkCharCE(values[i].c_str(), CE_UTF8));
    UNPROTECT(1);
    return out;
}
template <typename Container> SEXP integers(const Container &values) {
    SEXP out = Rf_allocVector(INTSXP, values.size());
    std::size_t i = 0;
    for (auto value : values) INTEGER(out)[i++] = static_cast<int>(value);
    return out;
}
void name_list(SEXP list, const std::vector<std::string> &names) {
    SEXP out = PROTECT(strings(names));
    Rf_setAttrib(list, R_NamesSymbol, out);
    UNPROTECT(1);
}
SEXP as_r_problem(const rcudd::WcnfProblem &p, SEXP expression) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 16));
    name_list(result, {"expression", "bdd_expression", "variables", "inputs", "target",
                       "max_operators", "shortcut", "objective", "lower_bound", "nvars",
                       "nclauses", "gates", "wcnf_file", "variable_indices", "encoding", "truth_table_rows"});
    SET_VECTOR_ELT(result, 0, expression);
    SET_VECTOR_ELT(result, 1, R_NilValue); // avoid potentially exponential factored rendering
    SET_VECTOR_ELT(result, 2, strings(p.variables));
    SEXP inputs = PROTECT(Rf_allocVector(VECSXP, p.variables.size()));
    name_list(inputs, p.variables);
    for (std::size_t column = 0; column < p.variables.size(); ++column) {
        SEXP bits = PROTECT(Rf_allocVector(LGLSXP, p.target.size()));
        for (std::size_t row = 0; row < p.target.size(); ++row) LOGICAL(bits)[row] = p.input(row, column);
        SET_VECTOR_ELT(inputs, column, bits);
        UNPROTECT(1);
    }
    SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -static_cast<int>(p.target.size());
    Rf_setAttrib(inputs, R_RowNamesSymbol, row_names);
    SEXP data_frame = PROTECT(Rf_mkString("data.frame"));
    Rf_classgets(inputs, data_frame);
    SET_VECTOR_ELT(result, 3, inputs);
    SEXP target = PROTECT(Rf_allocVector(LGLSXP, p.target.size()));
    for (std::size_t row = 0; row < p.target.size(); ++row) LOGICAL(target)[row] = p.target[row];
    SET_VECTOR_ELT(result, 4, target);
    SET_VECTOR_ELT(result, 5, Rf_ScalarInteger(p.max_operators));
    if (p.has_shortcut) SET_VECTOR_ELT(result, 6, strings({p.shortcut}));
    SET_VECTOR_ELT(result, 7, Rf_mkString("number of &, |, ^ and ! operators in a formula"));
    SET_VECTOR_ELT(result, 8, Rf_ScalarInteger(p.lower_bound));
    SET_VECTOR_ELT(result, 9, Rf_ScalarInteger(p.nvars));
    SET_VECTOR_ELT(result, 10, Rf_ScalarInteger(static_cast<int>(p.nclauses)));
    SEXP gates = PROTECT(Rf_allocVector(VECSXP, p.gates.size()));
    for (std::size_t i = 0; i < p.gates.size(); ++i) {
        const rcudd::WcnfGate &g = p.gates[i];
        SEXP gate = PROTECT(Rf_allocVector(VECSXP, 6));
        name_list(gate, {"kind", "left", "right", "left_value", "right_value", "out"});
        SEXP kind = PROTECT(integers(g.kind));
        name_list(kind, {"unused", "and", "or", "xor", "not"});
        SET_VECTOR_ELT(gate, 0, kind);
        SET_VECTOR_ELT(gate, 1, integers(g.left));
        SET_VECTOR_ELT(gate, 2, integers(g.right));
        SET_VECTOR_ELT(gate, 3, integers(g.left_value));
        SET_VECTOR_ELT(gate, 4, integers(g.right_value));
        SET_VECTOR_ELT(gate, 5, integers(g.out));
        SET_VECTOR_ELT(gates, i, gate);
        UNPROTECT(2);
    }
    SET_VECTOR_ELT(result, 11, gates);
    SET_VECTOR_ELT(result, 12, Rf_mkString(p.wcnf_file.c_str()));
    SET_VECTOR_ELT(result, 13, integers(p.variable_indices));
    SET_VECTOR_ELT(result, 14, Rf_mkString("native CUDD formula-tree WCNF"));
    SET_VECTOR_ELT(result, 15, Rf_ScalarInteger(static_cast<int>(p.target.size())));
    SEXP klass = PROTECT(Rf_mkString("rwbo_formula_problem"));
    Rf_classgets(result, klass);
    UNPROTECT(7);
    return result;
}
} // namespace

extern "C" SEXP bdd_wcnf_expression(SEXP expr_in, SEXP file_in, SEXP operators_in,
                                     SEXP variables_in, SEXP clauses_in) {
    try {
        const std::string expression = scalar_string(expr_in, "expr");
        scalar_string(file_in, "wcnf_file");
        // std::ofstream uses native filesystem encoding on Windows.
        const std::string path(Rf_translateChar(STRING_ELT(file_in, 0)));
        rcudd::WcnfOptions options;
        options.max_operators = scalar_bound(operators_in, "max_operators", 0);
        options.max_variables = scalar_bound(variables_in, "max_variables", 0);
        options.max_clauses = scalar_bound(clauses_in, "max_clauses", 1);
        Cudd manager;
        std::unordered_map<std::string, unsigned int> indices;
        std::vector<std::string> names;
        const BDD root = rcudd::parse_bdd_expression(expression, manager, indices, names);
        const rcudd::WcnfProblem problem = rcudd::write_bdd_wcnf(root, names, path, options);
        return as_r_problem(problem, expr_in);
    } catch (const std::exception &error) {
        Rf_error("Failed to export BDD WCNF: %s", error.what());
    }
    return R_NilValue;
}
