#include "rcudd.h"
#include "bdd_truth.h"
#include "../inst/include/rcudd/bdd_wcnf.hpp"
#include <cmath>
#include <climits>
#include <cstdint>
#include <numeric>
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
} // namespace
namespace rcudd {
SEXP as_r_problem(const WcnfProblem &p, SEXP expression, SEXP care_expression) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 18));
    name_list(result, {"expression", "bdd_expression", "variables", "inputs", "target",
                       "max_operators", "shortcut", "objective", "lower_bound", "nvars",
                       "nclauses", "gates", "wcnf_file", "variable_indices", "encoding", "truth_table_rows",
                       "care", "care_expression"});
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
    SEXP care = PROTECT(Rf_allocVector(LGLSXP, p.care.size()));
    for (std::size_t row = 0; row < p.care.size(); ++row) LOGICAL(care)[row] = p.care[row];
    SET_VECTOR_ELT(result, 16, care);
    SET_VECTOR_ELT(result, 17, care_expression);
    SEXP klass = PROTECT(Rf_mkString("rwbo_formula_problem"));
    Rf_classgets(result, klass);
    UNPROTECT(8);
    return result;
}
} // namespace

extern "C" SEXP bdd_wcnf_expression(SEXP expr_in, SEXP file_in, SEXP operators_in,
                                     SEXP variables_in, SEXP clauses_in, SEXP care_in) {
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
        const BDD care = rcudd::parse_bdd_expression(scalar_string(care_in, "care"), manager, indices, names);
        const rcudd::WcnfProblem problem = rcudd::write_bdd_wcnf(root, care, names, path, options);
        return rcudd::as_r_problem(problem, expr_in, care_in);
    } catch (const std::exception &error) {
        Rf_error("Failed to export BDD WCNF: %s", error.what());
    }
    return R_NilValue;
}
namespace {
SEXP field(SEXP list, const char *key) {
    if (TYPEOF(list) != VECSXP) throw std::invalid_argument("Problem/gate metadata must be a named list.");
    SEXP names = Rf_getAttrib(list, R_NamesSymbol);
    if (TYPEOF(names) != STRSXP || XLENGTH(names) != XLENGTH(list))
        throw std::invalid_argument("Problem/gate metadata must have names.");
    SEXP result = R_NilValue;
    bool found = false;
    for (R_xlen_t i = 0; i < XLENGTH(list); ++i) {
        if (STRING_ELT(names, i) == NA_STRING) throw std::invalid_argument("Metadata field names cannot be missing.");
        if (std::string(CHAR(STRING_ELT(names, i))) == key) {
            if (found) throw std::invalid_argument("Duplicate metadata field.");
            found = true; result = VECTOR_ELT(list, i);
        }
    }
    if (!found) throw std::invalid_argument(std::string("Missing metadata field: ") + key);
    return result;
}
std::vector<int> sat_ids(SEXP value, std::size_t expected, unsigned int nvars) {
    if (TYPEOF(value) != INTSXP || XLENGTH(value) != static_cast<R_xlen_t>(expected) || expected > nvars)
        throw std::invalid_argument("Invalid gate metadata dimensions/type.");
    std::vector<int> result(expected);
    for (std::size_t i = 0; i < expected; ++i) {
        const int id = INTEGER(value)[i];
        if (id == NA_INTEGER || id < 1 || static_cast<unsigned int>(id) > nvars)
            throw std::invalid_argument("Invalid gate SAT-id metadata.");
        result[i] = id;
    }
    return result;
}
rcudd::WcnfProblem from_r_problem(SEXP input) {
    rcudd::WcnfProblem p;
    p.max_operators = scalar_bound(field(input, "max_operators"), "max_operators", 0);
    p.lower_bound = scalar_bound(field(input, "lower_bound"), "lower_bound", 0);
    p.nvars = static_cast<int>(scalar_bound(field(input, "nvars"), "nvars", 1));
    p.nclauses = scalar_bound(field(input, "nclauses"), "nclauses", 1);
    SEXP names = field(input, "variables");
    if (TYPEOF(names) != STRSXP || XLENGTH(names) > 20)
        throw std::invalid_argument("Invalid essential variable metadata.");
    const std::size_t n = static_cast<std::size_t>(XLENGTH(names));
    for (std::size_t i = 0; i < n; ++i) {
        if (STRING_ELT(names, i) == NA_STRING) throw std::invalid_argument("Essential variable names cannot be missing.");
        p.variables.emplace_back(Rf_translateCharUTF8(STRING_ELT(names, i)));
    }
    SEXP indices = field(input, "variable_indices");
    if (TYPEOF(indices) != INTSXP || XLENGTH(indices) != static_cast<R_xlen_t>(n))
        throw std::invalid_argument("Invalid variable-index metadata.");
    for (std::size_t i = 0; i < n; ++i) {
        if (INTEGER(indices)[i] == NA_INTEGER || INTEGER(indices)[i] < 0)
            throw std::invalid_argument("Invalid variable-index metadata.");
        p.variable_indices.push_back(static_cast<unsigned int>(INTEGER(indices)[i]));
    }
    const std::size_t rows = std::size_t(1) << n;
    SEXP target = field(input, "target");
    if (TYPEOF(target) != LGLSXP || XLENGTH(target) != static_cast<R_xlen_t>(rows))
        throw std::invalid_argument("Invalid target metadata dimensions/type.");
    p.target.reserve(rows);
    for (std::size_t i = 0; i < rows; ++i) {
        if (LOGICAL(target)[i] == NA_LOGICAL) throw std::invalid_argument("Target metadata cannot contain NA.");
        p.target.push_back(LOGICAL(target)[i] != 0);
    }
    SEXP care = field(input, "care");
    if (TYPEOF(care) != LGLSXP || (XLENGTH(care) != 0 && XLENGTH(care) != static_cast<R_xlen_t>(rows)))
        throw std::invalid_argument("Invalid care metadata dimensions/type.");
    for (R_xlen_t i = 0; i < XLENGTH(care); ++i) {
        if (LOGICAL(care)[i] == NA_LOGICAL) throw std::invalid_argument("Care metadata cannot contain NA.");
        p.care.push_back(LOGICAL(care)[i] != 0);
    }
    SEXP file = field(input, "wcnf_file");
    scalar_string(file, "wcnf_file");
    p.wcnf_file = Rf_translateChar(STRING_ELT(file, 0));
    SEXP shortcut = field(input, "shortcut");
    if (shortcut != R_NilValue) { p.has_shortcut = true; p.shortcut = scalar_string(shortcut, "shortcut"); }
    SEXP gates = field(input, "gates");
    if (TYPEOF(gates) != VECSXP || XLENGTH(gates) > p.nvars ||
        (XLENGTH(gates) && XLENGTH(gates) != p.max_operators))
        throw std::invalid_argument("Invalid gate-count metadata.");
    if (XLENGTH(gates)) {
        const std::uint64_t k = static_cast<std::uint64_t>(XLENGTH(gates));
        const std::uint64_t expected_ids = k * (5 + 2 * (n + 2) + 3 * rows) + k * (k - 1);
        if (expected_ids != static_cast<std::uint64_t>(p.nvars))
            throw std::invalid_argument("SAT-variable count does not match gate metadata.");
    }
    p.gates.resize(static_cast<std::size_t>(XLENGTH(gates)));
    for (std::size_t i = 0; i < p.gates.size(); ++i) {
        SEXP gate = VECTOR_ELT(gates, i);
        rcudd::WcnfGate &out = p.gates[i];
        SEXP kinds = field(gate, "kind");
        const auto ids = sat_ids(kinds, 5, p.nvars);
        const char *kind_names[] = {"unused", "and", "or", "xor", "not"};
        SEXP labels = Rf_getAttrib(kinds, R_NamesSymbol);
        if (TYPEOF(labels) != STRSXP || XLENGTH(labels) != 5)
            throw std::invalid_argument("Invalid operator-name metadata.");
        for (int j = 0; j < 5; ++j) {
            if (STRING_ELT(labels, j) == NA_STRING || std::string(CHAR(STRING_ELT(labels, j))) != kind_names[j])
                throw std::invalid_argument("Invalid operator-name metadata.");
            out.kind[j] = ids[j];
        }
        out.left = sat_ids(field(gate, "left"), n + 2 + i, p.nvars);
        out.right = sat_ids(field(gate, "right"), n + 2 + i, p.nvars);
        out.left_value = sat_ids(field(gate, "left_value"), rows, p.nvars);
        out.right_value = sat_ids(field(gate, "right_value"), rows, p.nvars);
        out.out = sat_ids(field(gate, "out"), rows, p.nvars);
    }
    return p;
}

// Bind the truth metadata used for a minimum proof to the advertised original
// expression and context. Merely checking the final candidate would allow a
// broader forged care vector to overstate contextual optimality.
void verify_expression_metadata(SEXP input, const rcudd::WcnfProblem &problem) {
    Cudd manager;
    std::unordered_map<std::string, unsigned int> indices;
    std::vector<std::string> names = problem.variables;
    for (std::size_t i = 0; i < names.size(); ++i) {
        indices.emplace(names[i], static_cast<unsigned int>(i));
        manager.bddVar(static_cast<int>(i));
    }
    const BDD original = rcudd::parse_bdd_expression(
        scalar_string(field(input, "expression"), "expression"), manager, indices, names);
    const BDD care = rcudd::parse_bdd_expression(
        scalar_string(field(input, "care_expression"), "care_expression"), manager, indices, names);
    const BDD masked = original & care;
    for (unsigned int index : care.SupportIndices())
        if (index >= problem.variables.size())
            throw std::invalid_argument("Care expression has support outside the declared variable pool.");
    for (unsigned int index : masked.SupportIndices())
        if (index >= problem.variables.size())
            throw std::invalid_argument("Original expression under care has support outside the declared variable pool.");
    std::vector<unsigned int> pool(problem.variables.size());
    std::iota(pool.begin(), pool.end(), 0u);
    for (std::size_t row = 0; row < problem.target.size(); ++row) {
        const bool expected_care = problem.care.empty() || problem.care[row];
        if (rcudd::bdd_value(care, row, pool) != expected_care)
            throw std::invalid_argument("Care metadata failed the BDD equivalence check.");
        if (expected_care && rcudd::bdd_value(masked, row, pool) != problem.target[row])
            throw std::invalid_argument("Target metadata failed the BDD equivalence check.");
    }
}
} // namespace

extern "C" SEXP interpret_rwbo_expression(SEXP problem_in, SEXP output_in) {
    try {
        const rcudd::WcnfProblem problem = from_r_problem(problem_in);
        if (TYPEOF(output_in) != STRSXP || XLENGTH(output_in) != 1 || STRING_ELT(output_in, 0) == NA_STRING)
            throw std::invalid_argument("Solver output must be a non-missing character scalar.");
        const std::string output(Rf_translateCharUTF8(STRING_ELT(output_in, 0)));
        const rcudd::WcnfSolution solution = rcudd::interpret_rwbo_output(problem, output);
        verify_expression_metadata(problem_in, problem);
        SEXP result = PROTECT(Rf_allocVector(VECSXP, 6));
        name_list(result, {"expression", "operator_count", "optimal", "verified", "status", "wcnf_file"});
        if (solution.has_expression) SET_VECTOR_ELT(result, 0, strings({solution.expression}));
        else SET_VECTOR_ELT(result, 0, Rf_ScalarString(NA_STRING));
        SET_VECTOR_ELT(result, 1, Rf_ScalarInteger(solution.operator_count < 0 ? NA_INTEGER : solution.operator_count));
        SET_VECTOR_ELT(result, 2, Rf_ScalarLogical(solution.optimal));
        SET_VECTOR_ELT(result, 3, Rf_ScalarLogical(solution.verified));
        SET_VECTOR_ELT(result, 4, strings({solution.status}));
        SET_VECTOR_ELT(result, 5, field(problem_in, "wcnf_file"));
        UNPROTECT(1);
        return result;
    } catch (const std::exception &error) {
        Rf_error("Failed to interpret Rwbo output: %s", error.what());
    }
    return R_NilValue;
}
