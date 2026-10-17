#include "bdd_prepare.h"
#include <algorithm>
#include <climits>
#include <cmath>
#include <cctype>
#include <stdexcept>
#include <unordered_set>

namespace rcudd {
namespace {
SEXP field(SEXP config, const char *name) {
    if (TYPEOF(config) != VECSXP) throw std::invalid_argument("RWBO config must be a named list.");
    SEXP labels = Rf_getAttrib(config, R_NamesSymbol);
    if (TYPEOF(labels) != STRSXP || XLENGTH(labels) != XLENGTH(config))
        throw std::invalid_argument("RWBO config must be a named list.");
    for (R_xlen_t i = 0; i < XLENGTH(labels); ++i)
        if (STRING_ELT(labels, i) != NA_STRING && std::string(CHAR(STRING_ELT(labels, i))) == name)
            return VECTOR_ELT(config, i);
    throw std::invalid_argument(std::string("Missing RWBO config field: ") + name);
}
unsigned int bound(SEXP config, const char *name, unsigned int minimum) {
    SEXP value = field(config, name);
    if ((TYPEOF(value) != INTSXP && TYPEOF(value) != REALSXP) || XLENGTH(value) != 1)
        throw std::invalid_argument(std::string("Invalid RWBO config bound: ") + name);
    const double number = Rf_asReal(value);
    if (!std::isfinite(number) || number < minimum || number > INT_MAX - 1 || number != std::floor(number))
        throw std::invalid_argument(std::string("Invalid RWBO config bound: ") + name);
    return static_cast<unsigned int>(number);
}
SEXP metadata(const char *status, const std::string &message, SEXP problem,
              int operators, bool optimal) {
    SEXP out = PROTECT(Rf_allocVector(VECSXP, 5));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 5));
    const char *labels[] = {"status", "message", "problem", "operator_count", "optimal"};
    for (int i = 0; i < 5; ++i) SET_STRING_ELT(names, i, Rf_mkChar(labels[i]));
    Rf_setAttrib(out, R_NamesSymbol, names);
    SET_VECTOR_ELT(out, 0, Rf_mkString(status));
    SET_VECTOR_ELT(out, 1, Rf_mkString(message.c_str()));
    SET_VECTOR_ELT(out, 2, problem);
    SET_VECTOR_ELT(out, 3, Rf_ScalarInteger(operators));
    SET_VECTOR_ELT(out, 4, Rf_ScalarLogical(optimal));
    UNPROTECT(2);
    return out;
}
bool repeats_variable(const std::string &expression) {
    std::unordered_set<std::string> seen;
    std::size_t position = 0;
    while (position < expression.size()) {
        const auto separator = [](unsigned char ch) {
            return std::isspace(ch) || ch == '(' || ch == ')' || ch == '&' || ch == '|' || ch == '^' || ch == '!';
        };
        if (separator(expression[position])) { ++position; continue; }
        const std::size_t begin = position;
        while (position < expression.size() && !separator(expression[position])) ++position;
        std::string name = expression.substr(begin, position - begin), lower = name;
        std::transform(lower.begin(), lower.end(), lower.begin(), [](unsigned char ch) { return std::tolower(ch); });
        if (lower != "true" && lower != "false" && !seen.insert(name).second) return true;
    }
    return false;
}
}

RwboPreparation::RwboPreparation(SEXP config) : start(std::chrono::steady_clock::now()) {
    if (config == R_NilValue) return;
    enabled = true;
    options.max_variables = bound(config, "max_variables", 0);
    options.max_operators = bound(config, "max_operators", 0);
    options.max_clauses = bound(config, "max_clauses", 1);
    time_limit = bound(config, "time_limit", 1);
    SEXP flag = field(config, "available"), path = field(config, "directory");
    if (TYPEOF(flag) != LGLSXP || XLENGTH(flag) != 1 || LOGICAL(flag)[0] == NA_LOGICAL)
        throw std::invalid_argument("Invalid RWBO availability flag.");
    available = LOGICAL(flag)[0];
    if (TYPEOF(path) != STRSXP || XLENGTH(path) != 1 || STRING_ELT(path, 0) == NA_STRING)
        throw std::invalid_argument("Invalid RWBO directory.");
    directory = Rf_translateChar(STRING_ELT(path, 0));
    if (directory.empty()) throw std::invalid_argument("Invalid RWBO directory.");
}

void RwboPreparation::attach(SEXP summary, const BDD &root, const BDD &care,
                            const std::vector<std::string> &names, const std::string &original,
                            const std::string &care_expression) {
    if (!enabled) return;
    const std::string baseline(Rf_translateCharUTF8(STRING_ELT(VECTOR_ELT(summary, 3), 0)));
    const int operators = static_cast<int>(std::count_if(baseline.begin(), baseline.end(), [](char ch) {
        return ch == '!' || ch == '&' || ch == '|' || ch == '^';
    }));
    const char *status = "prepared";
    std::string message;
    bool optimal = false;
    SEXP problem = PROTECT(Rf_allocVector(VECSXP, 1)); // roots optional metadata
    bool has_problem = false;
    const auto support = root.SupportIndices();
    const int lower_bound = support.empty() ? 0 : std::max(1, static_cast<int>(support.size()) - 1);
    if (operators == 0 || (care.IsOne() && operators <= lower_bound)) {
        status = "minimal"; optimal = true;
    } else if (care.IsOne() && !repeats_variable(baseline)) {
        status = "not_candidate";
    } else if (!available) {
        status = "package_unavailable"; message = "Rwbo is not installed.";
    } else if (std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count() >= time_limit) {
        status = "time_limit"; message = "The shared preparation budget is exhausted.";
    } else {
        std::unordered_set<unsigned int> variables(support.begin(), support.end());
        for (unsigned int index : care.SupportIndices()) variables.insert(index);
        if (variables.size() > std::min(options.max_variables, 20u)) {
            status = "variable_limit"; message = "Expression and context exceed the variable limit.";
        } else try {
            WcnfOptions bounded = options;
            bounded.max_operators = std::min(options.max_operators, static_cast<unsigned int>(operators - 1));
            const auto native = write_bdd_wcnf(root, care, names,
                directory + "/formula-" + std::to_string(++counter) + ".wcnf", bounded);
            SEXP expr = PROTECT(Rf_ScalarString(Rf_mkCharCE(original.c_str(), CE_UTF8)));
            SEXP context = PROTECT(Rf_ScalarString(Rf_mkCharCE(care_expression.c_str(), CE_UTF8)));
            SEXP prepared = PROTECT(as_r_problem(native, expr, context));
            SET_VECTOR_ELT(problem, 0, prepared);
            UNPROTECT(3);
            has_problem = true;
        } catch (const std::exception &error) {
            status = "resource_limit"; message = error.what();
        }
    }
    SEXP value = PROTECT(metadata(status, message, has_problem ? VECTOR_ELT(problem, 0) : R_NilValue, operators, optimal));
    Rf_setAttrib(summary, Rf_install("rwbo"), value);
    UNPROTECT(2);
}
}
