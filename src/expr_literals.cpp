#include "rcudd.h"
#include <algorithm>
#include <cctype>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>
#include <R.h>
#include <Rinternals.h>
#include "cuddObj.hh"

namespace rcudd {

// Recursive descent parser for boolean expressions that builds BDD nodes
// directly. The public parse_expression method starts the process and calls
// the following helpers:
//
// - parse_or, parse_xor, parse_and: implement operator precedence and combine
//   the resulting BDDs with the corresponding CUDD operators.
// - parse_unary: handles negation and forwards to parse_primary.
// - parse_primary: processes parentheses recursively and creates new variables
//   in the manager when necessary.
//
// Each method consumes only as much of the raw string as needed and leaves the
// rest to downstream calls.
class RecursiveParser {
public:
    RecursiveParser(const std::string &expression,
                    Cudd &manager,
                    std::unordered_map<std::string, unsigned int> &var_index,
                    std::vector<std::string> &index_to_name,
                    std::vector<std::string> *seen_vars)
        : expr(expression),
          mgr(manager),
          vars(var_index),
          idx_to_name(index_to_name),
          encountered_vars(seen_vars) {}

    // Entry point: parse the entire expression and ensure no trailing characters
    // remain unused.
    BDD parse_expression() {
        BDD result = parse_or();
        skip_ws();
        if (pos != expr.size()) {
            throw std::runtime_error("Unexpected symbol in expression: '" +
                                     std::string(1, expr[pos]) + "'");
        }
        return result;
    }

private:
    const std::string &expr;
    Cudd &mgr;
    std::unordered_map<std::string, unsigned int> &vars;
    std::vector<std::string> &idx_to_name;
    std::vector<std::string> *encountered_vars;
    std::size_t pos{0};

    // Skip any whitespace characters.
    void skip_ws() {
        while (pos < expr.size() && std::isspace(static_cast<unsigned char>(expr[pos]))) {
            ++pos;
        }
    }

    // Check whether the next character matches the expected operator and
    // consume it if so.
    bool match(char expected) {
        skip_ws();
        if (pos < expr.size() && expr[pos] == expected) {
            ++pos;
            return true;
        }
        return false;
    }

    // Lowest-precedence OR level.
    BDD parse_or() {
        BDD left = parse_xor();
        while (true) {
            skip_ws();
            if (match('|')) {
                BDD right = parse_xor();
                left = left | right;
            } else {
                break;
            }
        }
        return left;
    }

    // XOR level.
    BDD parse_xor() {
        BDD left = parse_and();
        while (true) {
            skip_ws();
            if (match('^')) {
                BDD right = parse_and();
                left = left ^ right;
            } else {
                break;
            }
        }
        return left;
    }

    // AND level with higher precedence.
    BDD parse_and() {
        BDD left = parse_unary();
        while (true) {
            skip_ws();
            if (match('&')) {
                BDD right = parse_unary();
                left = left & right;
            } else {
                break;
            }
        }
        return left;
    }

    // Handle negation recursively.
    BDD parse_unary() {
        skip_ws();
        if (match('!')) {
            return !parse_unary();
        }
        return parse_primary();
    }

    // Check whether a character is an operator or whitespace. Everything else
    // is read as part of a variable name.
    static bool is_op_or_space(char c) {
        return std::isspace(static_cast<unsigned char>(c)) || c == '!' || c == '&' ||
               c == '|' || c == '^' || c == '(' || c == ')';
    }

    // Process parentheses recursively or create variables when a name is read.
    BDD parse_primary() {
        skip_ws();
        if (match('(')) {
            BDD inner = parse_or();
            if (!match(')')) {
                throw std::runtime_error("Unbalanced parentheses in expression.");
            }
            return inner;
        }

        if (pos >= expr.size()) {
            throw std::runtime_error("Unexpected end of expression.");
        }

        // Anything that is not an operator is read as a variable name
        if (!is_op_or_space(expr[pos])) {
            std::size_t start = pos;
            while (pos < expr.size() && !is_op_or_space(expr[pos])) {
                ++pos;
            }
            std::string name = expr.substr(start, pos - start);
            if (name.empty()) {
                throw std::runtime_error("Variable name is empty in expression.");
            }

            std::string lower_name = name;
            std::transform(lower_name.begin(), lower_name.end(), lower_name.begin(),
                           [](unsigned char c) { return static_cast<char>(std::tolower(c)); });

            if (lower_name == "true") {
                return mgr.bddOne();
            }
            if (lower_name == "false") {
                return mgr.bddZero();
            }

            if (encountered_vars != nullptr) {
                encountered_vars->push_back(name);
            }

            auto it = vars.find(name);
            unsigned int idx;
            if (it == vars.end()) {
                idx = static_cast<unsigned int>(vars.size());
                vars.emplace(name, idx);
                idx_to_name.push_back(name);
            } else {
                idx = it->second;
            }
            return mgr.bddVar(idx);
        }

        throw std::runtime_error("Unknown symbol in expression: '" +
                                 std::string(1, expr[pos]) + "'");
    }
};

BDD parse_bdd_expression(const std::string &expr,
                        Cudd &mgr,
                        std::unordered_map<std::string, unsigned int> &var_idx,
                        std::vector<std::string> &idx_to_name,
                        std::vector<std::string> *encountered_vars) {
    // Shared entry point: build a BDD from an expression and create new
    // variables in the provided structures when needed.
    RecursiveParser parser(expr, mgr, var_idx, idx_to_name, encountered_vars);
    return parser.parse_expression();
}

std::string bdd_expression(const BDD &bdd,
                           const std::vector<std::string> &idx_to_name) {
    // CUDD expects names in BDD index order, not sorted support order.
    std::vector<const char *> names;
    names.reserve(idx_to_name.size());
    for (const auto &name : idx_to_name) {
        names.push_back(name.c_str());
    }
    return bdd.FactoredFormString(names.data());
}

} // namespace rcudd

extern "C" SEXP bdd_remaining_literals(SEXP expr_in) {
    // R interface: parse the expression, build the ROBDD and return either the
    // constant value or the remaining literal names.
    if (!Rf_isString(expr_in) || Rf_length(expr_in) != 1) {
        Rf_error("'expr' must be a single string.");
    }
    const char *expr_c = CHAR(STRING_ELT(expr_in, 0));
    std::string expr(expr_c ? expr_c : "");

    try {
        std::unordered_map<std::string, unsigned int> var_idx;
        std::vector<std::string> idx_to_name;
        Cudd mgr;

        BDD root = rcudd::parse_bdd_expression(expr, mgr, var_idx, idx_to_name);

        SEXP result = rcudd::bdd_summary(root, idx_to_name, idx_to_name);
        return result;
    } catch (const std::exception &ex) {
        Rf_error("Failed to build the ROBDD: %s", ex.what());
    }

    return R_NilValue; // should never be reached
}
