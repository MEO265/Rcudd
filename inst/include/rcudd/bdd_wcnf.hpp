#ifndef RCUDD_BDD_WCNF_HPP
#define RCUDD_BDD_WCNF_HPP

#include <array>
#include <cstddef>
#include <string>
#include <vector>

class BDD;

namespace rcudd {

struct WcnfOptions {
    unsigned int max_operators = 8;
    unsigned int max_variables = 10;
    std::size_t max_clauses = 2000000;
};

// SAT ids are positive and 1-based. Kind order: unused, and, or, xor, not.
// Source order: false, true, variables, preceding gates (all in vector order).
struct WcnfGate {
    std::array<int, 5> kind;
    std::vector<int> left, right, left_value, right_value, out;
};

struct WcnfProblem {
    std::vector<std::string> variables; // sorted union of root/care support
    std::vector<unsigned int> variable_indices; // original CUDD indices
    std::vector<bool> target;
    std::vector<bool> care; // empty: every row; otherwise only true rows constrain the root
    std::vector<WcnfGate> gates;
    unsigned int max_operators = 0;
    unsigned int lower_bound = 0;
    bool has_shortcut = false;
    std::string shortcut;
    int nvars = 0;
    std::size_t nclauses = 0;
    std::string wcnf_file;

    // Truth-table column j at row r: (r >> j) & 1; column zero changes fastest.
    bool input(std::size_t row, std::size_t column) const {
        return ((row >> column) & 1u) != 0;
    }
};

// Export exact formula-tree synthesis from an EXISTING CUDD BDD. No parsing,
// expression rendering, or manager ownership transfer. names_by_index[i] names
// original CUDD index i, irrespective of current variable levels/reordering.
// Essential names must be unique Rcudd tokens: nonempty, no whitespace or
// ! & | ^ ( ), and not true/false (case insensitive).
// Only essential variables are used, sorted by name. All 2^n assignments are
// constrained. Each &, |, ^, ! costs one, including repeated subexpressions.
// Throws std::invalid_argument/runtime_error before large allocations when
// bounds are invalid/exceeded. A hard safety cap of 20 essential variables
// applies even when max_variables is raised. The caller keeps root/manager
// alive throughout the call and must not reorder/use the manager concurrently.
// The output path uses the platform's filename encoding; success overwrites it.
WcnfProblem write_bdd_wcnf(const BDD &root,
                           const std::vector<std::string> &names_by_index,
                           const std::string &wcnf_file,
                           const WcnfOptions &options = WcnfOptions());

// Contextual variant: matches root only where care is true. Uses the union of
// root/care support, so the variable limit includes context variables. The
// optimum is over this variable pool; care/root must share a manager.
WcnfProblem write_bdd_wcnf(const BDD &root, const BDD &care,
                           const std::vector<std::string> &names_by_index,
                           const std::string &wcnf_file,
                           const WcnfOptions &options = WcnfOptions());

struct WcnfSolution {
    bool has_expression = false;
    std::string expression;
    int operator_count = -1;
    bool optimal = false;
    bool verified = false;
    std::string status = "UNKNOWN";
};

// Interpret the solver's complete stdout (not a filename). Validates metadata,
// complete signed model, WCNF hard clauses/objective, all-row operator semantics
// and target equality on care rows. Throws on invalid metadata/model/objective/clauses. UNKNOWN,
// UNSAT and missing models return an unverified result without an expression.
// Expects complete output lines, as returned by the corrected Rwbo wrapper.
// OPTIMUM FOUND yields optimal=true only after all validation succeeds;
// validated zero-cost BDD shortcuts are directly proven without solver output.
WcnfSolution interpret_rwbo_output(const WcnfProblem &problem,
                                    const std::string &output);
} // namespace rcudd
#endif
