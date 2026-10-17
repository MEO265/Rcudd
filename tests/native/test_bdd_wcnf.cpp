// Standalone native API regression test; see inst/scripts/README.md for build.
#include "cuddObj.hh"
#include "rcudd/bdd_wcnf.hpp"
#include "../../src/binary_lower_bound.h"
#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

static void same_truth(const BDD &root, const rcudd::WcnfProblem &problem, int manager_size) {
    std::vector<int> assignment(manager_size, 0);
    for (std::size_t row = 0; row < problem.target.size(); ++row) {
        for (std::size_t column = 0; column < problem.variable_indices.size(); ++column)
            assignment[problem.variable_indices[column]] = problem.input(row, column);
        assert(root.Eval(assignment.data()).IsOne() == problem.target[row]);
    }
}
template <typename Function> static void must_throw(Function function) {
    bool thrown = false;
    try { function(); } catch (const std::exception &) { thrown = true; }
    assert(thrown);
}

// Exhaustively project away every counter assignment for small gate counts.
// Each fixed gate is unused, NOT, or binary (cycle through all binary kinds).
// The old active-slot units are included because K=r uses them to rule out
// unused slots. The new clauses must accept exactly the old assignments with
// at least r binary gates, irrespective of witness values.
static void check_binary_projection() {
    for (std::size_t k = 1; k <= 5; ++k) {
        for (unsigned int r = 0; r <= k; ++r) {
            rcudd::BinaryLowerBound bound(k, r);
            std::vector<rcudd::WcnfGate> gates(k);
            int last_id = 0;
            for (auto &gate : gates) for (int &id : gate.kind) id = ++last_id;
            std::vector<int> auxiliary(static_cast<std::size_t>(bound.auxiliary_count()));
            for (int &id : auxiliary) id = ++last_id;
            std::vector<std::vector<int>> clauses;
            bound.clauses(gates, auxiliary, [&](const std::vector<int> &clause) { clauses.push_back(clause); });
            assert(clauses.size() == bound.clause_count());
            for (std::size_t i = k - r; i < k; ++i) clauses.push_back({-gates[i].kind[0]});
            std::size_t choices = 1;
            for (std::size_t i = 0; i < k; ++i) choices *= 3;
            for (std::size_t choice = 0; choice < choices; ++choice) {
                std::vector<bool> model(last_id + 1, false);
                std::size_t remaining = choice, binaries = 0;
                bool active_bound = true;
                for (std::size_t i = 0; i < k; ++i) {
                    const unsigned int category = remaining % 3; remaining /= 3;
                    const unsigned int op = category == 0 ? 0 : category == 1 ? 4 : 1 + i % 3;
                    model[gates[i].kind[op]] = true;
                    binaries += category == 2;
                    if (i >= k - r && category == 0) active_bound = false;
                }
                bool extension = false;
                for (std::size_t assignment = 0; assignment < (std::size_t(1) << auxiliary.size()); ++assignment) {
                    for (std::size_t i = 0; i < auxiliary.size(); ++i)
                        model[auxiliary[i]] = ((assignment >> i) & 1) != 0;
                    bool satisfied = true;
                    for (const auto &clause : clauses) {
                        bool value = false;
                        for (int literal : clause)
                            value = value || (literal > 0 ? model[literal] : !model[-literal]);
                        if (!value) { satisfied = false; break; }
                    }
                    if (satisfied) { extension = true; break; }
                }
                assert(extension == (active_bound && binaries >= r));
            }
        }
    }
    must_throw([] { rcudd::BinaryLowerBound invalid(2, 3); });
}

int main(int argc, char **argv) {
    check_binary_projection();
    if (argc != 2) { std::cerr << "Supply a fixture output directory.\n"; return 2; }
    const std::string directory = argv[1];
    const std::string fixture = directory + "/native-reordered.wcnf";
    Cudd manager(6, 0);
    // Index 0, 2, 4, and 5 are outside this root's essential support.
    // Names and initial creation order deliberately disagree with alphabetic order.
    const std::vector<std::string> names = {"spare0", "b", "spare2", "a", "spare4", "c"};
    BDD a = manager.bddVar(3), b = manager.bddVar(1), c = manager.bddVar(5);
    BDD root = !(a ^ b);
    const BDD saved_root = root;
    rcudd::WcnfOptions options;
    options.max_operators = 3;
    auto before = rcudd::write_bdd_wcnf(root, names, directory + "/native-before.wcnf", options);
    same_truth(root, before, manager.ReadSize());
    int permutation[] = {5, 3, 4, 1, 2, 0};
    manager.ShuffleHeap(permutation);
    assert(Cudd_ReadPerm(manager.getManager(), 3) == 1);
    auto after = rcudd::write_bdd_wcnf(root, names, fixture, options);
    assert(after.variable_indices == std::vector<unsigned int>({3, 1}));
    assert(after.variables == std::vector<std::string>({"a", "b"}));
    assert(before.target == after.target);
    same_truth(root, after, manager.ReadSize());
    assert(root == saved_root && manager.ReadSize() == 6);
    assert((root ^ saved_root).IsZero()); // manager/root still usable after export

    BDD mixed = !(b & ((!a) | c));
    auto complemented = rcudd::write_bdd_wcnf(mixed, names, directory + "/native-complemented.wcnf", options);
    same_truth(mixed, complemented, manager.ReadSize());
    assert(!complemented.has_shortcut && complemented.target.size() == 8);
    assert(complemented.binary_lower_bound == 2 && complemented.binary_auxiliary.size() == 4);
    auto constant = rcudd::write_bdd_wcnf(manager.bddZero(), {}, directory + "/native-constant.wcnf");
    assert(constant.has_shortcut && constant.shortcut == "false" && constant.nclauses == 2);
    auto positive = rcudd::write_bdd_wcnf(a, names, directory + "/native-variable.wcnf");
    assert(positive.has_shortcut && positive.shortcut == "a");
    options.max_operators = 0;
    auto impossible = rcudd::write_bdd_wcnf(!a, names, directory + "/native-unsat.wcnf", options);
    assert(!impossible.has_shortcut && impossible.nclauses == 3 && impossible.gates.empty());

    options.max_operators = 3;
    must_throw([&] { rcudd::write_bdd_wcnf(root, {}, fixture, options); });
    for (const std::string invalid : {"a b", "true", "FaLsE", "a&b", ""}) {
        auto bad_names = names; bad_names[3] = invalid;
        must_throw([&] { rcudd::write_bdd_wcnf(root, bad_names, fixture, options); });
    }
    auto duplicates = names; duplicates[3] = "b";
    must_throw([&] { rcudd::write_bdd_wcnf(root, duplicates, fixture, options); });
    options.max_variables = 1;
    must_throw([&] { rcudd::write_bdd_wcnf(root, names, fixture, options); });
    options.max_variables = 10; options.max_clauses = 1;
    must_throw([&] { rcudd::write_bdd_wcnf(root, names, fixture, options); });
    options.max_clauses = 2000000; options.max_operators = 1000000;
    must_throw([&] { rcudd::write_bdd_wcnf(root, names, fixture, options); });
    options.max_operators = 3;
    must_throw([&] { rcudd::write_bdd_wcnf(root, names, "", options); });
    options.max_operators = 1;
    auto negation = rcudd::write_bdd_wcnf(!a, names, directory + "/native-not.wcnf", options);
    const auto &gate = negation.gates[0];
    std::vector<bool> selected(negation.nvars + 1, false);
    for (int id : {gate.kind[4], gate.left[2], gate.left_value[1], gate.out[0]}) selected[id] = true;
    std::string output = "s OPTIMUM FOUND\no 1\nv";
    for (int id = 1; id <= negation.nvars; ++id) output += " " + std::to_string(selected[id] ? id : -id);
    output += " 0\n";
    const auto solution = rcudd::interpret_rwbo_output(negation, output);
    assert(solution.verified && solution.optimal && solution.expression == "!a" && solution.operator_count == 1 && solution.cost == 1);
    const auto unknown = rcudd::interpret_rwbo_output(negation, "s UNKNOWN");
    assert(!unknown.verified && !unknown.optimal && !unknown.has_expression && unknown.cost == -1);
    auto corrupt = negation; corrupt.gates[0].left[0] = negation.nvars + 1;
    must_throw([&] { rcudd::interpret_rwbo_output(corrupt, output); });
    corrupt = constant; corrupt.shortcut = "true";
    must_throw([&] { rcudd::interpret_rwbo_output(corrupt, ""); });
    auto trivial = rcudd::interpret_rwbo_output(constant, "");
    assert(trivial.verified && trivial.optimal && trivial.expression == "false" && trivial.cost == 0);

    // A context variable can be a cheaper equivalent proxy for the root.
    const BDD proxy_care = !(c ^ (a & b));
    auto proxy = rcudd::write_bdd_wcnf(a & b, proxy_care, names,
                                      directory + "/native-proxy.wcnf", options);
    assert(proxy.variables == std::vector<std::string>({"a", "b", "c"}));
    assert(proxy.has_shortcut && proxy.shortcut == "c" && proxy.care.size() == 8);
    const auto proxy_solution = rcudd::interpret_rwbo_output(proxy, "");
    assert(proxy_solution.verified && proxy_solution.optimal && proxy_solution.expression == "c");
    assert((proxy_care & ((a & b) ^ c)).IsZero());
    assert(!((a & b) ^ c).IsZero()); // equivalence is deliberately contextual
    auto empty = rcudd::write_bdd_wcnf(a ^ b, manager.bddZero(), names,
                                      directory + "/native-empty-care.wcnf", options);
    assert(empty.has_shortcut && empty.shortcut == "false");
    assert(rcudd::interpret_rwbo_output(empty, "").verified);

    // Context support is available to synthesis but does not imply n-1 gates.
    auto contextual = rcudd::write_bdd_wcnf(a ^ b, c, names,
                                           directory + "/native-contextual.wcnf", options);
    assert(contextual.lower_bound == 1 && contextual.variables.size() == 3);
    assert(contextual.binary_lower_bound == 0 && contextual.binary_auxiliary.empty());
    same_truth(a ^ b, contextual, manager.ReadSize());
    std::vector<int> assignment(manager.ReadSize(), 0);
    for (std::size_t row = 0; row < contextual.target.size(); ++row) {
        for (std::size_t column = 0; column < contextual.variable_indices.size(); ++column)
            assignment[contextual.variable_indices[column]] = contextual.input(row, column);
        assert(c.Eval(assignment.data()).IsOne() == contextual.care[row]);
    }
    const auto &xor_gate = contextual.gates[0];
    std::vector<bool> xor_selected(contextual.nvars + 1, false);
    for (int id : {xor_gate.kind[3], xor_gate.left[2], xor_gate.right[3]}) xor_selected[id] = true;
    for (std::size_t row = 0; row < contextual.target.size(); ++row) {
        xor_selected[xor_gate.left_value[row]] = contextual.input(row, 0);
        xor_selected[xor_gate.right_value[row]] = contextual.input(row, 1);
        xor_selected[xor_gate.out[row]] = contextual.input(row, 0) != contextual.input(row, 1);
    }
    std::string xor_output = "s OPTIMUM FOUND\no 2\nv";
    for (int id = 1; id <= contextual.nvars; ++id)
        xor_output += " " + std::to_string(xor_selected[id] ? id : -id);
    xor_output += " 0\n";
    const auto xor_solution = rcudd::interpret_rwbo_output(contextual, xor_output);
    assert(xor_solution.verified && xor_solution.optimal && xor_solution.expression == "a ^ b");
    assert(xor_solution.operator_count == 1 && xor_solution.cost == 2);
    auto bad_cost = xor_output;
    bad_cost.replace(bad_cost.find("o 2"), 3, "o 1");
    must_throw([&] { rcudd::interpret_rwbo_output(contextual, bad_cost); });
    std::ifstream wcnf(contextual.wcnf_file);
    std::string line;
    std::getline(wcnf, line);
    assert(line == "p wcnf " + std::to_string(contextual.nvars) + " " +
                   std::to_string(contextual.nclauses) + " 3");
    unsigned int soft = 0;
    while (std::getline(wcnf, line)) {
        std::istringstream clause(line);
        int weight;
        clause >> weight;
        assert(weight == 1 || weight == 3);
        soft += weight == 1;
    }
    assert(soft == 2);
    auto invalid_care = contextual; invalid_care.care.pop_back();
    must_throw([&] { rcudd::interpret_rwbo_output(invalid_care, xor_output); });
    Cudd other_manager;
    must_throw([&] { rcudd::write_bdd_wcnf(a, other_manager.bddOne(), names, fixture, options); });
    options.max_variables = 2;
    must_throw([&] { rcudd::write_bdd_wcnf(a & b, proxy_care, names, fixture, options); });
    std::cout << "Native existing-BDD/reordering/complement/resource checks passed.\n";
    return 0;
}
