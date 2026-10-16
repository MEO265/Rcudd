// Standalone native API regression test; see inst/scripts/README.md for build.
#include "cuddObj.hh"
#include "rcudd/bdd_wcnf.hpp"
#include <cassert>
#include <fstream>
#include <iostream>
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

int main(int argc, char **argv) {
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
    std::cout << "Native existing-BDD/reordering/complement/resource checks passed.\n";
    return 0;
}
