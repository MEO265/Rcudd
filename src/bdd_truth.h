#ifndef RCUDD_BDD_TRUTH_HPP
#define RCUDD_BDD_TRUTH_HPP

#include "cuddObj.hh"
#include <algorithm>
#include <cstddef>
#include <stdexcept>
#include <vector>

namespace rcudd {
// CUDD indices remain stable when variable levels are reordered. Complement
// bits on both the root and internal edges determine terminal polarity.
inline bool bdd_value(const BDD &root, std::size_t row,
                      const std::vector<unsigned int> &indices) {
    DdNode *node = root.getNode();
    bool negated = Cudd_IsComplement(node);
    node = Cudd_Regular(node);
    while (!Cudd_IsConstant(node)) {
        const unsigned int index = Cudd_NodeReadIndex(node);
        const auto found = std::find(indices.begin(), indices.end(), index);
        if (found == indices.end()) throw std::runtime_error("BDD support/index mismatch.");
        const std::size_t column = static_cast<std::size_t>(found - indices.begin());
        DdNode *next = ((row >> column) & 1u) ? Cudd_T(node) : Cudd_E(node);
        negated = negated != static_cast<bool>(Cudd_IsComplement(next));
        node = Cudd_Regular(next);
    }
    return !negated; // One regular BDD terminal, logical one.
}
}
#endif
