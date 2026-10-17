#ifndef RCUDD_BINARY_LOWER_BOUND_H
#define RCUDD_BINARY_LOWER_BOUND_H

#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <vector>

namespace rcudd {
// Redundant cardinality constraint for formula trees with n essential inputs:
// at least n-1 binary gates are necessary. Contextual pools have no such bound.
// Keep sizing and emission together so export preflight and validation agree.
class BinaryLowerBound {
public:
    BinaryLowerBound(std::size_t gates, unsigned int bound) : k(gates), r(bound) {
        if (r > k) throw std::invalid_argument("Binary lower bound exceeds gate count.");
    }
    std::uint64_t auxiliary_count() const {
        return general() ? static_cast<std::uint64_t>(r) * width() : 0;
    }
    std::uint64_t clause_count() const {
        return !r ? 0 : r == 1 ? 1 : r == k ? k :
            (2 * static_cast<std::uint64_t>(r) - 1) * width() + 1;
    }
    template <typename Gates, typename Emit>
    void clauses(const Gates &gates, const std::vector<int> &auxiliary, Emit emit) const {
        if (gates.size() != k || auxiliary.size() != auxiliary_count())
            throw std::invalid_argument("Invalid binary-counter dimensions.");
        if (!r) return;
        auto binary = [&](std::vector<int> &clause, std::size_t i) {
            for (int op = 1; op <= 3; ++op) clause.push_back(gates[i].kind[op]);
        };
        if (r == k) {
            // The existing active-slot lower bound already forbids unused at
            // every slot here. Unit !NOT makes each slot binary and exposes
            // its second soft cost immediately to unit propagation.
            for (std::size_t i = 0; i < k; ++i) emit(std::vector<int>{-gates[i].kind[4]});
        } else if (r == 1) {
            std::vector<int> clause;
            for (std::size_t i = 0; i < k; ++i) binary(clause, i);
            emit(clause);
        } else {
            // S(i,j) witnesses >=j binary gates among the first i gates. Only
            // witnesses reachable backwards from S(k,r) are allocated. A true
            // S(i,j) requires S(i-1,j) OR (binary_i AND S(i-1,j-1)).
            // Missing S(i-1,i) is false; S(i,0) is true. These one-way clauses
            // preserve every gate assignment with >=r binary gates: extend it
            // with its actual prefix counts. Conversely induction on i proves
            // that a true witness cannot claim too many binary gates.
            auto id = [&](std::size_t i, std::size_t j) {
                return auxiliary[(j - 1) * width() + i - j];
            };
            for (std::size_t j = 1; j <= r; ++j) {
                for (std::size_t i = j; i <= k - r + j; ++i) {
                    std::vector<int> clause = {-id(i, j)};
                    if (i > j) clause.push_back(id(i - 1, j));
                    binary(clause, i - 1); emit(clause);
                    if (j > 1) {
                        clause = {-id(i, j), id(i - 1, j - 1)};
                        if (i > j) clause.push_back(id(i - 1, j));
                        emit(clause);
                    }
                }
            }
            emit(std::vector<int>{id(k, r)});
        }
    }
private:
    std::size_t k;
    unsigned int r;
    bool general() const { return r > 1 && r < k; }
    std::size_t width() const { return k - r + 1; }
};
} // namespace rcudd
#endif
