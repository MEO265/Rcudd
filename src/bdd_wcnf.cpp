#include "../inst/include/rcudd/bdd_wcnf.hpp"
#include "cuddObj.hh"
#include "bdd_truth.h"
#include <algorithm>
#include <climits>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <fstream>
#include <initializer_list>
#include <numeric>
#include <stdexcept>
#include <utility>

namespace rcudd {
namespace {
using Count = std::uint64_t;
const Count sat_limit = INT_MAX - 1;
Count add(Count a, Count b) {
    if (b > UINT64_MAX - a) throw std::runtime_error("Clause limit exceeded (count overflow).");
    return a + b;
}
Count mul(Count a, Count b) {
    if (a && b > UINT64_MAX / a) throw std::runtime_error("Clause limit exceeded (count overflow).");
    return a * b;
}
Count pairs(Count n) { return n < 2 ? 0 : mul(n, n - 1) / 2; }
void check_clauses(Count count, Count limit) {
    if (count > limit) throw std::runtime_error("Clause limit exceeded; reduce the search bounds.");
}

class Writer {
public:
    Writer(const std::string &path, int nvars, Count nclauses, Count top)
        : stream(path.c_str(), std::ios::out | std::ios::trunc), top(top) {
        if (!stream) throw std::runtime_error("Cannot open WCNF output file: " + path);
        stream.exceptions(std::ios::badbit | std::ios::failbit);
        stream << "p wcnf " << nvars << ' ' << nclauses << ' ' << top << '\n';
    }
    void hard(std::initializer_list<int> literals) {
        stream << top;
        for (int literal : literals) stream << ' ' << literal;
        stream << " 0\n";
        ++count;
    }
    void hard(const std::vector<int> &literals) {
        stream << top;
        for (int literal : literals) stream << ' ' << literal;
        stream << " 0\n";
        ++count;
    }
    void soft(int literal) { stream << "1 " << literal << " 0\n"; ++count; }
    void soft(std::initializer_list<int> literals) {
        stream << "1";
        for (int literal : literals) stream << ' ' << literal;
        stream << " 0\n";
        ++count;
    }
    template <typename Container> void at_most_one(const Container &ids) {
        for (std::size_t i = 0; i < ids.size(); ++i)
            for (std::size_t j = i + 1; j < ids.size(); ++j) hard({-ids[i], -ids[j]});
    }
    template <typename Container> void preceded(int prefix, const Container &ids) {
        stream << top << ' ' << prefix;
        for (int literal : ids) stream << ' ' << literal;
        stream << " 0\n";
        ++count;
    }
    void finish(Count expected) {
        if (count != expected) throw std::runtime_error("Internal WCNF clause-count mismatch.");
        stream.close();
    }
private:
    std::ofstream stream;
    Count top, count = 0;
};
} // namespace

static WcnfProblem write_problem(const BDD &root, const BDD *care,
                           const std::vector<std::string> &names_by_index,
                           const std::string &path, const WcnfOptions &options) {
    if (!root.getNode() || !root.manager()) throw std::invalid_argument("BDD root/manager is null.");
    if (care && (!care->getNode() || care->manager() != root.manager()))
        throw std::invalid_argument("Root and care BDDs must share a manager.");
    const bool contextual = care && !care->IsOne();
    if (path.empty() || path.find('\0') != std::string::npos)
        throw std::invalid_argument("WCNF file path is empty or contains NUL.");
    if (options.max_operators > sat_limit - 1 || options.max_clauses < 1 ||
        options.max_clauses > sat_limit)
        throw std::invalid_argument("Search bounds exceed supported integer limits.");
    WcnfProblem p;
    p.wcnf_file = path;
    p.max_operators = options.max_operators;
    p.variable_indices = root.SupportIndices();
    if (contextual) {
        for (unsigned int index : care->SupportIndices())
            if (std::find(p.variable_indices.begin(), p.variable_indices.end(), index) == p.variable_indices.end())
                p.variable_indices.push_back(index);
    }
    if (p.variable_indices.size() > std::min(options.max_variables, 20u))
        throw std::runtime_error("BDD support exceeds truth-table limit; the encoding has 2^n rows.");
    for (unsigned int index : p.variable_indices) {
        if (index >= names_by_index.size() || names_by_index[index].empty() ||
            names_by_index[index].find('\0') != std::string::npos)
            throw std::invalid_argument("Each essential CUDD index needs a nonempty name without NUL.");
    }
    std::sort(p.variable_indices.begin(), p.variable_indices.end(),
              [&](unsigned int a, unsigned int b) { return names_by_index[a] < names_by_index[b]; });
    for (unsigned int index : p.variable_indices) {
        const std::string &name = names_by_index[index];
        std::string lower = name;
        for (char &ch : lower) ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
        if (lower == "true" || lower == "false" ||
            std::any_of(name.begin(), name.end(), [](unsigned char ch) {
                return std::isspace(ch) || ch == '!' || ch == '&' || ch == '|' ||
                       ch == '^' || ch == '(' || ch == ')';
            })) throw std::invalid_argument("Essential variable names must be valid Rcudd variable tokens.");
        if (!p.variables.empty() && name == p.variables.back())
            throw std::invalid_argument("Essential variable names must be unique.");
        p.variables.push_back(name);
    }
    const Count n = p.variables.size(), k = options.max_operators;
    const Count rows = Count(1) << n, inputs = n + 2;
    const bool constant = root.IsOne() || root.IsZero();
    const bool positive_variable = n == 1 && !bdd_value(root, 0, p.variable_indices) &&
                                  bdd_value(root, 1, p.variable_indices);
    p.has_shortcut = constant || positive_variable;
    if (p.has_shortcut) p.shortcut = constant ? (root.IsOne() ? "true" : "false") : p.variables[0];
    Count cared_rows = rows;
    if (contextual) {
        // Count and detect zero-cost equivalents on the BDDs before any 2^n
        // enumeration. In particular, huge operator bounds fail preflight
        // without scanning a large context truth table first.
        const double count = care->CountMinterm(static_cast<int>(n));
        if (!std::isfinite(count) || count < 0 || count > rows || count != std::floor(count))
            throw std::runtime_error("Cannot count context truth rows.");
        cared_rows = static_cast<Count>(count);
        const bool all_false = care->Leq(!root), all_true = care->Leq(root);
        p.has_shortcut = all_false || all_true;
        if (p.has_shortcut) p.shortcut = all_false ? "false" : "true";
        else for (std::size_t column = 0; column < n; ++column) {
            // Cudd_EquivDC compares where DC=0 without creating BDD nodes.
            DdNode *variable = Cudd_bddIthVar(root.manager(), p.variable_indices[column]);
            if (Cudd_EquivDC(root.manager(), root.getNode(), variable, Cudd_Not(care->getNode()))) {
                p.has_shortcut = true; p.shortcut = p.variables[column]; break;
            }
        }
    }
    p.lower_bound = p.has_shortcut ? 0 : contextual ? 1 : static_cast<unsigned int>(std::max(Count(1), n - 1));
    const bool insufficient = !p.has_shortcut && k < p.lower_bound;
    Count clauses = 0, variables = 1;
    if (p.has_shortcut || insufficient) {
        clauses = p.has_shortcut ? 2 : 3;
        check_clauses(clauses, options.max_clauses);
    } else {
        // A cheap lower bound rejects huge k before a gate-count loop/allocation.
        const Count per_rows = add(mul(12, k), add(mul(mul(2, inputs), k),
                                                mul(mul(2, k), k - 1)));
        check_clauses(add(add(mul(rows, per_rows), cared_rows), mul(2, k)), options.max_clauses);
        variables = add(mul(k, add(5 + 2 * inputs, mul(3, rows))), mul(k, k - 1));
        if (variables > sat_limit) throw std::runtime_error("SAT variable limit exceeded.");
        clauses = add(add(cared_rows, mul(2, k)), p.lower_bound); // root, soft, lower bound
        for (Count i = 0; i < k; ++i) {
            const Count sources = inputs + i, remaining = k - i - 1;
            clauses = add(clauses, add(15 + 3 * sources, mul(3, pairs(sources))));
            if (remaining) clauses = add(clauses, add(2 + 2 * remaining, pairs(2 * remaining)));
            clauses = add(clauses, mul(rows, 2 * inputs + 4 * i + 12));
            check_clauses(clauses, options.max_clauses);
        }
    }
    // No exponentially sized storage, SAT ids, or file writes before preflight.
    p.target.reserve(static_cast<std::size_t>(rows));
    for (Count row = 0; row < rows; ++row)
        p.target.push_back(bdd_value(root, static_cast<std::size_t>(row), p.variable_indices));
    if (contextual) {
        p.care.reserve(static_cast<std::size_t>(rows));
        for (Count row = 0; row < rows; ++row) p.care.push_back(bdd_value(*care, row, p.variable_indices));
    }
    p.nvars = static_cast<int>(variables);
    p.nclauses = static_cast<std::size_t>(clauses);
    if (p.has_shortcut || insufficient) {
        // A soft clause avoids an Open-WBO 0.1.2 crash on hard-only inputs.
        Writer out(path, 1, clauses, 2);
        out.hard({1});
        if (insufficient) out.hard({-1});
        out.soft(1);
        out.finish(clauses);
        return p;
    }
    int next_id = 0;
    auto fresh = [&](std::size_t count) {
        std::vector<int> ids(count);
        for (int &id : ids) id = ++next_id;
        return ids;
    };
    p.gates.resize(static_cast<std::size_t>(k));
    for (std::size_t i = 0; i < p.gates.size(); ++i) {
        WcnfGate &g = p.gates[i];
        for (int &id : g.kind) id = ++next_id;
        g.left = fresh(inputs + i); g.right = fresh(inputs + i);
        g.left_value = fresh(rows); g.right_value = fresh(rows); g.out = fresh(rows);
    }
    if (next_id != p.nvars) throw std::runtime_error("Internal SAT variable-count mismatch.");
    Writer out(path, p.nvars, clauses, 2 * k + 1);
    for (std::size_t i = 0; i < p.gates.size(); ++i) {
        const WcnfGate &g = p.gates[i];
        const int unused = g.kind[0];
        out.hard(std::vector<int>(g.kind.begin(), g.kind.end()));
        out.at_most_one(g.kind);
        out.preceded(unused, g.left);
        out.at_most_one(g.left); out.at_most_one(g.right);
        for (int id : g.left) out.hard({-id, -unused});
        for (int id : g.right) { out.hard({-id, -unused}); out.hard({-id, -g.kind[4]}); }
        for (int op = 1; op <= 3; ++op) out.preceded(-g.kind[op], g.right);
        for (std::size_t left = 1; left < g.left.size(); ++left)
            for (std::size_t right = 0; right < left; ++right)
                out.hard({-g.left[left], -g.right[right]});
        if (i + 1 < p.gates.size()) {
            out.hard({unused, -p.gates[i + 1].kind[0]});
            std::vector<int> consumers;
            for (std::size_t j = i + 1; j < p.gates.size(); ++j) {
                consumers.push_back(p.gates[j].left[inputs + i]);
                consumers.push_back(p.gates[j].right[inputs + i]);
            }
            out.preceded(unused, consumers); out.at_most_one(consumers);
            for (int id : consumers) out.hard({-id, -unused});
        }
        for (std::size_t source = 0; source < g.left.size(); ++source) {
            for (std::size_t row = 0; row < rows; ++row) {
                for (int port = 0; port < 2; ++port) {
                    const int selector = port ? g.right[source] : g.left[source];
                    const int value = port ? g.right_value[row] : g.left_value[row];
                    if (source < inputs) {
                        const bool bit = source == 1 || (source >= 2 && p.input(row, source - 2));
                        out.hard({-selector, bit ? value : -value});
                    } else {
                        const int previous = p.gates[source - inputs].out[row];
                        out.hard({-selector, -value, previous});
                        out.hard({-selector, value, -previous});
                    }
                }
            }
        }
        for (std::size_t row = 0; row < rows; ++row) {
            const int a = g.left_value[row], b = g.right_value[row], value = g.out[row];
            int op = g.kind[1];
            out.hard({-op, -value, a}); out.hard({-op, -value, b}); out.hard({-op, value, -a, -b});
            op = g.kind[2];
            out.hard({-op, value, -a}); out.hard({-op, value, -b}); out.hard({-op, -value, a, b});
            op = g.kind[3];
            out.hard({-op, -a, -b, -value}); out.hard({-op, a, b, -value});
            out.hard({-op, a, -b, value}); out.hard({-op, -a, b, value});
            op = g.kind[4];
            out.hard({-op, -value, -a}); out.hard({-op, value, a});
        }
    }
    for (std::size_t i = p.gates.size() - p.lower_bound; i < p.gates.size(); ++i)
        out.hard({-p.gates[i].kind[0]});
    const std::vector<int> &root_out = p.gates.back().out;
    for (std::size_t row = 0; row < rows; ++row)
        if (p.care.empty() || p.care[row]) out.hard({p.target[row] ? root_out[row] : -root_out[row]});
    for (const WcnfGate &g : p.gates) {
        // Every active operator costs one; binary operators cost one more.
        // Unit-weight clauses retain the unweighted MaxSAT formulation.
        out.soft(g.kind[0]);
        out.soft({g.kind[0], g.kind[4]});
    }
    out.finish(clauses);
    return p;
}

WcnfProblem write_bdd_wcnf(const BDD &root, const std::vector<std::string> &names,
                           const std::string &path, const WcnfOptions &options) {
    return write_problem(root, nullptr, names, path, options);
}
WcnfProblem write_bdd_wcnf(const BDD &root, const BDD &care, const std::vector<std::string> &names,
                           const std::string &path, const WcnfOptions &options) {
    return write_problem(root, &care, names, path, options);
}
} // namespace rcudd
