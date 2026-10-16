#include "../inst/include/rcudd/bdd_wcnf.hpp"
#include <algorithm>
#include <cctype>
#include <climits>
#include <cstdint>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <unordered_set>

namespace rcudd {
namespace {
void invalid(const std::string &message) { throw std::runtime_error(message); }
long long number(const std::string &token) {
    std::size_t used = 0;
    long long value;
    try { value = std::stoll(token, &used); }
    catch (...) { invalid("Invalid integer in solver output or WCNF."); return 0; }
    if (used != token.size()) invalid("Invalid integer in solver output or WCNF.");
    return value;
}
std::string trim(const std::string &text) {
    const std::size_t start = text.find_first_not_of(" \t\r\n");
    if (start == std::string::npos) return "";
    return text.substr(start, text.find_last_not_of(" \t\r\n") - start + 1);
}
void validate_problem(const WcnfProblem &p) {
    const std::size_t n = p.variables.size();
    if (n > 20 || p.variable_indices.size() != n || p.target.size() != (std::size_t(1) << n) ||
        p.nvars < 1 || p.nvars > INT_MAX - 1 || p.nclauses < 2 || p.nclauses > INT_MAX - 1 ||
        p.max_operators > INT_MAX - 2 || p.wcnf_file.empty())
        invalid("Invalid problem metadata dimensions or bounds.");
    std::unordered_set<unsigned int> indices;
    for (std::size_t column = 0; column < n; ++column) {
        const std::string &name = p.variables[column];
        std::string lower = name;
        for (char &ch : lower) ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
        if (name.empty() || name.find('\0') != std::string::npos || lower == "true" || lower == "false" ||
            (column && p.variables[column - 1] >= name) || !indices.insert(p.variable_indices[column]).second ||
            std::any_of(name.begin(), name.end(), [](unsigned char ch) {
                return std::isspace(ch) || ch == '!' || ch == '&' || ch == '|' || ch == '^' || ch == '(' || ch == ')';
            })) invalid("Invalid essential variable metadata.");
        bool essential = false;
        for (std::size_t row = 0; row < p.target.size(); ++row)
            if (!p.input(row, column) && p.target[row] != p.target[row | (std::size_t(1) << column)]) {
                essential = true; break;
            }
        if (!essential) invalid("Problem metadata includes an inessential variable.");
    }
    const unsigned int lower = p.has_shortcut ? 0 : static_cast<unsigned int>(n > 1 ? n - 1 : 1);
    if (p.lower_bound != lower) invalid("Invalid support lower-bound metadata.");
    if (p.has_shortcut) {
        if (!p.gates.empty() || p.nvars != 1 || p.nclauses != 2)
            invalid("Invalid shortcut metadata.");
        if (n == 0) {
            if (p.shortcut != (p.target[0] ? "true" : "false")) invalid("Shortcut does not match target.");
        } else if (n != 1 || p.shortcut != p.variables[0] || p.target[0] || !p.target[1]) {
            invalid("Shortcut does not match target.");
        }
        return;
    }
    if (n == 0 || (n == 1 && !p.target[0] && p.target[1]))
        invalid("Missing zero-cost shortcut metadata.");
    if (p.gates.empty()) {
        if (p.max_operators >= lower || p.nvars != 1 || p.nclauses != 3)
            invalid("Invalid bounded-UNSAT metadata.");
        return;
    }
    if (p.gates.size() != p.max_operators || p.max_operators < lower)
        invalid("Invalid gate-count metadata.");
    std::int64_t next = 0;
    auto ids = [&](const auto &values, std::size_t size) {
        if (values.size() != size) invalid("Invalid gate metadata dimensions.");
        for (int value : values)
            if (++next > p.nvars || value != next) invalid("Invalid or duplicate gate SAT-id metadata.");
    };
    for (std::size_t i = 0; i < p.gates.size(); ++i) {
        const WcnfGate &gate = p.gates[i];
        ids(gate.kind, 5); ids(gate.left, n + 2 + i); ids(gate.right, n + 2 + i);
        ids(gate.left_value, p.target.size()); ids(gate.right_value, p.target.size()); ids(gate.out, p.target.size());
    }
    if (next != p.nvars) invalid("SAT-variable count does not match gate metadata.");
    const std::uint64_t k = p.gates.size(), rows = p.target.size(), inputs = n + 2;
    std::uint64_t clauses = rows + k + p.lower_bound;
    auto pairs = [](std::uint64_t count) { return count < 2 ? 0 : count * (count - 1) / 2; };
    for (std::uint64_t i = 0; i < k; ++i) {
        const std::uint64_t sources = inputs + i, remaining = k - i - 1;
        clauses += 15 + 3 * sources + 3 * pairs(sources) + rows * (2 * inputs + 4 * i + 12);
        if (remaining) clauses += 2 + 2 * remaining + pairs(2 * remaining);
        if (clauses > static_cast<std::uint64_t>(p.nclauses)) invalid("Clause count does not match gate metadata.");
    }
    if (clauses != p.nclauses) invalid("Clause count does not match gate metadata.");
}

struct ParsedOutput {
    std::string status = "UNKNOWN";
    std::vector<std::string> model_lines;
    bool has_cost = false;
    long long cost = 0;
};
ParsedOutput parse_output(const std::string &output) {
    std::istringstream stream(output);
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(stream, line)) {
        if (!line.empty() && line.back() == '\r') line.pop_back();
        lines.push_back(line);
    }
    ParsedOutput result;
    for (const std::string &item : lines) {
        if (item.compare(0, 2, "s ") == 0) result.status = trim(item.substr(2));
        else if (item.compare(0, 2, "v ") == 0) result.model_lines.push_back(item.substr(2));
        else if (item.compare(0, 2, "o ") == 0) {
            const long long cost = number(trim(item.substr(2)));
            if (cost < 0 || cost > INT_MAX - 1) invalid("Invalid solver objective value.");
            result.has_cost = true; result.cost = cost;
        }
    }
    return result;
}
std::vector<std::int8_t> assignment(const ParsedOutput &output, int nvars) {
    std::vector<std::int8_t> values(static_cast<std::size_t>(nvars) + 1, -1);
    std::size_t assigned = 0;
    for (const std::string &line : output.model_lines) {
        std::istringstream stream(line);
        std::string token;
        while (stream >> token) {
            const long long literal = number(token);
            if (literal == 0) continue;
            if (literal < -static_cast<long long>(nvars) || literal > nvars)
                invalid("Solver model is incomplete or invalid: out-of-range assignment.");
            const std::size_t id = static_cast<std::size_t>(literal < 0 ? -literal : literal);
            if (values[id] != -1) invalid("Solver model is incomplete or invalid: duplicate/conflicting assignment.");
            values[id] = literal > 0;
            ++assigned;
        }
    }
    if (assigned != static_cast<std::size_t>(nvars)) invalid("Solver model is incomplete or invalid.");
    return values;
}
int check_wcnf(const WcnfProblem &problem, const std::vector<std::int8_t> &values) {
    std::ifstream stream(problem.wcnf_file.c_str());
    if (!stream) invalid("Cannot read WCNF output file.");
    std::string line;
    bool header = false;
    long long top = 0, clauses = 0, cost = 0;
    while (std::getline(stream, line)) {
        line = trim(line);
        if (line.empty() || line[0] == 'c') continue;
        std::istringstream tokens(line);
        std::string token;
        if (!header) {
            std::string format, nv, nc, weight, extra;
            if (!(tokens >> token >> format >> nv >> nc >> weight) || token != "p" || format != "wcnf" ||
                (tokens >> extra)) invalid("Invalid WCNF header.");
            top = number(weight);
            const long long expected_top = problem.gates.empty() ? 2 : static_cast<long long>(problem.max_operators) + 1;
            if (number(nv) != problem.nvars || number(nc) != static_cast<long long>(problem.nclauses) || top != expected_top)
                invalid("The WCNF does not match the saved problem.");
            header = true;
            continue;
        }
        if (++clauses > static_cast<long long>(problem.nclauses)) invalid("WCNF clause count mismatch.");
        tokens >> token;
        const long long weight = number(token);
        if (weight <= 0) invalid("Invalid WCNF clause weight.");
        bool satisfied = false, terminated = false;
        while (tokens >> token) {
            const long long literal = number(token);
            if (literal == 0) { terminated = true; break; }
            if (literal < -static_cast<long long>(problem.nvars) || literal > problem.nvars)
                invalid("Invalid out-of-range WCNF literal.");
            const std::size_t id = static_cast<std::size_t>(literal < 0 ? -literal : literal);
            satisfied = satisfied || (values[id] == (literal > 0));
        }
        if (!terminated || (tokens >> token)) invalid("Invalid WCNF clause terminator.");
        if (!satisfied) {
            if (weight >= top) invalid("Solver model violates a hard clause.");
            cost += weight;
            if (cost > INT_MAX - 1) invalid("WCNF objective overflow.");
        }
    }
    if (stream.bad() || !header || clauses != static_cast<long long>(problem.nclauses))
        invalid("WCNF clause count mismatch or read error.");
    return static_cast<int>(cost);
}
struct DecodedGate { int op = 0; std::size_t left = 0, right = 0; };
std::vector<DecodedGate> decode(const WcnfProblem &problem, const std::vector<std::int8_t> &model, int cost) {
    std::vector<DecodedGate> decoded(problem.gates.size());
    std::vector<int> consumers(problem.gates.size(), 0);
    bool used = false;
    int operators = 0;
    auto selected = [&](const auto &ids, int expected) {
        int count = 0; std::size_t selected = 0;
        for (std::size_t i = 0; i < ids.size(); ++i) if (model[ids[i]]) { ++count; selected = i; }
        if (count != expected) invalid("Invalid gate/operator/source assignment.");
        return selected;
    };
    const std::size_t inputs = problem.variables.size() + 2;
    for (std::size_t i = 0; i < decoded.size(); ++i) {
        const WcnfGate &gate = problem.gates[i];
        DecodedGate &item = decoded[i];
        item.op = static_cast<int>(selected(gate.kind, 1));
        item.left = selected(gate.left, item.op == 0 ? 0 : 1);
        item.right = selected(gate.right, item.op == 0 || item.op == 4 ? 0 : 1);
        if (item.op == 0) { if (used) invalid("Unused gates do not form a prefix."); continue; }
        used = true; ++operators;
        if (item.op != 4 && item.left > item.right) invalid("Commutative gate sources are not ordered.");
        auto consume = [&](std::size_t source) {
            if (source >= inputs) {
                const std::size_t previous = source - inputs;
                if (previous >= i || !decoded[previous].op) invalid("Gate consumes an unused or future source.");
                ++consumers[previous];
            }
        };
        consume(item.left); if (item.op != 4) consume(item.right);
    }
    if (decoded.empty() || !decoded.back().op || operators != cost)
        invalid("Solver cost does not match the decoded formula.");
    for (std::size_t i = 0; i + 1 < decoded.size(); ++i)
        if (consumers[i] != (decoded[i].op ? 1 : 0)) invalid("Decoded operators do not form a formula tree.");
    return decoded;
}
void verify_truth(const WcnfProblem &problem, const std::vector<DecodedGate> &gates,
                  const std::vector<std::int8_t> &model) {
    const std::size_t inputs = problem.variables.size() + 2;
    std::vector<bool> gate_values(gates.size());
    for (std::size_t row = 0; row < problem.target.size(); ++row) {
        auto value = [&](std::size_t source) {
            return source == 1 || (source >= 2 && source < inputs && problem.input(row, source - 2)) ||
                   (source >= inputs && gate_values[source - inputs]);
        };
        for (std::size_t i = 0; i < gates.size(); ++i) {
            const DecodedGate &gate = gates[i];
            if (!gate.op) continue;
            const bool left = value(gate.left), right = gate.op == 4 ? false : value(gate.right);
            const bool result = gate.op == 1 ? left && right : gate.op == 2 ? left || right :
                                gate.op == 3 ? left != right : !left;
            const WcnfGate &ids = problem.gates[i];
            if (model[ids.left_value[row]] != left || model[ids.out[row]] != result ||
                (gate.op != 4 && model[ids.right_value[row]] != right))
                invalid("Solver model violates decoded operator semantics.");
            gate_values[i] = result;
        }
        if (gate_values.back() != problem.target[row]) invalid("Decoded formula failed the truth-table equivalence check.");
    }
}
std::string render(const WcnfProblem &problem, const std::vector<DecodedGate> &gates) {
    struct Event { std::size_t source; int parent; std::string text; bool literal; };
    const std::size_t inputs = problem.variables.size() + 2;
    std::vector<Event> events = {{inputs + gates.size() - 1, 0, "", false}};
    std::string expression;
    const int precedence[] = {0, 3, 1, 2, 4};
    const char *symbols[] = {"", " & ", " | ", " ^ ", "!"};
    while (!events.empty()) {
        const Event event = events.back(); events.pop_back();
        if (event.literal) { expression += event.text; continue; }
        if (event.source < inputs) {
            expression += event.source == 0 ? "false" : event.source == 1 ? "true" : problem.variables[event.source - 2];
            continue;
        }
        const DecodedGate &gate = gates[event.source - inputs];
        const int level = precedence[gate.op];
        const bool parens = level < event.parent;
        if (parens) { expression += '('; events.push_back({0, 0, ")", true}); }
        if (gate.op == 4) expression += '!';
        else {
            events.push_back({gate.right, level, "", false});
            events.push_back({0, 0, symbols[gate.op], true});
        }
        events.push_back({gate.left, level, "", false});
    }
    return expression;
}
} // namespace

WcnfSolution interpret_rwbo_output(const WcnfProblem &problem, const std::string &output) {
    validate_problem(problem);
    WcnfSolution result;
    if (problem.has_shortcut) {
        result.has_expression = true; result.expression = problem.shortcut;
        result.operator_count = 0; result.optimal = true; result.verified = true;
        result.status = "TRIVIAL OPTIMUM";
        return result;
    }
    const ParsedOutput parsed = parse_output(output);
    result.status = parsed.status;
    if ((parsed.status != "OPTIMUM FOUND" && parsed.status != "SATISFIABLE") || parsed.model_lines.empty()) return result;
    const std::vector<std::int8_t> model = assignment(parsed, problem.nvars);
    const int cost = check_wcnf(problem, model);
    if (parsed.has_cost && cost != parsed.cost) invalid("Solver cost does not match its model.");
    const std::vector<DecodedGate> gates = decode(problem, model, cost);
    verify_truth(problem, gates, model);
    result.expression = render(problem, gates);
    result.has_expression = true; result.operator_count = cost;
    result.verified = true; result.optimal = parsed.status == "OPTIMUM FOUND";
    return result;
}
} // namespace rcudd
