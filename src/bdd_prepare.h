#ifndef RCUDD_BDD_PREPARE_HPP
#define RCUDD_BDD_PREPARE_HPP

#include "rcudd.h"
#include "../inst/include/rcudd/bdd_wcnf.hpp"
#include <chrono>

namespace rcudd {
// One preparation session shares a budget across all branches of a call.
class RwboPreparation {
public:
    explicit RwboPreparation(SEXP config);
    void attach(SEXP summary, const BDD &root, const BDD &care,
                const std::vector<std::string> &names, const std::string &original,
                const std::string &care_expression);
private:
    bool enabled = false, available = false;
    WcnfOptions options;
    std::string directory;
    double time_limit = 0;
    unsigned int counter = 0;
    std::chrono::steady_clock::time_point start;
};
}
#endif
