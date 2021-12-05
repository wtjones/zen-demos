#pragma once

#include "atom_node.hpp"
#include <string>
#include <vector>

namespace loitar {
class IntegerNode : public AtomNode {
public:
    IntegerNode(std::string token, int64_t value);
    int64_t get_value();

private:
    std::string m_token;
    int64_t m_value;
};
}
