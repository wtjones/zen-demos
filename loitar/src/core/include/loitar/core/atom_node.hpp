#pragma once

#include "node.hpp"
#include <string>
#include <vector>

namespace loitar {
class AtomNode : public Node {
public:
    AtomNode(std::string symbol);
    std::string get_symbol();

private:
    std::string m_symbol;
};
}