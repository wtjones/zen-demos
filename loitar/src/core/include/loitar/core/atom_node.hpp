#pragma once

#include "node.hpp"
#include <string>
#include <vector>

namespace loitar {
class AtomNode : public Node {
public:
    AtomNode(std::string token);
    std::string name() const;
    std::string get_token();

private:
    std::string m_token;
};
}
