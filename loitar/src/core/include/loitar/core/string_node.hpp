#pragma once

#include "atom_node.hpp"
#include <string>
#include <vector>

namespace loitar {
class StringNode : public AtomNode {
public:
    StringNode(std::string token, std::string value);
    std::string name() const;
    std::any value() const;

protected:
    void print(std::ostream& os) const;

private:
    std::string m_token;
    std::string m_value;
};
}
