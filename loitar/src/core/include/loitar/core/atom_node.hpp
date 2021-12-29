#pragma once

#include "node.hpp"
#include <iostream>
#include <string>
#include <vector>

namespace loitar {
class AtomNode : public Node {
public:
    AtomNode(std::string token);
    std::string name() const;
    std::string get_token() const;
    bool operator==(const Node& node) const;
    bool operator!=(const Node& node) const;
    bool operator<(const Node& node) const;
    bool operator<=(const Node& node) const;
    bool operator>(const Node& node) const;
    bool operator>=(const Node& node) const;

protected:
    std::string m_token;
    void print(std::ostream& os) const;
};

}
