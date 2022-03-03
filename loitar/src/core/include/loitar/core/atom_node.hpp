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
    std::any value() const;
    std::string get_token() const;
    bool is_container() const;
    std::vector<std::shared_ptr<Node>> get_elements() const;
    bool operator==(const Node& node) const;
    bool operator!=(const Node& node) const;
    bool operator<(const Node& node) const;
    bool operator<=(const Node& node) const;
    bool operator>(const Node& node) const;
    bool operator>=(const Node& node) const;
    void accept(NodeVisitor& dv) { dv.visit(*this); }

protected:
    std::string m_token;
    void print(std::ostream& os) const;
};

}
