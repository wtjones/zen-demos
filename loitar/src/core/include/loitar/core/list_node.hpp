#pragma once

#include "node.hpp"
#include <string>
#include <vector>

namespace loitar {
class ListNode : public Node {
public:
    ListNode();
    ListNode(
        std::vector<std::shared_ptr<Node>> elements);
    std::string name() const;
    std::any value() const;
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
    std::vector<std::shared_ptr<Node>> m_elements;
    void print(std::ostream& os) const;
};
}
