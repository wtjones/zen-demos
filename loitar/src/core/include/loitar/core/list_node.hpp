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
    std::vector<std::shared_ptr<Node>> get_elements();
    bool operator==(const Node& node) const;
    bool operator!=(const Node& node) const;

protected:
    std::vector<std::shared_ptr<Node>> m_elements;
    void print(std::ostream& os) const;
};
}
