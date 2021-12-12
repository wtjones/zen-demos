#pragma once

#include "node.hpp"
#include <string>
#include <vector>

namespace loitar {
class ListNode : public Node {
public:
    ListNode(
        std::vector<std::shared_ptr<Node>> elements);
    std::string name() const;
    std::vector<std::shared_ptr<Node>> get_elements();

private:
    std::vector<std::shared_ptr<Node>> m_elements;
};
}
