#pragma once

#include "list_node.hpp"
#include "node.hpp"
#include <string>
#include <vector>

namespace loitar {

class NilNode : public ListNode {
public:
    NilNode();
    std::string name() const;
    std::vector<std::shared_ptr<Node>> get_elements();
    bool operator==(const Node& node) const;
    bool operator!=(const Node& node) const;

protected:
    void print(std::ostream& os) const;

private:
    std::vector<std::shared_ptr<Node>> m_elements;
};
}
