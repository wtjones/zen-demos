#include "loitar/core/list_node.hpp"

namespace loitar {
ListNode::ListNode(
    std::vector<std::shared_ptr<Node>> elements)
{
    m_elements = elements;
    for (auto node : elements) {
        node->parent(shared_from_this());
    }
}

std::string ListNode::name() const
{
    return "ListNode";
}

std::vector<std::shared_ptr<Node>> ListNode::get_elements()
{
    return m_elements;
}

void ListNode::print(std::ostream& out) const
{
    out << "(";
    for (std::size_t i = 0; i != m_elements.size(); ++i) {
        out << *(m_elements[i]);
        out << (i == m_elements.size() - 1 ? "" : " ");
    }
    out << ")";
}

}
