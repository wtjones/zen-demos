#include "loitar/core/list_node.hpp"

namespace loitar {
ListNode::ListNode() { }

ListNode::ListNode(
    std::vector<std::shared_ptr<Node>> elements)
{
    m_elements = elements;
    for (auto node : elements) {
        node->parent(weak_from_this());
    }
}

std::string ListNode::name() const
{
    return "ListNode";
}

std::any ListNode::value() const
{
    auto result = std::vector<std::any>();
    for (auto node : m_elements) {
        result.push_back(node->value());
    }
    return result;
}

bool ListNode::is_container() const
{
    return true;
}

std::vector<std::shared_ptr<Node>> ListNode::get_elements() const
{
    return m_elements;
}

bool ListNode::operator==(const Node& node) const
{
    return true; // FIXME
}

bool ListNode::operator!=(const Node& node) const
{
    return !(*this == node);
}

bool ListNode::operator<(const Node& node) const
{
    return false;
}

bool ListNode::operator<=(const Node& node) const
{
    return false;
}

bool ListNode::operator>(const Node& node) const
{
    return !(*this <= node);
}

bool ListNode::operator>=(const Node& node) const
{
    return !(*this < node);
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
