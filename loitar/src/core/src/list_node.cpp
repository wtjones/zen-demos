#include "loitar/core/list_node.hpp"

namespace loitar {
ListNode::ListNode(
    std::vector<std::shared_ptr<Node>> elements)
{
    m_elements = elements;
}

std::vector<std::shared_ptr<Node>> ListNode::get_elements()
{
    return m_elements;
}

}
