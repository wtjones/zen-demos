#include "loitar/core/string_node.hpp"

namespace loitar {

StringNode::StringNode(std::string token, std::string value)
    : AtomNode(token)
{
    m_value = value;
}

std::string StringNode::get_value()
{
    return m_value;
}
}
