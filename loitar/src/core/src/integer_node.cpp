#include "loitar/core/integer_node.hpp"

namespace loitar {

IntegerNode::IntegerNode(std::string token, int64_t value)
    : AtomNode(token)
{
    m_value = value;
}
std::string IntegerNode::name() const
{
    return "IntegerNode";
}

int64_t IntegerNode::get_value()
{
    return m_value;
}
}
