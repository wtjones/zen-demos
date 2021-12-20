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

void IntegerNode::print(std::ostream& out) const
{
    out << std::to_string(m_value);
}
}
