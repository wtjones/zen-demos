#include "loitar/core/string_node.hpp"

namespace loitar {

StringNode::StringNode(std::string token, std::string value)
    : AtomNode(token)
{
    m_value = value;
}

std::string StringNode::name() const
{
    return "StringNode";
}

std::any StringNode::value() const
{
    return std::any(m_value);
}

void StringNode::print(std::ostream& out) const
{
    out << m_value;
}
}
