#include "loitar/core/atom_node.hpp"

namespace loitar {

AtomNode::AtomNode(std::string token)
{
    m_token = token;
}

std::string AtomNode::name() const
{
    return "AtomNode";
}

std::string AtomNode::get_token() const
{
    return m_token;
}

bool AtomNode::operator==(const Node& node) const
{
    if (node.name() != this->name()) {
        return false;
    }

    return (dynamic_cast<const AtomNode&>(node)).get_token() == m_token;
}

bool AtomNode::operator!=(const Node& node) const
{
    return !(*this == node);
}

void AtomNode::print(std::ostream& out) const
{
    out << m_token;
}
}
