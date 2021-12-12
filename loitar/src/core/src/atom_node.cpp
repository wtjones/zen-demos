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

std::string AtomNode::get_token()
{
    return m_token;
}
}
