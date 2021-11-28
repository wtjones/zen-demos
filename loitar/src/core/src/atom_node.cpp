#include "loitar/core/atom_node.hpp"

namespace loitar {

AtomNode::AtomNode(std::string symbol)
{
    m_symbol = symbol;
}
//AtomNode::~AtomNode() { }
std::string AtomNode::get_symbol()
{
    return m_symbol;
}
}