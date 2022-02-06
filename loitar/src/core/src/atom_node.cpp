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

std::any AtomNode::value() const
{
    return std::any(m_token);
}

std::string AtomNode::get_token() const
{
    return m_token;
}

bool AtomNode::is_container() const
{
    return false;
}

std::vector<std::shared_ptr<Node>> AtomNode::get_elements() const
{
    return std::vector<std::shared_ptr<Node>>();
}

bool AtomNode::operator==(const Node& node) const
{
    if (node.name() != this->name() || !node.value().has_value()) {
        return false;
    }

    return m_token == std::any_cast<std::string>(node.value());
}

bool AtomNode::operator!=(const Node& node) const
{
    return !(*this == node);
}

bool AtomNode::operator<(const Node& node) const
{
    return m_token < std::any_cast<std::string>(node.value());
}

bool AtomNode::operator<=(const Node& node) const
{
    return m_token <= std::any_cast<std::string>(node.value());
}

bool AtomNode::operator>(const Node& node) const
{
    return !(*this <= node);
}

bool AtomNode::operator>=(const Node& node) const
{
    return !(*this < node);
}

void AtomNode::print(std::ostream& out) const
{
    out << m_token;
}
}
