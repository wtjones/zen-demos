#pragma once

#include "atom_node.hpp"
#include <string>
#include <vector>

namespace loitar {
class IntegerNode : public AtomNode {
public:
    IntegerNode(std::string token, int64_t value);
    std::string name() const;
    std::any value() const;
    int64_t value2() const; // FIXME
    bool operator==(const Node& node) const;
    bool operator<(const Node& node) const;
    bool operator<=(const Node& node) const;
    void accept(NodeVisitor& dv) { dv.visit(*this); }

protected:
    void print(std::ostream& os) const;

private:
    std::string m_token;
    int64_t m_value;
};
}
