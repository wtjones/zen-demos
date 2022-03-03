#pragma once

#include "atom_node.hpp"
#include "list_node.hpp"
#include "node.hpp"
#include <string>
#include <vector>

namespace loitar {

class NilNode : public AtomNode {
public:
    NilNode();
    std::string name() const;
    std::any value() const;
    bool operator==(const Node& node) const;
    bool operator!=(const Node& node) const;
    void accept(NodeVisitor& dv) { dv.visit(*this); }

protected:
    void print(std::ostream& os) const;
};
}
