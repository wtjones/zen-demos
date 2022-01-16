#pragma once

#include "atom_node.hpp"
#include <string>
#include <vector>

namespace loitar {
class TrueNode : public AtomNode {
public:
    TrueNode();
    std::string name() const;
    std::any value() const;

protected:
    void print(std::ostream& os) const;
};
}
