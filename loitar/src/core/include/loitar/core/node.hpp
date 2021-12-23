#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <typeinfo>
#include <vector>

namespace loitar {
class Node : public std::enable_shared_from_this<Node> {

private:
    std::weak_ptr<Node> m_parent;

public:
    Node();
    virtual ~Node();
    virtual std::string name() const = 0;
    std::weak_ptr<Node> parent();
    void parent(std::weak_ptr<Node> parent_node);
    friend std::ostream& operator<<(std::ostream& out, const Node& node);

protected:
    virtual void print(std::ostream& os) const = 0;
};
}
