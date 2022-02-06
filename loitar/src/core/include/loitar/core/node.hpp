#pragma once

#include <any>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <typeinfo>
#include <vector>

namespace loitar {
class Node : public std::enable_shared_from_this<Node> {

private:
    const unsigned int m_id;
    std::weak_ptr<Node> m_parent;
    static unsigned int next_id;

public:
    Node();
    virtual ~Node();
    unsigned int id() const;
    virtual std::string name() const = 0;
    virtual std::any value() const = 0;
    std::weak_ptr<Node> parent();
    void parent(std::weak_ptr<Node> parent_node);
    virtual bool is_container() const = 0;
    virtual std::vector<std::shared_ptr<Node>> get_elements() const = 0;
    std::string to_string() const;
    friend std::ostream& operator<<(std::ostream& out, const Node& node);
    virtual bool operator==(const Node& node) const = 0;
    virtual bool operator!=(const Node& node) const = 0;
    virtual bool operator<(const Node& node) const = 0;
    virtual bool operator<=(const Node& node) const = 0;
    virtual bool operator>(const Node& node) const = 0;
    virtual bool operator>=(const Node& node) const = 0;

protected:
    virtual void print(std::ostream& os) const = 0;
};

}
