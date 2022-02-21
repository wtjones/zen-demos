#pragma once

#include "atom_node.hpp"
#include <string>
#include <vector>

namespace loitar {
class ArrayNode : public AtomNode {
public:
    ArrayNode(std::vector<size_t> dimensions);
    std::string name() const;
    std::any value() const;
    bool is_container() const;
    std::vector<std::shared_ptr<Node>> get_elements() const;
    std::string get_token() const;
    std::shared_ptr<Node> get_element(std::vector<size_t> index) const;
    void set_element(std::vector<size_t> index, std::shared_ptr<Node> node);

protected:
    void print(std::ostream& os) const;

private:
    std::vector<size_t> m_dimensions;
    std::vector<std::shared_ptr<Node>> m_elements;
    size_t get_index(std::vector<size_t> index) const;
    std::string print_array(size_t dim_index, std::vector<size_t>& index, size_t depth) const;
};
}
