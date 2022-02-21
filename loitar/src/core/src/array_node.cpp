#include "loitar/core/array_node.hpp"
#include "loitar/core/nil_node.hpp"
#include "spdlog/spdlog.h"

namespace loitar {

ArrayNode::ArrayNode(std::vector<size_t> dimensions)
    : AtomNode("nil")
{
    m_dimensions = dimensions;
    size_t num_elements = dimensions.size() == 0 ? 0 : dimensions[0];

    for (auto i = dimensions.begin() + 1; i != dimensions.end(); ++i) {
        num_elements *= *i;
    }

    for (auto i = 0; i < num_elements; i++) {
        m_elements.push_back(std::make_shared<NilNode>());
    }
}

std::string ArrayNode::name() const
{
    return "ArrayNode";
}

std::any ArrayNode::value() const
{
     auto result = std::vector<std::any>();
    for (auto node : m_elements) {
        result.push_back(node->value());
    }
    return result;
}

std::string ArrayNode::get_token() const
{
    return "";
}

bool ArrayNode::is_container() const
{
    return true;
}

std::vector<std::shared_ptr<Node>> ArrayNode::get_elements() const
{
    return m_elements;
}

std::shared_ptr<Node> ArrayNode::get_element(std::vector<size_t> index) const
{
    auto result_index = get_index(index);
    if (result_index == -1) {
        spdlog::error("get_element(): request index size must equal number of array dimensions");
        return nullptr;
    }

    return m_elements[result_index];
}

void ArrayNode::set_element(std::vector<size_t> index, std::shared_ptr<Node> node)
{
    auto result_index = get_index(index);
    if (result_index == -1) {
        spdlog::error("set_element(): request index size must equal number of array dimensions");
        return;
    }

    m_elements[result_index] = node;
}

void ArrayNode::print(std::ostream& out) const
{
    std::vector<size_t> index;
    for (auto d : m_dimensions) {
        index.push_back(0);
    }
    out << print_array(m_dimensions.size() - 1, index, 0);
}

std::string ArrayNode::print_array(size_t dim_index, std::vector<size_t>& index, size_t depth) const
{
    spdlog::trace("print_array() dim_index {} depth {}", dim_index, depth);
    assert(depth < m_dimensions.size());
    std::string result = "";
    if (dim_index > 0) {
        result += "(";
        for (size_t i = 0; i < m_dimensions[dim_index]; i++) {
            index[dim_index] = i;
            result += i == 0 ? "" : " ";

            result += print_array(dim_index - 1, index, depth + 1);
        }
        result += ")";

    } else {
        result += "(";
        for (size_t i = 0; i < m_dimensions[dim_index]; i++) {
            index[dim_index] = i;
            result += i == 0 ? "" : " ";
            result += get_element(index)->to_string();
        }
        result += ")";
    }
    return result;
}

/**
 * Given a multi-dimensional index, return the index of
 * the underlying single-dimensional vector of this array
 *
 *  for dimension = last dim to first dim + 1:
 *         lower_dim = multiply all dimensions lower than current
 *       result_index += lower_dim * current index
 *  result_index += i0
 **/
size_t ArrayNode::get_index(std::vector<size_t> index) const
{
    if (index.size() < m_dimensions.size()) {
        return -1;
    }

    size_t result_index = 0;

    if (m_dimensions.size() > 1) {
        for (auto i = m_dimensions.size(); i-- > 1;) {
            size_t lower_dimensions = 1;

            for (size_t j = 0; j < i; j++) {
                lower_dimensions *= m_dimensions[j];
            }

            result_index += lower_dimensions * index[i];
        }
    }

    result_index += index[0];
    spdlog::trace("get_index(): result_index: {}", result_index);
    return result_index;
}
}
