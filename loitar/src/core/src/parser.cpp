#include "loitar/core/parser.hpp"
#include "loitar/core/atom_node.hpp"
#include "loitar/core/list_node.hpp"
#include <memory>
#include <string>
#include <vector>

namespace loitar {
std::vector<std::shared_ptr<Node>> parse(std::string input)
{
    auto pos = 0;
    return parse_expression(input, pos); //std::make_shared<AtomNode>(input);
}

std::vector<std::shared_ptr<Node>> parse_expression(std::string input, int& pos)
{
    auto result = std::make_shared<std::vector<std::shared_ptr<Node>>>();
    auto done = false;

    auto nextCharPos = input.find_first_not_of(" \n\t", pos);
    done = nextCharPos == std::string::npos;
    while (!done) {
        auto nextChar = input.substr(nextCharPos, 1);
        if (nextChar == "(") {
            pos = nextCharPos;
            auto list = parse_list(input, pos);
            result->push_back(list);
        } else {
            pos = nextCharPos;
            auto atom = parse_atom(input, pos);
            result->push_back(atom);
        }
        pos++;
        nextCharPos = input.find_first_not_of(" \n\t", pos);
        done = nextCharPos == std::string::npos;
    }
}

std::shared_ptr<AtomNode> parse_atom(std::string input, int& pos)
{
}

std::shared_ptr<ListNode> parse_list(std::string input, int& pos)
{
    spdlog::trace("parse_list: parsing at pos " + std::to_string(pos));
    auto elements = parse_expression(input, pos);
    return std::make_shared<ListNode>(elements);
}

}
