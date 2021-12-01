#include "loitar/core/parser.hpp"
#include "loitar/core/atom_node.hpp"
#include "loitar/core/list_node.hpp"
#include <assert.h>
#include <memory>
#include <string>
#include <vector>

namespace loitar {
std::vector<std::shared_ptr<Node>> parse(std::string input)
{
    auto pos = 0;
    return parse_expression(input, pos, 0);
}

std::vector<std::shared_ptr<Node>> parse_expression(std::string input, int& pos, int depth)
{
    std::vector<std::shared_ptr<Node>> result;
    spdlog::trace("parse_expression: pos: {}, depth {}", std::to_string(pos), std::to_string(depth));
    assert(depth < max_depth);

    auto done = false;
    auto nextCharPos = input.find_first_not_of(" \n\t", pos);
    done = nextCharPos == std::string::npos;

    while (!done) {
        auto nextChar = input.substr(nextCharPos, 1);
        spdlog::trace("parse_expression char: '{}' pos: {}, depth {}",
            nextChar, std::to_string(pos), std::to_string(depth));
        if (nextChar == ")") {
            if (depth == 0) {
                assert("Token ')' was not expected.");
            }
            pos++;
            spdlog::trace("end of expression found");
            done = true;
        } else if (nextChar == "(") {
            pos = nextCharPos;
            auto list = parse_list(input, pos, depth + 1);
            result.push_back(list);
        } else {
            pos = nextCharPos;
            auto atom = parse_atom(input, pos, depth + 1);
            result.push_back(atom);
        }
        //pos++;
        nextCharPos = input.find_first_not_of(" \n\t", pos);
        if (nextCharPos == std::string::npos) {
            if (depth > 0) {
                assert("end of input not expected");
            }
            done = true;
        }
    }
    spdlog::trace("return from expression");
    return result;
}

std::shared_ptr<AtomNode> parse_atom(std::string input, int& pos, int depth)
{
    spdlog::trace("parse_atom: pos: {}, depth {}", std::to_string(pos), std::to_string(depth));

    auto nextCharPos = input.find_first_of(" )", pos);
    if (nextCharPos == std::string::npos) {
        spdlog::warn("Atom: expected space or ')' but found npos.");
        return nullptr;
    }

    auto nextChar = input.substr(nextCharPos, 1);
    auto token = input.substr(pos, nextCharPos - pos);
    spdlog::trace("Atom found '{}'", token);
    pos = nextCharPos;

    return std::make_shared<AtomNode>("");
}

std::shared_ptr<ListNode> parse_list(std::string input, int& pos, int depth)
{
    spdlog::trace("parse_list: pos: {}, depth {}", std::to_string(pos), std::to_string(depth));
    pos++; // TODO check for npos
    auto elements = parse_expression(input, pos, depth + 1);
    return std::make_shared<ListNode>(elements);
}

}
