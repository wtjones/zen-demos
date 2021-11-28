#pragma once

#include "atom_node.hpp"
#include "list_node.hpp"
#include "node.hpp"
#include "spdlog/spdlog.h"
namespace loitar {

std::vector<std::shared_ptr<Node>> parse_expression(std::string input, int& pos);

std::shared_ptr<AtomNode> parse_atom(std::string input, int& pos);

std::shared_ptr<ListNode> parse_list(std::string input, int& pos);

std::vector<std::shared_ptr<Node>> parse(std::string input);

}
