#pragma once

#include "atom_node.hpp"
#include "list_node.hpp"
#include "nil_node.hpp"
#include "node.hpp"
#include "spdlog/spdlog.h"
#include "true_node.hpp"

namespace loitar {

const int max_depth = 100;

std::shared_ptr<Node> parse_node(std::string input, int& pos, int depth);

std::vector<std::shared_ptr<Node>> parse_expression(std::string input, int& pos, int depth);

std::shared_ptr<AtomNode> parse_atom(std::string input, int& pos, int depth);

void parse_comment(std::string input, int& pos, int depth);

std::shared_ptr<ListNode> parse_list(std::string input, int& pos, int depth);

std::vector<std::shared_ptr<Node>> parse(std::string input);

}
