#pragma once

#include "atom_node.hpp"
#include "list_node.hpp"
#include "nil_node.hpp"
#include "node.hpp"
#include "spdlog/spdlog.h"
#include "true_node.hpp"

namespace loitar {

const int max_depth = 10;
const int max_parse = 1000; // sanity check for development

std::vector<std::shared_ptr<Node>> parse_expression(std::string input, int& pos, int depth);

std::shared_ptr<Node> parse_atom_or_nil(std::string input, int& pos, int depth);

std::shared_ptr<ListNode> parse_list(std::string input, int& pos, int depth);

std::vector<std::shared_ptr<Node>> parse(std::string input);

}
