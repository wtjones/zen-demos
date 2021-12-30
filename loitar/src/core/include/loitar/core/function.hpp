#pragma once

#include "environment.hpp"
#include "evaluator_types.hpp"
#include "list_node.hpp"
#include "node.hpp"
#include <functional>
#include <string>

namespace loitar {

typedef struct Function {
    std::string name;
    bool eval_params;
    std::function<EvaluatorNodeResult(std::vector<std::shared_ptr<Node>>)> body;
} Function;

}
