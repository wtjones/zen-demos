#include "loitar/core/syslib/lists.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

Function list(Environment& env)
{
    Function func {
        .name = "list",
        .eval_params = true,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.list with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            result.value = std::make_shared<ListNode>(params);
            return result;
        }
    };
    return func;
}

void apply_syslib_lists(Environment& env)
{
    env.add_function(list(env));
}

}
