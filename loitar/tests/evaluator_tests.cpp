#include "loitar/core/atom_node.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/parser.hpp"
#include "loitar/core/string_node.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("Scalars should evaluate", "")
{
    std::vector<std::shared_ptr<Node>> expr {
        std::make_shared<IntegerNode>("100", 100),
        std::make_shared<IntegerNode>("200", 200)
    };
    auto results = evaluate(expr);
    REQUIRE(expr.size() == results.value.size());
    REQUIRE(results.messages.size() == 0);
    auto int0 = std::dynamic_pointer_cast<IntegerNode>(results.value[0]);
    REQUIRE(int0->get_value() == 100);
    auto int1 = std::dynamic_pointer_cast<IntegerNode>(results.value[1]);
    REQUIRE(int1->get_value() == 200);
}
