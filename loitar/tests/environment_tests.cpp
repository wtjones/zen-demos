#include "loitar/core/environment.hpp"
#include "loitar/core/function.hpp"
#include "loitar/core/integer_node.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("Should get function", "")
{
    Function f = {
        .name = "test_func",
        .body = [](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            return EvaluatorNodeResult {};
        }
    };

    Environment sut;
    sut.add_function(f);
    REQUIRE(sut.has_function(f.name) == true);
    REQUIRE(sut.get_function(f.name).name == f.name);
}

TEST_CASE("Should get variable in local scope")
{
    Environment sut;

    auto global0 = std::make_shared<IntegerNode>("", 33);
    auto local0 = std::make_shared<IntegerNode>("", 55);

    sut.push_block();

    sut.set_variable("var0", local0, ScopeType::local);
    sut.set_variable("var0", global0, ScopeType::global);
    auto result = sut.get_variable("var0");
    REQUIRE(result != nullptr);
    REQUIRE(result->id() == local0->id());

    sut.pop_block();
    result = sut.get_variable("var0");
    REQUIRE(result != nullptr);
    REQUIRE(result->id() == global0->id());
}
