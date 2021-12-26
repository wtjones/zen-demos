#include "loitar/core/environment.hpp"
#include "loitar/core/function.hpp"
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
