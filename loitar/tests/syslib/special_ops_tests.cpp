#include "loitar/core/repl.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("Quote should return unevaluated eleemnt", "")
{
    const std::string expression = "(quote (1 2 3))";
    const std::string expected = "(1 2 3)";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}
