#include "loitar/core/repl.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("aref should get element at array position", "")
{
    const std::string expression = "(aref (make-array '(2 3)) 0 2 )";
    const std::string expected = "nil";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}

TEST_CASE("aref should get character at string position", "")
{
    const std::string expression = "(aref \"abcd\" 2)";
    const std::string expected = "c";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}
