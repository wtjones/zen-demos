#include "loitar/core/repl.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("Loop for from construct iterates", "")
{
    const std::string expression = R"(
(setq *total* 0)
(loop for a from 1 to 3
    do (setq *total* (+ a *total*)))
*total*
)";
    const std::string expected = "6";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}

TEST_CASE("Loop for from construct can return", "")
{
    const std::string expression = R"(
(loop for a from 10 to 20
    do (return a))
)";
    const std::string expected = "10";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}

TEST_CASE("Construct cond returns first true expression", "")
{
    const std::string expression = "(cond ((= 1 2)  4)(t (+ 8 8)) (t 7))";
    const std::string expected = "16";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}

TEST_CASE("Construct cond returns nil when empty", "")
{
    const std::string expression = "(cond)";
    const std::string expected = "nil";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}

TEST_CASE("Construct cond returns nil by default", "")
{
    const std::string expression = "(cond (nil 1))";
    const std::string expected = "nil";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}
