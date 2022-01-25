#include "loitar/core/atom_node.hpp"
#include "loitar/core/environment.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/parser.hpp"
#include "loitar/core/repl.hpp"
#include "loitar/core/string_node.hpp"
#include "loitar/core/syslib/syslib.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("Add function should evaluate", "")
{
    Environment env;
    syslib::apply_syslib(env);

    std::vector<std::shared_ptr<Node>>
        elements = {
            std::make_shared<AtomNode>("+"),
            std::make_shared<IntegerNode>("200", 200),
            std::make_shared<IntegerNode>("300", 300)
        };
    std::vector<std::shared_ptr<Node>> expr = {
        std::make_shared<ListNode>(elements)
    };
    auto results = evaluate(env, expr);
    REQUIRE(results.messages.size() == 0);
    auto int0 = std::any_cast<int64_t>(results.value[0]->value());
    REQUIRE(int0 == 500);
}

TEST_CASE("Subtract function should evaluate", "")
{
    Environment env;
    syslib::apply_syslib(env);

    std::vector<std::shared_ptr<Node>>
        elements = {
            std::make_shared<AtomNode>("-"),
            std::make_shared<IntegerNode>("200", 200),
            std::make_shared<IntegerNode>("300", 300)
        };
    std::vector<std::shared_ptr<Node>> expr = {
        std::make_shared<ListNode>(elements)
    };
    auto results = evaluate(env, expr);
    REQUIRE(results.messages.size() == 0);
    auto int0 = std::any_cast<int64_t>(results.value[0]->value());
    REQUIRE(int0 == -100);
}

TEST_CASE("Multiply function should evaluate", "")
{
    Environment env;
    syslib::apply_syslib(env);

    std::vector<std::shared_ptr<Node>>
        elements = {
            std::make_shared<AtomNode>("*"),
            std::make_shared<IntegerNode>("2", 2),
            std::make_shared<IntegerNode>("3", 3)
        };
    std::vector<std::shared_ptr<Node>> expr = {
        std::make_shared<ListNode>(elements)
    };
    auto results = evaluate(env, expr);
    REQUIRE(results.messages.size() == 0);
    auto int0 = std::any_cast<int64_t>(results.value[0]->value());
    REQUIRE(int0 == 6);
}

TEST_CASE("Mod function should evaluate", "")
{
    const std::string expression = "(mod 4 16) (mod 16 4)";
    const std::string expected = "4/n0";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    REQUIRE("4" == result.value.front()->to_string());
    REQUIRE("0" == result.value.back()->to_string());
}
