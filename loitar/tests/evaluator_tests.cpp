#include "loitar/core/atom_node.hpp"
#include "loitar/core/environment.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/parser.hpp"
#include "loitar/core/string_node.hpp"
#include "loitar/core/syslib/syslib.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("Scalars should evaluate", "")
{
    Environment env;
    syslib::apply_syslib(env);

    std::vector<std::shared_ptr<Node>> expr {
        std::make_shared<IntegerNode>("100", 100),
        std::make_shared<IntegerNode>("200", 200)
    };
    auto results = evaluate(env, expr);
    REQUIRE(expr.size() == results.value.size());
    REQUIRE(results.messages.size() == 0);

    auto int0 = std::any_cast<int64_t>(results.value[0]->value());
    REQUIRE(int0 == 100);
    auto int1 = std::any_cast<int64_t>(results.value[1]->value());
    REQUIRE(int1 == 200);
}

TEST_CASE("List without function should not evaluate", "")
{
    Environment env;
    syslib::apply_syslib(env);

    std::vector<std::shared_ptr<Node>> elements = {
        std::make_shared<IntegerNode>("200", 200)
    };
    std::vector<std::shared_ptr<Node>> expr = {
        std::make_shared<ListNode>(elements)
    };
    auto results = evaluate(env, expr);
    REQUIRE(results.value.size() == 0);
    REQUIRE(results.messages.size() == 1);
}

TEST_CASE("List params should evaluate", "")
{
    Environment env;
    syslib::apply_syslib(env);

    std::vector<std::shared_ptr<Node>>
        inner_elements = {
            std::make_shared<AtomNode>("+"),
            std::make_shared<IntegerNode>("1", 1),
            std::make_shared<IntegerNode>("1", 1)
        };
    std::vector<std::shared_ptr<Node>>
        elements = {
            std::make_shared<AtomNode>("+"),
            std::make_shared<IntegerNode>("2", 2),
            std::make_shared<IntegerNode>("2", 2),
            std::make_shared<ListNode>(inner_elements)
        };
    std::vector<std::shared_ptr<Node>> expr = {
        std::make_shared<ListNode>(elements)
    };
    auto results = evaluate(env, expr);
    REQUIRE(results.messages.size() == 0);
    auto int0 = std::any_cast<int64_t>(results.value[0]->value());
    REQUIRE(int0 == 6);
}
