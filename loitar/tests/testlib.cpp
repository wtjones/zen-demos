#define CATCH_CONFIG_RUNNER
#include "loitar/core/atom_node.hpp"
#include "loitar/core/parser.hpp"
#include "spdlog/sinks/basic_file_sink.h"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("Atoms should parse", "")
{
    const std::string expression = "(list 1 2)";
    auto actual = parse(expression);

    auto elements = std::dynamic_pointer_cast<ListNode>(actual.front())->get_elements();

    REQUIRE(elements.size() == 3);

    auto a0 = std::dynamic_pointer_cast<AtomNode>(elements[0]);
    auto a1 = std::dynamic_pointer_cast<AtomNode>(elements[1]);
    auto a2 = std::dynamic_pointer_cast<AtomNode>(elements[2]);

    bool result = a0->get_token() == "list"
        && a1->get_token() == "1"
        && a2->get_token() == "2";

    REQUIRE(result);
}

TEST_CASE("Lists should parse", "")
{
    const std::string expression = "(list 1 2 (list 8 9))";
    auto actual = parse(expression);

    auto elements = std::dynamic_pointer_cast<ListNode>(actual.front())->get_elements();
    REQUIRE(elements.size() == 4);

    auto atom0 = std::dynamic_pointer_cast<AtomNode>(elements[0]);

    REQUIRE(atom0->get_token() == "list");
    REQUIRE(
        std::dynamic_pointer_cast<AtomNode>(elements[1])->get_token() == "1");

    // nested list
    auto list1_elements = std::dynamic_pointer_cast<ListNode>(elements[3])->get_elements();
    REQUIRE(list1_elements.size() == 3);
    REQUIRE(
        std::dynamic_pointer_cast<AtomNode>(list1_elements[1])->get_token() == "8");
}

int main(int argc, char* argv[])
{
    // global setup...
    auto logger = spdlog::basic_logger_mt("test_logger", "/tmp/loitar.log");
    spdlog::set_default_logger(logger);
    spdlog::set_level(spdlog::level::trace);
    spdlog::trace("Starting tests...");
    int result = Catch::Session().run(argc, argv);

    // global clean-up...

    return result;
}
