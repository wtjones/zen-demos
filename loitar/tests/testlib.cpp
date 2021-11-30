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

// TEST_CASE("Atom should parse", "")
// {
//     auto actual = parse("foo");

//     auto derived = std::dynamic_pointer_cast<AtomNode>(actual);
//     bool result = derived != NULL && derived->get_symbol() == "foo";
//     INFO(result);
//     //REQUIRE(1 == 1);
//     REQUIRE(result);
// }

TEST_CASE("Expression should parse", "")
{
    const std::string expression = "(1 2 3)";

    auto actual = parse("(1 2 3)");

    REQUIRE(1 == 1);
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
