#define CATCH_CONFIG_RUNNER
#include "loitar/core/parser.hpp"
#include "spdlog/sinks/basic_file_sink.h"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <string>

using namespace loitar;

TEST_CASE("String node should print", "[StringNode::to_string]")
{
    auto expected = 42;
    auto actual = get_val();
    REQUIRE(expected == actual);
}

int main(int argc, char* argv[])
{
    // global setup...
    auto logger = spdlog::basic_logger_mt("test_logger", "/tmp/loitar.log");
    spdlog::set_default_logger(logger);
    spdlog::set_level(spdlog::level::trace);

    int result = Catch::Session().run(argc, argv);

    // global clean-up...

    return result;
}
