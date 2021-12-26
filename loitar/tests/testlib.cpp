#define CATCH_CONFIG_RUNNER
#include "spdlog/sinks/basic_file_sink.h"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

int main(int argc, char* argv[])
{
    // global setup...
    auto logger = spdlog::basic_logger_mt("testlib", "/tmp/loitar.log");
    logger->flush_on(spdlog::level::trace);
    spdlog::set_default_logger(logger);
    spdlog::set_level(spdlog::level::trace);
    spdlog::info("|************* Starting tests... ************|");
    int result = Catch::Session().run(argc, argv);

    // global clean-up...

    return result;
}
