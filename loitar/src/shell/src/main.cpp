#include "loitar/core/repl.hpp"
#include "spdlog/cfg/env.h"
#include "spdlog/sinks/basic_file_sink.h"
#include "spdlog/sinks/stdout_sinks.h"
#include "spdlog/sinks/syslog_sink.h"
#include "spdlog/spdlog.h"
#include <ctype.h>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>

static const char* execute_mode = "x";
int usage()
{
    std::cout << "usage: loitar <command>" << std::endl;
    std::cout << "Commands supported: x" << std::endl;
    return 0;
}

std::string get_arg(std::vector<std::string> args, std::string arg)
{
    int i = 1;

    while (i < args.size() + 1) {
        if (args[i] == arg) {
            i++;
            return args[i];
        }
        i++;
    }
    return "";
}

void init_logging()
{
    std::string ident = "loitar";

    auto err_sink = std::make_shared<spdlog::sinks::stderr_sink_st>();
    err_sink->set_pattern("[loitar] %v");
    err_sink->set_level(spdlog::level::err);

    auto file_sink = std::make_shared<spdlog::sinks::basic_file_sink_st>("/tmp/loitar.log", true);
    spdlog::sinks_init_list sink_list = { file_sink, err_sink };
    auto logger = std::make_shared<spdlog::logger>(ident, sink_list.begin(), sink_list.end());

    spdlog::set_default_logger(logger);
    spdlog::set_level(spdlog::level::trace);
    logger->flush_on(spdlog::level::info);
}

int execute(std::string input)
{
    loitar::Repl repl;
    auto result = repl.execute(input);
    for (auto e : result.value) {
        std::cout << *e << std::endl;
    }

    for (auto m : result.messages) {

        auto level = (const char*[]) {
            "info",
            "warning",
            "error",
        }[m.level];

        std::cout << "[eval] [" << level << "] " << m.message << std::endl;
    }
    return 0;
}

int main(int argc, char** argv)
{
    init_logging();

    std::vector<std::string> args(argv + 1, argv + argc);
    std::string out_file;
    bool verbose = false;

    if (argc >= 2) {
        if (args[0] == std::string(execute_mode)) {
            return execute(args[1]);
        }
        return usage();
    }

    return 0;
}
