#pragma once

#include <string>
#include <vector>

namespace loitar {
enum MessageLevel {
    info,
    warn,
    error
};

typedef struct ResultMessage {
    MessageLevel level;
    std::string message;
} ResultMessage;

template<typename T>
struct Result {
    T value;
    std::vector<ResultMessage> messages;
};
}
