#pragma once

#include "spdlog/spdlog.h"

namespace loitar {

struct NodeVisitor {
    virtual void visit(class ArrayNode&) {};
    virtual void visit(class AtomNode&) {};
    virtual void visit(class IntegerNode&) {};
    virtual void visit(class ListNode&) {};
    virtual void visit(class NilNode&) {};
    virtual void visit(class StringNode&) {};
    virtual void visit(class TrueNode&) {};
};
}
