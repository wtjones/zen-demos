#ifndef LAR_EXPRESSION_H
#define LAR_EXPRESSION_H

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef int32_t LarFixed;

typedef enum LarNodeType {
    LAR_NODE_ATOM_SYMBOL,
    LAR_NODE_ATOM_STRING,
    LAR_NODE_ATOM_BOOLEAN,
    LAR_NODE_ATOM_INTEGER,
    LAR_NODE_ATOM_FIXED,
    LAR_NODE_LIST
} LarNodeType;

typedef struct LarNode {
    LarNodeType node_type;
    union {
        union {
            char* symbol;
            char* val_string;
            int val_integer;
            LarFixed val_fixed;
            bool val_bool;
        } atom;
        struct {
            size_t length;
            struct LarNode* nodes;
        } list;
    };
} LarNode;

typedef struct LarExpressions {
    size_t count;
    LarNode** nodes;
} LarExpressions;

#endif
