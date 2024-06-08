#ifndef LAR_EXPRESSION_H
#define LAR_EXPRESSION_H

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define LAR_PARSE_EXPRESSIONS_MAX 10
#define LAR_PARSE_DEPTH_MAX 10
#define LAR_PARSE_TOKEN_MAX 255

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
            char* val_symbol;
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

/**
 * @brief Represents a parsed script of top-level expressions.
 *
 */
typedef struct LarScript {
    LarNode* expressions; // array of expressions
} LarScript;

typedef enum LarParseResult {
    LAR_PARSE_RESULT_OK,
    LAR_PARSE_RESULT_ERROR,
    LAR_PARSE_RESULT_END,
    LAR_PARSE_RESULT_PASS
} LarParseResult;

typedef enum LarParseExpressionType {
    LAR_PARSE_EXP_NONE,
    LAR_PARSE_EXP_ATOM,
    LAR_PARSE_EXP_LIST_START,
    LAR_PARSE_EXP_LIST_END,
} LarParseExpressionType;

void lar_free_expression(LarNode** node);
void lar_free_script(LarScript** script);

#endif
