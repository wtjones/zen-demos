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

/**
 * @brief Get first node in list
 *
 * @param list
 * @return LarNode* Returns NULL if node is not a list or is empty
 */
LarNode* lar_get_first(const LarNode* list);

/**
 * @brief Get a ":property_name" from a list node
 *
 * @param list
 * @param property_name
 * @param type
 * @return LarNode* NULL if not found or type does not match
 */
LarNode* lar_get_property_by_type(
    const LarNode* list, const char* property_name, LarNodeType type);

/**
 * @brief Get the first list that starts with the given symbol
 *
 * @param list
 * @param symbol
 * @return LarNode* NULL if not found or input is not a list
 */
LarNode* lar_get_list_by_symbol(const LarNode* list, const char* symbol);

LarNode* lar_get_list_node_by_index(const LarNode* list, size_t index);

bool lar_is_symbol(const LarNode* node, const char* symbol);

#endif
