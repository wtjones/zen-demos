#ifndef LAR_PARSE_H
#define LAR_PARSE_H

#include "expression.h"

#define LAR_PARSE_EXPRESSIONS_MAX 10
#define LAR_PARSE_DEPTH_MAX 10
#define LAR_PARSE_TOKEN_MAX 255

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

int lar_parse_expression(char* exp);

/**
 * @brief Parse a single expression from a string
 *
 * @param raw
 * @param node
 * @return LarParseResult
 */
LarParseResult lar_parse_single(
    const char* raw, LarNode** node);

/**
 * @brief Parse expressions from a string input
 *
 * @param raw
 * @param expressions
 * @return LarParseResult
 */
LarParseResult lar_parse(
    const char* raw, LarExpressions* expressions);

#endif
