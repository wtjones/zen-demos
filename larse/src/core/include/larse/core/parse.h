#ifndef LAR_PARSE_H
#define LAR_PARSE_H

#include "expression.h"

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
