#ifndef LAR_PARSE_H
#define LAR_PARSE_H

#include "expression.h"

/**
 * @brief Parse expressions from a string input
 * The script must be freed.
 *
 * @param raw
 * @param script
 * @return LarParseResult
 */
LarParseResult lar_parse_script(
    const char* raw, LarScript** script);

/**
 * @brief Parse a single expression from a string
 * The expression must be freed.
 *
 * @param raw
 * @param node
 * @return LarParseResult
 */
LarParseResult lar_parse_single(
    const char* raw, LarNode** node);

#endif
