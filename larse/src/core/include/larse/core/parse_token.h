#ifndef LAR_PARSE_TOKEN_H
#define LAR_PARSE_TOKEN_H

#include "expression.h"
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LAR_PARSE_EXPRESSIONS_MAX 10
#define LAR_PARSE_DEPTH_MAX 10
#define LAR_PARSE_TOKEN_MAX 255

/**
 * @brief Tries to parse a boolean out of given position.
 * If a valid boolean [yes|no], node is populated and result is OK
 * If not a boolean, result is PASS.
 *
 * @param file_buffer
 * @param buffer_pos
 * @param node
 * @return LarParseResult
 */
LarParseResult parse_token_atom_boolean(const char* file_buffer,
    int* buffer_pos,
    LarNode* node);

/**
 * @brief Tries to parse an integer out of given position.
 * A valid integer consists of an optional sign char followed
 * by digits.
 * If a valid integer, node is populated and result is OK.
 * If not a valid integer, result is PASS.
 *
 * @param file_buffer
 * @param buffer_pos
 * @param node
 * @return LarParseResult
 */
LarParseResult parse_token_atom_integer(
    const char* file_buffer,
    int* buffer_pos,
    LarNode* node);

/**
 * @brief Tries to parse a fixed point decimal out of given position.
 * A valid decimal consists of an optional sign char followed
 * by digits with a decimal point.
 * The result is a 16.16 fixed point numeric.
 *
 * If a valid decimal, node is populated and result is OK.
 * If not a valid integer, result is PASS.
 *
 * @param file_buffer
 * @param buffer_pos
 * @param node
 * @return LarParseResult
 */
LarParseResult parse_token_atom_fixed(
    const char* file_buffer,
    int* buffer_pos,
    LarNode* node);

/**
 * @brief Tries to parse a string out of given position.
 * If a valid string, node is populated and result is OK
 * If a string but not valid, result is ERROR.
 * If not a string, result is PASS.
 * A valid string starts with a double quote.
 *
 * @param file_buffer
 * @param buffer_pos
 * @param node
 * @return LarParseResult
 */
LarParseResult parse_token_atom_string(const char* file_buffer,
    int* buffer_pos,
    LarNode* node);

LarParseResult parse_token_atom_symbol(
    const char* file_buffer,
    int* buffer_pos,
    LarNode* node);

LarParseResult parse_token_atom(
    const char* file_buffer,
    int* buffer_pos,
    LarNode* node);

#endif
