#ifndef LAR_REPR_H
#define LAR_REPR_H

#include "expression.h"

#define LAR_REPR_INDENT 2
#define LAR_REPR_MAX_LINE 255
static const char* LAR_REPR_LIST = "[List]";
static const char* LAR_REPR_ATOM_SYMBOL = "[Atom: symbol]";
static const char* LAR_REPR_ATOM_STRING = "[Atom: string]";
static const char* LAR_REPR_ATOM_BOOLEAN = "[Atom: boolean]";
static const char* LAR_REPR_ATOM_INTEGER = "[Atom: integer]";
static const char* LAR_REPR_ATOM_FIXED = "[Atom: fixed 16.16]";

/**
 * @brief Returns a formated s-expression.
 *
 * @param node
 * @return char* Formatted expression. Must be freed.
 */
char* lar_repr_expression(LarNode* node);

#endif
