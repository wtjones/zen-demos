#ifndef MERGE_H
#define MERGE_H

#include "expression.h"

/**
 * @brief
 *
 * @param src0
 * @param src1
 * @param dest
 */
bool lar_merge_list(LarNode* dest, const LarNode* src0, const LarNode* src1, int depth);

/**
 * @brief Apply script src1 onto src0 to produce dest.
 *
 *
 * @param src0
 * @param src1
 * @param dest
 */
LarScript* lar_merge_script(const LarScript* src0, const LarScript* src1);

#endif
