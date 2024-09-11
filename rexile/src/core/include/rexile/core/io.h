#ifndef CORE_IO_H
#define CORE_IO_H

#include "rexile/core/deck.h"
#include <stdbool.h>
#include <stdio.h>

/**
 * @brief Load a card stack from a file
 *
 * Expected format:
 *  A ♠
 *  10 ♣
 *  J ♦
 *  Q ♥
 *
 * @param path
 * @param stack
 * @return true
 * @return false
 */
bool card_stack_load(const char* path, CardStack* stack);

#endif
