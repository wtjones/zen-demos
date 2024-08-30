
#include "rexile/core/deck.h"
#include "rexile/core/game.h"
#include "test_support.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

int test_stack_can_push()
{
    // Arrange
    CardStack stack;
    card_stack_clear(&stack);

    Card card = { .suit = HEARTS, .rank = ACE };

    // Act
    size_t count = card_stack_push(&stack, card);

    // Assert
    assert(count == 1);
    assert(stack.cards[0].suit == HEARTS);
    assert(stack.cards[0].rank == ACE);

    return 0;
}

int test_stack_can_pop()
{
    // Arrange
    CardStack stack;
    card_stack_clear(&stack);

    Card card = { .suit = HEARTS, .rank = ACE };
    card_stack_push(&stack, card);

    // Act
    Card popped = card_stack_pop(&stack);

    // Assert
    assert(popped.suit == HEARTS);
    assert(popped.rank == ACE);

    return 0;
}

int test_stack_can_populate()
{
    // Arrange
    CardStack stack;
    card_stack_clear(&stack);

    Card source_cards[] = {
        { .suit = HEARTS, .rank = ACE },
        { .suit = HEARTS, .rank = TWO },
        { .suit = HEARTS, .rank = THREE }
    };

    // Act
    card_stack_populate(&stack, source_cards, sizeof(source_cards) / sizeof(source_cards[0]));

    // Assert
    assert(stack.count == 3);
    assert(stack.cards[0].rank == ACE);
    assert(stack.cards[1].rank == TWO);
    assert(stack.cards[2].rank == THREE);

    return 0;
}

int test_empty_deck_loses()
{
    // Arrange
    return 0;
}

int test_invalid_placement_returns_invald()
{
    // Arrange
    Card source_cards[] = {
        { .suit = CLUBS, .rank = KING },
        { .suit = HEARTS, .rank = TWO },
        { .suit = HEARTS, .rank = QUEEN }
    };
    CardStack deck;
    card_stack_clear(&deck);
    card_stack_populate(&deck, source_cards, sizeof(source_cards) / sizeof(source_cards[0]));

    Game game;
    game_init2(&game, &deck);

    // Act
    BoardCellPosition pos = { .row = 1, .col = 1 };
    GameResult result = game_action_place(&game, pos);
    log_info("Game action result: %d", result);

    // Assert
    bool pass = result == GAME_RESULT_INVALID;
    return !pass;
}

TestFn test_fns[] = {
    { "test_stack_can_push", test_stack_can_push },
    { "test_stack_can_pop", test_stack_can_pop },
    { "test_stack_can_populate", test_stack_can_populate },
    { "test_empty_deck_loses", test_empty_deck_loses },
    { "test_invalid_placement_returns_invald", test_invalid_placement_returns_invald }
};

int main(int argc, const char** argv)
{
    FILE* log_file = fopen("/tmp/rexile.log", "w");
    log_add_fp(log_file, LOG_INFO);
    log_set_quiet(1);
    log_set_level(LOG_INFO);

    if (argc == 1) {
        fprintf(stderr, "Test name required.\n");
        return 1;
    }

    size_t num_fns = sizeof(test_fns) / sizeof(test_fns[0]);

    TestFn* test_fn = test_fn_lookup(test_fns, num_fns, argv[1]);
    if (test_fn == NULL) {
        fprintf(stderr, "Error: Test %s not found.\n", argv[1]);
        return 1;
    }
    return test_fn->fn();
}
