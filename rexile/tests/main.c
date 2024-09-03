
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
    assert(stack.count == 1);
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
    // Try to place face card invalid
    BoardCellPosition pos1 = { .row = 1, .col = 1 };
    GameResult result1 = game_action_place(&game, pos1);
    // Place a valid card
    BoardCellPosition pos2 = { .row = 0, .col = 1 };
    GameResult result2 = game_action_place(&game, pos2);

    // Try to place card on occupied cell
    BoardCellPosition pos3 = { .row = 0, .col = 1 };
    GameResult result3 = game_action_place(&game, pos3);

    log_info("Game action result: %d", result1);

    // Assert

    assert(result1 == GAME_RESULT_INVALID);
    assert(result2 == GAME_RESULT_OK);
    assert(result3 == GAME_RESULT_INVALID);
    assert(game.move_count == 1);

    return 0;
}

int test_combine_allows_valid_cards()
{
    // Arrange
    Card source_cards[] = {
        { .suit = CLUBS, .rank = KING },
        { .suit = CLUBS, .rank = QUEEN },
        { .suit = DIAMONDS, .rank = QUEEN },
        { .suit = CLUBS, .rank = JACK },
        { .suit = HEARTS, .rank = TWO },
        { .suit = HEARTS, .rank = THREE },
        { .suit = HEARTS, .rank = FOUR },
        { .suit = HEARTS, .rank = FIVE },
        { .suit = HEARTS, .rank = SIX },
        { .suit = HEARTS, .rank = SEVEN },
        { .suit = HEARTS, .rank = EIGHT },
        { .suit = HEARTS, .rank = NINE },
        { .suit = HEARTS, .rank = TEN },
        { .suit = HEARTS, .rank = JACK },
        { .suit = HEARTS, .rank = QUEEN },
        { .suit = HEARTS, .rank = KING },
        { .suit = SPADES, .rank = ACE },
        { .suit = SPADES, .rank = TWO },
        { .suit = SPADES, .rank = THREE },
        { .suit = SPADES, .rank = FOUR },
        { .suit = SPADES, .rank = FIVE },
        { .suit = SPADES, .rank = SIX },
        { .suit = SPADES, .rank = SEVEN },
        { .suit = SPADES, .rank = EIGHT },
        { .suit = SPADES, .rank = NINE },
        { .suit = SPADES, .rank = TEN },
        { .suit = SPADES, .rank = JACK },
        { .suit = SPADES, .rank = QUEEN },
        { .suit = SPADES, .rank = KING }
    };
    CardStack deck;
    card_stack_clear(&deck);
    card_stack_populate(&deck, source_cards, sizeof(source_cards) / sizeof(source_cards[0]));

    Game game;
    game_init2(&game, &deck);

    // Fill board with face cards on the first row
    for (int r = 0; r < 4; r++) {
        for (int c = 0; c < 4; c++) {
            BoardCellPosition pos = { .row = r, .col = c };
            game_action_place(&game, pos);
        }
    }

    assert(game.state == GAME_COMBINE);

    // Act
    // Single card < 10 is inadaquate
    BoardCellPosition positions[] = {
        { .row = 1, .col = 0 }
    };

    GameResult result1 = game_action_combine(
        &game, positions, sizeof(positions) / sizeof(positions[0]));

    log_info("Game action result: %d", result1);

    // Cannot discard face cards
    BoardCellPosition positions2[] = {
        { .row = 0, .col = 0 }
    };

    GameResult result2 = game_action_combine(
        &game, positions2, sizeof(positions2) / sizeof(positions2[0]));

    // Should be able to combine with value 10
    BoardCellPosition positions3[] = {
        { .row = 1, .col = 3 },
        { .row = 2, .col = 1 }
    };

    GameResult result3 = game_action_combine(
        &game, positions3, sizeof(positions3) / sizeof(positions3[0]));

    // Assert
    assert(result1 == GAME_RESULT_INVALID);
    assert(result2 == GAME_RESULT_INVALID);
    assert(result3 == GAME_RESULT_OK);

    return 0;
}

TestFn test_fns[] = {
    { "test_stack_can_push", test_stack_can_push },
    { "test_stack_can_pop", test_stack_can_pop },
    { "test_stack_can_populate", test_stack_can_populate },
    { "test_empty_deck_loses", test_empty_deck_loses },
    { "test_invalid_placement_returns_invald", test_invalid_placement_returns_invald },
    { "test_combine_allows_valid_cards", test_combine_allows_valid_cards }
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
