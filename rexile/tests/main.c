
#include "rexile/core/deck.h"
#include "rexile/core/game.h"
#include "rexile/core/io.h"
#include "rexile/core/repr.h"
#include "rexile/core/score.h"
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
        { .suit = HEARTS, .rank = KING },
        { .suit = CLUBS, .rank = JACK },
        { .suit = CLUBS, .rank = KING },
        { .suit = HEARTS, .rank = JACK },
        { .suit = HEARTS, .rank = QUEEN }
    };
    CardStack deck;
    card_stack_clear(&deck);
    card_stack_populate(&deck, source_cards, sizeof(source_cards) / sizeof(source_cards[0]));

    Game game;
    game_init(&game, &deck);

    // Act
    // Invalid - Try to place face card on non-face cell
    BoardCellPosition pos1 = { .row = 1, .col = 1 };
    GameResult result1 = game_action_place(&game, pos1);

    // Valid - Place queen card on queen
    BoardCellPosition pos2 = { .row = 0, .col = 1 };
    GameResult result2 = game_action_place(&game, pos2);

    // Invalid - Try to place jack on occupied cell
    BoardCellPosition pos3 = { .row = 0, .col = 1 };
    GameResult result3 = game_action_place(&game, pos3);

    // Valid - place
    // BoardCellPosition pos3a = { .row = 1, .col = 1 };

    // Invalid - Try to place jack on king cell
    BoardCellPosition pos4 = { .row = 0, .col = 3 };
    GameResult result4 = game_action_place(&game, pos4);

    // Valid - place jack on jack
    BoardCellPosition pos4a = { .row = 1, .col = 0 };
    GameResult result4a = game_action_place(&game, pos4a);

    // Valid - place king on king cell
    BoardCellPosition pos5 = { .row = 0, .col = 3 };
    GameResult result5 = game_action_place(&game, pos5);

    log_info("Game action result: %d", result1);

    // Assert

    assert(result1 == GAME_RESULT_INVALID);
    assert(result2 == GAME_RESULT_OK);
    assert(result3 == GAME_RESULT_INVALID);
    assert(result4 == GAME_RESULT_INVALID);
    assert(result4a == GAME_RESULT_OK);
    assert(result5 == GAME_RESULT_OK);
    assert(game.move_count == 3);

    return 0;
}

int test_unable_to_place_face_card_loses()
{
    // Arrange
    Card source_cards[] = {
        { .suit = HEARTS, .rank = SEVEN },
        { .suit = HEARTS, .rank = KING },
        { .suit = CLUBS, .rank = KING },
        { .suit = CLUBS, .rank = FOUR },
        { .suit = HEARTS, .rank = THREE },
        { .suit = HEARTS, .rank = TWO }
    };
    CardStack deck;
    card_stack_clear(&deck);
    card_stack_populate(&deck, source_cards, sizeof(source_cards) / sizeof(source_cards[0]));

    Game game;
    game_init(&game, &deck);

    // Act
    BoardCellPosition pos;
    GameResult result;

    pos.row = 0;
    pos.col = 0;
    result = game_action_place(&game, pos);

    pos.row = 0;
    pos.col = 3;
    result = game_action_place(&game, pos);

    pos.row = 3;
    pos.col = 0;
    result = game_action_place(&game, pos);

    pos.row = 3;
    pos.col = 3;
    result = game_action_place(&game, pos);

    // King cells are filled and the next card is a king.
    // The game should be lost.

    log_info("Game action result: %d", result);

    // Assert

    assert(game.state == GAME_LOSE);

    return 0;
}

int test_unable_to_combine_loses()
{
    // Arrange

    // Prep deck with only face and 2 cards
    Card source_cards[] = {
        { .suit = HEARTS, .rank = KING },
        { .suit = HEARTS, .rank = QUEEN },
        { .suit = CLUBS, .rank = QUEEN },
        { .suit = CLUBS, .rank = KING },
        { .suit = HEARTS, .rank = JACK },
        { .suit = HEARTS, .rank = TWO },
        { .suit = CLUBS, .rank = TWO },
        { .suit = CLUBS, .rank = JACK },
        { .suit = SPADES, .rank = JACK },
        { .suit = SPADES, .rank = TWO },
        { .suit = DIAMONDS, .rank = TWO },
        { .suit = DIAMONDS, .rank = JACK },
        { .suit = SPADES, .rank = KING },
        { .suit = SPADES, .rank = QUEEN },
        { .suit = DIAMONDS, .rank = QUEEN },
        { .suit = DIAMONDS, .rank = KING },
        { .suit = HEARTS, .rank = ACE }
    };
    CardStack deck;
    card_stack_clear(&deck);
    card_stack_populate(&deck, source_cards, sizeof(source_cards) / sizeof(source_cards[0]));
    card_stack_reverse(&deck);

    Game game;
    game_init(&game, &deck);

    // Act
    BoardCellPosition pos;

    for (int r = 0; r < 4; r++) {
        for (int c = 0; c < 4; c++) {
            pos.row = r;
            pos.col = c;
            assert(game_action_place(&game, pos) == GAME_RESULT_OK);
        }
    }

    // Assert
    assert(game.state == GAME_LOSE);

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
        { .suit = HEARTS, .rank = FOUR },
        { .suit = HEARTS, .rank = THREE },
        { .suit = HEARTS, .rank = TWO },
        { .suit = SPADES, .rank = ACE },
        { .suit = SPADES, .rank = TWO },
        { .suit = SPADES, .rank = THREE },
        { .suit = SPADES, .rank = FOUR },
        { .suit = SPADES, .rank = FIVE },
        { .suit = SPADES, .rank = SIX },
        { .suit = SPADES, .rank = SEVEN },
        { .suit = SPADES, .rank = EIGHT },
        { .suit = SPADES, .rank = NINE },
        { .suit = SPADES, .rank = KING },
        { .suit = SPADES, .rank = QUEEN },
        { .suit = SPADES, .rank = QUEEN },
        { .suit = SPADES, .rank = KING }
    };
    CardStack deck;
    card_stack_clear(&deck);
    card_stack_populate(&deck, source_cards, sizeof(source_cards) / sizeof(source_cards[0]));

    Game game;
    game_init(&game, &deck);

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

int test_can_load_card_stack()
{
    // Arrange
    CardStack stack;

    // Act
    log_info("Loading card stack from file");
    bool result = card_stack_load("./tests/data/small_deck.txt", &stack);

    // Assert
    assert(result == true);
    assert(stack.count == 4);

    return 0;
}

int test_can_repr_move_ledger()
{
    // Arrange
    Card source_cards[] = {
        { .suit = CLUBS, .rank = KING },
        { .suit = CLUBS, .rank = QUEEN },
        { .suit = DIAMONDS, .rank = QUEEN },
        { .suit = CLUBS, .rank = KING },
        { .suit = HEARTS, .rank = TEN },
        { .suit = HEARTS, .rank = TWO },
        { .suit = HEARTS, .rank = EIGHT },
        { .suit = HEARTS, .rank = FIVE },
        { .suit = HEARTS, .rank = SIX },
        { .suit = HEARTS, .rank = SEVEN },
        { .suit = HEARTS, .rank = NINE },
        { .suit = HEARTS, .rank = FOUR },
        { .suit = HEARTS, .rank = THREE },
        { .suit = HEARTS, .rank = TWO },
        { .suit = SPADES, .rank = ACE },
        { .suit = SPADES, .rank = TWO },
        { .suit = SPADES, .rank = THREE },
        { .suit = SPADES, .rank = FOUR },
        { .suit = SPADES, .rank = FIVE },
        { .suit = SPADES, .rank = SIX },
        { .suit = SPADES, .rank = SEVEN },
        { .suit = SPADES, .rank = EIGHT },
        { .suit = SPADES, .rank = NINE },
        { .suit = SPADES, .rank = KING },
        { .suit = SPADES, .rank = QUEEN },
        { .suit = SPADES, .rank = QUEEN },
        { .suit = SPADES, .rank = KING }

    };
    CardStack deck;
    card_stack_clear(&deck);
    card_stack_populate(&deck, source_cards, sizeof(source_cards) / sizeof(source_cards[0]));
    card_stack_reverse(&deck);
    Game game;
    game_init(&game, &deck);

    // Act
    for (int r = 0; r < 4; r++) {
        for (int c = 0; c < 4; c++) {
            BoardCellPosition pos = { .row = r, .col = c };
            GameResult result = game_action_place(&game, pos);
            assert(result == GAME_RESULT_OK);
        }
    }

    BoardCellPosition positions2[] = {
        { .row = 1, .col = 0 }
    };

    GameResult result2 = game_action_combine(
        &game, positions2, sizeof(positions2) / sizeof(positions2[0]));

    assert(result2 == GAME_RESULT_OK);

    BoardCellPosition positions3[] = {
        { .row = 1, .col = 1 },
        { .row = 1, .col = 2 }
    };

    GameResult result3 = game_action_combine(
        &game, positions3, sizeof(positions3) / sizeof(positions3[0]));

    assert(result3 == GAME_RESULT_OK);

    char buffer[50000];
    repr_move_ledger(buffer, sizeof(buffer), &game);
    log_info("Move ledger:\n %s", buffer);

    // Assert
    assert(strlen(buffer) > 0);

    return 0;
}

int test_can_save_scores()
{
    // Arrange
    const char* path = "/tmp/rexile_scores_test";

    ScoreBoard scores;
    ScoreBoard scores_in;
    remove(path);
    scores_load(path, &scores);

    GameScore score = { .score = 100, .moves = 6, .final_state = GAME_LOSE };
    get_score_default_name(score.name, SCORE_NAME_SIZE);
    GameScore score2 = { .score = 200, .moves = 4, .final_state = GAME_WIN, .name = "abc" };

    scores_add(&scores, &score);
    scores_add(&scores, &score2);

    // Act
    bool result = scores_save(path, &scores);
    assert(result);

    bool r2 = scores_load(path, &scores_in);
    assert(r2);

    // Assert
    for (size_t i = 0; i < scores.count; i++) {
        assert(scores_in.scores[i].score == scores.scores[i].score);
        assert(scores_in.scores[i].moves == scores.scores[i].moves);
        assert(scores_in.scores[i].final_state == scores.scores[i].final_state);
        assert(strcmp(scores_in.scores[i].name, scores.scores[i].name) == 0);
    }

    return 0;
}

TestFn test_fns[] = {
    { "test_stack_can_push", test_stack_can_push },
    { "test_stack_can_pop", test_stack_can_pop },
    { "test_stack_can_populate", test_stack_can_populate },
    { "test_empty_deck_loses", test_empty_deck_loses },
    { "test_invalid_placement_returns_invald", test_invalid_placement_returns_invald },
    { "test_unable_to_place_face_card_loses", test_unable_to_place_face_card_loses },
    { "test_combine_allows_valid_cards", test_combine_allows_valid_cards },
    { "test_can_load_card_stack", test_can_load_card_stack },
    { "test_unable_to_combine_loses", test_unable_to_combine_loses },
    { "test_can_repr_move_ledger", test_can_repr_move_ledger },
    { "test_can_save_scores", test_can_save_scores }

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
