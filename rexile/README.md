# Rexile

A solitaire sometimes known as *Kings in the Corners Solitaire*. Inspired by the Macintosh game *On the Edge* by John Stiles.

> "Rexile" is a portmanteau of "rex," the Latin word for king, and "exile." It plays on the idea of kings being on the edge or out of their usual domain, fitting the theme of your game.

See [TODO](TODO.md)

## Rules

```text
+---+---+---+---+
| K | Q | Q | K |
+---+---+---+---+
| J |   |   | J |
+---+---+---+---+
| J |   |   | J |
+---+---+---+---+
| K | Q | Q | K |
+---+---+---+---+
```

### Placement phase

Draw and place cards with rules:

* Place Kings in the corners
* Queens on the top and bottom edges
* Jacks on the left and right edges
* Pip cards (2-10, Aces) go in any empty cell.

If unable to place a face card, you lose.
If all face cards are placed, you win.
When grid is full, proceed to combine phase.

### Combine phase

Pair non-face cards to total 10 and remove them. Ace is worth ten.
If unable to combine yet grid is full, you lose.
When all combinations are made, return to placement phase.

### Winning

Place all face cards.

### Losing

When either:

* Unable to place a face card
* Deck is exhausted

## Scoring

* Place a royal: 100 points
* Combine one or more cards to equal 10: 10 points

## Variants

Not implemented, just listing some that I have seen.

* Combine with 15, not 10
* Non-corner borders can have either J/Q
* Deck must be empty to win.

## Run

```bash
./run_build.sh
./build/rexile
```

Logs are written to `/tmp/rexile.log`

## Controls

Cursor: arrow keys
Place/select: enter/space
Quit: q
Dump ledger: l

## Design

### App states

* APP_INIT
* APP_GAME_STARTED
* APP_PROMPT_NEW_GAME
* APP_QUIT

### Game states

* GAME_PLACE             // Place cards on empty spots
* GAME_COMBINE           // Board full, combine matches
* GAME_LOSE
* GAME_WIN

### Layout

* A 3 x 3 grid of cells
  * each cell has a card stack with a max of 1
  * each cells specifies allowed ranks
    * All cells allow non-face ranks
    * Borders allow specific face ranks
* A draw deck with the top card face up
* A discard pile

### Actions

* Move the top card from N stacks to a destination stack
  * Valid scenarios
    * Draw deck to empty cell
    * 1 or 2 cell stacks to discard
      * Must be combine phase
      * Must equal 10 non-face ranks

### Phases

#### Placement

Actions

* Move the top card from the draw deck to an empty cell.
  * Must be a rank allowed by the cell.

Rules

* Phase begins when:
  * Game starts
  * Placement action is made during combine phase
  * (optional) Combinations cannot be made during combine phase
* Phase ends when board is full

#### Combine

Actions

* Move 1 or 2 placed non-face cards that add up to 10 to the discard pile.
* Placement action - ends combine phase

Rules

* Phase begins when:
  * Board is full during placement phase
* Phase ends when:
  * Placement action is made

## Design 2

### init

state = place

### init move

draw card

* Assumes at least one card.

### move

place/combine card

* Relies on post move validation to ensure game isn't lost.

### post move

switch type
  place:
    if complete:
      state = win
    if full:
      if combine possible:
        state = combine
      else:
        state = lose
    if empty deck:
      stat = lose
    if no moves:
      state = lose
    else:
      state = place
  combine:
    if_no_moves:
      state = lose
    state = combine_place

## ledger example

| type   | move | old_state       | new_state       |
| ------ | ---- | --------------- | --------------- |
| place  | KH   | place           | place           |
| place  | 5S   | place           | place           |
| ...    |      |                 |                 |
| place  | JC   | place           | combine         |
| combine| x    | combine         | combine_place   |
| combine| x    | combine_place   | combine_place   |
| place  | 3C   | combine_place   | place           |
| place  | QH   | place           | win             |
