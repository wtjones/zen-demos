# Rexile

> "Rexile" is a portmanteau of "rex," the Latin word for king, and "exile." It plays on the idea of kings being on the edge or out of their usual domain, fitting the theme of your game.

See [TODO](TODO.md)

## Run

```
./run_build.sh
./build/rexile
```

## Design

### App states

- APP_INIT
- APP_GAME_STARTED
- APP_PROMPT_NEW_GAME
- APP_QUIT

### Game states

- GAME_PLACE             // Place cards on empty spots
- GAME_COMBINE           // Board full, combine matches
- GAME_LOSE
- GAME_WIN
