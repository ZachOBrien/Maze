# Memorandum

TO: CS4500 Course Staff  
FROM: Colin Rozzi, Zach O'Brien  
DATE: October 13, 2022  
SUBJECT: Design of the Player

## A Player needs to know:

`PlayerState`, which includes
- A representation of the board, including the extra tile, all players' home tiles, and its goal treasure.
- All players' current positions
- The order in which players are taking turns
- Whether it has visited its goal


## A Player implements the following functionality:

| purpose | signature |
|-------------------|-----------|
| make a move. Boolean indicates validity of move | `Move -> Boolean`|
| receive updates about the game state | `PlayerState -> .` |
| get the current board | `PlayerState -> Board` |
| shift and insert (to experiment with possible moves) | `Board ShiftDirection Index -> Board` |
| figure out which tiles it can reach | `Board -> [Listof Tile]` |
| get its goal treasure | `PlayerState -> [Listof Gem]` |
| get goal treasure location | `PlayerState -> GridPosn` |


