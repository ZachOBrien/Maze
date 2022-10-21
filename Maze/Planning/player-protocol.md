# Memorandum

TO: CS4500 Course Staff  
FROM: Colin Rozzi, Zach O'Brien  
DATE: October 20, 2022  
SUBJECT: Design of the Player Protocol

The referee implementation will program to a player protocol. That player protocol implements the following functionality:


| Purpose | Signature |
|---------|-----------|
| Get a player's action on their turn | `(PublicState -> Action)` |


**Data Definitions**


```
A PublicState is a structure:
    (struct Board Tile Player)
interpretation: A player knows the board, the extra tile, and all of its personal information.

An Action is one of:
    - Move
    - #f
interpretation: A player acts by either making a move or making no move (passing turn).

A Move is a structure:
    (struct ShiftDirection Natural Orientation GridPosn)
interpretation: A Move has a direction to shift a row or column, the index of the row or column to shift, the number of degrees to rotate the spare tile, and a position to move the currently active player to after the shift.
```
