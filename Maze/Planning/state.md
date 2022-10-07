# Memorandum

TO: CS4500 Course Staff  
FROM: Colin Rozzi, Zach O'Brien  
DATE: October 7, 2022  
SUBJECT: Design of Game State

The Game State is a class which holds the following information as fields:

| Information | Racket Data Representation|  
|-----|----|
|A game board|`[Listof [Listof Tile]]`|
|An extra tile|`Tile`|
|Players| `[Listof Player]`|
|Player turn order | `[Listof PlayerID]` |
|Current player to act | `PlayerID`|
|Phase of the game| `{'registration, 'active, 'complete}`|
|The previous player action| `PlayerAction`|

Where `Tile` and `GridPosn` are as defined in milestone 2.  
A `Player` is a `(struct PlayerID GridPosn Boolean [Listof Gems] GridPosn Date)`  
interpretation: A player has an ID, a home location, the treasure they are supposed to find,  a current location, and a date of birth.

A `PlayerAction` is a `(struct (U 'row 'col) (U 'up 'down 'left 'right) Natural Orientation GridPosn)`  
interpretation: A player makes an action by choosing whether to shift a row or column, which direction it will be shifted, the index of the row or column, the orientation to insert the old extra tile with, and the location they are moving to.

A `PlayerID` is a `Natural`  
interpretation: The player's unique identification number

This `GameState` class will implement an interface which provides the following methods to enable a referee to run a game from start to completion:

| Purpose | Signature |
|--------|---------|
| get the current board | `get-board : ( . -> . Board)`|
| get a player's position | `get-player-position : (PlayerID . -> . GridPosn)` |
| get the extra tile | `get-extra-tile : ( . -> . Tile)`|
| get whose turn it is | `get-current-player : ( . -> . PlayerID)` |
| winner? | `get-winner : ( . -> (U PlayerId #f))` |
| get the last turn | `get-previous-action : ( . -> . PlayerAction)` | 
| get connected tiles | `all-reachable-from : (GridPosn . -> . [Listof [Listof GridPosn]])` |
| shift a row or column | `execute-player-action : (PlayerAction . -> . )` |
