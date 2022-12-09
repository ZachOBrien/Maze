# Common

Components of the game shared by clients and server.


## Component Interactions

```
                              +--------------+
                              | Gamestate    |
                              +--------------+
   +-----------------+        |              |         +-----------------------------+
   | Board           |<-------| board        |    +--->| PlayerInfo                  |
   +-----------------+        |              |    |    +-----------------------------+
   |                 |   +----| extra-tile   |  list   |                             |
+--| MatrixOf[Tile]  |   |    |              |    |    | <information about player>  |
|  +-----------------+   |    | players      |----+    +-----------------------------+
|                        |    |              |
|                        |    | goals        |----> <a list of goals (positions)>
|  +----------------+    |    |              |
+->| Tile           |<---+    | prev-shift   |----> <previous shift performed>
   +----------------+         +--------------+
   |                |
   | connector      |
   |                |
   | orientation    |
   |                |
   | gems           |
   +----------------+

```

## Directory Structure

| File | Purpose |
| --------- | ------- |
| [board.rkt](board.rkt) | Implements functionality of a Maze game board |
| [gem.rkt](gem.rkt) | Functionality for gems |
| [list-utils.rkt](list-utils.rkt) | Utilities for working with lists |
| [math.rkt](math.rkt) | Math used throughout project |
| [player-info.rkt](player-info.rkt) | Information the referee and player know about a player |
| [rulebook.rkt](rulebook.rkt) | Has the rules of the game | 
| [state.rkt](state.rkt) | Represents the state of the game |
| [tile.rkt](tile.rkt) | Represents a Tile on the game board | 