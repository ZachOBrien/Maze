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
