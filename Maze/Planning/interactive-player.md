# Memorandum

TO: CS4500 Course Staff  
FROM: Colin Rozzi, Zach O'Brien  
DATE: October 27, 2022  
SUBJECT: Design of an Interactive Player Mechanism


General Layout of a Graphical User Interface:

```
+------------------------+
| +---------+            |
| |         | dir [ up ] |
| | (board) | idx [ 2  ] |
| |         | [ MOVE ]   |
| +---------+            |
| Turn: Yellow           |
| Your goal: (x, y)      |
+------------------------+
```

A GUI for the Maze game should include:

- A visual representation of the board, which includes the avatars
  of other players
- Which player is currently taking their turn
- Some way to inform the player where their goal tile is
- Input fields for the player to specify which row/col they want to move
- A button to allow a player to send their move
