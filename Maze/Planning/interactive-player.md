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

## Program Design Elements

In alignment with an MVC pattern:
- State (model). Under the hood, the GUI will need a `PlayerState`, which keeps track of all state info with respect ot a single player. I.e., all info except the goal tiles of other players. It can use this `PlayerState` to do things like render the board, and show other players' positions.
- A controller. The controller will handle updates to the `PlayerState` and react to the user interacting with the GUI
- A view. The view component will render the `PlayerState`.
