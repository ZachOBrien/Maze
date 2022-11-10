# Memorandum

TO: CS4500 Course Staff  
FROM: Colin Rozzi, Zach O'Brien  
DATE: November 9, 2022  
SUBJECT: Accomodating changes to the software

## Proposed Changes

- Blank tiles for the board

    - DIFFICULTY: 2
    - This change would just involve augmenting our data definition for a tile to include a tile with no connectors, orientation, or gem, and then adding a single line of code to functions like `tiles-connected?`. For example, if players cannot move onto a blank tile from any tile, we could implement that by just checking if the move-to tile is blank and immediately returning false.

- Use movable tiles as goals

    - DIFFICULTY: 2
    - Currently, we have a two-line function which updates a player's position if they are placed on a row or column that gets shifted. We could define a similarly concise function which updates a player's goal position if it was shifted.

- Ask player to sequentially pursue several goals, one at a time

    - DIFFICULTY: 2
    - Our player data representation would need to change. Instead of having a field with a single goal and a boolean field representing if they have visited the goal, we would have a queue of goals they need to visit and a list of goals they have visited.
