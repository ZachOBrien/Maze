# Memorandum

TO: CS4500 Course Staff  
FROM: Colin Rozzi, Zach O'Brien  
DATE: December 1, 2022  
SUBJECT: Changes to codebase to accomodate chasing multiple goals

# Chasing Multiple Goals

Our `PlayerInfo` module has a couple changes. `PlayerInfo`s now keep a collection of the goals they have successfully visited. `PlayerInfo`s also now have a boolean flag `going-home?` which indicates whether they are on their way home or not. `PlayerInfo` has a new method `receive-next-goal`, which takes a `PlayerInfo` and either a `GridPosn` or `false`, where false indicates the `PlayerInfo` should go home, and adds the current goal to the goal queue, and updates the goal and `going-home?` accordingly.

Our `Gamestate` now has a queue of goals. There is also a new method `assign-next-goal`, which assigns the first goal in the queue to the `PlayerInfo` it is passed. If there are no goals left, it tells that to the `PlayerInfo` so it can go home.

Our `Referee` has three changes. The first is our method of determining when to end a game and when to send a setup. Before, we were checking for setup calls after a `Player` takes a move, and a game-over after every turn. Now, we are checking both only after a `Player` takes a move, and bubbling up if a `Player` won with a boolean flag. The second change is to our setup. Now, when a `Player` reaches a goal and is not `going-home?`, the referee calls `assign-next-goal-and-send-setup`, which calls `assign-next-goal` in the `Gamestate`, and sends the `Player` a setup with its new `PlayerInfo`. Third, our `determine-winners` function changed to accommodate the win conditions outlined in the spec.
