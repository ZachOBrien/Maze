# Chasing Multiple Goals

November 28, 2022


## Required Changes

1. Implement a `create-initial-state` function, which randomly picks homes and goals and assigns them to players.


## Completed

1. `PlayerInfo` must keep a collection of the goals they succesfully visited

1. `PlayerInfo` data definition must include a new boolean flag, `going-home?`, which indicates that when this player was assigned a new goal, no goals remained so they are directed to go home.

1. The `Gamestate` must keep a queue of goals
    - When a player visits their current goal, the state assigns them a new one by popping it off of this queue. When the queue is empty, players are informed there are no goals remaining, and should go home.
    - When no goals remain, the `PlayerInfo`'s `goal-pos` field will be `#f`

1. We need a new function `assign-next-goal`, in the state
    - `assign-next-goal : Gamestate -> Gamestate`
    - Assigns a new goal to the current player and removes it from the queue of goals. If there are no goals left, the player-info's goal is set to `#f`

1. `determine-winners` function **first** finds the maximum number of goals reached by any player, then rules out any players that did not visit this many goals. The remaining computation stays the same.

1. Write new function `assign-next-goal-and-send-setup`
    - Assigns a new goal to a player, sends the updated gamestate and goal to the player, and returns the gamestate after they correctly (or illegally) respond.

1. `player-win?` is no longer `player-win?`. Now, it is `player-terminates-game?` because a player visiting their home when there are no more goals ends the game, but it is not guaranteed that they won, because they might have reached fewer goals than another player.
    - And now, there are 3 conditions for ending the game:
        1. In curr-state, the player's goal is `#f` (there are no more goals in the state's queue)
        1. ? The player has visited at least one goal
        1. The player is on their home
        1. The player was not on their home in the previous turn

