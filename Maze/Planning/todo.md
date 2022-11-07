# Technical Debt Cleanup Todo List

November 7, 2022

Ranked in order of priority.

1. Always check for legal player behavior on calls across Referee/Player boundary, and kick players who misbehave.

2. Make sure state is updated in `send-setup` calls (i.e., if a player is kicked, later players should not receive a state which has that kicked player)

3. End the game if all non-kicked players pass. Currently, we only end the game if **every player** who was playing at the start of a round passes.

4. Move serializing/deserializing into submodules

5. Gamestate should have one top-level function to execute an Action

6. Remove unused functions

7. Typo check all purpose statement/signatures

8. Check our examples to make sure that the naming of PlayerStates/RefereeStates is correct.

9. Double-check all contracts which involve any of `player-state?`, `referee-state?`, or `gamestate?` to make sure they accept only the correct kind of state.

10. Check gamestate's getters, and see if we could replace them with functions
    - e.g. `gamestate-prev-shift` can be replaced with something like `undoes-prev-move?`

