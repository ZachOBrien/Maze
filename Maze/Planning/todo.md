# Technical Debt Cleanup Todo List

November 7, 2022

Ranked in order of priority.

4. Move serializing/deserializing into submodules

6. Remove unused functions

7. Typo check all purpose statement/signatures

8. Check our examples to make sure that the naming of PlayerStates/RefereeStates is correct.

9. Double-check all contracts which involve any of `player-state?`, `referee-state?`, or `gamestate?` to make sure they accept only the correct kind of state.

10. Check gamestate's getters, and see if we could replace them with functions
    - e.g. `gamestate-prev-shift` can be replaced with something like `undoes-prev-move?`


## Completed

### IMPORTANT: We only list commits here that are relevant for the actual TODO items. Other commits, like for this week's design task and integration testing task, are not included here.

1. Always check for legal player behavior on calls across Referee/Player boundary, and kick players who misbehave.

    Commit(s) that fix the issue:

    [9896fb2 feat: All calls across referee-player boundary checked and handled](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/9896fb2fa7007769dc7ac0dadb890af5cbb51a06)

2. Make sure state is updated in `send-setup` calls (i.e., if a player is kicked, later players should not receive a state which has that kicked player)

    Commit(s) that fix the issue:

    [ec2d7ae feat: functional referee and returning names](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/ec2d7ae945ba56299c9989cd82a466f510659a0d)

    [e1ba27f Revert "wip: setup working with safe apply to all"](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/e1ba27feb64c095b2ba45d0df40fec14e05ab087)

    [a981b35 wip: setup working with safe apply to all](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/a981b3567f312965673b714fcd7dc8eeda4d8bdc)

    [431feb6 wip: working referee no classes](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/431feb6251ed77994295be35ba6e5a88d7ae19e7)

    [0b5e5bf fix: Essentially undoing previous commit, reverting code to working state](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/0b5e5bf22824eb86cc7811d047b470100d2d771c)

    [7b020d0 wip: working on properly kicking players](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/7b020d040e7d7978972fe31cca5796cd7d6c632d)

3. End the game if all non-kicked players pass. Currently, we only end the game if **every player** who was playing at the start of a round passes.

    Commit(s) that fix the issue:

    [04b8dad fix: Miscellaneous small fixes](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/04b8dadb5c79249ee4c2e9c33f0227214a94fcc0)

    [9fc2f0a fix: Miscellaneous small fixes](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/9fc2f0a1952e941fb818839089b39f5c244608bf)

5. (Todo Item 5) Gamestate should have one top-level function to execute an Action

    Commit(s) that fix the issue:

    [0c17b6d test: Augment tests for safely executing turn](https://github.khoury.northeastern.edu/CS4500-F22/crozzi-obrienz/commit/0c17b6dec71db254467a5c91e0a6b4a3fcb44f69)
