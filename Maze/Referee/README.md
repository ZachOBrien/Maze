# Referee

The Referee facilitates running the game by coordinating players' turns and enforcing the rules of the game. It also scores the game to determine which players won.

## Component Interactions

```
+-----------------------------------------------------------------+
| Referee                                                         |
+-----------------------------------------------------------------+
|                                                                 |
| fn: run-game (Players Gamestate Observers -> Winners Cheaters)  |
+-----------------------------------------------------------------+
              |                       |
              |                       | 
              |                       |
              |                       |
              v                       v
     +------------------+        +------------------+
     | Player           |        | Gamestate        |
     +------------------+        +------------------+
     |                  |
     | + fn: name       |
     | + fn: setup      |
     | + fn: take-turn  |
     | + fn: win        |
     +------------------+        
```

## Directory Structure

| File | Purpose |
| --------- | ------- |
| [observer.rkt](observer.rkt) | A GUI for visualizing game states |
| [referee.rkt](referee.rkt) | Implements a referee responsible for coordinating players and enforcing rules |
