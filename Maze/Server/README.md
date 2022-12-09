# Server

Mechanism for listening for client requests, accepting and processing those requests, and passing players to the referee


## Component Interactions

```
+---------------------------------------------------------------------------------+
| Server                                                                          |
+---------------------------------------------------------------------------------+
|                                                                                 |
| fn: main (Gamestate observers port -> [Listof [Listof String] [Listof String]]) |
+---------------------------------------------------------------------------------+
              |                       |
              |                       | 
              |                       |
              |                       |
              v                       v
     +------------------+        +------------------+
     | ProxyPlayer      |        | Referee          |
     +------------------+        +------------------+
     |                  |        |                  |
     | + fn: name       |        | + fn: run-game   |
     | + fn: setup      |        +------------------+
     | + fn: take-turn  |
     | + fn: win        |
     +------------------+        
```

## Directory Structure

| File | Purpose |
| --------- | ------- |
| [server.rkt](server.rkt) | Provides functionality for creating a Maze server | 

