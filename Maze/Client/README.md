# Client

Mechanism for connecting to a game server over TCP, and handling requests from the server


## Component Interactions

```
+-----------------------------------------------+
| Client                                        |
+-----------------------------------------------+
|                                               |
| fn: run-client (Hostname Port Player -> Void) |
+-----------------------------------------------+
                                  |
                                  |
                                  v
                         +------------------+
                         | Player           |
                         +------------------+
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
| [client.rkt](client.rkt) | A client which connects to a game server over TCP |