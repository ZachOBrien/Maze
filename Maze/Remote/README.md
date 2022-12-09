# Remote

Functionality for running the base game functionality in a remote context

## Directory Structure

| File | Purpose |
| --------- | ------- |
| [player.rkt](player.rkt) | Implements a remote proxy player |
| [referee.rkt](referee.rkt) | Implements a remote proxy referee, which establishes the same context as the "real" referee |
| [safety.rkt](safety.rkt) | Implements functionality for handling untrusted code gracefully |
| [tcp-conn.rkt](tcp-conn.rkt) | Implements a wrapper around a TCP connection |
