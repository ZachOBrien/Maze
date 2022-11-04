# Memorandum

TO: CS4500 Course Staff  
FROM: Colin Rozzi, Zach O'Brien  
DATE: November 3, 2022  
SUBJECT: Designing a distributed system


## Overview

We will use a client-server architecture to turn the monolithic codebase into a distributed system. There are two top-level types of programs in this system:

- Referee (server) program
- Player (client) program


# Referee Side 

## 1. Gathering Players

Once the referee server program reaches a steady state listening for new connections from prospective players, it starts collecting new players as pairs of input and output ports. Once the referee has enough players connected, it creates a list of `PlayerProxy`s, which can be passed directly to our referee with no changes to the referee code necessary.

### The Proxy Layer: PlayerProxy

According to the remote-proxy design pattern, we will create a new `PlayerProxy` class to serve as a proxy for remote players. This class will have the following public API (indicated with +) and private fields (indicated with -) of type input-port and output-port to use for communication.

```
class PlayerProxy

- sending-port : input-port?
- receiving-port : output-port?

+ name : (-> string?)
+ propose-board0 : (-> natural-number/c natural-number/c board?)
+ setup : (-> player-state? grid-posn? any)
+ take-turn : (-> player-state? action?)
+ win : (-> boolean? any)
```

The public-facing API is identical to the one from Milestone 6 "Logical Interactions". The referee should not be concerned with the mechanics of serializing/deserializing data structures.

With this design, our Referee implementation will not have to change at all. We simply make `PlayerProxy` conform to the interface given in "Logical Interactions" that we used to complete Milestone 6.

Hidden from clients of `PlayerProxy`, the `PlayerProxy` class will serialize or deserialize data being sent or received, respectively, over the network. **It will not implement this functionality itself, rather it will rely on the serializing/deserializing code we have written for integration testing previous milestones (with any changes necessary as we learn more details of the official spec).** All network communication will pass JSON values of the forms specified in previous milestones.

## 2. Running the Game

Our existing code to run a game will work without any changes necessary. Communication will proceed exactly as it is described in Milestone 5's "Logical Interactions" (https://course.ccs.neu.edu/cs4500f22/local_protocol.html). The referee starts by making calls to `PlayerProxy`s to propose boards:

```
   PlayerProxy                           Referee
        |                                  | 
        |       propose-board0(N, N)       | 
        | <------------------------------  |
        |            board                 | 
        | ==============================>  |
        |                                  | 
        .                                  . 
        .                                  . Continues for all players
        .                                  .
```

Once boards are proposed, the Referee begins asking players for their move choices. Play continues until the game is over, at which point the referee will signal who won and who lost via the `won` method.


# Player Side

## 1. Requesting to Join Game

The Player (client) program will initiate connection to a Referee (server) program. Once the TCP connection is established, it will listen for a new remote procedure call (RPC) from the referee.

## 2. Steady State

Once the Player program has succesfully connected to the Referee server, it is in its steady state where it waits and listens for a new RPC from the referee. Once it receives an RPC, it responds as quickly as possible. Communication with the referee terminates after some timeout period, or when the referee indicates the player won or lost the game.
