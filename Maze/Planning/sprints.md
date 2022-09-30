# Memorandum

TO: CS4500 Course Staff  
FROM: Colin Rozzi, Zach O'Brien  
DATE: September 30, 2022  
SUBJECT: Plan for First Three Sprints  

In sprint 1 we will create data definitions for basic game elements, design a model for the game state, and design a controller (referee) which can coordinate players and observers with respect to the game state. First we will create data definitions for basic elements like tiles, treasures, players’ avatars and castles, and the board. Then we will design and implement an interface for a game state model which allows updating the board state in ways like moving player avatars and moving and rotating tiles. This interface will be responsible for all game state updates. Then we can design and implement an interface for the referee which supports functionality like starting a new game, deciding the legality of a given move, and allowing players to shift rows/columns and rotate tiles.

In sprint 2 we will design and implement an interface for players, then use the completed player component and referee component from sprint 1 to design and test a player-referee interface. Also in this milestone we will create the observer-referee interface. An observer is anyone observing the game. This will include each of the players, as well as any third parties that may be viewing the game. The model we will implement for the players will be similar to the general game model used by the referee, but without unnecessary or private information like other players’ castles. From here, we can begin to create a basic strategy for picking best moves from the set of legal moves.

In sprint 3 we will implement communication over TCP. This will involve building a server component which handles TCP communication streams between the referee and players or observers implemented in sprint 2. This step implements core functionality such as a registration period and the ability to run full games with remote clients. We can then implement our reward system, as well as things like advertisements or views.
