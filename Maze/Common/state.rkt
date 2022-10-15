#lang racket

;;; This module provides a data definition and logic for a Maze game state


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)
(require racket/list)

(provide
 (contract-out
  [move?       contract?]
  [gamestate?  contract?]
  ; Create a new Move
  [move-new (-> shift-direction? natural-number/c orientation? grid-posn? move?)]
  ; Create a new Gamestate
  [gamestate-new
   (-> board? tile? (non-empty-listof player?) (non-empty-listof player-id?) gamestate?)]
  ; Carry out a player's move by shifting a row or column, inserting a tile, and moving
  ; the player onto the newly inserted tile if they were pushed off the board during the shift
  [execute-move (-> gamestate? move? gamestate?)]
  ; Check if a player can reach a position from their current position
  [player-can-reach-pos? (-> gamestate? grid-posn? boolean?)]
  ; Check if a player is currently placed on their goal tile
  [player-on-goal? (-> gamestate? boolean?)]
  ; Check if a player is currently placed on their home tile
  [player-on-home? (-> gamestate? boolean?)]
  ; Remove the currently active player from the game and ends their turn
  [remove-player (-> gamestate? gamestate?)]
  ; End the current player's turn and switch to the next player's turn
  [end-current-turn (-> gamestate? gamestate?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "tile.rkt")
(require "board.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS



;; A PlayerID is a Natural
;; interpretation: A player's unique ID
(define player-id? natural-number/c)

;; An AvatarColor is one of:
;; - "red"
;; - "green"
;; - "yellow"
;; - "blue"
;; interpretation: The color of a player's avatar
(define avatar-color? (or/c "red" "green" "yellow" "blue"))


;; A Player is a structure:
;;    (struct PlayerID GridPosn GridPosn [Listof Gem] Date AvatarColor)
;; interpretation: A player has an ID, a current position, home position, goal treasure,
;;                 birthday, and avatar color
(struct player [id curr-pos home-pos goal-treasures dob color])

;; GridPosn GridPosn [Listof Gem] Date AvatarColor -> Player
;; Create a new player
(define (player-new id curr-pos home-pos goal-treasures dob color)
  (player id curr-pos home-pos goal-treasures dob color))


;; A Move is a structure:
;;    (struct ShfitDirection Natural Orientation GridPosn)
;; interpretation: A move made by a player. A move has a ShiftDirection, the index of
;;                 a row/col to shift, the orientation of the newly inserted tile,
;;                 and the position the player will move to
(struct move [shift-dir index orientation pos])

;; ShiftDirection Natural Orientation GridPosn -> Move
(define (move-new shift-dir index orientation pos)
  (move shift-dir index orientation pos))


;; A Gamestate is a structure:
;;    (struct Board Tile [NonEmptyListof Player] [NonEmptyListof PlayerID] PlayerID (-> Move Boolean))
;; interpretation: A Gamestate has a board, an extra tile, players, the order in which the players
;;                 take turns, the ID of the currently acting player, and a function representing
;;                 whether a move would reverse the previously made move
(struct gamestate [board extra-tile players player-turn-order current-player reverses-prev-move])

;; Board Tile [NonEmptyListof Player] [NonEmptyListof PlayerID] -> Gamestate
;; Create a new gamestate
(define (gamestate-new board extra-tile players player-turn-order)
  (gamestate board extra-tile players player-turn-order (first player-turn-order) (λ (mv) #f)))


;; Player Player -> Boolean
;; Are the two players the same?
(define (player=? p1 p2)
  (and
   (= (player-id p1) (player-id p2))
   (equal? (player-curr-pos p1) (player-curr-pos p2))
   (equal? (player-home-pos p1) (player-home-pos p2))
   (equal? (player-goal-treasures p1) (player-goal-treasures p2))
   (equal? (player-dob p1) (player-dob p2))
   (equal? (player-color p1) (player-color p2))))

;; Player GridPosn -> Boolean
;; Returns True if the player is on the given position
(define (player-on-pos? p pos)
  (equal? (player-curr-pos p) pos))

;; Player GridPosn -> Player
;; Move a player to the given gridposn
(define (player-move-to p pos)
  (struct-copy player p [curr-pos pos]))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Gamestate Move -> Gamestate
;; Carry out a player's move by shifting a row or column, inserting a tile, and moving
;; the player onto the newly inserted tile if they were pushed off the board during the shift
(define (execute-move state mv)
  ; Update the board
  (define-values (new-board new-extra-tile) (get-next-board-and-extra-tile state mv))
  (define players-after-shift (move-players-on-pushed-tile state mv))
  (define final-players (move-player players-after-shift (get-current-player state) mv))
  (struct-copy gamestate state
               [board new-board]
               [extra-tile new-extra-tile]
               [players final-players]
               [reverses-prev-move (make-reverses-move? mv)]))


;; Gamestate Move -> (Board Tile)
;; Get board and extra tile which result from making a shift
(define (get-next-board-and-extra-tile state mv)
  (board-shift-and-insert
   (gamestate-board state)
   (move-shift-dir mv)
   (move-index mv)
   (tile-rotate
    (gamestate-extra-tile state)
    (move-orientation mv))))
  

;; Gamestate Move -> [Listof Player]
;; Move players who were pushed off the board onto the newly inserted tile
(define (move-players-on-pushed-tile state mv)
  (define inserted-tile-pos (get-inserted-tile-pos
                             (gamestate-board state) (move-shift-dir mv) (move-index mv)))
  (define pushed-tile-pos (get-pushed-tile-pos
                           (gamestate-board state) (move-shift-dir mv) (move-index mv)))
  (for/list ([plyr (gamestate-players state)])
    (if (player-on-pos? plyr pushed-tile-pos)
        (player-move-to plyr inserted-tile-pos)
        plyr)))

;; Gamestate Move -> [Listof Player]
;; Move the currently active player to a new tile according to their specified move
(define (move-player players curr-player mv)
  (define active-player-after-move (player-move-to curr-player (move-pos mv)))
  (for/list ([plyr players])
    (if (= (player-id curr-player) (player-id plyr))
        active-player-after-move
        plyr)))
  


;; Gamestate GridPosn -> Boolean
;; Check if the current player can reach a position from their current position
(define (player-can-reach-pos? state pos)
  (define curr-board (gamestate-board state))
  (define curr-player-pos (player-curr-pos (get-current-player state)))
  (define reachable (board-all-reachable-from (gamestate-board state) curr-player-pos))
  (if (member pos reachable) #t #f))


;; Gamestate -> Boolean
;; Check if a player is currently placed on their goal tile
(define (player-on-goal? state)
  (define curr-player (get-current-player state))
  (define curr-player-tile (board-get-tile-at (gamestate-board state) (player-curr-pos curr-player)))
  (tile-has-gems? curr-player-tile (player-goal-treasures curr-player)))


;; Gamestate -> Boolean
;; Check if a player is currently placed on their home tile
(define (player-on-home? state)
  (define curr-player (get-current-player state))
  (equal? (player-curr-pos curr-player) (player-home-pos curr-player)))


;; Gamestate -> Gamestate
;; Remove the currently active player from the game and ends their turn
(define (remove-player state)
  (define temp-current-player (gamestate-current-player state))
  (define state-after-turn-ended (end-current-turn state))
  (struct-copy gamestate state-after-turn-ended
               [player-turn-order
                (filter
                 (λ (player-id) (not (= player-id temp-current-player)))
                 (gamestate-player-turn-order state))]))
  

;; Gamestate -> Gamestate
; End the current player's turn and switch to the next player's turn
(define (end-current-turn state)
  (define turn-order (gamestate-player-turn-order state))
  (define curr-turn-index (index-of turn-order (gamestate-current-player state)))
  (define next-player-id (list-ref turn-order (modulo (add1 curr-turn-index) (length turn-order))))
  (struct-copy gamestate state
               [current-player next-player-id]))


;; Gamestate PlayerID -> Player
;; Retrieve a player by player ID
(define (get-player state pid)
  (first (filter (λ (player) (= pid (player-id player))) (gamestate-players state))))


;; Gamestate -> Player
;; Get the current player
(define (get-current-player state)
  (get-player state (gamestate-current-player state)))

;; Move -> (Move -> Boolean)
;; Make a function that returns true if another move is passed in that undos that move
(define (make-reverses-move? mv1)
  (λ (mv2) (and (= (move-index mv1)
                   (move-index mv2))
                (opposite-direction?
                 (move-shift-dir mv1)
                 (move-shift-dir mv2)))))

;; ShiftDirection ShiftDirection -> Boolean
;; True if given directions are opposite
(define (opposite-direction? dir1 dir2)
  (or (equal? (set dir1 dir2) (set 'left 'right))
      (equal? (set dir1 dir2) (set 'up 'down))))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (require (submod "tile.rkt" examples))
  (require (submod "board.rkt" examples))
  (define player0
    (player
     0 (cons 0 0) (cons 6 6) (list 'apatite 'aplite) (seconds->date (current-seconds)) "blue"))
  (define player1
    (player
     1
     (cons 1 1)
     (cons 5 5)
     (list 'blue-ceylon-sapphire 'bulls-eye)
     (seconds->date (+ (current-seconds) 1))
     "red"))
  (define player2
    (player
     2
     (cons 2 2)
     (cons 4 4)
     (list 'chrysolite 'citrine)
     (seconds->date (+ (current-seconds) 2))
     "green"))
  (define player3
    (player
     3
     (cons 3 3)
     (cons 3 3)
     (list 'jasper 'mexican-opal)
     (seconds->date (+ (current-seconds) 3))
     "yellow"))
  (define player4
    (player
     4
     (cons 4 4)
     (cons 2 2)
     (list 'peridot 'purple-oval)
     (seconds->date (+ (current-seconds) 4))
     "blue"))
  (define player5
    (player
     5
     (cons 0 6)
     (cons 5 5)
     (list 'blue-ceylon-sapphire 'bulls-eye)
     (seconds->date (+ (current-seconds) 5))
     "red"))
  (define player6
    (player
     6
     (cons 6 0)
     (cons 4 4)
     (list 'chrysolite 'citrine)
     (seconds->date (+ (current-seconds) 6))
     "green"))
  (define player7
    (player
     7
     (cons 6 6)
     (cons 3 3)
     (list 'jasper 'mexican-opal)
     (seconds->date (+ (current-seconds) 7))
     "yellow"))
  (define players0 (list player0 player1 player2 player3 player4))
  (define player-turn-order0 (list 0 1 2 3 4))
  ; player0 (a) not on goal or home
  ; first top left
  (define gamestate0 (gamestate-new board1 tile-extra players0 player-turn-order0))

  (define players1 (list player3 player4))
  (define player-turn-order1 (list 3 4))
  ; player1 (a) not on goal on home
  (define gamestate1 (gamestate-new board1 tile-extra players1 player-turn-order1))

  (define players2 (list player1 player2 player3 player4))
  (define player-turn-order2 (list 1 2 3 4))
  ; on goal not home
  (define gamestate2 (gamestate-new board1 tile-extra players2 player-turn-order2))

  (define players3 (list player1 player0 player5 player6 player7))
  (define player-turn-order3 (list 1 0 5 6 7))
  (define gamestate3 (gamestate-new board1 tile-extra players3 player-turn-order3))

  (define players4 (list player6 player5 player7))
  (define player-turn-order4 (list 6 5 7))
  ; first bottom left
  (define gamestate4 (gamestate-new board1 tile-extra players4 player-turn-order4))

  (define players5 (list player6 player5 player7))
  (define player-turn-order5 (list 7 5 6))
  ; first bottom right
  (define gamestate5 (gamestate-new board1 tile-extra players5 player-turn-order5))

  (define move00 (move-new 'up 0 0 (cons 1 1)))
  (define move10 (move-new 'down 6 0 (cons 1 1)))
  (define move20 (move-new 'left 0 0 (cons 1 1)))
  (define move30 (move-new 'right 6 0 (cons 1 1)))
  (define move0 (move-new 'up 0 0 (cons 1 1)))
  (define move1 (move-new 'down 6 90 (cons 1 1)))
  (define move2 (move-new 'left 0 180 (cons 1 1)))
  (define move3 (move-new 'right 6 270 (cons 1 1)))
  (define move5 (move-new 'up 4 0 (cons 2 0)))
  (define move6 (move-new 'down 3 90 (cons 3 0)))
  (define move7 (move-new 'left 5 180 (cons 1 2)))
  (define move8 (move-new 'right 2 270 (cons 1 1))))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "tile.rkt" examples)))

;; test execute-move shifts rows and cols
(module+ test
  ; test shifting rows
  (check-equal? (list-ref (gamestate-board (execute-move gamestate0 move20)) 0)
                (list tile01 tile02 tile03 tile04 tile05 tile06 tile-extra))
  (check-equal? (list-ref (gamestate-board (execute-move gamestate0 move30)) 6)
                (list tile-extra tile60 tile61 tile62 tile63 tile64 tile65))
  ; test shifting cols
  (check-equal? (map (λ (row) (list-ref row 0)) (gamestate-board (execute-move gamestate0 move00)))
                (list tile10 tile20 tile30 tile40 tile50 tile60 tile-extra))
  (check-equal? (map (λ (row) (list-ref row 6)) (gamestate-board (execute-move gamestate0 move10)))
                (list tile-extra tile06 tile16 tile26 tile36 tile46 tile56)))

;; test execute-move rotates and inserts tile
(module+ test
  ; test rotating+inserting tile
  (check-equal? (list-ref (list-ref (gamestate-board (execute-move gamestate0 move0)) 6) 0)
                tile-extra)
  (check-equal? (list-ref (list-ref (gamestate-board (execute-move gamestate0 move1)) 0) 6)
                (tile-make 'straight 270 empty))
  (check-equal? (list-ref (list-ref (gamestate-board (execute-move gamestate0 move2)) 0) 6)
                (tile-make 'straight 0 empty))
  (check-equal? (list-ref (list-ref (gamestate-board (execute-move gamestate0 move3)) 6) 0)
                (tile-make 'straight 90 empty)))

;; test players are moved onto newly inserted tile if they need to be
(module+ test
  ; test moving players on moved tile
  (check-true (player-on-pos?
               (list-ref (gamestate-players (execute-move gamestate3 move0)) 1)
               (cons 6 0)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (execute-move gamestate3 move1)) 4)
               (cons 0 6)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (execute-move gamestate3 move2)) 1)
               (cons 0 6)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (execute-move gamestate3 move3)) 4)
               (cons 6 0))))

;; test player is moved to the right tile
(module+ test
  ; test moving player
  (check-true (player-on-pos?
               (list-ref (gamestate-players (execute-move gamestate0 move5)) 0)
               (cons 2 0)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (execute-move gamestate0 move6)) 0)
               (cons 3 0)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (execute-move gamestate0 move7)) 0)
               (cons 1 2)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (execute-move gamestate0 move8)) 0)
               (cons 1 1))))


;; test player-can-reach-pos?
(module+ test
  (check-true (player-can-reach-pos? gamestate0 (cons 1 1)))
  (check-true (player-can-reach-pos? gamestate0 (cons 5 0)))
  (check-true (player-can-reach-pos? gamestate0 (cons 1 0)))
  (check-false (player-can-reach-pos? gamestate0 (cons 0 3)))
  (check-false (player-can-reach-pos? gamestate0 (cons 6 6))))

;; test player-on-goal?
(module+ test
  (check-false (player-on-goal? gamestate0))
  (check-false (player-on-goal? gamestate1))
  (check-true (player-on-goal? gamestate2)))

;; test player-on-home?
(module+ test
  (check-false (player-on-home? gamestate0))
  (check-true (player-on-home? gamestate1))
  (check-false (player-on-home? gamestate2)))

;; test remove-player
(module+ test
  (check-equal? (gamestate-player-turn-order (remove-player gamestate0))
                (list 1 2 3 4))
  (check-equal? (gamestate-player-turn-order (remove-player gamestate1))
                (list 4))
  (check-equal? (gamestate-player-turn-order (remove-player gamestate2))
                (list 2 3 4)))

;; test end-current-turn
(module+ test
  (check-equal? (gamestate-current-player (end-current-turn gamestate0))
               1)
  (check-equal? (gamestate-current-player (end-current-turn gamestate1))
               4)
  (check-equal? (gamestate-current-player (end-current-turn gamestate2))
               2))

;; Test player-on-pos
(module+ test
  (check-true (player-on-pos? player0 (cons 0 0))))

;; Test player-move-to
(module+ test
  (check-true (player=?
               (player-move-to player0 (cons 3 3))
               (player
                0
                (cons 3 3)
                (cons 6 6)
                (list 'apatite 'aplite)
                (seconds->date (current-seconds))
                "blue")))
  (check-true (player=?
               (player-move-to player0 (cons 6 6))
               (player
                0
                (cons 6 6)
                (cons 6 6)
                (list 'apatite 'aplite)
                (seconds->date (current-seconds))
                "blue"))))

;; test opposite-direction?
(module+ test
  (check-true (opposite-direction? 'up 'down))
  (check-true (opposite-direction? 'down 'up))
  (check-true (opposite-direction? 'right 'left))
  (check-true (opposite-direction? 'left 'right))
  (check-false (opposite-direction? 'up 'right))
  (check-false (opposite-direction? 'down 'left))
  (check-false (opposite-direction? 'left 'up))
  (check-false (opposite-direction? 'right 'down)))

;; test make-undo-last-move-check
(module+ test
  (check-false ((make-reverses-move? (move-new 'up 0 0 (cons 1 1)))
                (move-new 'up 0 0 (cons 1 1))))
  (check-true ((make-reverses-move? (move-new 'up 0 0 (cons 1 1)))
               (move-new 'down 0 0 (cons 1 1))))
  (check-false ((make-reverses-move? (move-new 'up 1 0 (cons 1 1)))
                (move-new 'down 0 0 (cons 1 1))))
  (check-false ((make-reverses-move? (move-new 'right 0 0 (cons 1 1)))
                (move-new 'right 0 0 (cons 1 1))))
  (check-true ((make-reverses-move? (move-new 'right 0 0 (cons 1 1)))
               (move-new 'left 0 0 (cons 1 1))))
  (check-false ((make-reverses-move? (move-new 'left 1 0 (cons 1 1)))
                (move-new 'right 0 0 (cons 1 1)))))