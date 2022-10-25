#lang racket

;;; This module provides a data definition and logic for a Maze game state


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)
(require racket/list)

(provide
 (contract-out
  [gamestate?  contract?]
  
  [last-action? contract?]
  ; Create a new Gamestate
  [gamestate-new
   (-> board? tile? (non-empty-listof player?) (or/c #f last-action?) gamestate?)]
  ; Shifts a row or column and inserts a tile in the empty space
  [gamestate-shift-and-insert
   (-> gamestate? shift-direction? natural-number/c orientation? gamestate?)]
  ; Move players that were on a row or column that was shifted
  [shift-players (-> (listof player?) board? shift-direction? natural-number/c (listof player?))]
  ; Move the currently active player to a new position
  [gamestate-move-player (-> gamestate? grid-posn? gamestate?)]
  ; Check if a player can reach a position from their current position
  [player-can-reach-pos? (-> gamestate? grid-posn? boolean?)]
  ; Check if a player is currently placed on their goal tile
  [player-on-goal? (-> gamestate? boolean?)]
  ; Check if a player is currently placed on their home tile
  [player-on-home? (-> gamestate? boolean?)]
  ; Remove the currently active player from the game and ends their turn
  [remove-player (-> gamestate? gamestate?)]
  ; End the current player's turn and switch to the next player's turn
  [end-current-turn (-> gamestate? gamestate?)]
  ;; All reachable from current player current position
  [all-reachable-from-active (-> gamestate? (listof grid-posn?))]
  ; Makes a playerstate for the currently active player from the gamestate
  [gamestate->player-state (-> gamestate? player-state?)]
  ; Changes the goal tile of the active player
  [change-active-player-goal (-> gamestate? grid-posn? gamestate?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "tile.rkt")
(require "board.rkt")
(require "gem.rkt")
(require "player.rkt")
(require "../Players/player-state.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

(define DEFAULT-SHIFT-STEP 1)


;; A LastAction is a pair:
;;    (Natural . ShiftDirection)
;; interpretation: The index and direction of the last shift made.
(define last-action? (cons/c natural-number/c shift-direction?))

(define (gamestate=? gs1 gs2 rec)
  (and (rec (gamestate-board gs1)      (gamestate-board gs2))
       (rec (gamestate-extra-tile gs1) (gamestate-extra-tile gs2))
       (rec (gamestate-players gs1)    (gamestate-players gs2))
       (rec (gamestate-last-move gs1)  (gamestate-last-move gs2))))

(define (gamestate-hash-code gs rec)
  (+ (* 1000 (rec (gamestate-last-move gs)))
     (* 100  (rec (gamestate-players gs)))
     (* 10   (rec (gamestate-extra-tile gs)))
     (* 1    (rec (gamestate-board gs)))))

(define (gamestate-secondary-hash-code gs rec)
  (+ (* 1000 (rec (gamestate-board gs)))
     (* 100  (rec (gamestate-extra-tile gs)))
     (* 10   (rec (gamestate-players gs)))
     (* 1    (rec (gamestate-last-move gs)))))

;; A Gamestate is a structure:
;;    (struct Board Tile [NonEmptyListof Player] (U LastAction #f))
;; interpretation: A Gamestate has a board, an extra tile, players arranged in the order they
;;                 take turns (with the currently acting player at the front of the list)
;;                 and the last move made
(struct gamestate [board extra-tile players last-move]
  #:methods gen:equal+hash
  [(define equal-proc gamestate=?)
   (define hash-proc  gamestate-hash-code)
   (define hash2-proc gamestate-secondary-hash-code)])

;; Board Tile [NonEmptyListof Player] -> Gamestate
;; Create a new gamestate
(define (gamestate-new board extra-tile players last-move)
  (gamestate board extra-tile players last-move))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; Gamestate ShiftDirection Natural Orientation -> Gamestate
;; Shifts a row or column and inserts a tile in the empty space
(define (gamestate-shift-and-insert state dir idx orientation)
  (define-values
    (new-board new-extra-tile)
    (board-shift-and-insert
     (gamestate-board state) dir idx (tile-rotate (gamestate-extra-tile state) orientation)))
  (define players-after-shift
    (shift-players (gamestate-players state) (gamestate-board state) dir idx))
  (gamestate new-board new-extra-tile players-after-shift (cons idx dir)))


;; [Listof Player] Board ShiftDirection Natural -> [Listof Player]
;; Move players that were on a row or column that was shifted
(define (shift-players players board dir idx)
  (define shift-step (if (shifts-forward? dir) DEFAULT-SHIFT-STEP (* -1 DEFAULT-SHIFT-STEP)))
  (for/list ([plyr players])
    (if (player-shifted? dir idx plyr)
        (shift-player plyr board dir shift-step)
        plyr)))


;; Player Board ShiftDirection Natural
;; Shifts a player along a row or column
(define (shift-player plyr board dir shift-step)
  (define player-row-pos (car (player-curr-pos plyr)))
  (define player-col-pos (cdr (player-curr-pos plyr)))
  (define new-pos (if (shifts-row? dir)    
                      (cons player-row-pos
                            (get-shifted-position player-col-pos shift-step (num-cols board)))
                      (cons (get-shifted-position player-row-pos shift-step (num-rows board))
                            player-col-pos)))
  (player-move-to plyr new-pos))
 ; (struct-copy player plyr [curr-pos new-pos]))


;; Natural Natural Natural -> Natural
;; Get the new index of a tile after a shift
(define (get-shifted-position start-idx shift num-tiles)
  (modulo (+ start-idx shift) num-tiles))


;; Gamestate ShiftDirection Natural Player -> Boolean
;; Create a function to check if a player is on a shifted row/col
(define (player-shifted? dir idx plyr)
  (cond [(shifts-row? dir) (= (car (player-curr-pos plyr)) idx)]
        [(shifts-col? dir) (= (cdr (player-curr-pos plyr)) idx)]))


;; Gamestate GridPosn -> Gamestate
;; Move the currently active player to a new tile according to their specified move
(define (gamestate-move-player state pos)
  (define players (gamestate-players state))
  (define curr-player-moved (player-move-to (first players) pos))
  (struct-copy gamestate state
               [players (cons curr-player-moved (rest players))]))


;; Player GridPosn -> Boolean
;; Returns True if the player is on the given position
(define (player-on-pos? p pos)
  (equal? (player-curr-pos p) pos))


;; Gamestate GridPosn -> Boolean
;; Check if the current player can reach a position from their current position
(define (player-can-reach-pos? state pos)
  (define reachable (all-reachable-from-active state))
  (if (member pos reachable) #t #f))


;; Gamestate -> [Listof Grid-Posn]
;; Find all positions reachable from the current active player's position
(define (all-reachable-from-active state)
  (board-all-reachable-from (gamestate-board state) (player-curr-pos (get-current-player state))))

;; Gamestate -> Boolean
;; Check if a player is currently placed on their goal tile
(define (player-on-goal? state)
  (define curr-player (get-current-player state))
  (equal? (player-curr-pos curr-player) (player-goal-pos curr-player)))


;; Gamestate -> Boolean
;; Check if a player is currently placed on their home tile
(define (player-on-home? state)
  (define curr-player (get-current-player state))
  (equal? (player-curr-pos curr-player) (player-home-pos curr-player)))


;; Gamestate -> Gamestate
;; Removes the currently active player from the game
(define (remove-player state)
  (struct-copy gamestate state
               [players (rest (gamestate-players state))]))


;; Gamestate -> Gamestate
; End the current player's turn and switch to the next player's turn
(define (end-current-turn state)
  (define plyrs (gamestate-players state))
  (struct-copy gamestate state
               [players (append (rest plyrs) (cons (first plyrs) empty))]))


;; Gamestate -> Player
;; Get the current player
(define (get-current-player state)
  (first (gamestate-players state)))

;; ShiftDirection ShiftDirection -> Boolean
;; True if given directions are opposite
(define (opposite-direction? dir1 dir2)
  (or (equal? (set dir1 dir2) (set 'left 'right))
      (equal? (set dir1 dir2) (set 'up 'down))))

;; Gamestate -> PlayerState
;; Makes a playerstate for the currently active player from the gamestate
(define (gamestate->player-state gstate)
  (player-state-new (gamestate-board gstate) (gamestate-extra-tile gstate) (first (gamestate-players gstate))))

;; Gamestate GridPosn -> Gamestate
;; Changes the goal tile of the active player
(define (change-active-player-goal state new-goal)
  (define new-players (cons (player-change-goal (get-current-player state) new-goal)
                            (rest (gamestate-players state))))
  (struct-copy gamestate state [players new-players]))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (require (submod "tile.rkt" examples))
  (require (submod "board.rkt" examples))
  (require (submod "player.rkt" examples))

  (define players0 (list player0 player1 player2 player3 player4))
  ; player0 (a) not on goal or home
  ; first top left
  (define gamestate0 (gamestate-new board1 tile-extra players0 #f))

  (define players1 (list player3 player4))
  ; player1 (a) not on goal on home
  (define gamestate1 (gamestate-new board1 tile-extra players1 #f))
  (define players2 (list player1 player2 player3 player4))
  ; on goal not home
  (define gamestate2 (gamestate-new board1 tile-extra players2 #f))

  (define players3 (list player1 player0 player5 player6 player7))
  (define gamestate3 (gamestate-new board1 tile-extra players3 #f))

  (define players4 (list player0 player1 player2 player5))
  (define gamestate4 (gamestate board1 tile-extra players4 #f))

  (define players5 (list player8 player5 player7))
  (define gamestate5 (gamestate-new board1 tile-extra players5 #f)))
  

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "tile.rkt" examples))
  (require (submod "player.rkt" examples)))

;; test execute-move shifts rows and cols
(module+ test
  ; test shifting rows
  (check-equal? (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 'left 0 0)) 0)
                (list tile01 tile02 tile03 tile04 tile05 tile06 tile-extra))
  (check-equal? (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 'right 6 0)) 6)
                (list tile-extra tile60 tile61 tile62 tile63 tile64 tile65))
  ; test shifting cols
  (check-equal? (map (λ (row) (list-ref row 0))
                     (gamestate-board (gamestate-shift-and-insert gamestate0 'up 0 0)))
                (list tile10 tile20 tile30 tile40 tile50 tile60 tile-extra))
  (check-equal? (map (λ (row) (list-ref row 6))
                     (gamestate-board (gamestate-shift-and-insert gamestate0 'down 6 0)))
                (list tile-extra tile06 tile16 tile26 tile36 tile46 tile56)))

;; test execute-move rotates and inserts tile
(module+ test
  ; test rotating+inserting tile
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 'up 0 0)) 6) 0)
                tile-extra)
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 'down 6 90)) 0) 6)
                (tile-new 'straight 270 (set)))
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 'left 0 180)) 0) 6)
                (tile-new 'straight 0 (set)))
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 'right 6 270)) 6)
                 0)
                (tile-new 'straight 90 (set))))

;; test players on a shifted row/col are moved accordingly
(module+ test
  ; test moving players on moved row
  (check-equal? (shift-players
                 (gamestate-players gamestate4)
                 (gamestate-board gamestate4)
                 'right
                 0)
                (list
                 (player-new (cons 0 1) (cons 6 6) (cons 5 1) #f "blue")
                 player1
                 player2
                 (player-new (cons 0 0) (cons 5 5) (cons 1 5) #f "red")))
  (check-equal? (shift-players
                 (gamestate-players gamestate4)
                 (gamestate-board gamestate4)
                 'left
                 0)
                (list
                 (player-new (cons 0 6) (cons 6 6) (cons 5 1) #f "blue")
                 player1
                 player2
                 (player-new (cons 0 5) (cons 5 5) (cons 1 5) #f "red")))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (gamestate-shift-and-insert gamestate3 'up 0 0)) 1)
               (cons 6 0)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (gamestate-shift-and-insert gamestate3 'down 6 90)) 4)
               (cons 0 6)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (gamestate-shift-and-insert gamestate3 'left 0 180)) 1)
               (cons 0 6)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (gamestate-shift-and-insert gamestate3 'right 6 270)) 4)
               (cons 6 0))))

;; test player is moved to the right tile
(module+ test
  ; test moving player
  (check-true (player-on-pos?
               (list-ref (gamestate-players (gamestate-move-player gamestate0 (cons 2 0))) 0)
               (cons 2 0)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (gamestate-move-player gamestate5 (cons 3 0))) 0)
               (cons 3 0)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (gamestate-move-player gamestate0 (cons 1 2))) 0)
               (cons 1 2)))
  (check-true (player-on-pos?
               (list-ref (gamestate-players (gamestate-move-player gamestate0 (cons 1 1))) 0)
               (cons 1 1))))

;; test player is moved to correct tile after shift moves row/col
(module+ test
  (check-equal? (player-curr-pos (first
                                  (gamestate-players
                                   (gamestate-move-player
                                    (gamestate-shift-and-insert gamestate0 'up 0 0) (cons 1 1)))))
              (cons 1 1))
  (check-equal? (player-curr-pos (first
                                  (gamestate-players
                                   (gamestate-move-player
                                    (gamestate-shift-and-insert gamestate5 'left 4 90) (cons 4 5)))))
               (cons 4 5)))


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
  (check-true  (player-on-goal? gamestate2)))

;; test player-on-home?
(module+ test
  (check-false (player-on-home? gamestate0))
  (check-true (player-on-home? gamestate1))
  (check-false (player-on-home? gamestate2)))

;; test remove-player
(module+ test
  (check-equal? (gamestate-players (remove-player gamestate0))
                (list player1 player2 player3 player4))
  (check-equal? (gamestate-players (remove-player gamestate1))
                (list player4))
  (check-equal? (gamestate-players (remove-player gamestate2))
                (list player2 player3 player4)))

;; test end-current-turn
(module+ test
  (check-equal? (get-current-player (end-current-turn gamestate0)) player1)
  (check-equal? (get-current-player (end-current-turn gamestate1)) player4)
  (check-equal? (get-current-player (end-current-turn gamestate2)) player2))

;; Test player-on-pos
(module+ test
  (check-true (player-on-pos? player0 (cons 0 0))))

;; Test player-move-to
(module+ test
  (check-equal? (player-move-to player0 (cons 3 3))
                (player-new
                 (cons 3 3)
                 (cons 6 6)
                 (cons 5 1)
                 #f
                 "blue"))
  (check-equal? (player-move-to player0 (cons 6 6))
                (player-new
                 (cons 6 6)
                 (cons 6 6)
                 (cons 5 1)
                 #f
                 "blue")))



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

