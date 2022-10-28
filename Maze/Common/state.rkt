#lang racket

;;; This module provides a data definition and logic for a Maze game state


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)
(require racket/list)
(require "player-info.rkt")


(provide
 (contract-out
  [gamestate?  contract?]
  [gamestate-board (-> gamestate? board?)]
  [gamestate-extra-tile (-> gamestate? tile?)]
  [gamestate-prev-shift (-> gamestate? shift?)]
  ; Create a new Gamestate
  [gamestate-new
   (-> board? tile? (non-empty-listof player-info?) (or/c #f shift?) gamestate?)]
  ; Shifts a row or column and inserts a tile in the empty space
  [gamestate-shift-and-insert
   (-> gamestate? shift? orientation? gamestate?)]
  ; Move players that were on a row or column that was shifted
  [shift-players (-> (listof player-info?) board? shift? (listof player-info?))]
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
  ; Get the current player's color
  [current-player-color (-> gamestate? avatar-color?)]
  ; End the current player's turn and switch to the next player's turn
  [end-current-turn (-> gamestate? gamestate?)]
  ;; All reachable from current player current position
  [all-reachable-from-active (-> gamestate? (listof grid-posn?))]
  ; Makes a playerstate for the currently active player from the gamestate
  [gamestate->player-state (-> gamestate? avatar-color? player-state?)]
  ; Changes the goal tile of the active player
  [change-active-player-goal (-> gamestate? grid-posn? gamestate?)]
  ; Get the list of players colors
  [get-player-color-list (-> gamestate? (listof avatar-color?))]
  ; Determine the distance of a player from their objective. If they have not found their treasure,
  ; that is their objective. If they have found their treasure, getting home is their objective.
  [euclidean-distance-from-objective (-> gamestate? avatar-color? number?)]
  ; Get a PlayerInfo by color
  [gamestate-get-by-color (-> gamestate? avatar-color? player-info?)]
  ; Has the game ended?
  [game-over? (-> gamestate? boolean?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "tile.rkt")
(require "board.rkt")
(require "gem.rkt")
(require "rulebook.rkt")
(require "../Players/player-state.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

(define DEFAULT-SHIFT-STEP 1)

;; A Gamestate is a structure:
;;    (struct Board Tile [NonEmptyListof PlayerInfo] (U Shift #f))
;; interpretation: A Gamestate has a board, an extra tile, players arranged in the order they
;;                 take turns (with the currently acting player at the front of the list)
;;                 and the last shift made
(struct gamestate [board extra-tile players prev-shift] #:transparent)

;; Board Tile [NonEmptyListof PlayerInfo] -> Gamestate
;; Create a new gamestate
(define (gamestate-new board extra-tile players prev-shift)
  (gamestate board extra-tile players prev-shift))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; Gamestate Shift Orientation -> Gamestate
;; Shifts a row or column and inserts a tile in the empty space
(define (gamestate-shift-and-insert state shft orientation)
  (define-values
    (new-board new-extra-tile)
    (board-shift-and-insert
     (gamestate-board state) shft (tile-rotate (gamestate-extra-tile state) orientation)))
  (define players-after-shift
    (shift-players (gamestate-players state) (gamestate-board state) shft))
  (gamestate new-board new-extra-tile players-after-shift shft))


;; [Listof PlayerInfo] Board Shift -> [Listof PlayerInfo]
;; Move players that were on a row or column that was shifted
(define (shift-players players board shft)
  (define shift-step (if (shifts-forward? (shift-direction shft)) DEFAULT-SHIFT-STEP (* -1 DEFAULT-SHIFT-STEP)))
  (for/list ([plyr players])
    (if (player-shifted? shft plyr)
        (shift-player plyr board (shift-direction shft) shift-step)
        plyr)))


;; PlayerInfo Board ShiftDirection Natural
;; Shifts a player along a row or column
(define (shift-player plyr board dir shift-step)
  (define player-row-pos (car (player-info-curr-pos plyr)))
  (define player-col-pos (cdr (player-info-curr-pos plyr)))
  (define new-pos (if (shifts-row? dir)    
                      (cons player-row-pos
                            (get-shifted-position player-col-pos shift-step (num-cols board)))
                      (cons (get-shifted-position player-row-pos shift-step (num-rows board))
                            player-col-pos)))
  (player-info-move-to plyr new-pos))


;; Natural Natural Natural -> Natural
;; Get the new index of a tile after a shift
(define (get-shifted-position start-idx shift num-tiles)
  (modulo (+ start-idx shift) num-tiles))


;; Gamestate Shift PlayerInfo -> Boolean
;; Create a function to check if a player is on a shifted row/col
(define (player-shifted? shft plyr)
  (cond [(shifts-row? (shift-direction shft)) (= (car (player-info-curr-pos plyr)) (shift-index shft))]
        [(shifts-col? (shift-direction shft)) (= (cdr (player-info-curr-pos plyr)) (shift-index shft))]))


;; Gamestate GridPosn -> Gamestate
;; Move the currently active player to a new tile according to their specified move
(define (gamestate-move-player state pos)
  (define players (gamestate-players state))
  (define curr-player-moved (player-info-move-to (first players) pos))
  (struct-copy gamestate state
               [players (cons curr-player-moved (rest players))]))


;; PlayerInfo GridPosn -> Boolean
;; Returns True if the player is on the given position
(define (player-on-pos? p pos)
  (equal? (player-info-curr-pos p) pos))


;; Gamestate GridPosn -> Boolean
;; Check if the current player can reach a position from their current position
(define (player-can-reach-pos? state pos)
  (define reachable (all-reachable-from-active state))
  (if (member pos reachable) #t #f))


;; Gamestate -> [Listof Grid-Posn]
;; Find all positions reachable from the current active player's position
(define (all-reachable-from-active state)
  (board-all-reachable-from (gamestate-board state) (player-info-curr-pos (get-current-player state))))

;; Gamestate -> Boolean
;; Check if a player is currently placed on their goal tile
(define (player-on-goal? state)
  (define curr-player (get-current-player state))
  (equal? (player-info-curr-pos curr-player) (player-info-goal-pos curr-player)))


;; Gamestate -> Boolean
;; Check if a player is currently placed on their home tile
(define (player-on-home? state)
  (define curr-player (get-current-player state))
  (equal? (player-info-curr-pos curr-player) (player-info-home-pos curr-player)))


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


;; Gamestate -> PlayerInfo
;; Get the current player
(define (get-current-player state)
  (first (gamestate-players state)))

;; Gamestate AvatarColor -> PlayerState
;; Makes a playerstate for the player who is represented by the color from the gamestate
(define (gamestate->player-state gstate color)
  (player-state-new
   (gamestate-board gstate)
   (gamestate-extra-tile gstate)
   (gamestate-get-by-color gstate color)
   (gamestate-prev-shift gstate)))

;; Gamestate GridPosn -> Gamestate
;; Changes the goal tile of the active player
(define (change-active-player-goal state new-goal)
  (define new-players (cons (player-info-change-goal (get-current-player state) new-goal)
                            (rest (gamestate-players state))))
  (struct-copy gamestate state [players new-players]))

;; Gamestate AvatarColor -> (U PlayerInfo #f)
;; Get a PlayerInfo with the corresponding color
(define (gamestate-get-by-color state color)
  (findf (lambda (plyr) (equal? color (player-info-color plyr))) (gamestate-players state)))
  

;; Gamestate -> AvatarColor
;; Get the current player's color
(define (current-player-color gstate)
  (player-info-color (get-current-player gstate)))

;; Gamestate -> [Listof AvatarColor]
;; Get the list of avatar player colors
(define (get-player-color-list gstate)
  (map player-info-color (gamestate-players gstate)))

;; Gamestate AvatarColor -> Number
;; Determine the distance of a player from their objective. If they have not found their treasure,
;; that is their objective. If they have found their treasure, getting home is their objective.
(define (euclidean-distance-from-objective state color)
  (define plyr (gamestate-get-by-color color))
  (if (player-info-visited-goal? plyr)
      (euclidean-dist (player-info-curr-pos plyr) (player-info-home-pos plyr))
      (euclidean-dist (player-info-curr-pos plyr) (player-info-goal-pos plyr))))

;; Gamestate -> Boolean
;; Has the game ended?
(define (game-over? state)
  (or
   (empty? (gamestate-players state))
   (not (empty? (filter (λ (plyr) (player-info-visited-goal-returned-home? plyr))) (gamestate-players state)))))
   

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (require (submod "tile.rkt" examples))
  (require (submod "board.rkt" examples))
  (require (submod "player-info.rkt" examples))

  (define player-infos0 (list player-info0 player-info1 player-info2 player-info3 player-info4))
  ; player-info0 (a) not on goal or home
  ; first top left
  (define gamestate0 (gamestate-new board1 tile-extra player-infos0 #f))

  (define player-infos1 (list player-info3 player-info4))
  ; player-info1 (a) not on goal on home
  (define gamestate1 (gamestate-new board1 tile-extra player-infos1 #f))
  (define player-infos2 (list player-info1 player-info2 player-info3 player-info4))
  ; on goal not home
  (define gamestate2 (gamestate-new board1 tile-extra player-infos2 #f))

  (define player-infos3 (list player-info1 player-info0 player-info5 player-info6 player-info7))
  (define gamestate3 (gamestate-new board1 tile-extra player-infos3 #f))

  (define player-infos4 (list player-info0 player-info1 player-info2 player-info5))
  (define gamestate4 (gamestate board1 tile-extra player-infos4 #f))

  (define player-infos5 (list player-info8 player-info5 player-info7))
  (define gamestate5 (gamestate-new board1 tile-extra player-infos5 #f)))
  

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "board.rkt" examples))
  (require (submod "tile.rkt" examples))
  (require (submod "player-info.rkt" examples)))

;; test execute-move shifts rows and cols
(module+ test
  ; test shifting rows
  (check-equal? (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'left 0) 0)) 0)
                (list tile01 tile02 tile03 tile04 tile05 tile06 tile-extra))
  (check-equal? (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'right 6) 0)) 6)
                (list tile-extra tile60 tile61 tile62 tile63 tile64 tile65))
  ; test shifting cols
  (check-equal? (map (λ (row) (list-ref row 0))
                     (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'up 0) 0)))
                (list tile10 tile20 tile30 tile40 tile50 tile60 tile-extra))
  (check-equal? (map (λ (row) (list-ref row 6))
                     (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'down 6) 0)))
                (list tile-extra tile06 tile16 tile26 tile36 tile46 tile56)))

;; test execute-move rotates and inserts tile
(module+ test
  ; test rotating+inserting tile
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'up 0) 0)) 6) 0)
                tile-extra)
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'down 6) 90)) 0) 6)
                (tile-new 'straight 270 (set)))
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'left 0) 180)) 0) 6)
                (tile-new 'straight 0 (set)))
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'right 6) 270)) 6)
                 0)
                (tile-new 'straight 90 (set))))

;; test players on a shifted row/col are moved accordingly
(module+ test
  ; test moving players on moved row
  (check-equal? (shift-players
                 (gamestate-players gamestate4)
                 (gamestate-board gamestate4)
                 (shift-new 'right 0))
                (list
                 (player-info-new (cons 0 1) (cons 6 6) (cons 5 1) #f "blue")
                 player-info1
                 player-info2
                 (player-info-new (cons 0 0) (cons 5 5) (cons 1 5) #f "red")))
  (check-equal? (shift-players
                 (gamestate-players gamestate4)
                 (gamestate-board gamestate4)
                 (shift-new 'left 0))
                (list
                 (player-info-new (cons 0 6) (cons 6 6) (cons 5 1) #f "blue")
                 player-info1
                 player-info2
                 (player-info-new (cons 0 5) (cons 5 5) (cons 1 5) #f "red")))
  (check-true (player-info-on-pos?
               (list-ref (gamestate-players (gamestate-shift-and-insert gamestate3 (shift-new 'up 0) 0)) 1)
               (cons 6 0)))
  (check-true (player-info-on-pos?
               (list-ref (gamestate-players (gamestate-shift-and-insert gamestate3 (shift-new 'down 6) 90)) 4)
               (cons 0 6)))
  (check-true (player-info-on-pos?
               (list-ref (gamestate-players (gamestate-shift-and-insert gamestate3 (shift-new 'left 0) 180)) 1)
               (cons 0 6)))
  (check-true (player-info-on-pos?
               (list-ref (gamestate-players (gamestate-shift-and-insert gamestate3 (shift-new 'right 6) 270)) 4)
               (cons 6 0))))

;; test player is moved to the right tile
(module+ test
  ; test moving player
  (check-true (player-info-on-pos?
               (list-ref (gamestate-players (gamestate-move-player gamestate0 (cons 2 0))) 0)
               (cons 2 0)))
  (check-true (player-info-on-pos?
               (list-ref (gamestate-players (gamestate-move-player gamestate5 (cons 3 0))) 0)
               (cons 3 0)))
  (check-true (player-info-on-pos?
               (list-ref (gamestate-players (gamestate-move-player gamestate0 (cons 1 2))) 0)
               (cons 1 2)))
  (check-true (player-info-on-pos?
               (list-ref (gamestate-players (gamestate-move-player gamestate0 (cons 1 1))) 0)
               (cons 1 1))))

;; test player is moved to correct tile after shift moves row/col
(module+ test
  (check-equal? (player-info-curr-pos (first
                                  (gamestate-players
                                   (gamestate-move-player
                                    (gamestate-shift-and-insert gamestate0 (shift-new 'up 0) 0) (cons 1 1)))))
              (cons 1 1))
  (check-equal? (player-info-curr-pos (first
                                  (gamestate-players
                                   (gamestate-move-player
                                    (gamestate-shift-and-insert gamestate5 (shift-new 'left 4) 90) (cons 4 5)))))
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
                (list player-info1 player-info2 player-info3 player-info4))
  (check-equal? (gamestate-players (remove-player gamestate1))
                (list player-info4))
  (check-equal? (gamestate-players (remove-player gamestate2))
                (list player-info2 player-info3 player-info4)))

;; test end-current-turn
(module+ test
  (check-equal? (get-current-player (end-current-turn gamestate0)) player-info1)
  (check-equal? (get-current-player (end-current-turn gamestate1)) player-info4)
  (check-equal? (get-current-player (end-current-turn gamestate2)) player-info2))

;; Test player-on-pos
(module+ test
  (check-true (player-info-on-pos? player-info0 (cons 0 0))))

;; Test player-info-move-to
(module+ test
  (check-equal? (player-info-move-to player-info0 (cons 3 3))
                (player-info-new
                 (cons 3 3)
                 (cons 6 6)
                 (cons 5 1)
                 #f
                 "blue"))
  (check-equal? (player-info-move-to player-info0 (cons 6 6))
                (player-info-new
                 (cons 6 6)
                 (cons 6 6)
                 (cons 5 1)
                 #f
                 "blue")))


;; Test gamestate->player-state
(module+ test
  (check-equal? (gamestate->player-state gamestate0 (current-player-color gamestate0))
                (player-state-new board1 tile-extra player-info0 #f)))

(module+ test
  (check-equal? (get-player-color-list gamestate0) (list "blue" "red" "green" "yellow" "blue")))
