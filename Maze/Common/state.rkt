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
  [referee-state? contract?]
  [player-state? contract?]
  [gamestate-board (-> gamestate? board?)]
  [gamestate-extra-tile (-> gamestate? tile?)]
  [gamestate-prev-shift (-> gamestate? shift?)]
  [move?          contract?]
  [move-orientation (-> move? orientation?)]
  [move-shift (-> move? shift?)]
  [move-pos (-> move? grid-posn?)]
  [move-new (-> grid-posn? shift? orientation? move?)]
  ; Get the current player
  [gamestate-current-player (-> gamestate? player-info?)]
  ; Create a new player state
  [player-state-new (-> board? tile? player-state-player-infos? (or/c #f shift?) player-state?)]
  ; Create a new referee state
  [referee-state-new (-> board? tile? (listof ref-player-info?) (or/c #f shift?) referee-state?)]
  ; Shifts a row or column and inserts a tile in the empty space
  [gamestate-shift-and-insert (-> gamestate? shift? orientation? gamestate?)]
  ; Move players that were on a row or column that was shifted
  [shift-players (-> gamestate? shift? (listof player-info?))]
  ; Move the currently active player to a new position
  [gamestate-move-player (-> gamestate? grid-posn? gamestate?)]
  ; Check if the current player can reach a position from their current position
  [player-can-reach-pos? (-> gamestate? grid-posn? boolean?)]
  ; Check if the current player is currently placed on their treasure tile
  [player-on-treasure? (-> gamestate? boolean?)]
  ; Check if the curent player is currently placed on their home tile
  [player-on-home? (-> gamestate? boolean?)]
  ; Remove the currently active player from the game and ends their turn
  [remove-player (-> gamestate? gamestate?)]
  ; Get the current player's color
  [current-player-color (-> gamestate? avatar-color?)]
  ; End the current player's turn and switch to the next player's turn
  [end-current-turn (-> gamestate? gamestate?)]
  ;; All reachable from current player current position
  [all-reachable-from-active (-> gamestate? (listof grid-posn?))]
  ; Makes a playerstate for a specific player in the referee state. 
  [referee-state->player-state (-> referee-state? avatar-color? player-state?)]
  ; Changes the treasure tile of the active player
  [change-active-player-treasure (-> gamestate? grid-posn? gamestate?)]
  ; Get the list of players colors
  [get-player-color-list (-> gamestate? (listof avatar-color?))]
  ; Determine the distance of a player from their objective. If they have not found their treasure,
  ; that is their objective. If they have found their treasure, getting home is their objective.
  [euclidean-distance-from-objective (-> referee-state? avatar-color? number?)]
  ; Get a PlayerInfo by color
  [gamestate-get-by-color (-> gamestate? avatar-color? player-info?)]
  ; Has the game ended?
  [game-over? (-> gamestate? boolean?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "tile.rkt")
(require "board.rkt")
(require "gem.rkt")
(require "math.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

(define DEFAULT-SHIFT-STEP 1)

;; A Gamestate is one of:
;;    - PlayerState
;;    - RefereeState
;; interpretation: A gamestate either has all the information a referee knows, or
;;                 only the information that one player should know

;; (struct Board Tile [NonEmptyListof PlayerInfo] (U Shift #f))
;; interpretation: A Gamestate has a board, an extra tile, players arranged in the order they
;;                 take turns (with the currently acting player at the front of the list)
;;                 and the last shift made
(struct gamestate [board extra-tile players prev-shift] #:transparent)

(define player-state-player-infos?
  (or/c empty? (cons/c ref-player-info? (listof pub-player-info?))))

;; Gamestate -> Boolean
;; Is this gamestate a PlayerState?
(define (player-state? state)
  (and (gamestate? state)
       (player-state-player-infos? (gamestate-players state))))

;; Gamestate -> Boolean
;; Is this gamestate a RefereeState?
(define (referee-state? state)
  (and (gamestate? state)
       ((listof ref-player-info?) (gamestate-players state))))
  
;; Board Tile (U empty (cons RefPlayerInfo [Listof PubPlayerInfo])) Shift -> Gamestate
;; Create a new player state
(define (player-state-new board extra-tile players prev-shift)
  (gamestate board extra-tile players prev-shift))

;; Board Tile [Listof RefPlayerInfo] Shift -> Gamestate
;; Create a new referee state
(define (referee-state-new board extra-tile players prev-shift)
  (gamestate board extra-tile players prev-shift))

;; A Move is a structure:
;;    (struct GridPosn Shift Orientation)
;; interpretation: A Move has a position to move the currently active player to after the shift,
;;                 a shift, and the number of degrees to rotate the spare tile
(struct move [pos shift orientation] #:transparent)

(define (move-new pos shft orientation)
  (move pos shft orientation))

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
    (shift-players state shft))
  (gamestate new-board new-extra-tile players-after-shift shft))


;; Gamestate Shift -> [Listof PlayerInfo]
;; Move the gamestates list of players that were on a row or column that was shifted
(define (shift-players state shft)
  (define shift-step (if (shifts-forward? (shift-direction shft))
                         DEFAULT-SHIFT-STEP
                         (* -1 DEFAULT-SHIFT-STEP)))
  (for/list ([plyr (gamestate-players state)])
    (if (player-shifted? shft plyr)
        (shift-player plyr (gamestate-board state) (shift-direction shft) shift-step)
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
  (board-all-reachable-from (gamestate-board state) (player-info-curr-pos (gamestate-current-player state))))

;; Gamestate -> Boolean
;; Check if a player is currently placed on their treasure tile
(define (player-on-treasure? state)
  (on-treasure? (gamestate-current-player state)))


;; Gamestate -> Boolean
;; Check if a player is currently placed on their home tile
(define (player-on-home? state)
  (define curr-player (gamestate-current-player state))
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
(define (gamestate-current-player state)
  (first (gamestate-players state)))

;; Gamestate AvatarColor -> PlayerState
;; Makes a playerstate for the player who is represented by the color from the gamestate
;; WARNING: In a PlayerState, the list of players does not signify turn order as in RefereeState
(define (referee-state->player-state state color)
  (define plyr (gamestate-get-by-color state color))
  (define plyr-infos (move-to-front plyr (gamestate-players state)))
  (gamestate
   (gamestate-board state)
   (gamestate-extra-tile state)
   (cons (first plyr-infos) (map ref-player-info->pub-player-info (rest plyr-infos)))
   (gamestate-prev-shift state)))

;; Gamestate GridPosn -> Gamestate
;; Changes the treasure position of the active player
(define (change-active-player-treasure state new-treasure)
  (define new-players (cons (change-treasure (gamestate-current-player state) new-treasure)
                            (rest (gamestate-players state))))
  (struct-copy gamestate state [players new-players]))

;; Gamestate AvatarColor -> (U PlayerInfo #f)
;; Get a PlayerInfo with the corresponding color
(define (gamestate-get-by-color state color)
  (findf (lambda (plyr) (equal? color (player-info-color plyr))) (gamestate-players state)))
  
;; Gamestate -> AvatarColor
;; Get the current player's color
(define (current-player-color gstate)
  (player-info-color (gamestate-current-player gstate)))

;; Gamestate -> [Listof AvatarColor]
;; Get the list of avatar player colors
(define (get-player-color-list gstate)
  (map player-info-color (gamestate-players gstate)))

;; Gamestate AvatarColor -> Number
;; Determine the distance of a player from their objective. If they have not found their treasure,
;; that is their objective. If they have found their treasure, getting home is their objective.
(define (euclidean-distance-from-objective state color)
  (define plyr (gamestate-get-by-color state color))
  (if (player-info-visited-treasure? plyr)
      (euclidean-dist (player-info-curr-pos plyr) (player-info-home-pos plyr))
      (euclidean-dist (player-info-curr-pos plyr) (player-info-treasure-pos plyr))))

;; Gamestate -> Boolean
;; Has the game ended?
(define (game-over? state)
  (or
   (empty? (gamestate-players state))
   (not (empty? (filter (λ (plyr) (visited-treasure-and-on-home? plyr))) (gamestate-players state)))))


;; [Listof Any] -> [Listof Any]
;; Move an item to the front of the list, if it exists. If more than one of the element
;; exists, only the first occurrence is moved.
(define (move-to-front elem lst)
  (cond
    [(not (member elem lst)) lst]
    [else (cons elem (remove elem lst))]))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (require (submod "tile.rkt" examples))
  (require (submod "board.rkt" examples))
  (require (submod "player-info.rkt" examples))

  (define player-infos0 (list player-info0 player-info1 player-info2 player-info3 player-info4))
  ; player-info0 (a) not on treasure or home
  ; first top left
  (define gamestate0 (gamestate board1 tile-extra player-infos0 #f))

  (define player-infos1 (list player-info3 player-info4))
  ; player-info1 (a) not on treasure on home
  (define gamestate1 (gamestate board1 tile-extra player-infos1 #f))
  (define player-infos2 (list player-info1 player-info2 player-info3 player-info4))
  ; on treasure not home
  (define gamestate2 (gamestate board1 tile-extra player-infos2 #f))

  (define player-infos3 (list player-info1 player-info0 player-info5 player-info6 player-info7))
  (define gamestate3 (gamestate board1 tile-extra player-infos3 #f))

  (define player-infos4 (list player-info0 player-info1 player-info2 player-info5))
  (define gamestate4 (gamestate board1 tile-extra player-infos4 #f))

  (define player-infos5 (list player-info8 player-info5 player-info7))
  (define gamestate5 (gamestate board1 tile-extra player-infos5 #f))

  (define player-state0 (gamestate board1 tile-extra (list player-info2) (shift-new 'up 0)))
  (define player-state1 (gamestate board1 tile-extra (list player-info7) (shift-new 'down 4)))
  (define player-state-nowhere-to-go (gamestate board-nowhere-to-go tile-extra (list player-info3) (shift-new 'right 4))))
  

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
                (tile-new 'straight 270 (set 'yellow-baguette 'yellow-beryl-oval)))
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'left 0) 180)) 0) 6)
                (tile-new 'straight 0 (set 'yellow-baguette 'yellow-beryl-oval)))
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'right 6) 270)) 6)
                 0)
                (tile-new 'straight 90 (set 'yellow-baguette 'yellow-beryl-oval))))

;; test players on a shifted row/col are moved accordingly
(module+ test
  ; test moving players on moved row
  (check-equal? (shift-players
                 gamestate4
                 (shift-new 'right 0))
                (list
                 (ref-player-info-new (cons 0 1) (cons 6 6) (cons 5 1) #f "blue")
                 player-info1
                 player-info2
                 (ref-player-info-new (cons 0 0) (cons 5 5) (cons 1 5) #f "red")))
  (check-equal? (shift-players
                 gamestate4
                 (shift-new 'left 0))
                (list
                 (ref-player-info-new (cons 0 6) (cons 6 6) (cons 5 1) #f "blue")
                 player-info1
                 player-info2
                 (ref-player-info-new (cons 0 5) (cons 5 5) (cons 1 5) #f "red")))
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
  (check-true (player-can-reach-pos? gamestate0 (cons 1 2)))
  (check-false (player-can-reach-pos? gamestate0 (cons 1 0)))
  (check-false (player-can-reach-pos? gamestate0 (cons 0 3)))
  (check-false (player-can-reach-pos? gamestate0 (cons 6 6))))

;; test player-on-treasure?
(module+ test
  (check-false (player-on-treasure? gamestate0))
  (check-false (player-on-treasure? gamestate1))
  (check-true  (player-on-treasure? gamestate2)))

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
  (check-equal? (gamestate-current-player (end-current-turn gamestate0)) player-info1)
  (check-equal? (gamestate-current-player (end-current-turn gamestate1)) player-info4)
  (check-equal? (gamestate-current-player (end-current-turn gamestate2)) player-info2))

;; Test player-on-pos
(module+ test
  (check-true (player-info-on-pos? player-info0 (cons 0 0))))

;; Test player-info-move-to
(module+ test
  (check-equal? (player-info-move-to player-info0 (cons 3 3))
                (ref-player-info-new
                 (cons 3 3)
                 (cons 6 6)
                 (cons 5 1)
                 #f
                 "blue"))
  (check-equal? (player-info-move-to player-info0 (cons 6 6))
                (ref-player-info-new
                 (cons 6 6)
                 (cons 6 6)
                 (cons 5 1)
                 #f
                 "blue")))


;; Test referee-state->player-state
(module+ test
  (check-equal? (referee-state->player-state gamestate0 (current-player-color gamestate0))
                (gamestate board1
                           tile-extra
                           (list player-info0
                                 (ref-player-info->pub-player-info player-info1)
                                 (ref-player-info->pub-player-info player-info2)
                                 (ref-player-info->pub-player-info player-info3)
                                 (ref-player-info->pub-player-info player-info4))
                           #f)))

(module+ test
  (check-equal? (get-player-color-list gamestate0) (list "blue" "purple" "green" "yellow" "blue")))

(module+ test
  (check-equal? (move-to-front 1 '(2 3 1 4)) '(1 2 3 4))
  (check-equal? (move-to-front 1 '(2 3 4)) '(2 3 4))
  (check-equal? (move-to-front 1 '()) '())
  (check-equal? (move-to-front 5 '(5 6 7)) '(5 6 7))
  (check-equal? (move-to-front 7 '(5 6 7)) '(7 5 6))
  (check-equal? (move-to-front 1 '(5 6 1 8 1 7)) '(1 5 6 8 1 7)))