#lang racket/base

;;; This module provides a data definition and logic for a Maze game state


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)
(require racket/list)
(require racket/function)
(require racket/bool)
(require "player-info.rkt")


(provide
 (contract-out
  [gamestate?  contract?]
  [referee-state? contract?]
  [player-state? contract?]
  [gamestate-board (-> gamestate? board?)]
  [gamestate-extra-tile (-> gamestate? tile?)]
  [gamestate-prev-shift (-> gamestate? (or/c shift? #f))]
  [gamestate-players (-> gamestate? (listof player-info?))]
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
  [referee-state-new (-> board? tile? (listof ref-player-info?) (listof grid-posn?) (or/c #f shift?) referee-state?)]
  ; Execute a move
  [gamestate-execute-move (-> gamestate? move? gamestate?)]
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
  ; Remove a player from the game
  [remove-player-by-color (-> gamestate? avatar-color? gamestate?)]
  ; Get the current player's color
  [current-player-color (-> gamestate? avatar-color?)]
  ; Assign the player their next goal, or tell them to go home
  [assign-next-goal (-> referee-state? avatar-color? referee-state?)]
  ; End the current player's turn and switch to the next player's turn
  [end-current-turn (-> gamestate? gamestate?)]
  ;; All reachable from current player current position
  [all-reachable-from-active (-> gamestate? (listof grid-posn?))]
  ; Makes a playerstate for a specific player in the referee state. 
  [referee-state->player-state (-> referee-state? avatar-color? player-state?)]
  ; Changes the treasure tile of the active player
  [replace-active-player-dummy-goal (-> gamestate? grid-posn? gamestate?)]
  ; Get the list of players colors
  [get-player-color-list (-> gamestate? (listof avatar-color?))]
  ; Get a PlayerInfo by color
  [gamestate-get-by-color (-> gamestate? avatar-color? player-info?)]
  ; Find the player(s) that have visited the highest number of goals
  [gamestate-players-with-max-num-goals (-> referee-state? (listof ref-player-info?))]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "tile.rkt")
(require "board.rkt")
(require "gem.rkt")
(require "math.rkt")
(require "list-utils.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

(define DEFAULT-SHIFT-STEP 1)

;; A Gamestate is one of:
;;    - PlayerState
;;    - RefereeState
;; interpretation: A gamestate either has all the information a referee knows, or
;;                 only the information that one player should know

;; (struct Board Tile [NonEmptyListof PlayerInfo] [Listof GridPosn] (U Shift #f))
;; interpretation: A Gamestate has a board, an extra tile, players arranged in the order they
;;                 take turns (with the currently acting player at the front of the list)
;;                 a queue of goals to be chased and the previous shift made
(struct gamestate [board extra-tile players goals prev-shift] #:transparent)

(define player-state-player-infos?
  (or/c empty? (cons/c ref-player-info? (listof pub-player-info?))))

;; Gamestate -> Boolean
;; Is this gamestate a PlayerState?
(define (player-state? state)
  (and (gamestate? state)
       (player-state-player-infos? (gamestate-players state))
       (equal? (gamestate-goals state) 'hidden)))

;; Gamestate -> Boolean
;; Is this gamestate a RefereeState?
(define (referee-state? state)
  (and (gamestate? state)
       ((listof ref-player-info?) (gamestate-players state))
       ((listof grid-posn?) (gamestate-goals state))))
  
;; Board Tile (U empty (cons RefPlayerInfo [Listof PubPlayerInfo])) Shift -> Gamestate
;; Create a new player state
(define (player-state-new board extra-tile players prev-shift)
  (gamestate board extra-tile players 'hidden prev-shift))

;; Board Tile [Listof RefPlayerInfo] [Listof GridPosn] Shift -> Gamestate
;; Create a new referee state
(define (referee-state-new board extra-tile players goals prev-shift)
  (gamestate board extra-tile players goals prev-shift))

;; A Move is a structure:
;;    (struct GridPosn Shift Orientation)
;; interpretation: A Move has a position to move the currently active player to after the shift,
;;                 a shift, and the number of degrees to rotate the spare tile
(struct move [pos shift orientation] #:transparent)

(define (move-new pos shft orientation)
  (move pos shft orientation))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; Gamestate Move -> Gamestate
;; Execute a move
(define (gamestate-execute-move state mv)
  (gamestate-move-player
   (gamestate-shift-and-insert state
                               (move-shift mv)
                               (move-orientation mv))
   (move-pos mv)))


;; Gamestate Shift Orientation -> Gamestate
;; Shifts a row or column and inserts a tile in the empty space
(define (gamestate-shift-and-insert state shft orientation)
  (define-values
    (new-board new-extra-tile)
    (board-shift-and-insert
     (gamestate-board state) shft (tile-rotate (gamestate-extra-tile state) orientation)))
  (define players-after-shift
    (shift-players state shft))
  (struct-copy gamestate state
               [board new-board]
               [extra-tile new-extra-tile]
               [players players-after-shift]
               [prev-shift shft]))


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


;; RefereeState AvatarColor -> RefereeState
;; Assign the next treasure in the queue to a player. If the queue is empty, tell them to go home
(define (assign-next-goal state color)
  (define plyr (gamestate-get-by-color state color))
  (define next-goal
    (cond
      [(empty? (gamestate-goals state)) #f]
      [(and (= (length (gamestate-goals state)) 1)
            (equal? (first (gamestate-goals state)) (player-info-home-pos plyr))) #f]
      [else (first (gamestate-goals state))]))
                                
  (define new-players (replacef (gamestate-players state)
                                (λ (plyr) (equal? color (player-info-color plyr)))
                                (receive-next-goal plyr next-goal)))

  (define new-goals (if (empty? (gamestate-goals state))
                        empty
                        (rest (gamestate-goals state))))
  
  (struct-copy gamestate state
               [players new-players]
               [goals new-goals]))

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


;; Gamestate AvatarColor -> Gamestate
;; Remove the player with the given color from the game
(define (remove-player-by-color state color)
  (struct-copy gamestate state
               [players (filter (λ (plyr) (not (equal? (player-info-color plyr) color)))
                                (gamestate-players state))]))


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
   'hidden
   (gamestate-prev-shift state)))

;; Gamestate GridPosn -> Gamestate
;; Changes the treasure position of the active player
(define (replace-active-player-dummy-goal state new-treasure)
  (define new-players (cons (replace-dummy-goal (gamestate-current-player state) new-treasure)
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

;; Gamestate -> [Listof PlayerInfo]
;; Find the player(s) that have visited the highest number of goals
(define (gamestate-players-with-max-num-goals state)
  (all-with-max-num-goals-visited (gamestate-players state)))

;; [Listof Any] -> [Listof Any]
;; Move an item to the front of the list, if it exists. If more than one of the element
;; exists, only the first occurrence is moved.
(define (move-to-front elem lst)
  (cond
    [(not (member elem lst)) lst]
    [else (cons elem (remove elem lst))]))


(module+ draw
  (require 2htdp/image)
  (require racket/function)
  (require (submod "tile.rkt" draw))
  (require (submod "board.rkt" draw))

  (define DEFAULT-TILE-SIZE 100)
  (define PIECE-SIZE (/ DEFAULT-TILE-SIZE 4))
  (define ARM-LENGTH (/ DEFAULT-TILE-SIZE 10))

  (provide
   (contract-out
    ;; Draw a referee state
    [referee-state->image (-> referee-state? hash? natural-number/c image?)]))

  ;; RefereeState [MultipleOf 10] -> Image
  ;; Draw a referee state
  (define (referee-state->image state color:name tile-size)
    (define board-with-players-img (board-and-players->image (gamestate-board state) (gamestate-players state) color:name tile-size))
    (define board+spare (beside/align "bottom" board-with-players-img (rectangle tile-size 1 "solid" "white") (tile->image (gamestate-extra-tile state) tile-size)))
    (define player-progress-img (draw-all-players-collected-goals (gamestate-players state) color:name))
    (define goal-queue-img (text (apply string-append (map pair->string (gamestate-goals state))) 14 "black"))
    (above (beside board+spare player-progress-img)
           goal-queue-img))
    
  ;; Board [Listof RefPlayerInfo] [HashTable AvatarColor : String] [MultipleOf 10] -> Image
  ;; Given a board image, adds player avatars, home locations, and goal positions
  (define (board-and-players->image board player-infos color:name tile-size)
    (define base-board-img (board->image board tile-size))
    (foldl (curryr add-player-info-to-board-image tile-size color:name) base-board-img player-infos))

  ;; String -> Number
  ;; Convert a hexidecimal string to a number
  (define (hex->number h)
    (string->number (string-append "#x" h)))

  ;; AvatarColor -> Color
  ;; Convert a AvatarColor to a usable color
  (define (usable-player-color color)
    (if (hex-color-code? color)
        (make-color (hex->number (substring color 0 2))
                    (hex->number (substring color 2 4))
                    (hex->number (substring color 4)))
        color))

  ;; Image Image GridPosn [MultipleOf 10] PositiveInteger -> Image
  (define (add-attribute-to-board-by-gridposn board-img attribute gp tile-size attribute-size)
    (define-values (y-val x-val) (values (car gp) (cdr gp)))
    (define attribute-x-pos (- (+ (/ tile-size 2) (* x-val tile-size)) attribute-size))
    (define attribute-y-pos (- (+ (/ tile-size 2) (* y-val tile-size)) attribute-size))
    (underlay/xy board-img attribute-x-pos attribute-y-pos attribute))

  ;; RefPlayerInfo Name -> Image
  ;; Draw a RefPlayerInfo's avatar
  (define (draw-avatar plyr-info name size)
    (overlay (text name 14 "black")
             (circle size "solid" (usable-player-color (player-info-color plyr-info)))))
    
  ;; Image [MultipleOf 10] RefPlayerInfo -> Image
  (define (add-player-info-to-board-image plyr-info board-img tile-size color:name)
    (define avatar-size (/ tile-size 4))
    (define avatar (draw-avatar plyr-info (hash-ref color:name (player-info-color plyr-info)) avatar-size))
    (define board-img-with-avatar (add-attribute-to-board-by-gridposn board-img avatar (player-info-curr-pos plyr-info) tile-size avatar-size))

    (define goal-size (/ tile-size 4))
    (define goal (star goal-size "solid" (usable-player-color (player-info-color plyr-info))))
    (define board-img-with-goal (add-attribute-to-board-by-gridposn board-img-with-avatar goal (player-info-goal-pos plyr-info) tile-size goal-size))


    (define home-size (/ tile-size 4))
    (define home (triangle home-size "solid" (usable-player-color (player-info-color plyr-info))))
    (define board-img-with-home (add-attribute-to-board-by-gridposn board-img-with-goal home (player-info-home-pos plyr-info) tile-size home-size))
    board-img-with-home)

  ;; [Listof RefPlayerInfo [HashTable AvatarColor : String] -> Image
  (define (draw-all-players-collected-goals players color:name)
    (apply above (for/list ([plyr players])
                   (draw-collected-goals plyr (hash-ref color:name (player-info-color plyr))))))
           
  ;; RefPlayerInfo Name -> Image
  ;; Draw a RefPlayerInfo's collected goals next to their name
  (define (draw-collected-goals plyr-info name)
    (define goals-txt (text (apply string-append (map pair->string (player-info-goals-visited plyr-info))) 14 "black"))
    (define avatar (draw-avatar plyr-info name PIECE-SIZE))
    (beside avatar goals-txt)))
    

(module+ serialize
  (require json)
  (require (submod "board.rkt" serialize))
  (require (submod "tile.rkt" serialize))
  (require (submod "player-info.rkt" serialize))

  ; TODO: write some tests
  
  (provide
   (contract-out
    [json-public-state? contract?]
    [json-referee-state? contract?]
    ; Convert a RefereeState to a JsonRefereeState
    [ref-state->json-referee-state (-> referee-state? json-referee-state?)]
    ; Convert a PlayerState to a JsonPlayerState
    [player-state->json-public-state (-> player-state? json-public-state?)]
    ; Convert a JsonRefState into a RefState
    [json-referee-state->ref-state (-> json-referee-state? referee-state?)]
    ; Convert a JsonPlayerState into a PlayerState. Uses a dummy treasure value for the active player.
    [json-public-state->player-state (-> json-public-state? player-state?)]
    ; Convert a JsonPlayerState and a GridPosn into a PlayerState
    [json-public-state-and-goal-gridposn->player-state (-> json-public-state? grid-posn? player-state?)]))

  ;; Any -> Boolean
  ;; Is this object a hashtable JSON representation of a PlayerState
  (define (json-public-state? ht)
    (and (hash? ht)
         (hash-has-key? ht 'board)
         (hash-has-key? ht 'spare)
         (hash-has-key? ht 'plmt)
         (hash-has-key? ht 'last)
         (json-board? (hash-ref ht 'board))
         (json-tile?  (hash-ref ht 'spare))
         ((listof json-public-player-info?) (hash-ref ht 'plmt))
         (json-action? (hash-ref ht 'last))))

  ;; Any -> Boolean
  ;; Is this object a hashtable JSON representation of a RefereeState
  (define (json-referee-state? ht)
    (and (hash? ht)
         (hash-has-key? ht 'board)
         (hash-has-key? ht 'spare)
         (hash-has-key? ht 'plmt)
         (hash-has-key? ht 'last)
         (json-board? (hash-ref ht 'board))
         (json-tile?  (hash-ref ht 'spare))
         ((listof json-referee-player-info?) (hash-ref ht 'plmt))
         (json-action? (hash-ref ht 'last))
         (if (hash-has-key? ht 'goals)
             ((listof json-coordinate?) (hash-ref ht 'goals))
             #t)))

  ;; RefereeState -> JsonRefereeState
  ;; Convert a RefState into a JsonRefState
  (define (ref-state->json-referee-state ref-state)
    (hash 'board (board->json-board (gamestate-board ref-state))
          'spare (tile->json-tile (gamestate-extra-tile ref-state))
          'plmt (map referee-player-info->json-referee-player-info (gamestate-players ref-state))
          'last (prev-shift->json-action (gamestate-prev-shift ref-state))))

  ;; PlayerState -> JsonPlayerState
  ;; Convert a PlayerState into a JsonPlayerState
  (define (player-state->json-public-state plyr-state)
    (hash 'board (board->json-board (gamestate-board plyr-state))
          'spare (tile->json-tile (gamestate-extra-tile plyr-state))
          'plmt (map player-info->json-public-player-info (gamestate-players plyr-state))
          'last (prev-shift->json-action (gamestate-prev-shift plyr-state))))

  ;; JsonRefereeState -> RefereeState
  ;; Convert a JsonRefState into a RefState
  (define (json-referee-state->ref-state json-ref-state)
    (referee-state-new (json-board->board (hash-ref json-ref-state 'board))
                       (json-tile->tile (hash-ref json-ref-state 'spare))
                       (map json-referee-player-info->referee-player-info (hash-ref json-ref-state 'plmt))
                       (if (hash-has-key? json-ref-state 'goals)
                           (map json-coordinate->gridposn (hash-ref json-ref-state 'goals))
                           empty)
                       (json-action->prev-shift (hash-ref json-ref-state 'last))))
  
  ;; JsonPlayerState -> PlayerState
  ;; Convert a JsonPlayerState into a PlayerState
  ;; IMPORTANT: The active player is given a dummy treasure tile at (1, 1)
  (define (json-public-state->player-state json-pub-state)
    (json-public-state-and-goal-gridposn->player-state json-pub-state (cons 1 1)))

  ;; JsonPlayerState GridPosn -> PlayerState
  ;; Convert a JsonPlayerState and goal GridPosn into a PlayerState
  ;; TODO: Right now, this hard-codes that the player has NOT visited their treasure. That is probably a problem.
  (define (json-public-state-and-goal-gridposn->player-state json-plyr-state goal)
    
    (define public-plyr-list (map json-public-player-info->public-player-info (hash-ref json-plyr-state 'plmt)))
    (define active-player-as-ref-player (pub-player-info->ref-player-info (first public-plyr-list) goal '() #f))
    (define plyr-list-with-goal-for-active-player (cons active-player-as-ref-player (rest public-plyr-list)))
    
    (player-state-new (json-board->board (hash-ref json-plyr-state 'board))
                      (json-tile->tile (hash-ref json-plyr-state 'spare))
                      plyr-list-with-goal-for-active-player
                      (json-action->prev-shift (hash-ref json-plyr-state 'last)))))

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
  (define gamestate0 (gamestate board1 tile-extra player-infos0 empty #f))

  (define player-infos1 (list player-info3 player-info4))
  ; player-info1 (a) not on treasure on home
  (define gamestate1 (gamestate board1 tile-extra player-infos1 empty #f))
  (define player-infos2 (list player-info1 player-info2 player-info3 player-info4))
  ; on treasure not home
  (define gamestate2 (gamestate board1 tile-extra player-infos2 empty #f))

  (define player-infos3 (list player-info1 player-info0 player-info5 player-info6 player-info7))
  (define gamestate3 (gamestate board1 tile-extra player-infos3 empty #f))

  (define player-infos4 (list player-info0 player-info1 player-info2 player-info5))
  (define gamestate4 (gamestate board1 tile-extra player-infos4 empty #f))

  (define player-infos5 (list player-info8 player-info5 player-info7))
  (define gamestate5 (gamestate board1 tile-extra player-infos5 empty #f))

  (define gamestate6 (gamestate board1 tile-extra player-infos0 (list (cons 5 3) (cons 3 5)) #f))
  (define gamestate7 (gamestate board1 tile-extra (list ref-player-info10 ref-player-info11) (list (cons 1 5)) #f))
  (define gamestate8 (gamestate board1 tile-extra (list ref-player-info10 ref-player-info11) (list (cons 1 5)
                                                                                                   (cons 3 3)
                                                                                                   (cons 1 3)
                                                                                                   (cons 1 1)
                                                                                                   (cons 5 5)
                                                                                                   (cons 5 5)) #f))

  (define player-state0 (gamestate board1 tile-extra (list player-info2) 'hidden (shift-new 'up 0)))
  (define player-state1 (gamestate board1 tile-extra (list player-info7) 'hidden (shift-new 'down 4)))
  (define player-state-nowhere-to-go (gamestate board-nowhere-to-go tile-extra (list player-info3) 'hidden (shift-new 'right 4))))
  

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "board.rkt" examples))
  (require (submod "tile.rkt" examples))
  (require (submod "player-info.rkt" examples))
  (require (submod ".." serialize)))

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
                (tile-new 'straight 270 (list 'yellow-baguette 'yellow-beryl-oval)))
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'left 0) 180)) 0) 6)
                (tile-new 'straight 0 (list 'yellow-baguette 'yellow-beryl-oval)))
  (check-equal? (list-ref
                 (list-ref (gamestate-board (gamestate-shift-and-insert gamestate0 (shift-new 'right 6) 270)) 6)
                 0)
                (tile-new 'straight 90 (list 'yellow-baguette 'yellow-beryl-oval))))

;; test players on a shifted row/col are moved accordingly
(module+ test
  ; test moving players on moved row
  (check-equal? (shift-players
                 gamestate4
                 (shift-new 'right 0))
                (list
                 (ref-player-info-new (cons 0 1) (cons 6 6) (cons 5 1) empty #f "blue")
                 player-info1
                 player-info2
                 (ref-player-info-new (cons 0 0) (cons 5 5) (cons 1 5) empty #f "red")))
  (check-equal? (shift-players
                 gamestate4
                 (shift-new 'left 0))
                (list
                 (ref-player-info-new (cons 0 6) (cons 6 6) (cons 5 1) empty #f "blue")
                 player-info1
                 player-info2
                 (ref-player-info-new (cons 0 5) (cons 5 5) (cons 1 5) empty #f "red")))
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

;; test assign-next-goal
(module+ test
  (check-equal? (gamestate-players (assign-next-goal gamestate0 "blue"))
                (list (ref-player-info-new (cons 0 0) (cons 6 6) (cons 6 6) (list (cons 5 1)) #t "blue")
                      player-info1 player-info2 player-info3 player-info4))
  (check-equal? (gamestate-players (assign-next-goal gamestate0 "yellow"))
                (list player-info0 player-info1 player-info2
                      (ref-player-info-new (cons 3 3) (cons 3 3) (cons 3 3) (list (cons 1 3)) #t "yellow")
                      player-info4))
  (check-equal? (gamestate-players (assign-next-goal gamestate6 "yellow"))
                (list player-info0 player-info1 player-info2
                      (ref-player-info-new (cons 3 3) (cons 3 3) (cons 5 3) (list (cons 1 3)) #f "yellow")
                      player-info4))
  (check-equal? (gamestate-players (assign-next-goal gamestate6 "blue"))
                (list (ref-player-info-new (cons 0 0) (cons 6 6) (cons 5 3) (list (cons 5 1)) #f "blue")
                      player-info1 player-info2 player-info3 player-info4))
  (check-equal? (gamestate-goals (assign-next-goal gamestate6 "blue"))
                (list (cons 3 5)))
  (check-equal? (gamestate-goals (assign-next-goal (assign-next-goal gamestate6 "yellow") "blue"))
                empty)
  (check-equal? (gamestate-goals (assign-next-goal gamestate0 "blue"))
                empty))
              

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

;; test remove-player-by-color`
(module+ test
  (check-equal? (gamestate-players (remove-player-by-color gamestate0 "blue"))
                (list player-info1 player-info2 player-info3 player-info4))
  (check-equal? (gamestate-players (remove-player-by-color gamestate0 "purple"))
                (list player-info0 player-info2 player-info3 player-info4)))

;; test end-current-turn
(module+ test
  (check-equal? (gamestate-current-player (end-current-turn gamestate0)) player-info1)
  (check-equal? (gamestate-current-player (end-current-turn gamestate1)) player-info4)
  (check-equal? (gamestate-current-player (end-current-turn gamestate2)) player-info2))

;; Test player-on-pos
(module+ test
  (check-true (player-info-on-pos? player-info0 (cons 0 0))))


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
                           'hidden
                           #f)))
; test get-player-color-list
(module+ test
  (check-equal? (get-player-color-list gamestate0) (list "blue" "purple" "green" "yellow" "black")))

; test move-to-front
(module+ test
  (check-equal? (move-to-front 1 '(2 3 1 4)) '(1 2 3 4))
  (check-equal? (move-to-front 1 '(2 3 4)) '(2 3 4))
  (check-equal? (move-to-front 1 '()) '())
  (check-equal? (move-to-front 5 '(5 6 7)) '(5 6 7))
  (check-equal? (move-to-front 7 '(5 6 7)) '(7 5 6))
  (check-equal? (move-to-front 1 '(5 6 1 8 1 7)) '(1 5 6 8 1 7)))

; test ref-state->hash
(module+ test
  (check-equal? (ref-state->json-referee-state gamestate0)
                (hash 'spare (hash 'tilekey "│"
                                   '1-image "yellow-baguette"
                                   '2-image "yellow-beryl-oval")
                      'plmt (list (hash 'color  "blue"
                                        'current  (hash 'column# 0 'row# 0)
                                        'goto (hash 'column# 1 'row# 5)
                                        'home (hash 'column# 6 'row# 6))
                                  (hash 'color  "purple"
                                        'current  (hash 'column# 1 'row# 1)
                                        'goto (hash 'column# 1 'row# 1)
                                        'home (hash 'column# 5 'row# 5))
                                  (hash 'color "green"
                                        'current (hash 'column# 2 'row# 2)
                                        'goto (hash 'column# 3 'row# 3)
                                        'home (hash 'column# 4 'row# 4))
                                  (hash 'color "yellow"
                                        'current (hash 'column# 3 'row# 3)
                                        'goto (hash 'column# 3 'row# 1)
                                        'home (hash 'column# 3 'row# 3))
                                  (hash 'color "black"
                                        'current (hash 'column# 4 'row# 4)
                                        'goto (hash 'column# 5 'row# 5)
                                        'home (hash 'column# 2 'row# 2)))
                      'last 'null
                      'board (hash 'connectors
                                   (list (list "─" "┐" "└" "┘" "┌" "┬" "┤")
                                         (list "┴" "├" "┼" "│" "─" "┐" "└")
                                         (list "┘" "┌" "┬" "┤" "┴" "├" "┼")
                                         (list "│" "─" "┐" "└" "┘" "┌" "┬")
                                         (list "┤" "┴" "├" "┼" "│" "─" "┐")
                                         (list "└" "┘" "┌" "┬" "┤" "┴" "├")
                                         (list "┼" "│" "─" "┐" "└" "┘" "┌"))
                                   'treasures
                                   (list (list (list "alexandrite-pear-shape" "alexandrite")
                                               (list "almandine-garnet" "amethyst")
                                               (list "amethyst" "ametrine")
                                               (list "ammolite" "apatite")
                                               (list "aplite" "apricot-square-radiant")
                                               (list "aquamarine" "australian-marquise")
                                               (list "aventurine" "azurite"))
                                         (list (list "beryl" "black-obsidian")
                                               (list "black-obsidian" "black-onyx")
                                               (list "black-spinel-cushion" "blue-ceylon-sapphire")
                                               (list "blue-cushion" "blue-pear-shape")
                                               (list "blue-spinel-heart" "bulls-eye")
                                               (list "carnelian" "chrome-diopside")
                                               (list "chrysoberyl-cushion" "chrysolite"))
                                         (list (list "citrine-checkerboard" "citrine")
                                               (list "clinohumite" "color-change-oval")
                                               (list "cordierite" "diamond")
                                               (list "dumortierite" "emerald")
                                               (list "fancy-spinel-marquise" "garnet")
                                               (list "golden-diamond-cut" "goldstone")
                                               (list "grandidierite" "gray-agate"))
                                         (list (list "green-aventurine" "green-beryl-antique")
                                               (list "green-beryl" "green-princess-cut")
                                               (list "grossular-garnet" "hackmanite")
                                               (list "heliotrope" "hematite")
                                               (list "iolite-emerald-cut" "jasper")
                                               (list "jaspilite" "kunzite-oval")
                                               (list "kunzite" "labradorite"))
                                         (list (list "lapis-lazuli" "lemon-quartz-briolette")
                                               (list "magnesite" "mexican-opal")
                                               (list "moonstone" "morganite-oval")
                                               (list "moss-agate" "orange-radiant")
                                               (list "padparadscha-oval" "padparadscha-sapphire")
                                               (list "peridot" "pink-emerald-cut")
                                               (list "pink-opal" "pink-round"))
                                         (list (list "pink-spinel-cushion" "prasiolite")
                                               (list "prehnite" "purple-cabochon")
                                               (list "purple-oval" "purple-spinel-trillion")
                                               (list "purple-square-cushion" "raw-beryl")
                                               (list "raw-citrine" "red-diamond")
                                               (list "red-spinel-square-emerald-cut" "rhodonite")
                                               (list "rock-quartz" "rose-quartz"))
                                         (list (list "ruby-diamond-profile" "ruby")
                                               (list "sphalerite" "spinel")
                                               (list "star-cabochon" "stilbite")
                                               (list "sunstone" "super-seven")
                                               (list "tanzanite-trillion" "tigers-eye")
                                               (list "tourmaline-laser-cut" "tourmaline")
                                               (list "unakite" "white-square")))))))
