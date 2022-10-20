#lang racket/base

;;; This module provides data definitions and logic for a strategy to
;;; play the Maze game


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [move?          contract?]
  [action?        contract?]
  [strategy?      contract?]
  [player-state?  contract?]
  ; Riemann strategy
  [riemann-strategy   strategy?]
  ; Euclidean strategy
  [euclidean-strategy strategy?]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/match)
(require racket/list)
(require racket/function)

(require "state.rkt")
(require "board.rkt")
(require "tile.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; Player Player -> Boolean
;; Are the two players the same?
(define (move=? p1 p2 rec)
  (and (rec (player-curr-pos p1) (player-curr-pos p2))
       (rec (player-home-pos p1) (player-home-pos p2))
       (rec (player-goal-pos p1) (player-goal-pos p2))
       (rec (player-visited-goal p1) (player-visited-goal p2))
       (rec (player-color p1) (player-color p2))))

(define (player-hash-code pl rec)
  (+ (* 10000 (rec (player-curr-pos pl)))
     (* 1000  (rec (player-home-pos pl)))
     (* 100   (rec (player-goal-pos pl)))
     (* 10    (rec (player-visited-goal pl)))
     (* 1     (rec (player-color pl)))))

(define (player-secondary-hash-code pl rec)
  (+ (* 10000 (rec (player-color pl)))
     (* 1000  (rec (player-visited-goal pl)))
     (* 100   (rec (player-goal-pos pl)))
     (* 10    (rec (player-home-pos pl)))
     (* 1     (rec (player-curr-pos pl)))))

;; A Move is a structure:
;;    (struct ShiftDirection Natural Orientation GridPosn)
;; interpretation: A Move has a direction to shift a row or column, the index of the
;;                 row or column to shift, the number of degrees to rotate the spare
;;                 tile, and a position to move the currently active player to after the shift
(struct move [pos shift-direction idx orientation] #:transparent)


;; An Action is one of:
;;    - Move
;;    - #f
;; interpretation: A player acts by either making a move or making no move (passing turn)
(define action? (or/c #f move?))


;; A PlayerState is a structure:
;;    (struct Board Tile Player)
;; interpretation: A player knows the board, the extra tile, and all of its information
(struct player-state [board extra-tile player])
  

;; A Strategy is a function:
;;    (-> Gamestate Move)
;; interpretation: A strategy examines a gamestate and determines a move for the currently active
;;                 player to make
(define strategy? (-> player-state? action?))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; PlayerState -> Action
;; Determine the player's move using the riemann strategy
(define (riemann-strategy plyr-state)
  (define candidates (get-riemann-candidates
                      (player-state-board plyr-state)
                      (player-state-player plyr-state)))
  (get-first-valid-move plyr-state candidates))

;; Board Player -> [Listof GridPosn]
;; Order the possible candidates for riemann search
(define (get-riemann-candidates board plyr)
  (define goal-pos
    (if (player-visited-goal? plyr)
        (player-get-home-pos plyr)
        (player-get-goal-pos plyr)))
  (cons goal-pos (filter (lambda (pos)
                           (not (equal? pos goal-pos)))
                         (get-all-positions board))))


;; PlayerState -> Action
;; Determine a player's move using the euclidean strategy
(define (euclidean-strategy plyr-state)
  #f)
  

;; PlayerState [Listof Move] -> Action
;; Finds the first Move which is valid in a PlayerState
(define (get-first-valid-move plyr-state candidates)
  (findf
   (λ (mv) (valid-move? plyr-state mv))
   (all-possible-moves (player-state-board plyr-state) candidates)))   


;; PlayerState Move -> Boolean
;; Returns True if the 
(define (valid-move? plyr-state mv)
  (define old-board  (player-state-board plyr-state))
  (define old-player (player-state-player plyr-state))
  
  (define-values
    (new-board new-extra-tile)
    (board-shift-and-insert
     old-board
     (move-shift-direction mv)
     (move-idx mv)
     (tile-rotate (player-state-extra-tile plyr-state) (move-orientation mv))))
  
  (define new-player
    (first (shift-players (list
                           (player-state-player plyr-state))
                          old-board
                          (move-shift-direction mv)
                          (move-idx mv))))
  
  (member
   (move-pos mv)
   (board-all-reachable-from new-board (player-get-curr-pos new-player))))

;; Board -> [Listof GridPosn]
;; Get all possible positions in a gamestate
(define (get-all-positions board)
  (apply append (for/list ([x (in-range 0 (num-cols board))])
      (for/list ([y (in-range 0 (num-rows board))])
        (cons x y)))))


;; Board -> [Listof Move]
;; Get all possible board shift and inserts
(define (all-possible-moves board candidates)
  (map (λ (x) (apply move x))
       (cartesian-product candidates shift-directions (get-valid-shift-indices board) orientations)))


;; Board Move Tile -> Board
;; Apply a move to a board
(define (apply-move mv board spare)
  (define-values (new-board new-tile)
    (board-shift-and-insert
     board
     (move-shift-direction mv)
     (move-idx mv)
     (tile-rotate spare (move-orientation mv))))
  new-board)

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (require (submod "tile.rkt" examples))
  (require (submod "board.rkt" examples))
  (require (submod "state.rkt" examples))
  (define player-state-1 (player-state board1 tile-extra player2)))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "board.rkt" examples))
  (require (submod "state.rkt" examples)))


(module+ test
  (check-equal? (riemann-strategy player-state-1) (move (cons 3 3) 'right 2 0)))


(module+ test
  (check-equal? (get-riemann-candidates board1 player1)
                (list (cons 1 1)
                      (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6)
                      (cons 1 0)            (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5) (cons 1 6)
                      (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5) (cons 2 6)
                      (cons 3 0) (cons 3 1) (cons 3 2) (cons 3 3) (cons 3 4) (cons 3 5) (cons 3 6)
                      (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                      (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                      (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6))))

;; test get-all-positions
(module+ test
  (check-equal? (get-all-positions board1)
                (list (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6)
                      (cons 1 0) (cons 1 1) (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5) (cons 1 6)
                      (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5) (cons 2 6)
                      (cons 3 0) (cons 3 1) (cons 3 2) (cons 3 3) (cons 3 4) (cons 3 5) (cons 3 6)
                      (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                      (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                      (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6))))