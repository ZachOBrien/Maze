#lang racket/base

;;; This module provides data definitions and logic for a strategy to
;;; play the Maze game


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [move?          contract?]
  [strategy?      contract?]
  [player-state?  contract?]
  ; Gets all positions possible in the state
  [get-all-positions (-> board? (listof grid-posn?))]))
#;((
  [riemann-strategy   strategy?]
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


;; A Move is a structure:
;;    (struct ShiftDirection Natural Orientation GridPosn)
;; interpretation: A Move has a direction to shift a row or column, the index of the
;;                 row or column to shift, the number of degrees to rotate the spare
;;                 tile, and a position to move the currently active player to after the shift
(struct move [shift-direction idx orientation pos] #:transparent)


;; A PlayerState is a structure:
;;    (struct Board Tile Player)
;; interpretation: A player knows the board, the extra tile, and all of its information
(struct player-state [board extra-tile player])
  

;; A Strategy is a function:
;;    (-> Gamestate Move)
;; interpretation: A strategy examines a gamestate and determines a move for the currently active
;;                 player to make
(define strategy? (-> player-state? move?))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; Board -> [Listof GridPosn]
;; Get all possible positions in a gamestate
(define (get-all-positions board)
  (apply append (for/list ([x (in-range 0 (num-cols board))])
      (for/list ([y (in-range 0 (num-rows board))])
        (cons x y)))))


;; Board Player -> [Listof GridPosn]
;; Order the possible candidates for riemann search
(define (riemann-ordering board plyr)
  (define goal-pos (player-get-goal-pos plyr))
  (cons goal-pos (filter (lambda (pos)
                           (not (equal? pos goal-pos)))
                         (get-all-positions board))))


;; Board -> [Listof Move]
;; Get all possible board shift and inserts
(define (all-possible-moves board candidates)
  (define indices (range 0 (num-cols board) 2))
  (define directions (list 'up 'down 'left 'right))
  (define rotations (list 0 90 180 270))
  (map (λ (x) (apply move x)) (cartesian-product directions indices rotations candidates)))


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


;; PlayerState -> Move
;; Determine the player's move using the riemann strategy
(define (riemann-strategy state)
  (define curr-pos (player-get-curr-pos (player-state-player state)))
  (define candidates (riemann-ordering (player-state-board state) (player-state-player state)))
  (define possible-moves (all-possible-moves (player-state-board state) candidates))

  (findf (λ (mv) (member
                  (move-pos mv)
                  (board-all-reachable-from
                   (apply-move mv (player-state-board state) (player-state-extra-tile state)) curr-pos)))
           possible-moves))


;; PlayerState -> Move
;; Determine the player's move using the euclidean strategy
#;
(define (euclidean-strategy state)
  (define goal-tile ...)
  (define candidates ...)
  
  (define ordered_candidates (sort candidates -laksjdf-compare-row-col)

  (define all-boards ...)

  (iterate-over-list (member? result) candidates all-boards)))


(module+ examples
  (provide (all-defined-out))
  (require (submod "tile.rkt" examples))
  (require (submod "board.rkt" examples))
  (require (submod "state.rkt" examples))
  (define player-state-1 (player-state board1 tile-extra player1)))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "board.rkt" examples))
  (require (submod "state.rkt" examples)))


(module+ test
  (check-equal? (riemann-strategy player-state-1) 0))


(module+ test
  (check-equal? (riemann-ordering board1 player1)
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