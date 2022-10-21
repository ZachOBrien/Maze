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
(define (move=? mv1 mv2 rec)
  (and (rec (move-pos mv1) (move-pos mv2))
       (rec (move-shift-direction mv1) (move-shift-direction mv2))
       (rec (move-idx mv1) (move-idx mv2))
       (rec (move-orientation mv1) (move-orientation mv2))))

(define (move-hash-code mv rec)
  (+ (* 1000 (rec (move-pos mv)))
     (* 100  (rec (move-shift-direction mv)))
     (* 10   (rec (move-idx mv)))
     (* 1    (rec (move-orientation mv)))))

(define (move-secondary-hash-code mv rec)
  (+ (* 1000 (rec (move-orientation mv)))
     (* 100  (rec (move-idx mv)))
     (* 10   (rec (move-shift-direction mv)))
     (* 1    (rec (move-pos mv)))))

;; A Move is a structure:
;;    (struct ShiftDirection Natural Orientation GridPosn)
;; interpretation: A Move has a direction to shift a row or column, the index of the
;;                 row or column to shift, the number of degrees to rotate the spare
;;                 tile, and a position to move the currently active player to after the shift
(struct move [pos shift-direction idx orientation]
  #:methods gen:equal+hash
  [(define equal-proc move=?)
   (define hash-proc  move-hash-code)
   (define hash2-proc move-secondary-hash-code)]
  #:transparent)


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
  (define goal-pos (get-goal-gp plyr))
  (cons goal-pos (filter (lambda (pos)
                           (not (equal? pos goal-pos)))
                         (get-all-positions board))))


;; PlayerState -> Action
;; Determine a player's move using the euclidean strategy
(define (euclidean-strategy plyr-state)
  (define candidates (get-euclidean-candidates
                      (player-state-board plyr-state)
                      (player-state-player plyr-state)))
  (get-first-valid-move plyr-state candidates))


;; PlayerState -> Action
;; Order the possible candidates for euclidean search
(define (get-euclidean-candidates board plyr)
  (define goal-pos (get-goal-gp plyr))
  (define all-candidates (get-all-positions board))
  (sort all-candidates (lambda (pos1 pos2) (compare-euclidean-dist goal-pos pos1 pos2))))


;; GridPosn GridPosn GrisPosn -> Boolean
;; Compares two gridposns and returns whether the first is less than the second
(define (compare-euclidean-dist goal pos1 pos2)
  (if (= (euclidian-dist goal pos1) (euclidian-dist goal pos2))
      (compare-row-col pos1 pos2)
      (< (euclidian-dist goal pos1) (euclidian-dist goal pos2))))


;; GridPosn GridPosn -> Natural
;; Computes the euclidean distance between two gridposns
(define (euclidian-dist pos1 pos2)
  (sqrt (+ (expt (- (car pos2) (car pos1)) 2) (expt (- (cdr pos2) (cdr pos1)) 2))))

;; Player -> GridPosn
;; Determines the current goal for the player
(define (get-goal-gp plyr)
  (if (player-visited-goal? plyr)
      (player-get-home-pos plyr)
      (player-get-goal-pos plyr)))
    
    
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
  
  (and (not (equal? (move-pos mv) (player-get-curr-pos new-player)))
       (member
        (move-pos mv)
        (board-all-reachable-from new-board (player-get-curr-pos new-player)))))

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


;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (require (submod "tile.rkt" examples))
  (require (submod "board.rkt" examples))
  (require (submod "state.rkt" examples))
  
  (define player-state-1 (player-state board1 tile-extra player2))
  (define player-state-2 (player-state board1 tile-extra player7))
  (define player-state-nowhere-to-go (player-state board-nowhere-to-go tile-extra player3))
  (define cand-list-1 (list (cons 1 1) (cons 0 1) (cons 1 0) (cons 1 2) (cons 2 1) (cons 0 0) (cons 0 2)
                            (cons 2 0) (cons 2 2) (cons 1 3) (cons 3 1) (cons 0 3) (cons 2 3) (cons 3 0)
                            (cons 3 2) (cons 3 3) (cons 1 4) (cons 4 1) (cons 0 4) (cons 2 4) (cons 4 0)
                            (cons 4 2) (cons 3 4) (cons 4 3) (cons 1 5) (cons 5 1) (cons 0 5) (cons 2 5)
                            (cons 5 0) (cons 5 2) (cons 4 4) (cons 3 5) (cons 5 3) (cons 1 6) (cons 4 5)
                            (cons 5 4) (cons 6 1) (cons 0 6) (cons 2 6) (cons 6 0) (cons 6 2) (cons 3 6)
                            (cons 6 3) (cons 5 5) (cons 4 6) (cons 6 4) (cons 5 6) (cons 6 5) (cons 6 6)))
  (define cand-list-2 (list (cons 1 3) (cons 0 3) (cons 1 2) (cons 1 4) (cons 2 3) (cons 0 2) (cons 0 4)
                            (cons 2 2) (cons 2 4) (cons 1 1) (cons 1 5) (cons 3 3) (cons 0 1) (cons 0 5)
                            (cons 2 1) (cons 2 5) (cons 3 2) (cons 3 4) (cons 3 1) (cons 3 5) (cons 1 0)
                            (cons 1 6) (cons 4 3) (cons 0 0) (cons 0 6) (cons 2 0) (cons 2 6) (cons 4 2)
                            (cons 4 4) (cons 3 0) (cons 3 6) (cons 4 1) (cons 4 5) (cons 5 3) (cons 5 2)
                            (cons 5 4) (cons 4 0) (cons 4 6) (cons 5 1) (cons 5 5) (cons 5 0) (cons 5 6)
                            (cons 6 3) (cons 6 2) (cons 6 4) (cons 6 1) (cons 6 5) (cons 6 0) (cons 6 6)))
  (define cand-list-3 (list (cons 3 3) (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5)
                            (cons 0 6) (cons 1 0) (cons 1 1) (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5)
                            (cons 1 6) (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5)
                            (cons 2 6) (cons 3 0) (cons 3 1) (cons 3 2) (cons 3 4) (cons 3 5) (cons 3 6)
                            (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                            (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                            (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6))))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "board.rkt" examples))
  (require (submod "state.rkt" examples)))

; test riemann-strategy
(module+ test
  (check-equal? (riemann-strategy player-state-1) (move (cons 3 3) 'right 2 0)) ; Player can reach goal tile
  (check-equal? (riemann-strategy player-state-2) (move (cons 0 0) 'right 6 90)) ; Player cannot reach goal tile, reaches 0 0
  (check-false (riemann-strategy player-state-nowhere-to-go))) ; Player cannot go anywhere

; test euclidean-strategy
(module+ test
  (check-false (euclidean-strategy player-state-nowhere-to-go)) ; Player cannot go anywhere
  (check-equal? (euclidean-strategy player-state-2) (move (cons 5 2) 'right 6 90)) ; Player cannot reach goal tile, reaches closest tile
  (check-equal? (euclidean-strategy player-state-1) (move (cons 3 3) 'right 2 0))) ; Player can reach goal tile

; test get-euclidean-strategy
(module+ test
  (check-equal? (get-euclidean-candidates board1 player1)
                (list (cons 1 1) (cons 0 1) (cons 1 0) (cons 1 2) (cons 2 1) (cons 0 0) (cons 0 2)
                      (cons 2 0) (cons 2 2) (cons 1 3) (cons 3 1) (cons 0 3) (cons 2 3) (cons 3 0)
                      (cons 3 2) (cons 3 3) (cons 1 4) (cons 4 1) (cons 0 4) (cons 2 4) (cons 4 0)
                      (cons 4 2) (cons 3 4) (cons 4 3) (cons 1 5) (cons 5 1) (cons 0 5) (cons 2 5)
                      (cons 5 0) (cons 5 2) (cons 4 4) (cons 3 5) (cons 5 3) (cons 1 6) (cons 4 5)
                      (cons 5 4) (cons 6 1) (cons 0 6) (cons 2 6) (cons 6 0) (cons 6 2) (cons 3 6)
                      (cons 6 3) (cons 5 5) (cons 4 6) (cons 6 4) (cons 5 6) (cons 6 5) (cons 6 6)))
  (check-equal? (get-euclidean-candidates board1 player2)
                (list (cons 3 3) (cons 2 3) (cons 3 2) (cons 3 4) (cons 4 3) (cons 2 2) (cons 2 4)
                      (cons 4 2) (cons 4 4) (cons 1 3) (cons 3 1) (cons 3 5) (cons 5 3) (cons 1 2)
                      (cons 1 4) (cons 2 1) (cons 2 5) (cons 4 1) (cons 4 5) (cons 5 2) (cons 5 4)
                      (cons 1 1) (cons 1 5) (cons 5 1) (cons 5 5) (cons 0 3) (cons 3 0) (cons 3 6)
                      (cons 6 3) (cons 0 2) (cons 0 4) (cons 2 0) (cons 2 6) (cons 4 0) (cons 4 6)
                      (cons 6 2) (cons 6 4) (cons 0 1) (cons 0 5) (cons 1 0) (cons 1 6) (cons 5 0)
                      (cons 5 6) (cons 6 1) (cons 6 5) (cons 0 0) (cons 0 6) (cons 6 0) (cons 6 6)))
  (check-equal? (get-euclidean-candidates board1 player3)
                (list (cons 1 3) (cons 0 3) (cons 1 2) (cons 1 4) (cons 2 3) (cons 0 2) (cons 0 4)
                      (cons 2 2) (cons 2 4) (cons 1 1) (cons 1 5) (cons 3 3) (cons 0 1) (cons 0 5)
                      (cons 2 1) (cons 2 5) (cons 3 2) (cons 3 4) (cons 3 1) (cons 3 5) (cons 1 0)
                      (cons 1 6) (cons 4 3) (cons 0 0) (cons 0 6) (cons 2 0) (cons 2 6) (cons 4 2)
                      (cons 4 4) (cons 3 0) (cons 3 6) (cons 4 1) (cons 4 5) (cons 5 3) (cons 5 2)
                      (cons 5 4) (cons 4 0) (cons 4 6) (cons 5 1) (cons 5 5) (cons 5 0) (cons 5 6)
                      (cons 6 3) (cons 6 2) (cons 6 4) (cons 6 1) (cons 6 5) (cons 6 0) (cons 6 6))))

; test get-riemann-candidates
(module+ test
  (check-equal? (get-riemann-candidates board1 player1)
                (list (cons 1 1)
                      (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6)
                      (cons 1 0)            (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5) (cons 1 6)
                      (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5) (cons 2 6)
                      (cons 3 0) (cons 3 1) (cons 3 2) (cons 3 3) (cons 3 4) (cons 3 5) (cons 3 6)
                      (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                      (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                      (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6)))
  (check-equal? (get-riemann-candidates board1 player2)
                (list (cons 3 3)
                      (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6)
                      (cons 1 0) (cons 1 1) (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5) (cons 1 6)
                      (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5) (cons 2 6)
                      (cons 3 0) (cons 3 1) (cons 3 2)            (cons 3 4) (cons 3 5) (cons 3 6)
                      (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                      (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                      (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6)))
  (check-equal? (get-riemann-candidates board1 player3)
                (list (cons 1 3)
                      (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6)
                      (cons 1 0) (cons 1 1) (cons 1 2)            (cons 1 4) (cons 1 5) (cons 1 6)
                      (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5) (cons 2 6)
                      (cons 3 0) (cons 3 1) (cons 3 2) (cons 3 3) (cons 3 4) (cons 3 5) (cons 3 6)
                      (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                      (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                      (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6))))

; test compare-euclidean-dist
(module+ test
  (check-true (compare-euclidean-dist (cons 0 0) (cons 1 1) (cons 6 6)))
  (check-false (compare-euclidean-dist (cons 0 0) (cons 1 0) (cons 0 1)))
  (check-false (compare-euclidean-dist (cons 3 3) (cons 6 6) (cons 0 0)))
  (check-true  (compare-euclidean-dist (cons 3 3) (cons 0 0) (cons 6 6)))
  (check-true (compare-euclidean-dist (cons 2 4) (cons 2 3) (cons 6 6))))

; test valid-move?
(module+ test
  (check-not-false (valid-move? player-state-1 (move (cons 3 1) 'down 2 0)))
  (check-not-false (valid-move? player-state-1 (move (cons 3 3) 'right 2 0)))
  (check-false (valid-move? player-state-1 (move (cons 3 3) 'up 0 0)))
  (check-false (valid-move? player-state-1 (move (cons 3 2) 'down 2 0)))
  (check-false (valid-move? player-state-1 (move (cons 1 1) 'right 6 0)))
  (check-false (valid-move? player-state-1 (move (cons 3 1) 'left 6 0))))

; test euclidian-dist
(module+ test
  (check-true (< (- (euclidian-dist (cons 0 0) (cons 1 1)) 1.4142) .0001))
  (check-equal? (euclidian-dist (cons 1 1) (cons 5 1)) 4)
  (check-true (< (- (euclidian-dist (cons 1 1) (cons 6 6)) 7.0710)  .0001)))

;; test get-all-positions
(module+ test
  (check-equal? (get-all-positions board1)
                (list (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6)
                      (cons 1 0) (cons 1 1) (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5) (cons 1 6)
                      (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5) (cons 2 6)
                      (cons 3 0) (cons 3 1) (cons 3 2) (cons 3 3) (cons 3 4) (cons 3 5) (cons 3 6)
                      (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                      (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                      (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6)))
    (check-equal? (get-all-positions board2)
                (list (cons 0 0) (cons 0 1) (cons 0 2)
                      (cons 1 0) (cons 1 1) (cons 1 2)
                      (cons 2 0) (cons 2 1) (cons 2 2)))
    (check-equal? (get-all-positions board-nowhere-to-go)
                (list (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6)
                      (cons 1 0) (cons 1 1) (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5) (cons 1 6)
                      (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5) (cons 2 6)
                      (cons 3 0) (cons 3 1) (cons 3 2) (cons 3 3) (cons 3 4) (cons 3 5) (cons 3 6)
                      (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                      (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                      (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6))))

; test get-goal-gp
(module+ test
  (check-equal? (get-goal-gp player1) (cons 1 1))
  (check-equal? (get-goal-gp player2) (cons 3 3))
  (check-equal? (get-goal-gp player3) (cons 1 3))
  (check-equal? (get-goal-gp player4) (cons 5 5))
  (check-equal? (get-goal-gp player9) (cons 3 3)))

; test get-first-valid-move
(module+ test
  (check-equal? (get-first-valid-move player-state-1 cand-list-1) (move (cons 3 1) 'down 2 0))
  (check-equal? (get-first-valid-move player-state-1 cand-list-2) (move (cons 3 3) 'right 2 0))
  (check-equal? (get-first-valid-move player-state-2 cand-list-3) (move (cons 3 3) 'up 0 0)))
