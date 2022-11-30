#lang racket/base

;;; This module provides data definitions and logic for a strategy to
;;; play the Maze game


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [action?        contract?]
  [strategy?      contract?]
  ; Riemann strategy
  [riemann-strategy   strategy?]
  ; Euclidean strategy
  [euclidean-strategy strategy?]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/match)
(require racket/list)
(require racket/function)
(require racket/set)
(require racket/bool)

(require "../Common/state.rkt")
(require "../Common/player-info.rkt")
(require "../Common/rulebook.rkt")
(require "../Common/board.rkt")
(require "../Common/tile.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; An Action is one of:
;;    - Move
;;    - #f
;; interpretation: A player acts by either making a move or making no move (passing turn)
(define action? (or/c move? #f))

;; A Strategy is a function:
;;    (-> PlayerState Move)
;; interpretation: A strategy examines a player state and determines a move for the currently active
;;                 player to make
(define strategy? (-> player-state? action?))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; PlayerState -> Action
;; Determine the player's move using the Riemann strategy
(define (riemann-strategy plyr-state)
  (get-first-valid-candidate-move plyr-state (get-riemann-candidates
                                              (gamestate-board plyr-state)
                                              (gamestate-current-player plyr-state))))

;; Board RefPlayerInfo -> [Listof GridPosn]
;; Order the possible candidates for Riemann search
(define (get-riemann-candidates board plyr)
  (define goal-pos (player-info-goal-pos plyr))
  (cons goal-pos (filter (lambda (pos)
                           (not (equal? pos goal-pos)))
                         (get-all-positions board))))

;; PlayerState -> Action
;; Determine a player's move using the Euclidean strategy
(define (euclidean-strategy plyr-state)
  (get-first-valid-candidate-move plyr-state (get-euclidean-candidates
                                              (gamestate-board plyr-state)
                                              (gamestate-current-player plyr-state))))

;; PlayerState -> [Listof GridPosn]
;; Order the possible candidates for Euclidean search
(define (get-euclidean-candidates board plyr)
  (define goal-pos (player-info-goal-pos plyr))
  (define all-candidates (get-all-positions board))
  (sort all-candidates (lambda (pos1 pos2) (compare-euclidean-dist goal-pos pos1 pos2))))


;; GridPosn GridPosn GrisPosn -> Boolean
;; Returns true of `pos1` is not farther from `goal` than `pos2`
(define (compare-euclidean-dist goal pos1 pos2)
  (if (= (euclidian-dist goal pos1) (euclidian-dist goal pos2))
      (compare-row-col pos1 pos2)
      (< (euclidian-dist goal pos1) (euclidian-dist goal pos2))))

;; GridPosn GridPosn -> Natural
;; Computes the euclidean distance between two gridposns
(define (euclidian-dist pos1 pos2)
  (sqrt (+ (expt (- (car pos2) (car pos1)) 2) (expt (- (cdr pos2) (cdr pos1)) 2))))

;; Board -> [Listof GridPosn]
;; Get all possible positions in a board
(define (get-all-positions board)
  (apply append (for/list ([x (in-range 0 (num-rows board))])
                  (for/list ([y (in-range 0 (num-cols board))])
                    (cons x y)))))

;; PlayerState [Listof Move] -> Action
;; Finds the first Move which is valid in a PlayerState
(define (get-first-valid-candidate-move plyr-state candidates)
  (findf
   (curry valid-move? plyr-state)
   (get-all-candidate-moves (gamestate-board plyr-state) candidates)))

;; Board -> [Listof Move]
;; Get all possible moves that can be made on a board
(define (get-all-candidate-moves board candidates)
  (define lr-shifts (get-valid-row-shifts board))
  (define ud-shifts (get-valid-col-shifts board))
  (define shifts (append lr-shifts ud-shifts))
  (map (curry apply move-new)
       (cartesian-product candidates shifts orientations)))


(module+ serialize
  (provide
   (contract-out
    ; Convert a JSON representation of a choice into an action
    [json-choice->action (-> json-choice? action?)]
    ; Convert an action into a JSON representation of a choice
    [action->json-choice (-> action? json-choice?)]))

  (require (submod "../Common/board.rkt" serialize))

  (module+ test
    (require rackunit))
   
  (define json-choice? (or/c "PASS" (list/c natural-number/c
                                            json-direction?
                                            orientation?
                                            hash?)))

  ;; JsonChoice -> Action
  ;; Convert the JSON representation of a "choice" (action) to Action
  (define (json-choice->action jsexpr)
    (if (equal? jsexpr "PASS")
        #f
        (move-new (json-coordinate->gridposn (list-ref jsexpr 3))
                  (shift-new (json-direction->shift-direction (list-ref jsexpr 1))
                             (list-ref jsexpr 0))
                  (list-ref jsexpr 2))))

  (module+ test
    (check-equal? (json-choice->action "PASS") #f))

  ;; Action -> (U String List)
  ;; Convert an action to json
  (define (action->json-choice act)
    (cond
      [(move? act) (list (shift-index (move-shift act))
                         (string-upcase (symbol->string (shift-direction (move-shift act))))
                         (move-orientation act)
                         (gridposn->json-coordinate (move-pos act)))]
      [(false? act) "PASS"]))

  (module+ test
    (check-equal? (action->json-choice #f) "PASS")))
                             

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (require (submod "../Common/tile.rkt" examples))
  (require (submod "../Common/board.rkt" examples))
  (require (submod "../Common/state.rkt" examples))
  

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
  (require (submod "../Common/board.rkt" examples))
  (require (submod "../Common/state.rkt" examples))
  (require (submod "../Common/player-info.rkt" examples)))

; test riemann-strategy
(module+ test
  ; Player can reach goal tile
  (check-equal? (riemann-strategy player-state0) (move-new (cons 3 3) (shift-new 'left 0) 0))
  ; Player cannot reach goal tile, reaches closest
  (check-equal? (riemann-strategy player-state1) (move-new (cons 0 4) (shift-new 'down 6) 90))
  ; Player cannot go anywhere
  (check-false (riemann-strategy player-state-nowhere-to-go)))

; test euclidean-strategy
(module+ test
  ; Player cannot go anywhere
  (check-false (euclidean-strategy player-state-nowhere-to-go))
  ; Player cannot reach goal tile, reaches closest tile
  (check-equal? (euclidean-strategy player-state1) (move-new (cons 6 1) (shift-new 'right 6) 90))
  ; Player can reach goal tile
  (check-equal? (euclidean-strategy player-state0) (move-new (cons 3 3) (shift-new 'left 0) 0)))

; test get-euclidean-strategy
(module+ test
  (check-equal? (get-euclidean-candidates board1 player-info1)
                (list (cons 1 1) (cons 0 1) (cons 1 0) (cons 1 2) (cons 2 1) (cons 0 0) (cons 0 2)
                      (cons 2 0) (cons 2 2) (cons 1 3) (cons 3 1) (cons 0 3) (cons 2 3) (cons 3 0)
                      (cons 3 2) (cons 3 3) (cons 1 4) (cons 4 1) (cons 0 4) (cons 2 4) (cons 4 0)
                      (cons 4 2) (cons 3 4) (cons 4 3) (cons 1 5) (cons 5 1) (cons 0 5) (cons 2 5)
                      (cons 5 0) (cons 5 2) (cons 4 4) (cons 3 5) (cons 5 3) (cons 1 6) (cons 4 5)
                      (cons 5 4) (cons 6 1) (cons 0 6) (cons 2 6) (cons 6 0) (cons 6 2) (cons 3 6)
                      (cons 6 3) (cons 5 5) (cons 4 6) (cons 6 4) (cons 5 6) (cons 6 5) (cons 6 6)))
  (check-equal? (get-euclidean-candidates board1 player-info2)
                (list (cons 3 3) (cons 2 3) (cons 3 2) (cons 3 4) (cons 4 3) (cons 2 2) (cons 2 4)
                      (cons 4 2) (cons 4 4) (cons 1 3) (cons 3 1) (cons 3 5) (cons 5 3) (cons 1 2)
                      (cons 1 4) (cons 2 1) (cons 2 5) (cons 4 1) (cons 4 5) (cons 5 2) (cons 5 4)
                      (cons 1 1) (cons 1 5) (cons 5 1) (cons 5 5) (cons 0 3) (cons 3 0) (cons 3 6)
                      (cons 6 3) (cons 0 2) (cons 0 4) (cons 2 0) (cons 2 6) (cons 4 0) (cons 4 6)
                      (cons 6 2) (cons 6 4) (cons 0 1) (cons 0 5) (cons 1 0) (cons 1 6) (cons 5 0)
                      (cons 5 6) (cons 6 1) (cons 6 5) (cons 0 0) (cons 0 6) (cons 6 0) (cons 6 6)))
  (check-equal? (get-euclidean-candidates board1 player-info3)
                (list (cons 1 3) (cons 0 3) (cons 1 2) (cons 1 4) (cons 2 3) (cons 0 2) (cons 0 4)
                      (cons 2 2) (cons 2 4) (cons 1 1) (cons 1 5) (cons 3 3) (cons 0 1) (cons 0 5)
                      (cons 2 1) (cons 2 5) (cons 3 2) (cons 3 4) (cons 3 1) (cons 3 5) (cons 1 0)
                      (cons 1 6) (cons 4 3) (cons 0 0) (cons 0 6) (cons 2 0) (cons 2 6) (cons 4 2)
                      (cons 4 4) (cons 3 0) (cons 3 6) (cons 4 1) (cons 4 5) (cons 5 3) (cons 5 2)
                      (cons 5 4) (cons 4 0) (cons 4 6) (cons 5 1) (cons 5 5) (cons 5 0) (cons 5 6)
                      (cons 6 3) (cons 6 2) (cons 6 4) (cons 6 1) (cons 6 5) (cons 6 0) (cons 6 6))))

; test get-riemann-candidates
(module+ test
  (check-equal? (get-riemann-candidates board1 player-info1)
                (list (cons 1 1)
                      (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6)
                      (cons 1 0)            (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5) (cons 1 6)
                      (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5) (cons 2 6)
                      (cons 3 0) (cons 3 1) (cons 3 2) (cons 3 3) (cons 3 4) (cons 3 5) (cons 3 6)
                      (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                      (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                      (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6)))
  (check-equal? (get-riemann-candidates board1 player-info2)
                (list (cons 3 3)
                      (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6)
                      (cons 1 0) (cons 1 1) (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5) (cons 1 6)
                      (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) (cons 2 4) (cons 2 5) (cons 2 6)
                      (cons 3 0) (cons 3 1) (cons 3 2)            (cons 3 4) (cons 3 5) (cons 3 6)
                      (cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4) (cons 4 5) (cons 4 6)
                      (cons 5 0) (cons 5 1) (cons 5 2) (cons 5 3) (cons 5 4) (cons 5 5) (cons 5 6)
                      (cons 6 0) (cons 6 1) (cons 6 2) (cons 6 3) (cons 6 4) (cons 6 5) (cons 6 6)))
  (check-equal? (get-riemann-candidates board1 player-info3)
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
  (check-true (valid-move? player-state0 (move-new (cons 3 1) (shift-new 'down 2) 0)))
  ; Check that undoing the previous move is invalid, even if the rest of the move is legal
  (check-false (valid-move? player-state0 (move-new (cons 3 1) (shift-new 'up 2) 0)))
  (check-true (valid-move? player-state0 (move-new (cons 3 3) (shift-new 'right 2) 0)))
  (check-true (valid-move? player-state0 (move-new (cons 3 3) (shift-new 'up 0) 0)))
  (check-false (valid-move? player-state0 (move-new (cons 3 2) (shift-new 'down 2) 0)))
  (check-false (valid-move? player-state0 (move-new (cons 1 1) (shift-new 'right 6) 0)))
  (check-false (valid-move? player-state0 (move-new (cons 3 1) (shift-new 'left 6) 0))))

; test euclidian-dist
(module+ test
  (check-true (< (- (euclidian-dist (cons 0 0) (cons 1 1)) 1.4142) .0001))
  (check-equal? (euclidian-dist (cons 1 1) (cons 5 1)) 4)
  (check-true (< (- (euclidian-dist (cons 1 1) (cons 6 6)) 7.0710)  .0001)))

; test all-possible-moves
(module+ test
  (check-equal? (set-count (list->set (get-all-candidate-moves board1 cand-list-1))) 3136)
  (check-equal? (set-count (list->set (get-all-candidate-moves board2 cand-list-1))) 1568)
  (check-equal? (set-count (list->set (get-all-candidate-moves board1 cand-list-2))) 3136))

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

; test get-first-valid-move
(module+ test
  (check-equal? (get-first-valid-candidate-move player-state0 cand-list-1) (move-new (cons 1 1) (shift-new 'left 2) 0))
  (check-equal? (get-first-valid-candidate-move player-state0 cand-list-2) (move-new (cons 1 3) (shift-new 'left 0) 0))
  (check-equal? (get-first-valid-candidate-move player-state1 cand-list-3) (move-new (cons 0 4) (shift-new 'down 6) 90)))

