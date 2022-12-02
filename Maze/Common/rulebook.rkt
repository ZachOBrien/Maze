#lang racket

;;; This module provides data definitions and logic for the rules of the game

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  [valid-move? (-> gamestate? move? boolean?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "state.rkt")
(require "player-info.rkt")
(require "board.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Gamestate Move -> Boolean
;; Returns True if the move is valid in the state
(define (valid-move? state mv)
  (define state-after-shift (gamestate-shift-and-insert state
                                                        (move-shift mv)
                                                        (move-orientation mv)))

  (define players-after-shift (shift-players state (move-shift mv)))
  (define active-player-after-move (first players-after-shift))

  (and (member
        (move-pos mv)
        (board-all-reachable-from (gamestate-board state-after-shift)
                                  (player-info-curr-pos active-player-after-move)))
       (not (player-info-on-pos? active-player-after-move (move-pos mv)))
       (not (shift-undoes-shift? (move-shift mv) (gamestate-prev-shift state)))))
 

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (require (submod "tile.rkt" examples))
  (require (submod "board.rkt" examples))
  (require (submod "player-info.rkt" examples)))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "tile.rkt" examples))
  (require (submod "board.rkt" examples))
  (require (submod "state.rkt" examples))
  (require (submod "player-info.rkt" examples)))

; test valid-move? board extra-tile prev-shift plyr-info orientation shift pos) (-> grid-posn? shift? orientation? move?)]
(module+ test
  (check-false (valid-move?
                (referee-state-new board1 tile-extra (list player-info1) empty (shift-new 'up 2))
                (move-new (cons 3 1) (shift-new 'down 2) 0)))
  (check-false (valid-move?
                (referee-state-new board1 tile-extra (list player-info1) empty (shift-new 'up 2))
                (move-new (cons 5 1) (shift-new 'right 2) 90)))
  (check-false (valid-move?
                (referee-state-new board1 tile-extra (list player-info1) empty (shift-new 'right 2))
                (move-new (cons 3 3) (shift-new 'down 2) 180)))
  (check-false (valid-move?
                (referee-state-new board1 tile-extra (list player-info1) empty (shift-new 'left 2))
                (move-new (cons 1 1) (shift-new 'left 2) 270)))
  (check-not-false (valid-move?
                    (referee-state-new board1 tile-extra (list player-info1) empty (shift-new 'down 2))
                    (move-new (cons 1 2) (shift-new 'right 6) 0)))
  (check-not-false (valid-move?
                    (referee-state-new board1 tile-extra (list player-info1) empty (shift-new 'down 6))
                    (move-new (cons 0 0) (shift-new 'right 6) 0)))
  (check-not-false (valid-move?
                    (referee-state-new board1 tile-extra (list player-info1) empty (shift-new 'down 6))
                    (move-new (cons 2 0) (shift-new 'right 2) 90))))
