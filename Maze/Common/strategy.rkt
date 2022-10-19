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
  [riemann-strategy   strategy?]
  [euclidean-strategy strategy?]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/match)
(require racket/list)
(require racket/function)

(require "state.rkt")
(require "board.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; A Move is a structure:
;;    (struct ShiftDirection Natural Orientation GridPosn)
;; interpretation: A Move has a direction to shift a row or column, the index of the
;;                 row or column to shift, the number of degrees to rotate the spare
;;                 tile, and a position to move the currently active player to after the shift
(struct move [shift-direction idx orientation pos])


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


;; PlayerState -> Move
;; Determine the player's move using the riemann strategy
(define (riemann-strategy state) 0)


;; PlayerState -> Move
;; Determine the player's move using the euclidean strategy
(define (euclidean-strategy state) 0)
