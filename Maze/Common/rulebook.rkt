#lang racket

;;; This module provides data definitions and logic for a player

;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out
  [valid-move? (-> board? tile? shift? player-info? move? boolean?)]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "../Common/board.rkt")
(require "../Common/tile.rkt")
(require "strategy.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Board Tile Shift PlayerInfo Move -> Boolean
;; Returns true if the move is valid
;; PlayerState Move -> Boolean
;; Returns True if the move is valid in the state
(define (valid-move? board extra-tile prev-shift plyr-info mv)
  (define-values
    (new-board new-extra-tile)
    (board-shift-and-insert board (move-shift mv) (tile-rotate extra-tile (move-orientation mv))))
  (define new-player (first (shift-players (list plyr-info) board (move-shift mv))))
  (and (not (equal? (move-pos mv) (player-info-curr-pos new-player)))
       (not (shift-undoes-shift? (move-shift mv) prev-shift))
       (member (move-pos mv) (board-all-reachable-from new-board (player-info-curr-pos new-player)))))

;; --------------------------------------------------------------------
;; TESTS

