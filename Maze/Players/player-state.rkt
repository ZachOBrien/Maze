#lang racket

;;; This module provides data definitions and logic for a strategy to
;;; play the Maze game


;; --------------------------------------------------------------------
;; MODULE INTERFACE


(require racket/contract)

(provide
 (contract-out
  [player-state? contract?]
  ; Create a new PlayerState
  [player-state-new (-> board? tile? player? (or/c shift? #f) player-state?)]
  ; Get the board
  [player-state-board (-> player-state? board?)]
  ; Get the extra tile
  [player-state-extra-tile (-> player-state? tile?)]
  ; Get the player
  [player-state-player (-> player-state? player?)]
  ; Get the previous shift
  [player-state-prev-shift (-> player-state? (or/c shift? #f))]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "../Common/board.rkt")
(require "../Common/tile.rkt")
(require "../Common/player.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; A PlayerState is a structure:
;;    (struct Board Tile Player (U Shift #f)
;; interpretation: A player knows the board, the extra tile, its personal information, and
;;                 the previous shift
(struct player-state [board extra-tile player prev-shift] #:transparent)


;; Board Tile Player -> PlayerState
;; Create a new player state
(define (player-state-new board extra-tile player prev-shift)
  (player-state board extra-tile player prev-shift))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(module+ examples
  (provide (all-defined-out))
  (require (submod "../Common/tile.rkt" examples))
  (require (submod "../Common/board.rkt" examples))
  (require (submod "../Common/player.rkt" examples))
  
  (define player-state-1 (player-state board1 tile-extra player2 (shift-new 'up 0)))
  (define player-state-2 (player-state board1 tile-extra player7 (shift-new 'down 4)))
  (define player-state-nowhere-to-go (player-state board-nowhere-to-go tile-extra player3 (shift-new 'right 4))))
  