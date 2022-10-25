#lang racket

;;; This module provides data definitions and logic for a strategy to
;;; play the Maze game


;; --------------------------------------------------------------------
;; MODULE INTERFACE


(require racket/contract)

(provide
 (contract-out
  [player-state? contract?]
  ; Create a new player state
  [player-state-new (-> board? tile? player? prev-shift? player-state?)]
  ; Get the board from the PlayerState
  [player-state-get-board (-> player-state? board?)]
  ; Get the extra tile from the PlayerState
  [player-state-get-extra-tile (-> player-state? tile?)]
  ; Get the player from the PlayerState
  [player-state-get-player (-> player-state? player?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "../Common/board.rkt")
(require "../Common/tile.rkt")
(require "../Common/player.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; A PlayerState is a structure:
;;    (struct Board Tile Player PrevShift)
;; interpretation: A player knows the board, the extra tile, and all of its information
(struct player-state [board extra-tile player prev-shift])


;; Board Tile Player -> PlayerState
;; Create a new player state
(define (player-state-new board extra-tile player prev-shift)
  (player-state board extra-tile player prev-shift))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; PlayerState -> Board
;; Get the board from the PlayerState
(define (player-state-get-board plyr-state)
  (player-state-board plyr-state))

;; Playerstate -> Tile
;; Get the extra tile from the PlayerState
(define (player-state-get-extra-tile plyr-state)
  (player-state-extra-tile plyr-state))

;; Playerstate -> Player
;; Get the player from the PlayerState
(define (player-state-get-player plyr-state)
  (player-state-player plyr-state))

(module+ examples
  (provide (all-defined-out))
  (require (submod "../Common/tile.rkt" examples))
  (require (submod "../Common/board.rkt" examples))
  (require (submod "../Common/player.rkt" examples))
  
  (define player-state-1 (player-state board1 tile-extra player2))
  (define player-state-2 (player-state board1 tile-extra player7))
  (define player-state-nowhere-to-go (player-state board-nowhere-to-go tile-extra player3)))
  