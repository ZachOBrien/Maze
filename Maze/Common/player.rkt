#lang racket

;;; This module provides a data definition and logic for a Maze player representation


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/draw)
(require racket/contract)

(provide
 (contract-out
  [player-id? contract?]
  [player? contract?]
  [move?   contract?]
  ;; Create a new Player
  [player-new (-> player-id? grid-posn? grid-posn? (listof gem?) date? (is-a?/c color%) player?)]
  ;; Create a new Move
  [move-new (-> shift-direction? natural-number/c orientation? grid-posn? move?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "gem.rkt")
(require "tile.rkt")
(require "board.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; A PlayerID is a Natural
;; interpretation: A player's unique ID
(define player-id? natural-number/c)

;; A Player is a structure:
;;    (struct PlayerID GridPosn GridPosn [Listof Gem] Date Color%)
;; interpretation: A player has an ID, a current position, home position, goal treasure,
;;                 birthday, and avatar color
(struct player [id curr-pos home-pos goal-treasures dob color])

;; GridPosn GridPosn [Listof Gem] Date Color% -> Player
;; Create a new player
(define (player-new id curr-pos home-pos goal-treasures dob color)
  (player id curr-pos home-pos goal-treasures dob color))


;; A Move is a structure:
;;    (struct
;; interpretation: A move made by a player. A move has a ShiftDirection, the index of
;;                 a row/col to shift, the orientation of the newly inserted tile,
;;                 and the position the player will move to
(struct move [shift-dir index orientation pos])

;; ShiftDirection Natural Orientation GridPosn -> Move
(define (move-new shift-dir index orientation pos)
  (move shift-dir index orientation pos))
  