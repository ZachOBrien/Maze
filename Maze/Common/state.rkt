#lang racket

;;; This module provides a data definition and logic for a Maze game state


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "player.rkt")
(require "tile.rkt")
(require "board.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(define/contract gamestate%
  (class/c (field [current-player natural-number/c])
           (field [player-turn-order (listof player-id?)])
           (field [board board?])
           (field [extra-tile tile?])
           (field [players (listof player?)])
           (field [previous-move move?])
           ;; Shift a row or column, insert a tile, and move the player onto the newly
           ;; inserted tile if needed
           [execute-move (-> move? (is-a?/c gamestate%))]
           ;; Check if a player can reach a position from their current position
           [player-can-reach-pos? (-> grid-posn? boolean?)]
           ;; Check if a player is currently placed on their goal tile
           [player-on-goal? (-> boolean?)]
           ;; Check if a player is currently placed on their home tile
           [player-on-home? (-> boolean?)]
           ;; Remove the currently active player from the game
           [remove-player (-> (is-a?/c gamestate%))]
           ;; End the current player's turn and switch to the next player's turn
           [end-current-turn (-> (is-a?/c gamestate%))])
  (class object%
    (super-new)))


                                 