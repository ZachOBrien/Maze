#lang racket

;;; This module provides a data definition and logic for a Maze game state


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)
(require racket/list)

(provide
 (contract-out
  [move?       contract?]
  [gamestate?  contract?]
  ; Create a new Move
  [move-new (-> shift-direction? natural-number/c orientation? grid-posn? move?)]
  ; Create a new Gamestate
  [gamestate-new
   (-> board? tile? (non-empty-listof player?) (non-empty-listof player-id?) gamestate?)]
  ; Carry out a player's move by shifting a row or column, inserting a tile, and moving
  ; the player onto the newly inserted tile if they were pushed off the board during the shift
  [execute-move (-> gamestate? move? gamestate?)]
  ; Check if a player can reach a position from their current position
  [player-can-reach-pos? (-> gamestate? grid-posn? boolean?)]
  ; Check if a player is currently placed on their goal tile
  [player-on-goal? (-> gamestate? boolean?)]
  ; Check if a player is currently placed on their home tile
  [player-on-home? (-> gamestate? boolean?)]
  ; Remove the currently active player from the game
  [remove-player (-> gamestate? gamestate?)]
  ; End the current player's turn and switch to the next player's turn
  [end-current-turn (-> gamestate? gamestate?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "player.rkt")
(require "tile.rkt")
(require "board.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; A Move is a structure:
;;    (struct ShfitDirection Natural Orientation GridPosn)
;; interpretation: A move made by a player. A move has a ShiftDirection, the index of
;;                 a row/col to shift, the orientation of the newly inserted tile,
;;                 and the position the player will move to
(struct move [shift-dir index orientation pos])

;; ShiftDirection Natural Orientation GridPosn -> Move
(define (move-new shift-dir index orientation pos)
  (move shift-dir index orientation pos))


;; A Gamestate is a structure:
;;    (struct Board Tile [NonEmptyListof Player] [NonEmptyListof PlayerID] PlayerID (-> Move Boolean))
;; interpretation: A Gamestate has a board, an extra tile, players, the order in which the players
;;                 take turns, the ID of the currently acting player, and a function representing
;;                 whether a move would reverse the previously made move
(struct gamestate [board extra-tile players player-turn-order current-player reverses-prev-move])

;; Board Tile [NonEmptyListof Player] [NonEmptyListof PlayerID] -> Gamestate
;; Create a new gamestate
(define (gamestate-new board extra-tile players player-turn-order)
  (gamestate board extra-tile players player-turn-order (first player-turn-order) (λ (mv) #f)))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Gamestate Move -> Gamestate
;; Carry out a player's move by shifting a row or column, inserting a tile, and moving
;; the player onto the newly inserted tile if they were pushed off the board during the shift
(define (execute-move state mv)
  ...)


;; Gamestate GridPosn -> Boolean
;; Check if a player can reach a position from their current position
(define (player-can-reach-pos? state pos)
  ...)


;; Gamestate -> Boolean
;; Check if a player is currently placed on their goal tile
(define (player-on-goal? state)
  ...)


;; Gamestate -> Boolean
;; Check if a player is currently placed on their home tile
(define (player-on-home? state)
  ...)


;; Gamestate -> Gamestate
;; Remove the currently active player from the game
(define (remove-player state)
  ...)


;; Gamestate 
; End the current player's turn and switch to the next player's turn
(define (end-current-turn state)
  ...)


;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (define player0
    (player
     0 (cons 0 0) (cons 6 6) (list 'apatite 'aplite) (seconds->date (current-seconds)) "blue")))

(module+ test
  (require rackunit)
  (require (submod ".." examples)))
                                 