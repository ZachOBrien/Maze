#lang racket

;;; This module provides a data definition and logic for a Maze game state


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)
(require racket/list)

(provide
 (contract-out
  [move? contract?]
  ; Create a new Move
  [move-new (-> shift-direction? natural-number/c orientation? grid-posn? move?)]))

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
;;     (struct 

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(define/contract gamestate%
  (class/c
   ;; Shift a row or column, insert a tile, and move the player onto the newly
   ;; inserted tile if needed
   [execute-move (->m move? (is-a?/c gamestate%))]
   ;; Check if a player can reach a position from their current position
   [player-can-reach-pos? (->m grid-posn? boolean?)]
   ;; Check if a player is currently placed on their goal tile
   [player-on-goal? (->m boolean?)]
   ;; Check if a player is currently placed on their home tile
   [player-on-home? (->m boolean?)]
   ;; Remove the currently active player from the game
   [remove-player (->m (is-a?/c gamestate%))]
   ;; End the current player's turn and switch to the next player's turn
   [end-current-turn (->m (is-a?/c gamestate%))])
  (class object%
    (super-new)

    (init init-board
          init-extra-tile
          init-players
          init-player-order
          [init-previous-move (λ (move) #f)])

    (define board init-board)
    (define extra-tile init-extra-tile)
    (define players init-players)
    (define player-turn-order init-player-order)
    (define current-player (first player-turn-order))
    (define previous-move-check init-previous-move)

    (define (copy
             [new-board board]
             [new-extra-tile extra-tile]
             [new-players players]
             [new-player-turn-order]

    
    (define/public (execute-move mv)
      (define inserted-tile-pos (get-inserted-tile-pos board (move-shift-dir mv) (move-index mv)))
      (define pushed-tile-pos (get-pushed-tile-pos board (move-shift-dir mv) (move-index mv)))
      (define moved-players
        (for ([p players])
          (if (player-on-pos? p pushed-tile-pos)
              (player-move-to p open-space-pos)
              p)))

      (define/values (board extra-tile) (board-shift-and-insert
                                         board
                                         (move-shift-dir mv)
                                         (move-index mv)
                                         (rotate-tile extra-tile (move-orientation mv))))
      (
    
      

    
    (define/public (player-can-reach-pos? pos) #t)

    (define/public (player-on-goal?) #t)

    (define/public (player-on-home?) #t)

    (define/public (remove-player) this)

    (define/public (end-current-turn) this)
    

    ))


                                 