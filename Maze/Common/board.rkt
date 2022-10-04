#lang racket/base

;; --------------------------------------------------------------------
;  MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  ; Creates a new board
  [board-make (-> (listof (listof tile/c)) tile/c board?)]
  ; Creates a new tile
  [tile-make (-> connector/c orientation/c (listof gem/c) tile?)]))


;; --------------------------------------------------------------------
;  DEPENDENCIES

(require "gem.rkt")


;; --------------------------------------------------------------------
;  DATA DEFINITIONS

; interpretation: A board for the Maze games
(struct board [rows extra-tile])

(define (board-make rows extra-tile)
  (board rows extra-tile))


;; interpretation: Represents a tile in the game of labyrinth
(struct tile [connector orientation gems])

(define (tile-make connector orientation gems)
  (tile connector orientation gems))


;; A connector is one of:
;  - 'straight
;  - 'elbow
;  - 'tri
;  - 'cross
; interpretation: A pathway along a tile

(define connectors (list 'straight 'elbow 'tri 'cross))
(define connector/c (apply or/c connectors))


;; An orientation is one of:
;  - 'up
;  - 'down
;  - 'left
;  - 'right
; interpretation: A direction a tile could be facing

(define orientations (list 'up 'down 'left 'right))
(define orientation/c (apply or/c orientations))


;; A grid-posn is a position on a grid, specifically
;; a row index and column index

(struct grid-posn [row col])


;; --------------------------------------------------------------------
;  FUNCTIONALITY IMPLEMENTATION

(define (in-board? board pos)
  (define num-rows 5)
  (define num-cols 5)
  (and (< -1 pos-row num-rows)
       (< -1 pos-col num-cols)))
         
(define (get-neighbors board pos)
  (define all-neighbors
    (list (grid-pos (sub1 pos-row) pos-col)
          (grid-pos pos-row        (sub1 pos-col)
          (grid-pos (add1 pos-row) pos-col)
          (grid-pos pos-row        (add1 pos-col)))))
  (filter in-board? all-neighbors))






;; --------------------------------------------------------------------
;  TESTS
