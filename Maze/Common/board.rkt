#lang racket/base

;; --------------------------------------------------------------------
;  MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [board/c     contract?]
  [tile/c      contract?]
  [grid-posn/c contract?]
  ; Creates a new board
  [board-make (-> (listof (listof tile/c)) tile/c board?)]
  ; Creates a new tile
  [tile-make (-> connector/c orientation/c (listof gem/c) tile/c)]))


;; --------------------------------------------------------------------
;  DEPENDENCIES

(require "gem.rkt")


;; --------------------------------------------------------------------
;  DATA DEFINITIONS

; interpretation: A board for the Maze games
(struct board [rows extra-tile])
(define board/c board?)

(define (board-make rows extra-tile)
  (board rows extra-tile))


;; interpretation: Represents a tile in the game of labyrinth
(struct tile [connector orientation gems])
(define tile/c tile?)

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
(define grid-posn/c grid-posn?)


;; --------------------------------------------------------------------
;  FUNCTIONALITY IMPLEMENTATION

; Retrieves a list of grid-posns representing a specific position's neighbors
; board/c grid-posn/c -> (listof grid-posn/c)
(define (get-neighbors board pos)
  (define row (grid-posn-row pos))
  (define col (grid-posn-col pos))
  (define all-neighbors
    (list (grid-posn (sub1 row) col)
          (grid-posn row        (sub1 col))
          (grid-posn (add1 row) col)
          (grid-posn row        (add1 col))))
  (filter (lambda (x) (in-board? board x)) all-neighbors))

; Checks if a grid-posn is within the bounds of the board. That is,
; not a negative position or greater than the number of rows or cols.
; board/c grid-posn/c -> boolean?
(define (in-board? board pos)
  (define num-rows 5)
  (define num-cols 5)
  (and (< -1 (grid-posn-row pos) num-rows)
       (< -1 (grid-posn-col pos) num-cols)))


; Gets the number of rows in a board
; board/c -> exact-nonnegative-integer?
(define (num-rows board)
  (length board-rows))


; Gets the number of columns in a board
; board/c -> exact-nonnegative-integer?
(define (num-cols board)
  (length (car (board-rows board))))


;; --------------------------------------------------------------------
;  TESTS

(module+ test
  (require rackunit)
  (define board (board-make '('(1  2  3  4  5)
                              '(6  7  8  9  10)
                              '(11 12 13 14 15)
                              '(16 17 18 19 20)
                              '(21 22 23 24 25))
                            0)))

; test get-neighbors
(module+ test
  (check-equal? (get-neighbors board (grid-posn 0 0)) (list (grid-posn 1 0) (grid-posn 0 1))))