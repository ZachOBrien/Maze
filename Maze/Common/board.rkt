#lang racket/base

;;; This module provides data definitions and logic for a Maze game board,
;;; including board tiles.


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)
(require racket/list)

(provide
 (contract-out
  [board?       contract?]
  [tile?        contract?]
  [grid-posn?   contract?]
  [connector?   contract?]
  [orientation? contract?]
  ; Constructs a new board
  [board-make (-> (listof (listof tile?)) tile? board?)]
  ; Constructs a new tile
  [tile-make (-> connector? orientation? (listof gem?) tile?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "gem.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; A Board is a structure:
;;   (board [Listof [Listof Tile]] Tile)
;; interpretation: A square matrix of Maze game tiles, and an extra tile
(struct board [rows extra-tile])
(define (board-make rows extra-tile)
  (board rows extra-tile))


;; A Tile is a structure:
;;    (tile Connector Orientation [Listof Gem])
;; interpretation: Represents a tile in the game of labyrinth
(struct tile [connector orientation gems])
(define (tile-make connector orientation gems)
  (tile connector orientation gems))


;; A Connector is one of:
;;   - 'straight
;;   - 'elbow
;;   - 'tri
;;   - 'cross
;; interpretation: A pathway along a tile which determines whether a tile
;;                 is "connected" to its neighbor
(define connectors (list 'straight 'elbow 'tri 'cross))
(define connector? (apply or/c connectors))


;; An Orientation is one of:
;  - 0
;  - 90
;  - 180
;  - 270
; interpretation: A direction a tile could be facing
(define orientations (list 0 90 180 270))
(define orientation? (apply or/c orientations))


;; A GridPosn is a pair:
;;   (cons Natural Natural)
;; interpretation: A position on a grid in terms of [row index, column index]
(define grid-posn? (cons/c natural-number/c natural-number/c))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; Board GridPosn -> [Listof GridPosn]
;; Returns the positions of all tiles reachable from the given position
(define (board-all-reachable-from board pos)
  empty)


;; Board GridPosn GridPosn -> Boolean
;; Returns true if the two adjacent tiles are connected
(define (board-adjacent-connected? board pos1 pos2)
  (define tile1 (board-get-at pos1))
  (define tile2 (board-get-at pos2))
  (define-values (row1 col1 row2 col2) (values (car pos1) (cdr pos1) (car pos2) (cdr pos2)))
  (cond
    [(and (= col1 col2) (= (row1) (sub1 row2))) (tile-connected-vertical? tile1 tile2)]     ; pos1 above pos2
    [(and (= col1 col2) (= (row1) (add1 row2))) (tile-connected-vertical? tile2 tile1)]     ; pos1 below pos2
    [(and (= row1 row2) (= (col1) (sub1 col2))) (tile-connected-horizontal? tile1 tile2)]   ; pos1 to left of pos2
    [(and (= row1 row2) (= (col1) (add1 col2))) (tile-connected-horizontal? tile2 tile1)]   ; pos1 to right of pos2
    [else #f]))


;; Board GridPosn -> Tile
;; Gets the tile at a position in the board
(define (board-get-at board pos)
  (list-ref (list-ref (board-rows board) (car pos)) (cdr pos)))


;; Tile Tile -> Boolean
;; Returns true if you can travel from one tile to its adjacent neighbor horizontally
(define (tile-connected-horizontal? left right)
  (and (open-on-right (tile-connector left) (tile-orientation left))
       (open-on-left (tile-connector right) (tile-orientation right))))


;; Tile Tile -> Boolean
;; Returns true if you can travel from one tile to its adjacent neighbor vertically
(define (tile-connected-vertical? top bottom)
  (and (open-on-bottom (tile-connector top) (tile-orientation top))
       (open-on-top (tile-connector bottom) (tile-orientation bottom))))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its top edge
(define (open-on-top connector orientation)
  (define up-con (list ('straight 0)
                       ('straight 180)
                       ('elbow 0)
                       ('elbow 270)
                       ('tri 90)
                       ('tri 270)
                       ('tri 180)
                       ('cross 0)
                       ('cross 180)
                       ('cross 270)
                       ('cross 90)))
  (if (member (cons connector orientation) up-con) #t #f))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its bottom edge
(define (open-on-bottom connector orientation)
  (define down-con (list ('straight 0)
                         ('straight 180)
                         ('elbow 180)
                         ('elbow 90)
                         ('tri 90)
                         ('tri 270)
                         ('tri 0)
                         ('cross 0)
                         ('cross 180)
                         ('cross 270)
                         ('cross 90)))
  (if (member (cons connector orientation) down-con) #t #f))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its left edge
(define (open-on-left connector orientation)
  (define left-con (list ('straight 90)
                         ('straight 270)
                         ('elbow 180)
                         ('elbow 270)
                         ('tri 180)
                         ('tri 270)
                         ('tri 0)
                         ('cross 0)
                         ('cross 180)
                         ('cross 270)
                         ('cross 90)))
  (if (member (cons connector orientation) left-con) #t #f))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its right edge
(define (open-on-right connector orientation)
  (define right-con (list ('straight 90)
                          ('straight 270)
                          ('elbow 0)
                          ('elbow 90)
                          ('tri 180)
                          ('tri 90)
                          ('tri 0)
                          ('cross 0)
                          ('cross 180)
                          ('cross 270)
                          ('cross 90)))
  (if (member (cons connector orientation) right-con) #t #f))


;; Board GridPosn -> [Listof GridPosn]
;; Retrieves a list of GridPosns representing a specific position's neighbors
(define (board-get-neighbors board pos)
  (define-values (row col) (values (car pos) (cdr pos)))
  (define all-neighbors
    (list (cons (sub1 row) col)
          (cons row        (sub1 col))
          (cons (add1 row) col)
          (cons row        (add1 col))))
  (filter (lambda (x) (in-board? board x)) all-neighbors))


;; Board GridPosn -> Boolean
;; Checks if a GridPosn is within the bounds of the board. That is,
;; not a negative position or greater than the number of rows or cols.
(define (in-board? board pos)
  (and (< -1 (car pos) (num-rows board))
       (< -1 (cdr pos) (num-cols board))))


;; Board -> PositiveInteger
;; Gets the number of rows in a board
(define (num-rows board)
  (length (board-rows board)))


;; Board -> PositiveInteger
;; Gets the number of columns in a board
(define (num-cols board)
  (length (first (board-rows board))))


;; --------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit)
  (define tile1  (tile 'elbow 90 empty))
  (define board1 (board empty empty)))
                 

;; test board-get-neighbors
(module+ test
  (check-equal? (board-get-neighbors board1 (cons 0 0))
                (list (cons 1 0) (cons 0 1)))
  (check-equal? (board-get-neighbors board1 (cons 0 2))
                (list (cons 0 1) (cons 1 2) (cons 0 3)))
  (check-equal? (board-get-neighbors board1 (cons 2 4))
                (list (cons 1 4) (cons 2 3) (cons 3 4)))
  (check-equal? (board-get-neighbors board1 (cons 4 4))
                (list (cons 3 4) (cons 4 3))))

;; test in-board?
(module+ test
  (check-true  (in-board? board1 (cons 0 0)))
  (check-true  (in-board? board1 (cons 4 0)))
  (check-false (in-board? board1 (cons 5 0)))
  (check-true  (in-board? board1 (cons 0 4)))
  (check-false (in-board? board1 (cons 0 5)))
  (check-true  (in-board? board1 (cons 4 4)))
  (check-false (in-board? board1 (cons 5 5))))

;; test num-rows
(module+ test
  (check-equal? (num-rows board1) 7))

;; test num-cols
(module+ test
  (check-equal? (num-cols board1) 7))


;; test board-all-reachable-from


;; test board-adjacent-connected?


;; test tile-connected-horizontal


;; test tile-connected-vertical


;; test board-get-at


;; test open-on-top


;; test open-on-right


;; test open-on-bottom


;; test open-on-left

