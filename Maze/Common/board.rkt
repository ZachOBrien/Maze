#lang racket/base

;;; This module provides data definitions and logic for a Maze game board,
;;; including board tiles.


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)


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
(require racket/match)
(require racket/list)

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
  (define tile1 (board-get-at board pos1))
  (define tile2 (board-get-at board pos2))
  (define-values (row1 col1 row2 col2) (values (car pos1) (cdr pos1) (car pos2) (cdr pos2)))
  (cond
    [(and (= col1 col2) (= row1 (sub1 row2))) (tile-connected-vertical? tile1 tile2)]     ; pos1 above pos2
    [(and (= col1 col2) (= row1 (add1 row2))) (tile-connected-vertical? tile2 tile1)]     ; pos1 below pos2
    [(and (= row1 row2) (= col1 (sub1 col2))) (tile-connected-horizontal? tile1 tile2)]   ; pos1 to left of pos2
    [(and (= row1 row2) (= col1 (add1 col2))) (tile-connected-horizontal? tile2 tile1)]   ; pos1 to right of pos2
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
#;
(define (open-on-top connector orientation)
  (define up-con (list (cons 'straight 0)
                       (cons 'straight 180)
                       (cons 'elbow 0)
                       (cons 'elbow 270)
                       (cons 'tri 90)
                       (cons 'tri 270)
                       (cons 'tri 180)
                       (cons 'cross 0)
                       (cons 'cross 180)
                       (cons 'cross 270)
                       (cons 'cross 90)))
  (if (member (cons connector orientation) up-con) #t #f))

(define (open-on-top connector orientation)
  (match* (connector orientation)
    [('cross _)     #t]
    [('tri o)      (not (= 0 o))]
    [('elbow o)    (or (= 0 o) (= 270 o))]
    [('straight o) (or (= 0 o) (= 180 o))]))

;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its bottom edge
(define (open-on-bottom connector orientation)
  (define down-con (list (cons 'straight 0)
                         (cons 'straight 180)
                         (cons 'elbow 180)
                         (cons 'elbow 90)
                         (cons 'tri 90)
                         (cons 'tri 270)
                         (cons 'tri 0)
                         (cons 'cross 0)
                         (cons 'cross 180)
                         (cons 'cross 270)
                         (cons 'cross 90)))
  (if (member (cons connector orientation) down-con) #t #f))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its left edge
(define (open-on-left connector orientation)
  (define left-con (list (cons 'straight 90)
                         (cons 'straight 270)
                         (cons 'elbow 180)
                         (cons 'elbow 270)
                         (cons 'tri 180)
                         (cons 'tri 90)
                         (cons 'tri 0)
                         (cons 'cross 0)
                         (cons 'cross 180)
                         (cons 'cross 270)
                         (cons 'cross 90)))
  (if (member (cons connector orientation) left-con) #t #f))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its right edge
(define (open-on-right connector orientation)
  (define right-con (list (cons 'straight 90)
                          (cons 'straight 270)
                          (cons 'elbow 0)
                          (cons 'elbow 90)
                          (cons 'tri 180)
                          (cons 'tri 270)
                          (cons 'tri 0)
                          (cons 'cross 0)
                          (cons 'cross 180)
                          (cons 'cross 270)
                          (cons 'cross 90)))
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
  (define tile00 (tile 'straight 90 empty))
  (define tile01 (tile 'elbow 180 empty))
  (define tile02 (tile 'elbow 0 empty))
  (define tile03 (tile 'elbow 90 empty))
  (define tile04 (tile 'elbow 270 empty))
  (define tile05 (tile 'tri 0 empty))
  (define tile06 (tile 'tri 270 empty))

  (define tile10 (tile 'tri 180 empty))
  (define tile11 (tile 'tri 90 empty))
  (define tile12 (tile 'cross 0 empty))
  (define tile13 (tile 'straight 0 empty))
  (define tile14 (tile 'straight 270 empty))
  (define tile15 (tile 'elbow 180 empty))
  (define tile16 (tile 'elbow 0 empty))

  (define tile20 (tile 'elbow 90 empty))
  (define tile21 (tile 'elbow 270 empty))
  (define tile22 (tile 'tri 0 empty))
  (define tile23 (tile 'tri 270 empty))
  (define tile24 (tile 'tri 180 empty))
  (define tile25 (tile 'tri 90 empty))
  (define tile26 (tile 'cross 270 empty))

  (define tile30 (tile 'straight 180 empty))
  (define tile31 (tile 'straight 270 empty))
  (define tile32 (tile 'elbow 180 empty))
  (define tile33 (tile 'elbow 0 empty))
  (define tile34 (tile 'elbow 90 empty))
  (define tile35 (tile 'elbow 270 empty))
  (define tile36 (tile 'tri 0 empty))

  (define tile40 (tile 'tri 270 empty))
  (define tile41 (tile 'tri 180 empty))
  (define tile42 (tile 'tri 90 empty))
  (define tile43 (tile 'cross 0 empty))
  (define tile44 (tile 'straight 0 empty))
  (define tile45 (tile 'straight 90 empty))
  (define tile46 (tile 'elbow 180 empty))
  
  (define tile50 (tile 'elbow 0 empty))
  (define tile51 (tile 'elbow 90 empty))
  (define tile52 (tile 'elbow 270 empty))
  (define tile53 (tile 'try 0 empty))
  (define tile54 (tile 'tri 270 empty))
  (define tile55 (tile 'tri 180 empty))
  (define tile56 (tile 'tri 90 empty))
  
  (define tile60 (tile 'cross    0 empty))
  (define tile61 (tile 'straight 0 empty))
  (define tile62 (tile 'straight 90 empty))
  (define tile63 (tile 'elbow 180 empty))
  (define tile64 (tile 'elbow 0   empty))
  (define tile65 (tile 'elbow 90 empty))
  (define tile66 (tile 'elbow 270 empty))

  (define tile-extra (tile 'straight 180 empty))

  (define row0 (list tile00 tile01 tile02 tile03 tile04 tile05 tile06))
  (define row1 (list tile10 tile11 tile12 tile13 tile14 tile15 tile16))
  (define row2 (list tile20 tile21 tile22 tile23 tile24 tile25 tile26))
  (define row3 (list tile30 tile31 tile32 tile33 tile34 tile35 tile36))
  (define row4 (list tile40 tile41 tile42 tile43 tile44 tile45 tile46))
  (define row5 (list tile50 tile51 tile52 tile53 tile54 tile55 tile56))
  (define row6 (list tile60 tile61 tile62 tile63 tile64 tile65 tile66))
  (define board1 (board (list row0 row1 row2 row3 row4 row5 row6) tile-extra)))

;; test board-get-neighbors
(module+ test
  (check-equal? (board-get-neighbors board1 (cons 0 0))
                (list (cons 1 0) (cons 0 1)))
  (check-equal? (board-get-neighbors board1 (cons 0 2))
                (list (cons 0 1) (cons 1 2) (cons 0 3)))
  (check-equal? (board-get-neighbors board1 (cons 2 6))
                (list (cons 1 6) (cons 2 5) (cons 3 6)))
  (check-equal? (board-get-neighbors board1 (cons 6 6))
                (list (cons 5 6) (cons 6 5))))

;; test in-board?
(module+ test
  (check-true  (in-board? board1 (cons 0 0)))
  (check-true  (in-board? board1 (cons 6 0)))
  (check-false (in-board? board1 (cons 7 0)))
  (check-true  (in-board? board1 (cons 0 6)))
  (check-false (in-board? board1 (cons 0 7)))
  (check-true  (in-board? board1 (cons 6 6)))
  (check-false (in-board? board1 (cons 7 7))))

;; test num-rows
(module+ test
  (check-equal? (num-rows board1) 7))

;; test num-cols
(module+ test
  (check-equal? (num-cols board1) 7))


;; test board-all-reachable-from


;; test board-adjacent-connected?
(module+ test
  (check-true (board-adjacent-connected? board1 (cons 0 0) (cons 0 1)))
  (check-false (board-adjacent-connected? board1 (cons 0 0) (cons 1 0)))
  (check-true (board-adjacent-connected? board1 (cons 6 6) (cons 5 6)))
  (check-false (board-adjacent-connected? board1 (cons 6 0) (cons 6 0))))

;; test tile-connected-horizontal
(module+ test
  (check-true (tile-connected-horizontal? tile00 tile01))
  (check-false (tile-connected-horizontal? tile01 tile02))
  (check-true (tile-connected-horizontal? tile55 tile56))
  (check-false (tile-connected-horizontal? tile60 tile61)))

;; test tile-connected-vertical
(module+ test
  (check-true (tile-connected-vertical? tile01 tile11))
  (check-false (tile-connected-vertical? tile00 tile10))
  (check-false (tile-connected-vertical? tile02 tile12)))

;; test board-get-at
(module+ test
  (check-equal? (board-get-at board1 (cons 0 0)) tile00)
  (check-not-equal? (board-get-at board1 (cons 3 1)) tile00)
  (check-equal? (board-get-at board1 (cons 6 6)) tile66))

;; test open-on-top
(module+ test
  (check-true (open-on-top 'straight 0))
  (check-false (open-on-top 'straight 90))
  (check-true (open-on-top 'straight 180))
  (check-false (open-on-top 'straight 270))
  
  (check-true (open-on-top 'elbow 0))
  (check-false (open-on-top 'elbow 90))
  (check-false (open-on-top 'elbow 180))
  (check-true (open-on-top 'elbow 270))
  
  (check-false (open-on-top 'tri 0))
  (check-true (open-on-top 'tri 90))
  (check-true (open-on-top 'tri 180))
  (check-true (open-on-top 'tri 270))
  
  (check-true (open-on-top 'cross 0))
  (check-true (open-on-top 'cross 90))
  (check-true (open-on-top 'cross 180))
  (check-true (open-on-top 'cross 270)))

;; test open-on-right
(module+ test
  (check-false (open-on-right 'straight 0))
  (check-true (open-on-right 'straight 90))
  (check-false (open-on-right 'straight 180))
  (check-true (open-on-right 'straight 270))
  
  (check-true (open-on-right 'elbow 0))
  (check-true (open-on-right 'elbow 90))
  (check-false (open-on-right 'elbow 180))
  (check-false (open-on-right 'elbow 270))
  
  (check-true (open-on-right 'tri 0))
  (check-false (open-on-right 'tri 90))
  (check-true (open-on-right 'tri 180))
  (check-true (open-on-right 'tri 270))
  
  (check-true (open-on-right 'cross 0))
  (check-true (open-on-right 'cross 90))
  (check-true (open-on-right 'cross 180))
  (check-true (open-on-right 'cross 270)))

;; test open-on-bottom
(module+ test
  (check-true (open-on-bottom 'straight 0))
  (check-false (open-on-bottom 'straight 90))
  (check-true (open-on-bottom 'straight 180))
  (check-false (open-on-bottom 'straight 270))
  
  (check-false (open-on-bottom 'elbow 0))
  (check-true (open-on-bottom 'elbow 90))
  (check-true (open-on-bottom 'elbow 180))
  (check-false (open-on-bottom 'elbow 270))
  
  (check-true (open-on-bottom 'tri 0))
  (check-true (open-on-bottom 'tri 90))
  (check-false (open-on-bottom 'tri 180))
  (check-true (open-on-bottom 'tri 270))

  (check-true (open-on-bottom 'cross 0))
  (check-true (open-on-bottom 'cross 90))
  (check-true (open-on-bottom 'cross 180))
  (check-true (open-on-bottom 'cross 270)))


;; test open-on-left
(module+ test
  (check-false (open-on-left 'straight 0))
  (check-true (open-on-left 'straight 90))
  (check-false (open-on-left 'straight 180))
  (check-true (open-on-left 'straight 270))
  
  (check-false (open-on-left 'elbow 0))
  (check-false (open-on-left 'elbow 90))
  (check-true (open-on-left 'elbow 180))
  (check-true (open-on-left 'elbow 270))
  
  (check-true (open-on-left 'tri 0))
  (check-true (open-on-left 'tri 90))
  (check-true (open-on-left 'tri 180))
  (check-false (open-on-left 'tri 270))

  (check-true (open-on-left 'cross 0))
  (check-true (open-on-left 'cross 90))
  (check-true (open-on-left 'cross 180))
  (check-true (open-on-left 'cross 270)))

