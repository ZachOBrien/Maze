#lang racket/base

;;; This module provides data definitions and logic for a Maze game board,
;;; including board tiles.


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [board?       contract?]
  [grid-posn?   contract?]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/match)
(require racket/list)
(require racket/function)

(require "tile.rkt")
(require "gem.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; A Board is a [Listof [Listof Tile]]
;; interpretation: A square matrix of Maze game tiles with dimensions of odd length
(define board? (listof (listof tile?)))


;; A GridPosn is a pair:
;;   (cons Natural Natural)
;; interpretation: A position on a grid in terms of [row index, column index]
(define grid-posn? (cons/c natural-number/c natural-number/c))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; Board GridPosn -> [Listof GridPosn]
;; Returns the positions of all tiles reachable from the given position
(define (board-all-reachable-from board pos)
  (all-reachable-from-acc board (list pos) '()))


;; Board [Listof GridPosn] [Listof GridPosn] -> [Listof GridPosn]
;; Finds a connected pathway through the board using breadth-first search
(define (all-reachable-from-acc board queue visited)
  (cond
    [(empty? queue) (reverse visited)]
    [else (define current-pos (first queue))
          (all-reachable-from-acc board
                                  (append
                                   (rest queue)
                                   (get-connected-unvisited-neighbors board current-pos visited))
                                  (cons current-pos visited))]))


;; Board GridPosn [Listof GridPosn] -> [Listof GridPosn]
;; Gets all directly connected, unvisited neighbors of a tile at current-pos
(define (get-connected-unvisited-neighbors board current-pos visited)
  (filter (λ (p)
                 (and
                  (not (member p visited))
                  (board-adjacent-connected? board current-pos p)))
          (board-get-directly-connected-neighbors board current-pos)))


;; Board GridPosn GridPosn -> Boolean
;; Returns true if the two adjacent tiles are connected
(define (board-adjacent-connected? board pos1 pos2)
  (define tile1 (board-get-at board pos1))
  (define tile2 (board-get-at board pos2))
  (define-values (row1 col1 row2 col2) (values (car pos1) (cdr pos1) (car pos2) (cdr pos2)))
  (cond
    [(and (= col1 col2) (= row1 (sub1 row2))) (tile-connected-vertical? tile1 tile2)]
    [(and (= col1 col2) (= row1 (add1 row2))) (tile-connected-vertical? tile2 tile1)]
    [(and (= row1 row2) (= col1 (sub1 col2))) (tile-connected-horizontal? tile1 tile2)]
    [(and (= row1 row2) (= col1 (add1 col2))) (tile-connected-horizontal? tile2 tile1)]
    [else #f]))


;; Board GridPosn -> [Listof GridPosn]
;; Retrieves a list of GridPosns for tiles which are directly connected
;; to the tile at the given GridPosn
(define (board-get-directly-connected-neighbors board pos)
  (filter (λ (p) (board-adjacent-connected? board pos p))
          (board-get-neighbors board pos)))


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

;; Board Natural -> [Listof Tile]
;; Gets the row at the index in the board
(define (get-row board idx)
  (list-ref board idx))

;; Board Natural -> [Listof Tile]
;; Gets the column at the index in the board
(define (get-col board idx)
  (map (λ (row) (list-ref row idx)) board))

;; Board Natural [Listof Tile] -> Board
;; Replaces the row at the index in the board with the given row
(define (replace-row board idx row)
  (list-set board idx row))

;; Board Natural [Listof Tile] -> Board
;; Replaces the column at the index in the board with the given column
(define (replace-col old-board idx col)
  (for/list ([row old-board]
             [new-tile col])
    (list-set row idx new-tile)))

;; Board GridPosn -> Boolean
;; Checks if a GridPosn is within the bounds of the board. That is,
;; not a negative position or greater than the number of rows or cols.
(define (in-board? board pos)
  (and (< -1 (car pos) (num-rows board))
       (< -1 (cdr pos) (num-cols board))))


;; Board GridPosn -> Tile
;; Gets the tile at a position in the board
(define (board-get-at board pos)
  (list-ref (list-ref board (car pos)) (cdr pos)))

;; Board -> PositiveInteger
;; Gets the number of rows in a board
(define (num-rows board)
  (length board))


;; Board -> PositiveInteger
;; Gets the number of columns in a board
(define (num-cols board)
  (length (first board)))


;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (require (submod "tile.rkt" examples))
  (provide (all-defined-out))
  (define row0 (list tile00 tile01 tile02 tile03 tile04 tile05 tile06))
  (define row1 (list tile10 tile11 tile12 tile13 tile14 tile15 tile16))
  (define row2 (list tile20 tile21 tile22 tile23 tile24 tile25 tile26))
  (define row3 (list tile30 tile31 tile32 tile33 tile34 tile35 tile36))
  (define row4 (list tile40 tile41 tile42 tile43 tile44 tile45 tile46))
  (define row5 (list tile50 tile51 tile52 tile53 tile54 tile55 tile56))
  (define row6 (list tile60 tile61 tile62 tile63 tile64 tile65 tile66))
  (define board1 (list row0 row1 row2 row3 row4 row5 row6))

  (define row0_2 (list tile00 tile01 tile02))
  (define row1_2 (list tile10 tile11 tile12))
  (define row2_2 (list tile20 tile21 tile22))
  (define board2 (list row0_2 row1_2 row2_2)))

(module+ test
  (require rackunit)
  (require (submod "tile.rkt" examples))
  (require (submod ".." examples)))

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

;; test get-row
(module+ test
  (check-equal? (get-row board1 0) row0))

;; test get-col
(module+ test
  (check-equal? (get-col board1 0) (list tile00 tile10 tile20 tile30 tile40 tile50 tile60)))

;; test replace-row
(module+ test
  (check-equal? (replace-row board2 0 (list tile66 tile66 tile66))
                (list (list tile66 tile66 tile66)
                      (list tile10 tile11 tile12)
                      (list tile20 tile21 tile22)))
  (check-equal? (replace-row board2 1 (list tile66 tile66 tile66))
                (list (list tile00 tile01 tile02)
                      (list tile66 tile66 tile66)
                      (list tile20 tile21 tile22)))
  (check-equal? (replace-row board2 2 (list tile66 tile66 tile66))
                (list (list tile00 tile01 tile02)
                      (list tile10 tile11 tile12)
                      (list tile66 tile66 tile66))))

;; test replace-col
(module+ test
  (check-equal? (replace-col board2 0 (list tile66 tile66 tile66))
                (list (list tile66 tile01 tile02)
                      (list tile66 tile11 tile12)
                      (list tile66 tile21 tile22)))
  (check-equal? (replace-col board2 1 (list tile66 tile66 tile66))
                (list (list tile00 tile66 tile02)
                      (list tile10 tile66 tile12)
                      (list tile20 tile66 tile22)))
  (check-equal? (replace-col board2 2 (list tile66 tile66 tile66))
                (list (list tile00 tile01 tile66)
                      (list tile10 tile11 tile66)
                      (list tile20 tile21 tile66))))

;; test num-rows
(module+ test
  (check-equal? (num-rows board1) 7))

;; test num-cols
(module+ test
  (check-equal? (num-cols board1) 7))


;; test board-all-reachable-from
(module+ test
  (check-equal? (board-all-reachable-from board1 (cons 0 2))
                (list (cons 0 2)))
  (check-equal? (board-all-reachable-from board1 (cons 0 6))
                (list (cons 0 6) (cons 1 6)))
  (check-equal? (board-all-reachable-from board1 (cons 1 6))
                (list (cons 1 6) (cons 0 6)))
  (check-equal? (board-all-reachable-from board1 (cons 1 6))
                (list (cons 1 6) (cons 0 6)))
  (check-equal? (board-all-reachable-from board1 (cons 0 0))
                (list
                 (cons 0 0)
                 (cons 0 1)
                 (cons 1 1)
                 (cons 1 0)
                 (cons 2 1)
                 (cons 2 0)
                 (cons 3 0)
                 (cons 4 0)
                 (cons 5 0)
                 (cons 4 1)
                 (cons 4 2)
                 (cons 3 2)
                 (cons 5 2)
                 (cons 3 1)
                 (cons 5 1)
                 (cons 6 1))))
         

;; test board-adjacent-connected?
(module+ test
  (check-true (board-adjacent-connected? board1 (cons 0 0) (cons 0 1)))
  (check-false (board-adjacent-connected? board1 (cons 0 0) (cons 1 0)))
  (check-true (board-adjacent-connected? board1 (cons 6 6) (cons 5 6)))
  (check-false (board-adjacent-connected? board1 (cons 6 0) (cons 6 0))))

;; test board-get-at
(module+ test
  (check-equal? (board-get-at board1 (cons 0 0)) tile00)
  (check-not-equal? (board-get-at board1 (cons 3 1)) tile00)
  (check-equal? (board-get-at board1 (cons 6 6)) tile66))

