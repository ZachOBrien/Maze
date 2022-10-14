#lang racket/base

;;; This module provides data definitions and logic for a Maze game board,
;;; including board tiles.


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [board?           contract?]
  [grid-posn?       contract?]
  [shift-direction? contract?]
  ; Shift a row or column at an index and insert a new tile
  [board-shift-and-insert (-> board? shift-direction? natural-number/c tile? (values board? tile?))]
  ; Get a list of the board positions reachable from a given board position
  [board-all-reachable-from (-> board? grid-posn? (listof grid-posn?))]
  ; Retrieve the tile at a specific position on the board
  [board-get-tile-at (-> board? grid-posn? tile?)]
  ; Get the position of the tile that is newly inserted as a result of a shift and insert
  [get-inserted-tile-pos (-> board? shift-direction? natural-number/c grid-posn?)]
  ; Get the position of the tile that was pushed off the board during a shift and insert
  [get-pushed-tile-pos (-> board? shift-direction? natural-number/c grid-posn?)]))


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


;; A ShiftDirection is one of:
;; - 'up
;; - 'down
;; - 'left
;; - 'right
;; interpretation: A direction in which a row or column can be shifted.
;;                 Columns may only be shifted up and down. Rows may
;;                 only be shifted left and right.
(define shift-direction? (or/c 'up 'down 'left 'right))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; Board ShiftDirection Natural Tile -> (Board Tile)
;; Shifts a row or column and inserts a new tile in the empty space
(define (board-shift-and-insert board dir idx new-tile)
  (define tiles-to-shift (get-tiles-to-shift board dir idx))
  (define tiles-after-shift (get-tiles-after-shift tiles-to-shift dir new-tile))
  (define extra-tile (get-extra-tile-from-shift tiles-to-shift dir))
  (define new-board
    (if (shifts-row? dir)
        (replace-row board idx tiles-after-shift)
        (replace-col board idx tiles-after-shift)))
  (values new-board extra-tile))

;; Board ShiftDirection Natural -> [Listof Tile]
;; Retrieves the tiles which will be shifted given a direction and index
(define (get-tiles-to-shift board dir idx)
  (if (shifts-row? dir)
      (get-row board idx)
      (get-col board idx)))

;; [Listof Tile] ShiftDirection -> Tile
;; Retrieves the tile which would be pushed off the board as a result of a shift
(define (get-extra-tile-from-shift tiles dir)
  (if ((or/c 'right 'down) dir)
      (last tiles)
      (first tiles)))

;; [Listof Tile] ShiftDirection Tile -> [Listof Tile]
;; Shifts tiles by pushing a new tile onto the front or back of a list of tiles
(define (get-tiles-after-shift tiles dir new-tile)
  (if ((or/c 'right 'down) dir)
      (push-to-front tiles new-tile)
      (push-to-back tiles new-tile)))

;; ShiftDirection -> Boolean
;; True if the shift direction shifts a row, False if it shifts a column
(define (shifts-row? dir)
  ((or/c 'left 'right) dir))

;; [Listof Any] Any -> [Listof Any]
;; Puts the given item at the front of the list
;;  and cuts off the back item
(define (push-to-front lst item)
  (cons item (drop-right lst 1)))

;; [Listof Any] Any -> [Listof Any]
;; Puts the given item at the back of the list
;;  and cuts off the front item
(define (push-to-back lst item)
  (append (rest lst) (list item)))

;; Board ShiftDirection Natural -> GridPosn
;; Gets the position left open once a row or column is shifted
(define (get-inserted-tile-pos board dir idx)
  (match dir
    ['up (cons (sub1 (num-rows board)) idx)]
    ['down (cons 0 idx)]
    ['left (cons idx (sub1 (num-cols board)))]
    ['right (cons idx 0)]))

;; Board ShiftDirection Natural -> GridPosn
;; Gets the position pushed off once a row or column is shifted
(define (get-pushed-tile-pos board dir idx)
  (match dir
    ['down (cons (sub1 (num-rows board)) idx)]
    ['up (cons 0 idx)]
    ['right (cons idx (sub1 (num-cols board)))]
    ['left (cons idx 0)]))

;; Board GridPosn -> [Listof GridPosn]
;; Get a list of the board positions reachable from a given board position
(define (board-all-reachable-from board pos)
  (all-reachable-from-acc board (list pos) '()))

;; Board [Listof GridPosn] [Listof GridPosn] -> [Listof GridPosn]
;; Finds a connected pathway through the board using breadth-first search
(define (all-reachable-from-acc board queue visited)
  (cond
    [(empty? queue) (reverse visited)]
    [(member (first queue) visited) (all-reachable-from-acc board (rest queue) visited)]
    [else (define current-pos (first queue))
          (all-reachable-from-acc board
                                  (append
                                   (rest queue)
                                   (board-get-connected-neighbors board current-pos))
                                  (cons current-pos visited))]))

;; Board GridPosn GridPosn -> Boolean
;; Returns true if the two adjacent tiles are connected
(define (board-adjacent-connected? board pos1 pos2)
  (define tile1 (board-get-tile-at board pos1))
  (define tile2 (board-get-tile-at board pos2))
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
(define (board-get-connected-neighbors board pos)
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
(define (board-get-tile-at board pos)
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
  (define board2 (list row0_2 row1_2 row2_2))

  ; A small board which has a cycle amongst the tile paths
  (define row0_3 (list tile03 tile01 tile11))
  (define row1_3 (list tile02 tile04 tile22))
  (define row2_3 (list tile33 tile44 tile55))
  (define board3 (list row0_3 row1_3 row2_3)))

(module+ test
  (require rackunit)
  (require (submod "tile.rkt" examples))
  (require (submod ".." examples)))

;; Test shifts-row?
(module+ test
  (check-true (shifts-row? 'left))
  (check-true (shifts-row? 'right))
  (check-false (shifts-row? 'up))
  (check-false (shifts-row? 'down)))

;; Test get-tiles-to-shift
(module+ test
  (check-equal? (get-tiles-to-shift board2 'left 0) row0_2)
  (check-equal? (get-tiles-to-shift board2 'right 0) row0_2)
  (check-equal? (get-tiles-to-shift board2 'left 2) row2_2)
  (check-equal? (get-tiles-to-shift board2 'right 2) row2_2)
  (check-equal? (get-tiles-to-shift board2 'down 0) (list tile00 tile10 tile20)))

;; Test get-extra-tile-from-shift
(module+ test
  (check-equal? (get-extra-tile-from-shift row0_2 'left) tile00)
  (check-equal? (get-extra-tile-from-shift row0_2 'right) tile02))

;; Test get-tiles-after-shift
(module+ test
  (check-equal? (get-tiles-after-shift row0_2 'left tile66) (list tile01 tile02 tile66))
  (check-equal? (get-tiles-after-shift row0_2 'right tile66) (list tile66 tile00 tile01)))

;; Test shifting row/col and inserting a tile
(module+ test
  (test-case
   "Board shift row right on top row correctly"
   (let-values
     ([(new-board new-extra-tile)
      (board-shift-and-insert board2 'right 0 tile-extra)])
     (check-equal? new-extra-tile tile02)
     (check-equal?
      new-board
      (list (list tile-extra tile00 tile01)
            (list tile10     tile11 tile12)
            (list tile20     tile21 tile22)))))
  (test-case
   "Board shift row right on bottom row correctly"
   (let-values
     ([(new-board new-extra-tile)
      (board-shift-and-insert board2 'right 2 tile-extra)])
     (check-equal? new-extra-tile tile22)
     (check-equal?
      new-board
      (list (list tile00 tile01 tile02)
            (list tile10 tile11 tile12)
            (list tile-extra tile20 tile21)))))
  (test-case
   "Board shift row left on top row correctly"
   (let-values
       ([(new-board new-extra-tile)
         (board-shift-and-insert board2 'left 0 tile-extra)])
     (check-equal? new-extra-tile tile00)
     (check-equal?
      new-board
      (list (list tile01 tile02 tile-extra)
            (list tile10 tile11 tile12)
            (list tile20 tile21 tile22)))))
  (test-case
   "Board shift row left on bottom row correctly"
   (let-values
       ([(new-board new-extra-tile)
         (board-shift-and-insert board2 'left 2 tile-extra)])
     (check-equal? new-extra-tile tile20)
     (check-equal?
      new-board
      (list (list tile00 tile01 tile02)
            (list tile10 tile11 tile12)
            (list tile21 tile22 tile-extra)))))
  (test-case
   "Board shift column up on left column correctly"
   (let-values
     ([(new-board new-extra-tile)
      (board-shift-and-insert board2 'up 0 tile-extra)])
     (check-equal? new-extra-tile tile00)
     (check-equal?
      new-board
      (list (list tile10     tile01 tile02)
            (list tile20     tile11 tile12)
            (list tile-extra tile21 tile22)))))
  (test-case
   "Board shift column up on right column correctly"
   (let-values
     ([(new-board new-extra-tile)
      (board-shift-and-insert board2 'up 2 tile-extra)])
     (check-equal? new-extra-tile tile02)
     (check-equal?
      new-board
      (list (list tile00 tile01 tile12)
            (list tile10 tile11 tile22)
            (list tile20 tile21 tile-extra)))))
  (test-case
   "Board shift column down on left column correctly"
   (let-values
     ([(new-board new-extra-tile)
      (board-shift-and-insert board2 'down 0 tile-extra)])
     (check-equal? new-extra-tile tile20)
     (check-equal?
      new-board
      (list (list tile-extra tile01 tile02)
            (list tile00     tile11 tile12)
            (list tile10     tile21 tile22)))))
  (test-case
   "Board shift column down on right column correctly"
   (let-values
     ([(new-board new-extra-tile)
      (board-shift-and-insert board2 'down 2 tile-extra)])
     (check-equal? new-extra-tile tile22)
     (check-equal?
      new-board
      (list (list tile00 tile01 tile-extra)
            (list tile10 tile11 tile02)
            (list tile20 tile21 tile12))))))

;; test get-open-pos
(module+ test
  (check-equal? (get-inserted-tile-pos board1 'up 0) (cons 6 0))
  (check-equal? (get-inserted-tile-pos board1 'up 6) (cons 6 6))
  (check-equal? (get-inserted-tile-pos board1 'down 0) (cons 0 0))
  (check-equal? (get-inserted-tile-pos board1 'down 6) (cons 0 6))
  (check-equal? (get-inserted-tile-pos board1 'right 0) (cons 0 0))
  (check-equal? (get-inserted-tile-pos board1 'right 6) (cons 6 0))
  (check-equal? (get-inserted-tile-pos board1 'left 0) (cons 0 6))
  (check-equal? (get-inserted-tile-pos board1 'left 6) (cons 6 6)))

;; test get-pushed-pos
(module+ test
  (check-equal? (get-pushed-tile-pos board1 'up 0) (cons 0 0))
  (check-equal? (get-pushed-tile-pos board1 'up 6) (cons 0 6))
  (check-equal? (get-pushed-tile-pos board1 'down 0) (cons 6 0))
  (check-equal? (get-pushed-tile-pos board1 'down 6) (cons 6 6))
  (check-equal? (get-pushed-tile-pos board1 'right 0) (cons 0 6))
  (check-equal? (get-pushed-tile-pos board1 'right 6) (cons 6 6))
  (check-equal? (get-pushed-tile-pos board1 'left 0) (cons 0 0 ))
  (check-equal? (get-pushed-tile-pos board1 'left 6) (cons 6 0)))

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
  (check-equal? (get-row board1 0) row0)
  (check-equal? (get-row board2 0) row0_2))

;; test get-col
(module+ test
  (check-equal? (get-col board1 0) (list tile00 tile10 tile20 tile30 tile40 tile50 tile60))
  (check-equal? (get-col board2 0) (list tile00 tile10 tile20)))

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
                 (cons 6 1)))
  (check-equal? (board-all-reachable-from board3 (cons 0 0))
                (list (cons 0 0) (cons 1 0) (cons 0 1) (cons 1 1))))
         

;; test board-adjacent-connected?
(module+ test
  (check-true (board-adjacent-connected? board1 (cons 0 0) (cons 0 1)))
  (check-false (board-adjacent-connected? board1 (cons 0 0) (cons 1 0)))
  (check-true (board-adjacent-connected? board1 (cons 6 6) (cons 5 6)))
  (check-false (board-adjacent-connected? board1 (cons 6 0) (cons 6 0))))

;; test board-get-at
(module+ test
  (check-equal? (board-get-tile-at board1 (cons 0 0)) tile00)
  (check-not-equal? (board-get-tile-at board1 (cons 3 1)) tile00)
  (check-equal? (board-get-tile-at board1 (cons 6 6)) tile66))
