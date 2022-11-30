#lang racket/base

;;; This module provides data definitions and logic for a Maze game board,
;;; including board tiles.


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

;; Board -> (Shift -> Boolean)
;; Creates a function to check for valid shift in the given board
(define (valid-shift? board)
  (λ (shft) (and shift?
                 (if (shifts-row? (shift-direction shft))
                     (valid-row-shift-index? board (shift-index shft))
                     (valid-col-shift-index? board (shift-index shft))))))

;; Board Natural -> Boolean)
;; Check if a shift index is valid for a row
(define (valid-row-shift-index? board index)
  (and (natural-number/c index)
       (even? index)
       (< index (num-rows board))))

;; Board Natural -> Boolean)
;; Check if a shift index is valid for a row
(define (valid-col-shift-index? board index)
  (and (natural-number/c index)
       (even? index)
       (< index (num-cols board))))
                         
;; [SquareListof Any] -> Boolean
;; Checks if the square list has odd dimensions
(define (odd-dims? square-lst)
  (odd? (length (first square-lst))))

;; [Listof [Listof Any]] -> Boolean
;; Checks if the list of lists is square
(define (square? lst)
  (and (apply = (map length lst))
       (= (length lst) (length (first lst)))))

(provide
 (contract-out
  [board?           contract?]
  [grid-posn?       contract?]
  [shift-direction? contract?]
  [shift-directions (listof shift-direction?)]
  
  [shift?           contract?]
  [shift-direction (-> shift? shift-direction?)]
  [shift-index (-> shift? natural-number/c)]
  ; Returns a function to determine if a shift is valid
  [valid-shift? (-> board? (-> shift? boolean?))]
  
  ; Create a new Shift
  [shift-new (-> shift-direction? natural-number/c shift?)]
  ; Create a random board
  [create-random-board (-> (and/c natural-number/c odd?) board?)]
  ; Shift a row or column at an index and insert a new tile
  [board-shift-and-insert (->i
                           ([b board?]
                            [s (b) (valid-shift? b)]
                            [t tile?])
                            (values (rb board?) (rt tile?)))]
  ; Get a list of the board positions reachable from a given board position
  [board-all-reachable-from (-> board? grid-posn? (listof grid-posn?))]
  ; Retrieve the tile at a specific position on the board
  [board-get-tile-at (-> board? grid-posn? tile?)]
  ;; True if direction shifts row/col forward, false if back
  [shifts-forward? (-> shift-direction? boolean?)]
  ;; True if the shift direction shifts a row, False if it shifts a column
  [shifts-row? (-> shift-direction? boolean?)]
  ;; True if the shift direction shifts a column, False if it shifts a row
  [shifts-col? (-> shift-direction? boolean?)]
  ;; Number of rows in a board
  [num-rows (-> (listof (listof any/c)) (and/c integer? positive?))]
  ;; Number of columns in a board
  [num-cols (-> (listof (listof any/c)) (and/c integer? positive?))]
  ;; Compares two GridPosns in row-then-column order
  [compare-row-col (-> grid-posn? grid-posn? boolean?)]
  ;; Do these shifts undo each other?
  [shift-undoes-shift? (-> (or/c shift? #f) (or/c shift? #f) boolean?)]
  ;; Get row shifts which are valid in the board
  [get-valid-row-shifts (-> board? (listof shift?))]
  ;; Get col shifts which are valid in the board
  [get-valid-col-shifts (-> board? (listof shift?))]))
  
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/match)
(require racket/list)
(require racket/function)
(require racket/set)
(require racket/bool)

(require "tile.rkt")
(require "gem.rkt")
(require "list-utils.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; A Board is a [Listof [Listof Tile]]
;; interpretation: A rectangular matrix of Maze game tiles
(define board? (and/c (non-empty-listof (non-empty-listof tile?))))
                      

;; A GridPosn is a pair:
;;   (cons Natural Natural)
;; interpretation: A position on a grid in terms of [row index, column index].
;;                 The topmost row and leftmost column have index 0.
(define grid-posn? (cons/c natural-number/c natural-number/c))


;; A ShiftDirection is one of:
;; - 'up
;; - 'down
;; - 'left
;; - 'right
;; interpretation: A direction in which a row or column can be shifted.
;;                 Columns may only be shifted up and down. Rows may
;;                 only be shifted left and right.
(define shift-directions (list 'left 'right 'up 'down))
(define shift-direction? (apply or/c shift-directions))

;; A Shift is a structure:
;;    (struct ShiftDirection Natural)
;; interpretation: The direction and index of a shift
(struct shift [direction index] #:transparent)

;; Natural ShiftDirection -> Shift
;; Create a new Shift
(define (shift-new direction index)
  (shift direction index))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; Natural -> Board
;; Randomly creates a new board. The dimension must be odd.
(define (create-random-board dim)
  (define randomized-gems (shuffle gems))
  (define tiles (map create-random-tile (chunk-list gems 2)))
  (for/list ([row dim])
    (for/list ([col dim])
      (list-ref tiles (+ (* row dim) col)))))

;; Board Shift Tile -> (Board Tile)
;; Shifts a row or column and inserts a new tile in the empty space
(define (board-shift-and-insert board shft new-tile)
  (define tiles-to-shift (get-tiles-to-shift board shft))
  (define tiles-after-shift (get-tiles-after-shift tiles-to-shift (shift-direction shft) new-tile))
  (define extra-tile (get-extra-tile-from-shift tiles-to-shift (shift-direction shft)))
  (define new-board
    (if (shifts-row? (shift-direction shft))
        (replace-row board (shift-index shft) tiles-after-shift)
        (replace-col board (shift-index shft) tiles-after-shift)))
  (values new-board extra-tile))

;; Board Shift -> [Listof Tile]
;; Retrieves the tiles which will be shifted given a direction and index
(define (get-tiles-to-shift board shft)
  (if (shifts-row? (shift-direction shft))
      (get-row board (shift-index shft))
      (get-col board (shift-index shft))))

;; [Listof Tile] ShiftDirection -> Tile
;; Retrieves the tile which would be pushed off the board as a result of a shift
(define (get-extra-tile-from-shift tiles dir)
  (if (shifts-forward? dir)
      (last tiles)
      (first tiles)))

;; [Listof Tile] ShiftDirection Tile -> [Listof Tile]
;; Shifts tiles by pushing a new tile onto the front or back of a list of tiles
(define (get-tiles-after-shift tiles dir new-tile)
  (if (shifts-forward? dir)
      (push-to-front tiles new-tile)
      (push-to-back tiles new-tile)))

;; ShiftDirection -> Boolean
;; True if direction shifts row/col forward, false if back
(define (shifts-forward? dir)
  ((or/c 'right 'down) dir))

;; ShiftDirection -> Boolean
;; True if the shift direction shifts a row, False if it shifts a column
(define (shifts-row? dir)
  ((or/c 'left 'right) dir))

;; ShiftDirection -> Boolean
;; True if the shift direction shifts a column, False if it shifts a row
(define (shifts-col? dir)
  ((or/c 'up 'down) dir))

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

;; [Listof [Listof Any]] -> PositiveInteger
;; Gets the number of rows in a 2d matrix
(define (num-rows matrix)
  (length matrix))

;; [Listof [Listof Any]] -> Boolean
;; Returns true if a matrix has the same number of items in every row
(define (all-rows-same-length? matrix)
  (apply = (map length matrix)))

;; [Listof [Listof Any]] -> PositiveInteger
;; Gets the number of columns in a 2d matrix.
;; IMPORTANT: Assumes each row has the same length
(define (num-cols matrix)
  (length (first matrix)))

;; [Listof [Listof Any]] [Listof [Listof Any]] -> Boolean
;; Do two 2d matrices have the same number of rows and columns?
(define (same-shape? matrix1 matrix2)
  (and (= (num-rows matrix1) (num-rows matrix2))
       (= (num-cols matrix1) (num-cols matrix2))))

;; GridPosn GridPosn -> Boolean
;; Compares two GridPosns in row-then-column order
(define (compare-row-col pos1 pos2)
  (define-values (row1 col1 row2 col2) (values (car pos1) (cdr pos1) (car pos2) (cdr pos2)))
  (cond
    [(= row1 row2) (<= col1 col2)]
    [(< row1 row2) #t]
    [(> row1 row2) #f]))


;; (U Shift #f) (U Shift #f) -> Boolean
;; Do these shifts undo each other?
(define (shift-undoes-shift? shft1 shft2)
  (cond
    [(or (false? shft1) (false? shft2)) #f]
    [else (and (= (shift-index shft1) (shift-index shft2))
               (opposite-direction? (shift-direction shft1) (shift-direction shft2)))]))

;; ShiftDirection ShiftDirection -> Boolean
;; True if given directions are opposite
(define (opposite-direction? dir1 dir2)
  (or (equal? (set dir1 dir2) (set 'left 'right))
      (equal? (set dir1 dir2) (set 'up 'down))))

;; Board -> [Listof Shift]
;; Get row shifts which are valid in the board
(define (get-valid-row-shifts board)
  (define row-shift-indices (filter (curry valid-row-shift-index? board) (range 0 (num-rows board))))
  (map (curry apply shift-new) (map reverse (cartesian-product row-shift-indices (list 'left 'right)))))

;; Board -> [Listof Shift]
;; Get col shifts which are valid in the board
(define (get-valid-col-shifts board)
  (define col-shift-indices (filter (curry valid-col-shift-index? board) (range 0 (num-cols board))))
  (map (curry apply shift-new) (map reverse (cartesian-product col-shift-indices (list 'up 'down)))))



(module+ draw
  (require 2htdp/image)
  (require racket/function)
  (require (submod "tile.rkt" draw))

  (provide
   (contract-out
    [board->image (-> board? natural-number/c image?)]))

  ;; Board [MultipleOf 10] -> Image
  ;; Draw a board
  (define (board->image board tile-size)
    (apply above (for/list ([row board])
                   (apply beside (map (curryr tile->image tile-size) row))))))

(module+ serialize
  (require json)
  (require (submod "tile.rkt" serialize))
  (require (submod "gem.rkt" serialize))

  (provide
   (contract-out
    [json-coordinate? contract?]
    [json-direction?  contract?]
    [json-board?      contract?]
    [json-action?     contract?]
    ; Convert a JsonAction to a Shift or #f
    [json-action->prev-shift (-> json-action? (or/c #f shift?))]
    ; Convert a JSON representation of direction to a ShiftDirection
    [json-direction->shift-direction (-> json-direction? shift-direction?)]
    ; Converts a GridPosn into a JsonCoordinate
    [gridposn->json-coordinate (-> grid-posn? json-coordinate?)]
    ; Converts a JsonCoordinate into a GridPosn
    [json-coordinate->gridposn (-> json-coordinate? grid-posn?)]
    ; Converts a Board into a JsonBoard
    [board->json-board (-> board? json-board?)]
    ; Convert a shift to a spec-specified Action
    [shift->json-action (-> (or/c shift? #f) json-action?)]
    ; Convert a spec-specified Action to a shift
    [prev-shift->json-action (-> (or/c shift? #f) json-action?)]
    ; Convert a json representation of a board to a board
    [json-board->board (-> json-board? board?)]))

  (module+ test
    (require rackunit))

  ;; JSON representation of the previous move
  (define json-action? (or/c 'null (cons/c natural-number/c (cons/c string? empty))))

  ;; JsonAction -> Move
  ;; Makes a move from the list
  (define (json-action->prev-shift action)
    (if (equal? action 'null)
        #f
        (shift-new (json-direction->shift-direction (first (rest action)))
                   (first action))))

  (module+ test
    (check-equal? (json-action->prev-shift (list 0 "UP"))
                  (shift-new 'up 0))
    (check-equal? (json-action->prev-shift (list 4 "RIGHT"))
                  (shift-new 'right 4))
    (check-equal? (json-action->prev-shift 'null)
                  #f))

  ;; (U Shift #f) -> JsonAction
  ;; Makes a move from the list
  (define (prev-shift->json-action prev-shift)
    (if (false? prev-shift)
        'null
        (list (shift-index prev-shift) (string-upcase (symbol->string (shift-direction prev-shift))))))
  
  (module+ test
    (check-equal? (prev-shift->json-action (shift-new 'right 0))
                  (list 0 "RIGHT"))
    (check-equal? (prev-shift->json-action (shift-new 'down 6))
                  (list 6 "DOWN")))
  
  ;; Any -> Boolean
  ;; Is this object a json representation of a coordinate?
  (define (json-coordinate? ht)
    (and (hash? ht)
         (hash-has-key? ht 'row#)
         (hash-has-key? ht 'column#)
         (natural-number/c (hash-ref ht 'row#))
         (natural-number/c (hash-ref ht 'column#))))

  (module+ test
    (check-true (contract? json-coordinate?))
    (check-true (json-coordinate? (hash 'row# 1 'column# 3)))
    (check-false (json-coordinate? (hash 'row# -1 'column# 3)))
    (check-false (json-coordinate? (hash 'row 1 'column 3))))

  ;; JsonCoordinate -> GridPosn
  ;; Convert a JsonCoordinate into a GridPosn
  (define (json-coordinate->gridposn ht)
    (cons (hash-ref ht 'row#) (hash-ref ht 'column#)))

  (module+ test
    (check-equal? (json-coordinate->gridposn (hash 'row# 1 'column# 1)) (cons 1 1))
    (check-equal? (json-coordinate->gridposn (hash 'row# 10000 'column# 2)) (cons 10000 2)))

  ;; Any -> Boolean
  ;; Is this object a json representation of a direction?
  (define json-direction? (or/c "LEFT" "RIGHT" "UP" "DOWN"))

  (module+ test
    (check-true (contract? json-direction?))
    (check-true (json-direction? "LEFT"))
    (check-false (json-direction? "left")))

  ;; JsonDirection -> ShiftDirection
  ;; Convert a JSON representation of direction to a ShiftDirection
  (define (json-direction->shift-direction json-direction)
    (string->symbol (string-downcase json-direction)))

  (module+ test
    (check-equal? (json-direction->shift-direction "RIGHT") 'right)
    (check-equal? (json-direction->shift-direction "UP") 'up))
  
  ;; GridPosn -> JsonCoordinate
  ;; Convert a GridPosn into a JSON representation
  (define (gridposn->json-coordinate pos)
    (hash 'row# (car pos)
          'column# (cdr pos)))

  (module+ test
    (check-equal? (gridposn->json-coordinate (cons 1 3)) (hash 'row# 1 'column# 3)))

  ;; Any -> Boolean
  ;; Is this object a json representation of a board?
  (define (json-board? ht)
    (and (hash? ht)
         (hash-has-key? ht 'connectors)
         (hash-has-key? ht 'treasures)
         (all-rows-same-length? (hash-ref ht 'connectors))
         (all-rows-same-length? (hash-ref ht 'treasures))
         (same-shape? (hash-ref ht 'connectors) (hash-ref ht 'treasures))
         ((listof (listof json-connector?)) (hash-ref ht 'connectors))
         ((listof (listof json-treasure?)) (hash-ref ht 'treasures))))
  
  (module+ test
    (check-true (json-board? (hash 'connectors (list (list "│" "│" "│") (list "│" "│" "│") (list "│" "│" "│"))
                                   'treasures (list (list (list "citrine" "beryl") (list "citrine" "citrine") (list "beryl" "beryl"))
                                                    (list (list "bulls-eye" "beryl") (list "bulls-eye" "citrine") (list "bulls-eye" "beryl"))
                                                    (list (list "garnet" "beryl") (list "garnet" "citrine") (list "garnet" "beryl")))))))
  
  ;; Board -> JsonBoard
  ;; Converts a Board into a JSON representation of a board
  (define (board->json-board b)
    (define connectors (map (λ (row) (map get-json-connector row)) b))
    (define treasures (map (λ (row) (map get-json-treasure row)) b))
    (hash 'connectors connectors
          'treasures treasures))

  ;; TODO: Unit tests

  ;; Shift -> [Listof String]
  ;; Convert a shift to a spec-specified Action
  (define (shift->json-action shft)
    (if (false? shft)
        'null
        (list (shift-index shft) (symbol->string (shift-direction shft)))))

  ;; TODO: Unit tests

  ;; HashTable -> Board
  ;; Creates a matrix of tiles given a hashtable with matrices of connectors and treasures
  (define (json-board->board ht)
    (define connectors (hash-ref ht 'connectors))
    (define treasures (hash-ref ht 'treasures))
    (combine-matrices-elementwise json-connector-and-json-treasure->tile connectors treasures))

  ;; TODO: Unit tests
  
  ;; (Any -> Any) [Listof [Listof Any]] [Listof [Listof Any]] -> [Listof [Listof Any]]
  ;; Combine two matrices by applying proc to each matrix element-wise
  (define (combine-matrices-elementwise proc matrix1 matrix2)
    (for/list ([row_m1 matrix1]
               [row_m2 matrix2])
      (for/list ([val_m1 row_m1]
                 [val_m2 row_m2])
        (proc val_m1 val_m2))))


  (module+ test
    (define A (list '(1 2 3)
                    '(4 5 6)
                    '(7 8 9)))
    (define I (list '(1 0 0)
                    '(0 1 0)
                    '(0 0 1)))
    (check-equal?
     (combine-matrices-elementwise + A A)
     (list '(2 4 6)
           '(8 10 12)
           '(14 16 18)))
    (check-equal?
     (combine-matrices-elementwise * A I)
     (list '(1 0 0)
           '(0 5 0)
           '(0 0 9)))))

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
  (define board3 (list row0_3 row1_3 row2_3))

  (define board4 (list row0 row1))
  (define board5 (list row0 row1 row2 row3))

  (define row7 (list tile00 tile01 tile02 tile03))
  (define row8 (list tile00 tile01 tile02 tile03))
  (define row9 (list tile00 tile01 tile02 tile03))
  (define row10 (list tile00 tile01 tile02 tile03))

  (define board6 (list row7 row8 row9 row10))

  (define row-horiz (list tile-horiz tile-horiz tile-horiz tile-horiz tile-horiz tile-horiz tile-horiz))
  (define row-one-vert (list tile-horiz tile-horiz tile-horiz tile-vert tile-horiz tile-horiz tile-horiz))
  (define board-nowhere-to-go (list row-horiz row-horiz row-horiz row-one-vert row-horiz row-horiz row-horiz))

  (define board6x7 (list row4 row3 row2 row1 row0 row5))
  (define board7x6 (list (rest row4) (rest row3) (rest row2) (rest row1) (rest row0) (rest row5) (rest row6))))

(module+ test
  (require rackunit)
  (require (submod ".." serialize test))
  (require (submod "tile.rkt" examples))
  (require (submod ".." examples))
  (require (submod ".." serialize)))

;; Test create-random-board
(module+ test
  (check-equal? (num-rows (create-random-board 7)) 7)
  (check-equal? (num-cols (create-random-board 6)) 6))

;; Test valid-shift?
(module+ test
  (check-true ((valid-shift? board1) (shift 'left 0)))
  (check-true ((valid-shift? board1) (shift 'right 2)))
  (check-true ((valid-shift? board1) (shift 'left 6)))
  (check-false ((valid-shift? board1) (shift 'up 1)))
  (check-false ((valid-shift? board1) (shift 'down 3)))
  (check-true ((valid-shift? board7x6) (shift 'left 6))))

;; Test valid-shift-indices
(module+ test
  (check-true (valid-row-shift-index? board1 0))
  (check-true (valid-row-shift-index? board1 2))
  (check-true (valid-col-shift-index? board1 6))
  (check-false (valid-col-shift-index? board1 1))
  (check-false (valid-col-shift-index? board1 3))
  
  (check-true (valid-row-shift-index? board6x7 4))
  (check-false (valid-row-shift-index? board6x7 8))
  (check-false (valid-row-shift-index? board6x7 6))

  (check-true (valid-row-shift-index? board7x6 0))
  (check-true (valid-row-shift-index? board7x6 6))
  (check-false (valid-row-shift-index? board7x6 7))
  
  (check-true (valid-col-shift-index? board6x7 6))
  (check-false (valid-col-shift-index? board6x7 1))
  (check-false (valid-col-shift-index? board6x7 8)))

;; Test square?
(module+ test
  (check-true (square? board1))
  (check-true (square? board2))
  (check-true (square? board3))
  (check-false (square? board4))
  (check-false (square? board5)))

;; Test odd-dims?
(module+ test
  (check-false (odd-dims? board6))
  (check-true (odd-dims? board1)))

;; Test shifts-row?
(module+ test
  (check-true (shifts-row? 'left))
  (check-true (shifts-row? 'right))
  (check-false (shifts-row? 'up))
  (check-false (shifts-row? 'down)))

;; Test get-tiles-to-shift
(module+ test
  (check-equal? (get-tiles-to-shift board2 (shift 'left 0)) row0_2)
  (check-equal? (get-tiles-to-shift board2 (shift 'right 0)) row0_2)
  (check-equal? (get-tiles-to-shift board2 (shift 'left 2)) row2_2)
  (check-equal? (get-tiles-to-shift board2 (shift 'right 2)) row2_2)
  (check-equal? (get-tiles-to-shift board2 (shift 'down 0)) (list tile00 tile10 tile20)))

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
      (board-shift-and-insert board2 (shift 'right 0) tile-extra)])
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
      (board-shift-and-insert board2 (shift 'right 2) tile-extra)])
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
         (board-shift-and-insert board2 (shift 'left 0) tile-extra)])
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
         (board-shift-and-insert board2 (shift 'left 2) tile-extra)])
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
      (board-shift-and-insert board2 (shift 'up 0) tile-extra)])
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
      (board-shift-and-insert board2 (shift 'up 2) tile-extra)])
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
      (board-shift-and-insert board2 (shift 'down 0) tile-extra)])
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
      (board-shift-and-insert board2 (shift 'down 2) tile-extra)])
     (check-equal? new-extra-tile tile22)
     (check-equal?
      new-board
      (list (list tile00 tile01 tile-extra)
            (list tile10 tile11 tile02)
            (list tile20 tile21 tile12))))))

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
  (check-equal? (num-rows board1) 7)
  (check-equal? (num-rows board7x6) 7)
  (check-equal? (num-rows board6x7) 6))

;; test all-rows-same-length?
(module+ test
  (check-true (all-rows-same-length? (list empty)))
  (check-true (all-rows-same-length? (list (list 1))))
  (check-true (all-rows-same-length? (list (list 1 2))))
  (check-true (all-rows-same-length? (list empty empty)))
  (check-true (all-rows-same-length? (list (list 1 2) (list 3 4))))
  (check-false (all-rows-same-length? (list (list 1 2) (list 3))))
  (check-false (all-rows-same-length? (list (list 1 2) empty)))
  (check-false (all-rows-same-length? (list empty (list 1 2)))))

;; test num-cols
(module+ test
  (check-equal? (num-cols board1) 7)
  (check-equal? (num-cols board7x6) 6)
  (check-equal? (num-cols board6x7) 7))

;; test same-shape
(module+ test
  (check-true (same-shape? board1 board1))
  (check-false (same-shape? board7x6 board6x7))
  (check-false (same-shape? (list (list 1 2)) (list (list 1 2 3)))))


;; test board-all-reachable-from
(module+ test
  (check-equal? (board-all-reachable-from board1 (cons 0 2))
                (list (cons 0 2) (cons 0 3)))
  (check-equal? (board-all-reachable-from board1 (cons 0 6))
                (list (cons 0 6) (cons 0 5) (cons 1 6) (cons 0 4)))
  (check-equal? (board-all-reachable-from board1 (cons 1 6))
                (list (cons 1 6) (cons 0 6) (cons 0 5) (cons 0 4)))
  (check-equal? (board-all-reachable-from board1 (cons 0 0))
                (list
                 (cons 0 0)
                 (cons 0 1)
                 (cons 1 1)
                 (cons 1 2)))
  (check-equal? (board-all-reachable-from board3 (cons 0 0))
                (list (cons 0 0))))
         

;; test board-adjacent-connected?
(module+ test
  (check-true (board-adjacent-connected? board1 (cons 0 0) (cons 0 1)))
  (check-false (board-adjacent-connected? board1 (cons 0 0) (cons 1 0)))
  (check-false (board-adjacent-connected? board1 (cons 6 6) (cons 5 6)))
  (check-false (board-adjacent-connected? board1 (cons 6 0) (cons 6 0))))

;; test board-get-at
(module+ test
  (check-equal? (board-get-tile-at board1 (cons 0 0)) tile00)
  (check-not-equal? (board-get-tile-at board1 (cons 3 1)) tile00)
  (check-equal? (board-get-tile-at board1 (cons 6 6)) tile66))

;; test compare-row-col
(module+ test
  (check-true (compare-row-col (cons 0 0) (cons 0 0)))
  (check-true (compare-row-col (cons 0 0) (cons 0 1)))
  (check-false (compare-row-col (cons 0 1) (cons 0 0)))
  (check-false (compare-row-col (cons 1 0) (cons 0 1))))

(module+ test
  (check-equal? (get-valid-row-shifts board1)
                (list (shift 'left 0)
                      (shift 'right 0)
                      (shift 'left 2)
                      (shift 'right 2)
                      (shift 'left 4)
                      (shift 'right 4)
                      (shift 'left 6)
                      (shift 'right 6)))
  (check-equal? (get-valid-row-shifts board6x7)
                (list (shift 'left 0)
                      (shift 'right 0)
                      (shift 'left 2)
                      (shift 'right 2)
                      (shift 'left 4)
                      (shift 'right 4)))
  (check-equal? (get-valid-col-shifts board6x7)
                (list (shift 'up 0)
                      (shift 'down 0)
                      (shift 'up 2)
                      (shift 'down 2)
                      (shift 'up 4)
                      (shift 'down 4)
                      (shift 'up 6)
                      (shift 'down 6))))

;; test opposite-direction?
(module+ test
  (check-true (opposite-direction? 'up 'down))
  (check-true (opposite-direction? 'down 'up))
  (check-true (opposite-direction? 'right 'left))
  (check-true (opposite-direction? 'left 'right))
  (check-false (opposite-direction? 'up 'right))
  (check-false (opposite-direction? 'down 'left))
  (check-false (opposite-direction? 'left 'up))
  (check-false (opposite-direction? 'right 'down)))

;; test shift-undoes-shift?
(module+ test
  (check-true (shift-undoes-shift? (shift-new 'left 0) (shift-new 'right 0)))
  (check-false (shift-undoes-shift? (shift-new 'left 0) (shift-new 'right 1)))
  (check-false (shift-undoes-shift? (shift-new 'left 0) (shift-new 'left 0)))
  (check-false (shift-undoes-shift? (shift-new 'right 2) #f))
  (check-false (shift-undoes-shift? #f (shift-new 'up 3)))
  (check-false (shift-undoes-shift? #f #f)))

;; test board->json-board
(module+ test
  (check-equal? (board->json-board board1)
                (hash 'connectors
                      (list (list "─" "┐" "└" "┘" "┌" "┬" "┤")
                            (list "┴" "├" "┼" "│" "─" "┐" "└")
                            (list "┘" "┌" "┬" "┤" "┴" "├" "┼")
                            (list "│" "─" "┐" "└" "┘" "┌" "┬")
                            (list "┤" "┴" "├" "┼" "│" "─" "┐")
                            (list "└" "┘" "┌" "┬" "┤" "┴" "├")
                            (list "┼" "│" "─" "┐" "└" "┘" "┌"))
                      'treasures
                      (list (list (list "alexandrite-pear-shape" "alexandrite")
                                  (list "almandine-garnet" "amethyst")
                                  (list "amethyst" "ametrine")
                                  (list "ammolite" "apatite")
                                  (list "aplite" "apricot-square-radiant")
                                  (list "aquamarine" "australian-marquise")
                                  (list "aventurine" "azurite"))
                            (list (list "beryl" "black-obsidian")
                                  (list "black-obsidian" "black-onyx")
                                  (list "black-spinel-cushion" "blue-ceylon-sapphire")
                                  (list "blue-cushion" "blue-pear-shape")
                                  (list "blue-spinel-heart" "bulls-eye")
                                  (list "carnelian" "chrome-diopside")
                                  (list "chrysoberyl-cushion" "chrysolite"))
                            (list (list "citrine-checkerboard" "citrine")
                                  (list "clinohumite" "color-change-oval")
                                  (list "cordierite" "diamond")
                                  (list "dumortierite" "emerald")
                                  (list "fancy-spinel-marquise" "garnet")
                                  (list "golden-diamond-cut" "goldstone")
                                  (list "grandidierite" "gray-agate"))
                            (list (list "green-aventurine" "green-beryl-antique")
                                  (list "green-beryl" "green-princess-cut")
                                  (list "grossular-garnet" "hackmanite")
                                  (list "heliotrope" "hematite")
                                  (list "iolite-emerald-cut" "jasper")
                                  (list "jaspilite" "kunzite-oval")
                                  (list "kunzite" "labradorite"))
                            (list (list "lapis-lazuli" "lemon-quartz-briolette")
                                  (list "magnesite" "mexican-opal")
                                  (list "moonstone" "morganite-oval")
                                  (list "moss-agate" "orange-radiant")
                                  (list "padparadscha-oval" "padparadscha-sapphire")
                                  (list "peridot" "pink-emerald-cut")
                                  (list "pink-opal" "pink-round"))
                            (list (list "pink-spinel-cushion" "prasiolite")
                                  (list "prehnite" "purple-cabochon")
                                  (list "purple-oval" "purple-spinel-trillion")
                                  (list "purple-square-cushion" "raw-beryl")
                                  (list "raw-citrine" "red-diamond")
                                  (list "red-spinel-square-emerald-cut" "rhodonite")
                                  (list "rock-quartz" "rose-quartz"))
                            (list (list "ruby-diamond-profile" "ruby")
                                  (list "sphalerite" "spinel")
                                  (list "star-cabochon" "stilbite")
                                  (list "sunstone" "super-seven")
                                  (list "tanzanite-trillion" "tigers-eye")
                                  (list "tourmaline-laser-cut" "tourmaline")
                                  (list "unakite" "white-square"))))))


(module+ examples
  (define example-board
    (list
     (list
      (tile-new 'straight 0 (list 'stilbite 'zircon))
      (tile-new 'straight 90 (list 'stilbite 'zircon))
      (tile-new 'elbow 180 (list 'stilbite 'zircon))
      (tile-new 'elbow 0 (list 'stilbite 'zircon))
      (tile-new 'elbow 270 (list 'stilbite 'zircon))
      (tile-new 'elbow 90 (list 'stilbite 'zircon))
      (tile-new 'tri 0 (list 'stilbite 'zircon)))
     (list
      (tile-new 'straight 0 (list 'prasiolite 'carnelian))
      (tile-new 'straight 90 (list 'prasiolite 'carnelian))
      (tile-new 'elbow 180 (list 'prasiolite 'carnelian))
      (tile-new 'elbow 0 (list 'prasiolite 'carnelian))
      (tile-new 'elbow 270 (list 'prasiolite 'carnelian))
      (tile-new 'elbow 90 (list 'prasiolite 'carnelian))
      (tile-new 'tri 0 (list 'prasiolite 'carnelian)))
     (list
      (tile-new 'straight 0 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'straight 90 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 180 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 0 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 270 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 90 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'tri 0 (list 'fancy-spinel-marquise 'jasper)))
     (list
      (tile-new 'straight 0 (list 'peridot 'purple-cabochon))
      (tile-new 'straight 90 (list 'peridot 'purple-cabochon))
      (tile-new 'elbow 180 (list 'peridot 'purple-cabochon))
      (tile-new 'elbow 0 (list 'peridot 'purple-cabochon))
      (tile-new 'elbow 270 (list 'peridot 'purple-cabochon))
      (tile-new 'elbow 90 (list 'peridot 'purple-cabochon))
      (tile-new 'tri 0 (list 'peridot 'purple-cabochon)))
     (list
      (tile-new 'straight 0 (list 'diamond 'lapis-lazuli))
      (tile-new 'straight 90 (list 'diamond 'lapis-lazuli))
      (tile-new 'elbow 180 (list 'diamond 'lapis-lazuli))
      (tile-new 'elbow 0 (list 'diamond 'lapis-lazuli))
      (tile-new 'elbow 270 (list 'diamond 'lapis-lazuli))
      (tile-new 'elbow 90 (list 'diamond 'lapis-lazuli))
      (tile-new 'tri 0 (list 'diamond 'lapis-lazuli)))
     (list
      (tile-new 'straight 0 (list 'cordierite 'mexican-opal))
      (tile-new 'straight 90 (list 'cordierite 'mexican-opal))
      (tile-new 'elbow 180 (list 'cordierite 'mexican-opal))
      (tile-new 'elbow 0 (list 'cordierite 'mexican-opal))
      (tile-new 'elbow 270 (list 'cordierite 'mexican-opal))
      (tile-new 'elbow 90 (list 'cordierite 'mexican-opal))
      (tile-new 'tri 0 (list 'cordierite 'mexican-opal)))
     (list
      (tile-new 'straight 0 (list 'pink-opal 'red-diamond))
      (tile-new 'straight 90 (list 'pink-opal 'red-diamond))
      (tile-new 'elbow 180 (list 'pink-opal 'red-diamond))
      (tile-new 'elbow 0 (list 'pink-opal 'red-diamond))
      (tile-new 'elbow 270 (list 'pink-opal 'red-diamond))
      (tile-new 'elbow 90 (list 'pink-opal 'red-diamond))
      (tile-new 'tri 0 (list 'pink-opal 'red-diamond)))))

  (define spare-tile (tile-new 'elbow 90 (list 'lapis-lazuli 'pink-opal)))

  (define example-treasures
    (list
     (list (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon"))
     (list (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian"))
     (list (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper"))
     (list (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon"))
     (list (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli"))
     (list (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal"))
     (list (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond"))))


  (define example-connectors
    (list '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")))

  (define example-board-hash
    (hash 'connectors example-connectors
          'treasures example-treasures)))

(module+ test
  (require (submod ".." examples))
  (check-equal? (json-board->board example-board-hash) example-board))
