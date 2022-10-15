#lang racket

;;; This module provides logic for serializing and deserializing JSON representations
;;; of the Maze game's data definitions

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  [hash->board (-> hash? board?)]
  [gridposn->hash (-> grid-posn? hash?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "tile.rkt")
(require "board.rkt")

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(module+ examples
  (provide (all-defined-out)))

(module+ test
  (require rackunit))


;; String -> (values Connector Orientation)
;; Converts any orientation of connector string to the canonical string and its orientation
(define string-connector-conversion
  (hash "│" (cons 'straight 0)
        "─" (cons 'straight 90)
        "┐" (cons 'elbow 180)
        "└" (cons 'elbow 0)
        "┌" (cons 'elbow 90)
        "┘" (cons 'elbow 270)
        "┬" (cons 'tri 0)
        "├" (cons 'tri 270)
        "┴" (cons 'tri 180)
        "┤" (cons 'tri 90)
        "┼" (cons 'cross 0)))


;; String [Listof String] -> Tile
;; Converts a connector in string form and a list of gems in string form to a tile
(define (conn-and-gem->tile conn gems)
  (match-define (cons connector orientation) (hash-ref string-connector-conversion conn))
  (tile-make connector orientation (map string->symbol gems)))

(module+ test
  (check-equal?
   (conn-and-gem->tile "│" (list "aplite" "beryl"))
   (tile-make 'straight 0 (list 'aplite 'beryl)))
  (check-equal?
   (conn-and-gem->tile "┐" (list "amethyst" "beryl"))
   (tile-make 'elbow 180 (list 'amethyst 'beryl)))
  (check-equal?
   (conn-and-gem->tile "┴" (list "aplite" "beryl"))
   (tile-make 'tri 180 (list 'aplite 'beryl))))


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
         '(0 0 9))))


;; HashTable -> Board
;; Creates a matrix of tiles given a hashtable with matrices of connectors and treasures
(define (hash->board ht)
  (define connectors (hash-ref ht 'connectors))
  (define treasures (hash-ref ht 'treasures))
  (combine-matrices-elementwise conn-and-gem->tile connectors treasures))


;; GridPosn -> HashTable
;; Converts a GridPosn into a HashTable according to spec
(define (gridposn->hash pos)
  (hash 'row# (car pos)
        'column# (cdr pos)))


(module+ examples
  (define example-board
    (list
     (list
      (tile-make 'straight 0 (list 'stilbite 'zircon))
      (tile-make 'straight 90 (list 'stilbite 'zircon))
      (tile-make 'elbow 180 (list 'stilbite 'zircon))
      (tile-make 'elbow 0 (list 'stilbite 'zircon))
      (tile-make 'elbow 90 (list 'stilbite 'zircon))
      (tile-make 'elbow 270 (list 'stilbite 'zircon))
      (tile-make 'tri 0 (list 'stilbite 'zircon)))
     (list
      (tile-make 'straight 0 (list 'prasiolite 'carnelian))
      (tile-make 'straight 90 (list 'prasiolite 'carnelian))
      (tile-make 'elbow 180 (list 'prasiolite 'carnelian))
      (tile-make 'elbow 0 (list 'prasiolite 'carnelian))
      (tile-make 'elbow 90 (list 'prasiolite 'carnelian))
      (tile-make 'elbow 270 (list 'prasiolite 'carnelian))
      (tile-make 'tri 0 (list 'prasiolite 'carnelian)))
     (list
      (tile-make 'straight 0 (list 'fancy-spinel-marquise 'jasper))
      (tile-make 'straight 90 (list 'fancy-spinel-marquise 'jasper))
      (tile-make 'elbow 180 (list 'fancy-spinel-marquise 'jasper))
      (tile-make 'elbow 0 (list 'fancy-spinel-marquise 'jasper))
      (tile-make 'elbow 90 (list 'fancy-spinel-marquise 'jasper))
      (tile-make 'elbow 270 (list 'fancy-spinel-marquise 'jasper))
      (tile-make 'tri 0 (list 'fancy-spinel-marquise 'jasper)))
     (list
      (tile-make 'straight 0 (list 'peridot 'purple-cabochon))
      (tile-make 'straight 90 (list 'peridot 'purple-cabochon))
      (tile-make 'elbow 180 (list 'peridot 'purple-cabochon))
      (tile-make 'elbow 0 (list 'peridot 'purple-cabochon))
      (tile-make 'elbow 90 (list 'peridot 'purple-cabochon))
      (tile-make 'elbow 270 (list 'peridot 'purple-cabochon))
      (tile-make 'tri 0 (list 'peridot 'purple-cabochon)))
     (list
      (tile-make 'straight 0 (list 'diamond 'lapis-lazuli))
      (tile-make 'straight 90 (list 'diamond 'lapis-lazuli))
      (tile-make 'elbow 180 (list 'diamond 'lapis-lazuli))
      (tile-make 'elbow 0 (list 'diamond 'lapis-lazuli))
      (tile-make 'elbow 90 (list 'diamond 'lapis-lazuli))
      (tile-make 'elbow 270 (list 'diamond 'lapis-lazuli))
      (tile-make 'tri 0 (list 'diamond 'lapis-lazuli)))
     (list
      (tile-make 'straight 0 (list 'cordierite 'mexican-opal))
      (tile-make 'straight 90 (list 'cordierite 'mexican-opal))
      (tile-make 'elbow 180 (list 'cordierite 'mexican-opal))
      (tile-make 'elbow 0 (list 'cordierite 'mexican-opal))
      (tile-make 'elbow 90 (list 'cordierite 'mexican-opal))
      (tile-make 'elbow 270 (list 'cordierite 'mexican-opal))
      (tile-make 'tri 0 (list 'cordierite 'mexican-opal)))
     (list
      (tile-make 'straight 0 (list 'pink-opal 'red-diamond))
      (tile-make 'straight 90 (list 'pink-opal 'red-diamond))
      (tile-make 'elbow 180 (list 'pink-opal 'red-diamond))
      (tile-make 'elbow 0 (list 'pink-opal 'red-diamond))
      (tile-make 'elbow 90 (list 'pink-opal 'red-diamond))
      (tile-make 'elbow 270 (list 'pink-opal 'red-diamond))
      (tile-make 'tri 0 (list 'pink-opal 'red-diamond)))))

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

  (define example-hash
    (hash 'connectors example-connectors
          'treasures example-treasures)))


(module+ test
  (require (submod ".." examples))
  (check-equal? (hash->board example-hash) example-board))
