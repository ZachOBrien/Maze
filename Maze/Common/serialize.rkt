#lang racket

;;; This module provides logic for serializing and deserializing JSON representations
;;; of the Maze game's data definitions

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  ;; Convert a hashtable to a Boards
  [hash->board (-> hash? board?)]
  ;; Convert a GridPosn to a hashtable
  [gridposn->hash (-> grid-posn? hash?)]
  ;; Convert a hashtable to a Tile
  [hash->spare-tile (-> hash? tile?)]
  ;; Convert a hashtable to a Gamestate
  [hash->gamestate (-> hash? gamestate?)]
  ;; Convert a hashtable to a GridPosn
  [hash->gridposn (-> hash? grid-posn?)]
  ;; Convert a hashtable to a Player
  [hash->player (-> hash? player?)]
  ;; Convert a json action to a Move
  [json-action->last-action (-> (or/c (listof any/c) 'null) shift?)]
  ;; Convert a string direction to a symbol
  [string-direction->symbol (-> string? symbol?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "tile.rkt")
(require "board.rkt")
(require "state.rkt")
(require "player.rkt")

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
  (tile-new connector orientation (list->set (map string->symbol gems))))

(module+ test
  (check-equal?
   (conn-and-gem->tile "│" (list "aplite" "beryl"))
   (tile-new 'straight 0 (set 'aplite 'beryl)))
  (check-equal?
   (conn-and-gem->tile "┐" (list "amethyst" "beryl"))
   (tile-new 'elbow 180 (set 'amethyst 'beryl)))
  (check-equal?
   (conn-and-gem->tile "┴" (list "aplite" "beryl"))
   (tile-new 'tri 180 (set 'aplite 'beryl))))


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


;; HashTable -> Tile
;; Create the spare tile from a HashTable
(define (hash->spare-tile ht)
  (define conn (hash-ref ht 'tilekey))
  (define treasures (set (string->symbol (hash-ref ht '1-image))
                          (string->symbol (hash-ref ht '2-image))))
  (match-define (cons connector orientation) (hash-ref string-connector-conversion conn))
  (tile-new connector orientation treasures))

(module+ test
  (check-equal? (hash->spare-tile (hash 'tilekey "┌"
                                        '1-image "goldstone"
                                        '2-image "heliotrope"))
                (tile-new 'elbow 90 (set 'goldstone 'heliotrope)))
  (check-equal? (hash->spare-tile (hash 'tilekey "┼"
                                        '1-image "diamond"
                                        '2-image "unakite"))
                (tile-new 'cross 0 (set 'diamond 'unakite)))
  
  (check-equal? (hash->spare-tile (hash 'tilekey "─"
                                        '1-image "raw-beryl"
                                        '2-image "pink-opal"))
                (tile-new 'straight 90 (set 'raw-beryl 'pink-opal)))
  
  (check-equal? (hash->spare-tile (hash 'tilekey "┴"
                                        '1-image "hematite"
                                        '2-image "jasper"))
                (tile-new 'tri 180 (set 'hematite 'jasper))))


;; Hashtable -> GridPosn
;; Converts a hashtable to a gridposn
(define (hash->gridposn ht)
  (cons (hash-ref ht 'row#) (hash-ref ht 'column#)))


;; HashTable -> Player
;; Create a player from a HashTable
(define (hash->player ht)
  (player-new (hash->gridposn (hash-ref ht 'current))
              (hash->gridposn (hash-ref ht 'home))
              (cons 1 1)
              #f
              (hash-ref ht 'color)))

(module+ test
  (check-equal? (hash->player (hash 'current (hash 'row# 0 'column# 0)
                                    'home (hash 'row# 2 'column# 2)
                                    'color "blue"))
                (player-new (cons 0 0) (cons 2 2) (cons 1 1) #f "blue"))
  (check-equal? (hash->player (hash 'current (hash 'row# 6 'column# 1)
                                    'home (hash 'row# 3 'column# 4)
                                    'color "red"))
                (player-new (cons 6 1) (cons 3 4) (cons 1 1) #f "red")))

;; (U [Listof Any] 'null) -> Move
;; Makes a move from the list
(define (json-action->last-action action)
  (if (equal? action 'null)
      #f
      (shift-new (string-direction->symbol (first (rest action)))
                 (first action))))

(module+ test
  (check-equal? (json-action->last-action (list 0 "UP"))
                (shift-new 'up 0))
  (check-equal? (json-action->last-action (list 4 "RIGHT"))
                    (shift-new 'right 4))
  (check-equal? (json-action->last-action 'null)
                #f))

;; String -> Symbol
;; Convert a string direciton to a symbol direciton
(define (string-direction->symbol str)
  (string->symbol (string-downcase str)))

;; HashTable -> Gamestate
;; Makes a gamestate from a hashtable
(define (hash->gamestate ht)
  (gamestate-new
   (hash->board (hash-ref ht 'board))
   (hash->spare-tile (hash-ref ht 'spare))
   (map hash->player (hash-ref ht 'plmt))
   (json-action->last-action (hash-ref ht 'last))))
   

(module+ examples
  (define example-board
    (list
     (list
      (tile-new 'straight 0 (set 'stilbite 'zircon))
      (tile-new 'straight 90 (set 'stilbite 'zircon))
      (tile-new 'elbow 180 (set 'stilbite 'zircon))
      (tile-new 'elbow 0 (set 'stilbite 'zircon))
      (tile-new 'elbow 90 (set 'stilbite 'zircon))
      (tile-new 'elbow 270 (set 'stilbite 'zircon))
      (tile-new 'tri 0 (set 'stilbite 'zircon)))
     (list
      (tile-new 'straight 0 (set 'prasiolite 'carnelian))
      (tile-new 'straight 90 (set 'prasiolite 'carnelian))
      (tile-new 'elbow 180 (set 'prasiolite 'carnelian))
      (tile-new 'elbow 0 (set 'prasiolite 'carnelian))
      (tile-new 'elbow 90 (set 'prasiolite 'carnelian))
      (tile-new 'elbow 270 (set 'prasiolite 'carnelian))
      (tile-new 'tri 0 (set 'prasiolite 'carnelian)))
     (list
      (tile-new 'straight 0 (set 'fancy-spinel-marquise 'jasper))
      (tile-new 'straight 90 (set 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 180 (set 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 0 (set 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 90 (set 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 270 (set 'fancy-spinel-marquise 'jasper))
      (tile-new 'tri 0 (set 'fancy-spinel-marquise 'jasper)))
     (list
      (tile-new 'straight 0 (set 'peridot 'purple-cabochon))
      (tile-new 'straight 90 (set 'peridot 'purple-cabochon))
      (tile-new 'elbow 180 (set 'peridot 'purple-cabochon))
      (tile-new 'elbow 0 (set 'peridot 'purple-cabochon))
      (tile-new 'elbow 90 (set 'peridot 'purple-cabochon))
      (tile-new 'elbow 270 (set 'peridot 'purple-cabochon))
      (tile-new 'tri 0 (set 'peridot 'purple-cabochon)))
     (list
      (tile-new 'straight 0 (set 'diamond 'lapis-lazuli))
      (tile-new 'straight 90 (set 'diamond 'lapis-lazuli))
      (tile-new 'elbow 180 (set 'diamond 'lapis-lazuli))
      (tile-new 'elbow 0 (set 'diamond 'lapis-lazuli))
      (tile-new 'elbow 90 (set 'diamond 'lapis-lazuli))
      (tile-new 'elbow 270 (set 'diamond 'lapis-lazuli))
      (tile-new 'tri 0 (set 'diamond 'lapis-lazuli)))
     (list
      (tile-new 'straight 0 (set 'cordierite 'mexican-opal))
      (tile-new 'straight 90 (set 'cordierite 'mexican-opal))
      (tile-new 'elbow 180 (set 'cordierite 'mexican-opal))
      (tile-new 'elbow 0 (set 'cordierite 'mexican-opal))
      (tile-new 'elbow 90 (set 'cordierite 'mexican-opal))
      (tile-new 'elbow 270 (set 'cordierite 'mexican-opal))
      (tile-new 'tri 0 (set 'cordierite 'mexican-opal)))
     (list
      (tile-new 'straight 0 (set 'pink-opal 'red-diamond))
      (tile-new 'straight 90 (set 'pink-opal 'red-diamond))
      (tile-new 'elbow 180 (set 'pink-opal 'red-diamond))
      (tile-new 'elbow 0 (set 'pink-opal 'red-diamond))
      (tile-new 'elbow 90 (set 'pink-opal 'red-diamond))
      (tile-new 'elbow 270 (set 'pink-opal 'red-diamond))
      (tile-new 'tri 0 (set 'pink-opal 'red-diamond)))))

  (define spare-tile (tile-new 'elbow 270 (set 'lapis-lazuli 'pink-opal)))

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

  (define example-players1
    (list (hash 'current (hash 'row# 0 'column# 0) 'home (hash 'row# 6 'column# 6) 'color "blue")
          (hash 'current (hash 'row# 1 'column# 1) 'home (hash 'row# 5 'column# 5) 'color "red")
          (hash 'current (hash 'row# 2 'column# 2) 'home (hash 'row# 4 'column# 4) 'color "green")
          (hash 'current (hash 'row# 3 'column# 3) 'home (hash 'row# 3 'column# 3) 'color "yellow")))

  (define expected-players1
    (list (player-new (cons 0 0) (cons 6 6) (cons 1 1) #f "blue")
          (player-new (cons 1 1) (cons 5 5) (cons 1 1) #f "red")
          (player-new (cons 2 2) (cons 4 4) (cons 1 1) #f "green")
          (player-new (cons 3 3) (cons 3 3) (cons 1 1) #f "yellow")))

  (define example-board-hash
    (hash 'connectors example-connectors
          'treasures example-treasures)))

(module+ test
  (require (submod ".." examples))
  (check-equal? (hash->board example-board-hash) example-board))

(module+ test
  (require (submod ".." examples))
  (check-equal? (hash->gamestate (hash 'board example-board-hash
                                       'spare (hash 'tilekey "┘"
                                                    '1-image "lapis-lazuli"
                                                    '2-image "pink-opal")
                                       'plmt example-players1
                                       'last (list 0 "LEFT")))
                (gamestate-new example-board
                               spare-tile
                               expected-players1
                               (shift-new 'left 0))))
