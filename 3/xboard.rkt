#lang racket/base

(require json)

(define (read-board-and-coordinate input-port)
  (define board (read-json input-port))
  (define coordinate (read-json input-port))
  (values board coordinate))


;; Hashtable -> Board
#; 
(define (hash->board ht)
  (define connectors (hash-ref ht 'connectors))
  (define treasures (hash-ref ht 'treasures))
  (for
      (for)))

(define string-connector-conversion
  (hash "│" (values 'straight 0)
        "─" (values 'straight 90)
        "┐" (values 'elbow 180)
        "└" (values 'elbow 0)
        "┌" (values 'elbow 90)
        "┘" (values 'elbow 270)
        "┬" (values 'tri 0)
        "├" (values 'tri 270)
        "┴" (values 'tri 180)
        "┤" (values 'tri 90)
        "┼" (values 'cross 0)))

      
;; String [Listof String] -> Tile
;; Converts a connector in string form and a list of gems in string form to a tile
(define (conn-and-gem->tile conn gems)
  (define-values connector orientation (hash-ref string-connector-conversion conn))
  (make-tile
   connector
   orientation
   (map string->symbol gem-array)))

  ;match the conn
  ;make a tile with the string -> symbol thingy