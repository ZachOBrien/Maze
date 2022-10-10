#lang racket/base

;;; This module provides the data definition and functionality for a Maze game Tile


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)


(provide
 (contract-out
  [tile?        contract?]
  [connector?   contract?]
  [orientation? contract?]
  ; Constructs a new tile
  [tile-make (-> connector? orientation? (listof gem?) tile?)]
  ;; Returns true if you can travel from one tile to its adjacent neighbor vertically
  [tile-connected-vertical?   (-> tile? tile? boolean?)]
  ;; Returns true if you can travel from one tile to its adjacent neighbor horizontally
  [tile-connected-horizontal? (-> tile? tile? boolean?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/match)
(require racket/list)
(require racket/set)
(require racket/function)

(require "gem.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS



;; Tile Tile (-> Any Any Boolean) -> Boolean
;; Check if two tiles are equal
(define (tile=? tile1 tile2 recursive-equal?)
  (and (eq? (tile-connector tile1)
          (tile-connector tile2))
       (= (tile-orientation tile1)
          (tile-orientation tile2))
       (equal?
        (list->set (tile-gems tile1))
        (list->set (tile-gems tile2)))))

;; Tile (-> Any Integer) -> Integer
;; Computes a hash code for a given Tile
(define (tile-hash-code tile recursive-equal-hash)
  (+ (* 1000 (equal-hash-code (tile-connector tile)))
     (* 100 (tile-orientation tile))
     (* 1 (equal-hash-code (tile-gems tile)))))

;; Tile (-> Any Integer) -> Integer
;; Computes a secondary hash for a given Tile
(define (tile-secondary-hash-code tile recursive-equal-hash)
  (+ (* 1000 (tile-orientation tile))
     (* 100 (equal-secondary-hash-code (tile-gems tile)))
     (* 1 (equal-secondary-hash-code (tile-connector tile)))))

(define (tile-print tile port mode)
  (define recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))]))
  (define tile-string
    (string-append
     "["
     (symbol->string (tile-connector tile))
     ", "
     (number->string (tile-orientation tile))
     ", "
     (list->string (tile-gems tile))
     "]"))
  (recur tile-string port))

;  (when mode (write-string ">" port)))

;; A Tile is a structure:
;;    (tile Connector Orientation [Listof Gem])
;; interpretation: Represents a tile in the game of labyrinth
(struct tile [connector orientation gems]
  #:methods gen:equal+hash
  [(define equal-proc tile=?)
   (define hash-proc  tile-hash-code)
   (define hash2-proc tile-secondary-hash-code)]
  #:methods gen:custom-write
  [(define write-proc tile-print)])

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

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Tile Tile -> Boolean
(define (tile-connected-horizontal? left right)
  (and (open-on-right? (tile-connector left) (tile-orientation left))
       (open-on-left? (tile-connector right) (tile-orientation right))))


;; Tile Tile -> Boolean
(define (tile-connected-vertical? top bottom)
  (and (open-on-bottom? (tile-connector top) (tile-orientation top))
       (open-on-top? (tile-connector bottom) (tile-orientation bottom))))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its top edge
(define (open-on-top? connector orientation)
  (match* (connector orientation)
    [('cross _)     #t]
    [('tri o)      (not (= 0 o))]
    [('elbow o)    (or (= 0 o) (= 270 o))]
    [('straight o) (or (= 0 o) (= 180 o))]))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its bottom edge
(define (open-on-bottom? connector orientation)
  (match* (connector orientation)
    [('cross _)     #t]
    [('tri o)      (not (= 180 o))]
    [('elbow o)    (or (= 90 o) (= 180 o))]
    [('straight o) (or (= 0 o) (= 180 o))]))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its left edge
(define (open-on-left? connector orientation)
  (match* (connector orientation)
    [('cross _)     #t]
    [('tri o)      (not (= 270 o))]
    [('elbow o)    (or (= 180 o) (= 270 o))]
    [('straight o) (or (= 90 o) (= 270 o))]))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its right edge
(define (open-on-right? connector orientation)
  (match* (connector orientation)
    [('cross _)     #t]
    [('tri o)      (not (= 90 o))]
    [('elbow o)    (or (= 0 o) (= 90 o))]
    [('straight o) (or (= 90 o) (= 270 o))]))


;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
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

  (define tile-extra (tile 'straight 180 empty)))


(module+ test
  (require rackunit)
  (require (submod ".." examples)))

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

;; test open-on-top?
(module+ test
  (check-true (open-on-top? 'straight 0))
  (check-false (open-on-top? 'straight 90))
  (check-true (open-on-top? 'straight 180))
  (check-false (open-on-top? 'straight 270))
  
  (check-true (open-on-top? 'elbow 0))
  (check-false (open-on-top? 'elbow 90))
  (check-false (open-on-top? 'elbow 180))
  (check-true (open-on-top? 'elbow 270))
  
  (check-false (open-on-top? 'tri 0))
  (check-true (open-on-top? 'tri 90))
  (check-true (open-on-top? 'tri 180))
  (check-true (open-on-top? 'tri 270))
  
  (check-true (open-on-top? 'cross 0))
  (check-true (open-on-top? 'cross 90))
  (check-true (open-on-top? 'cross 180))
  (check-true (open-on-top? 'cross 270)))

;; test open-on-right
(module+ test
  (check-false (open-on-right? 'straight 0))
  (check-true (open-on-right? 'straight 90))
  (check-false (open-on-right? 'straight 180))
  (check-true (open-on-right? 'straight 270))
  
  (check-true (open-on-right? 'elbow 0))
  (check-true (open-on-right? 'elbow 90))
  (check-false (open-on-right? 'elbow 180))
  (check-false (open-on-right? 'elbow 270))
  
  (check-true (open-on-right? 'tri 0))
  (check-false (open-on-right? 'tri 90))
  (check-true (open-on-right? 'tri 180))
  (check-true (open-on-right? 'tri 270))
  
  (check-true (open-on-right? 'cross 0))
  (check-true (open-on-right? 'cross 90))
  (check-true (open-on-right? 'cross 180))
  (check-true (open-on-right? 'cross 270)))

;; test open-on-bottom
(module+ test
  (check-true (open-on-bottom? 'straight 0))
  (check-false (open-on-bottom? 'straight 90))
  (check-true (open-on-bottom? 'straight 180))
  (check-false (open-on-bottom? 'straight 270))
  
  (check-false (open-on-bottom? 'elbow 0))
  (check-true (open-on-bottom? 'elbow 90))
  (check-true (open-on-bottom? 'elbow 180))
  (check-false (open-on-bottom? 'elbow 270))
  
  (check-true (open-on-bottom? 'tri 0))
  (check-true (open-on-bottom? 'tri 90))
  (check-false (open-on-bottom? 'tri 180))
  (check-true (open-on-bottom? 'tri 270))

  (check-true (open-on-bottom? 'cross 0))
  (check-true (open-on-bottom? 'cross 90))
  (check-true (open-on-bottom? 'cross 180))
  (check-true (open-on-bottom? 'cross 270)))

;; test open-on-left
(module+ test
  (check-false (open-on-left? 'straight 0))
  (check-true (open-on-left? 'straight 90))
  (check-false (open-on-left? 'straight 180))
  (check-true (open-on-left? 'straight 270))
  
  (check-false (open-on-left? 'elbow 0))
  (check-false (open-on-left? 'elbow 90))
  (check-true (open-on-left? 'elbow 180))
  (check-true (open-on-left? 'elbow 270))
  
  (check-true (open-on-left? 'tri 0))
  (check-true (open-on-left? 'tri 90))
  (check-true (open-on-left? 'tri 180))
  (check-false (open-on-left? 'tri 270))

  (check-true (open-on-left? 'cross 0))
  (check-true (open-on-left? 'cross 90))
  (check-true (open-on-left? 'cross 180))
  (check-true (open-on-left? 'cross 270)))

;; test tile=?
(module+ test
  (check-equal? (tile-make 'straight 0 empty) (tile-make 'straight 0 empty))
  (check-not-equal? (tile-make 'straight 0 empty) (tile-make 'straight 90 empty))
  (check-not-equal? (tile-make 'elbow 0 empty) (tile-make 'straight 0 empty))
  (check-not-equal? 
   (tile-make 'straight 0 (list 'aplite 'beryl))
   (tile-make 'straight 0 (list 'aplite 'aplite)))
  (check-equal?
   (tile-make 'straight 0 (list 'aplite 'beryl))
   (tile-make 'straight 0 (list 'aplite 'beryl)))
    (check-equal?
   (tile-make 'straight 0 (list 'beryl 'aplite))
   (tile-make 'straight 0 (list 'aplite 'beryl))))