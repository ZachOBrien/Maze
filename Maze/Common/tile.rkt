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
  [orientations (listof orientation?)]
  ; Constructs a new tile
  [tile-new (-> connector? orientation? (set/c gem?) tile?)]
  ; Rotates a tile
  [tile-rotate (-> tile? orientation? tile?)]
  ; Check if a tile holds exactly some gems
  [tile-has-gems? (-> tile? (set/c gem?) boolean?)]
  ; Returns true if you can travel from one tile to its adjacent neighbor vertically
  [tile-connected-vertical?   (-> tile? tile? boolean?)]
  ; Returns true if you can travel from one tile to its adjacent neighbor horizontally
  [tile-connected-horizontal? (-> tile? tile? boolean?)]
  ; Create a tile with a random connector and orientation
  [create-random-tile (-> (set/c gem?) tile?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/match)
(require racket/list)
(require racket/set)
(require racket/function)

(require "gem.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; A Tile is a structure:
;;    (tile Connector Orientation [Setof Gem])
;; interpretation: Represents a tile in the game of labyrinth
(struct tile [connector orientation gems] #:transparent)

;; Connector Orientation [Setof Gem] -> Tile
;; Create a new tile
(define (tile-new connector orientation gems)
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
;;   - 0
;;   - 90
;;   - 180
;;   - 270
;; interpretation: A direction a tile could be facing. Connector shapes
;;                 have the following orientations:
;; "│" 0
;; "─" 90
;; "│" 180
;; "─" 270
;;
;; "└" 0
;; "┌" 90
;; "┐" 180
;; "┘" 270
;;
;; "┬" 0
;; "┤" 90
;; "┴" 180
;; "├" 270
;;
;; "┼" 0
(define orientations (list 0 90 180 270))
(define orientation? (apply or/c orientations))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Tile [Setof Gem] -> Boolean
;; Check if a tile holds specific gems
(define (tile-has-gems? tile gems)
  (equal? (tile-gems tile) gems))

;; Tile Orientation -> Tile
(define (tile-rotate t rotation)
  (tile-new (tile-connector t)
            (modulo (+ (tile-orientation t) rotation) 360)
            (tile-gems t)))

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


;; [Setof Gem] -> Tile
;; Create a tile with a random connector and orientation
(define (create-random-tile gems)
  (define conn (first (shuffle connectors)))
  (define ornt (first (shuffle orientations)))
  (tile conn ornt gems))


(module+ draw
  (require 2htdp/image)

  (provide
   (contract-out
    [tile->image (-> tile? natural-number/c image?)]))

  (define TILE-SIZE 100)
  (define ARM-LENGTH (/ TILE-SIZE 10))
  
  ;; Orientation -> Image
  ;; Draw one arm of a connector
  (define (arm-image orientation arm-length)
    (define arm-width (/ arm-length 3))
    (define vert0degree (put-pinhole (/ arm-width 2) arm-length
                                     (rectangle arm-width arm-length "solid" "sienna")))
    (rotate orientation vert0degree))

  ;; Orientation -> Image
  ;; Draw an elbow connector
  (define (elbow-image orientation arm-length)
    (define elbow0degree (overlay/pinhole (arm-image 0 arm-length)
                                          (arm-image 270 arm-length)))
    (rotate orientation elbow0degree))

  ;; Orientation -> Image
  ;; Draw a straight connector
  (define (straight-image orientation arm-length)
    (define straight0degree (overlay/pinhole (arm-image 0 arm-length)
                                             (arm-image 180 arm-length)))
    (rotate orientation straight0degree))

  ;; Orientation -> Image
  ;; Draw a tri connector
  (define (tri-image orientation arm-length)
    (define tri0degree (overlay/pinhole (arm-image 180 arm-length)
                                        (arm-image 90 arm-length)
                                        (arm-image 270 arm-length)))
    (rotate orientation tri0degree))

  ;; Orientation -> Image
  ;; Draw a cross connector
  (define (cross-image orientation arm-length)
    (define cross0degree (overlay/pinhole (arm-image 0 arm-length)
                                          (arm-image 90 arm-length)
                                          (arm-image 180 arm-length)
                                          (arm-image 270 arm-length)))
    (rotate orientation cross0degree))


  ;; Connector Orientation Natural -> Image
  ;; Draw a connector with some orientation and with arms of length `arm-length`
  (define (draw-connector connector orientation arm-length)
    (match connector
      ['straight (straight-image orientation arm-length)]
      ['elbow (elbow-image orientation arm-length)]
      ['tri (tri-image orientation arm-length)]
      ['cross (cross-image orientation arm-length)]))


  ;; Tile [MultipleOf 10] -> Image
  ;; Draw a tile, a square with side lengths `size`
  (define (tile->image t size)
    (define base-tile
      (overlay (square size "outline" "black")
               (clear-pinhole (overlay/pinhole (draw-connector (tile-connector t)
                                                               (tile-orientation t)
                                                               (/ size 2))
                                               (square size "solid" "navajowhite")))))
    base-tile
    #;
    (define gems-to-draw (set->list (tile-gems t)))
    #;
    (underlay/xy (underlay/xy base-tile
                              10 10
                              (scale 0.25 (gem->image (first gems-to-draw))))
                 (- size 30) (- size 30)
                 (scale 0.25 (gem->image (second gems-to-draw))))))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (define tile00 (tile 'straight 90 (set)))
  (define tile01 (tile 'elbow 180 (set)))
  (define tile02 (tile 'elbow 0 (set)))
  (define tile03 (tile 'elbow 90 (set)))
  (define tile04 (tile 'elbow 270 (set)))
  (define tile05 (tile 'tri 0 (set)))
  (define tile06 (tile 'tri 270 (set)))

  (define tile10 (tile 'tri 180 (set)))
  (define tile11 (tile 'tri 90 (set 'blue-ceylon-sapphire 'bulls-eye)))
  (define tile12 (tile 'cross 0 (set)))
  (define tile13 (tile 'straight 0 (set)))
  (define tile14 (tile 'straight 270 (set)))
  (define tile15 (tile 'elbow 180 (set)))
  (define tile16 (tile 'elbow 0 (set)))

  (define tile20 (tile 'elbow 90 (set)))
  (define tile21 (tile 'elbow 270 (set)))
  (define tile22 (tile 'tri 0 (set)))
  (define tile23 (tile 'tri 270 (set)))
  (define tile24 (tile 'tri 180 (set)))
  (define tile25 (tile 'tri 90 (set)))
  (define tile26 (tile 'cross 270 (set)))

  (define tile30 (tile 'straight 180 (set)))
  (define tile31 (tile 'straight 270 (set)))
  (define tile32 (tile 'elbow 180 (set)))
  (define tile33 (tile 'elbow 0 (set)))
  (define tile34 (tile 'elbow 90 (set)))
  (define tile35 (tile 'elbow 270 (set)))
  (define tile36 (tile 'tri 0 (set)))

  (define tile40 (tile 'tri 270 (set)))
  (define tile41 (tile 'tri 180 (set)))
  (define tile42 (tile 'tri 90 (set)))
  (define tile43 (tile 'cross 0 (set)))
  (define tile44 (tile 'straight 0 (set)))
  (define tile45 (tile 'straight 90 (set)))
  (define tile46 (tile 'elbow 180 (set)))
  
  (define tile50 (tile 'elbow 0 (set)))
  (define tile51 (tile 'elbow 90 (set)))
  (define tile52 (tile 'elbow 270 (set)))
  (define tile53 (tile 'tri 0 (set)))
  (define tile54 (tile 'tri 270 (set)))
  (define tile55 (tile 'tri 180 (set)))
  (define tile56 (tile 'tri 90 (set)))
  
  (define tile60 (tile 'cross    0 (set)))
  (define tile61 (tile 'straight 0 (set)))
  (define tile62 (tile 'straight 90 (set)))
  (define tile63 (tile 'elbow 180 (set)))
  (define tile64 (tile 'elbow 0   (set)))
  (define tile65 (tile 'elbow 90 (set)))
  (define tile66 (tile 'elbow 270 (set)))

  (define tile-extra (tile 'straight 180 (set)))

  (define tile-horiz (tile 'straight 90 (set 'bulls-eye 'blue-ceylon-sapphire)))
  (define tile-vert (tile 'straight 0 (set 'alexandrite 'blue-ceylon-sapphire))))


(module+ test
  (require rackunit)
  (require (submod ".." examples)))

;; test tile-has-gems?
(module+ test
  (check-true (tile-has-gems? tile11 (set 'blue-ceylon-sapphire 'bulls-eye)))
  (check-true (tile-has-gems? tile11 (set 'bulls-eye 'blue-ceylon-sapphire)))
  (check-false (tile-has-gems? tile11 (set 'alexandrite 'blue-ceylon-sapphire))))

;; test tile-rotate
(module+ test
  (check-equal? (tile-rotate tile00 90) (tile 'straight 180 (set)))
  (check-equal? (tile-rotate tile00 180) (tile 'straight 270 (set)))
  (check-equal? (tile-rotate tile00 270) (tile 'straight 0 (set)))
  (check-equal? (tile-rotate tile00 0) (tile 'straight 90 (set)))
  (check-equal? (tile-rotate tile66 270) (tile 'elbow 180 (set))))

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
  (check-equal? (tile-new 'straight 0 (set)) (tile-new 'straight 0 (set)))
  (check-not-equal? (tile-new 'straight 0 (set)) (tile-new 'straight 90 (set)))
  (check-not-equal? (tile-new 'elbow 0 (set)) (tile-new 'straight 0 (set)))
  (check-not-equal? 
   (tile-new 'straight 0 (set 'aplite 'beryl))
   (tile-new 'straight 0 (set 'aplite 'aplite)))
  (check-equal?
   (tile-new 'straight 0 (set 'aplite 'beryl))
   (tile-new 'straight 0 (set 'aplite 'beryl)))
  (check-equal?
   (tile-new 'straight 0 (set 'beryl 'aplite))
   (tile-new 'straight 0 (set 'aplite 'beryl))))