#lang racket

;;; This module provides a data definition and logic for a Maze player representation


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/draw)
(require racket/contract)

(provide
 (contract-out
  [player-id? contract?]
  [player? contract?]
  ; Create a new Player
  [player-new (-> player-id? grid-posn? grid-posn? (listof gem?) date? avatar-color? player?)]
  ; Get a player's ID
  [get-player-id (-> player? player-id?)]
  ; Get a player's current position
  [get-player-curr-pos (-> player? grid-posn?)]
  ; Check if a player is on a position
  [player-on-pos? (-> player? grid-posn? boolean?)]
  ; Move a player to a new position
  [player-move-to (-> player? grid-posn? player?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "gem.rkt")
(require "tile.rkt")
(require "board.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; A PlayerID is a Natural
;; interpretation: A player's unique ID
(define player-id? natural-number/c)

;; An AvatarColor is one of:
;; - "red"
;; - "green"
;; - "yellow"
;; - "blue"
;; interpretation: The color of a player's avatar
(define avatar-color? (or/c "red" "green" "yellow" "blue"))


;; A Player is a structure:
;;    (struct PlayerID GridPosn GridPosn [Listof Gem] Date AvatarColor)
;; interpretation: A player has an ID, a current position, home position, goal treasure,
;;                 birthday, and avatar color
(struct player [id curr-pos home-pos goal-treasures dob color])

;; GridPosn GridPosn [Listof Gem] Date Color% -> Player
;; Create a new player
(define (player-new id curr-pos home-pos goal-treasures dob color)
  (player id curr-pos home-pos goal-treasures dob color))


;; --------------------------------------------------------------------
;; FUNCTIONALITY

;; Player Player -> Boolean
;; Are the two players the same?
(define (player=? p1 p2)
  (and
   (= (player-id p1) (player-id p2))
   (equal? (player-curr-pos p1) (player-curr-pos p2))
   (equal? (player-home-pos p1) (player-home-pos p2))
   (equal? (player-goal-treasures p1) (player-goal-treasures p2))
   (equal? (player-dob p1) (player-dob p2))
   (equal? (player-color p1) (player-color p2))))


;; Player -> PlayerID
;; Get a player's ID
(define (get-player-id p)
  (player-id p))

;; Player -> GridPosn
;; Get a player's current position
(define (get-player-curr-pos p)
  (player-curr-pos p))

;; Player GridPosn -> Boolean
;; Returns True if the player is on the given position
(define (player-on-pos? p pos)
  (equal? (player-curr-pos p) pos))


;; Player GridPosn -> Player
;; Move a player to the given gridposn
(define (player-move-to p pos)
  (struct-copy player p [curr-pos pos]))


;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (define player0
    (player
     0 (cons 0 0) (cons 6 6) (list 'apatite 'aplite) (seconds->date (current-seconds)) "blue")))

(module+ test
  (require rackunit)
  (require (submod ".." examples)))

;; Test player-on-pos
(module+ test
  (check-true (player-on-pos? player0 (cons 0 0))))

;; Test player-move-to
(module+ test
  (check-true (player=?
               (player-move-to player0 (cons 3 3))
               (player
                0
                (cons 3 3)
                (cons 6 6)
                (list 'apatite 'aplite)
                (seconds->date (current-seconds))
                "blue")))
  (check-true (player=?
               (player-move-to player0 (cons 6 6))
               (player
                0
                (cons 6 6)
                (cons 6 6)
                (list 'apatite 'aplite)
                (seconds->date (current-seconds))
                "blue"))))