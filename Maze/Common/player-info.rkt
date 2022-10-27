#lang racket

;;; This module provides a data definition and logic for a player


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  [player-info? contract?]
  ; Create a new player
  [player-info-new (-> grid-posn? grid-posn? grid-posn? boolean? avatar-color? player-info?)]
  ; Get a player's goal position
  [player-info-goal-pos (-> player-info? grid-posn?)]
  ; Get a player's home position
  [player-info-home-pos (-> player-info? grid-posn?)]
  ; Get a player's current position
  [player-info-curr-pos (-> player-info? grid-posn?)]
  ; Check if a player is on a position
  [player-info-on-pos? (-> player-info? grid-posn? boolean?)]
  ; True if a player has already visited their goal
  [player-info-visited-goal? (-> player-info? boolean?)]
  ; Move a player to the given gridposn
  [player-info-move-to (-> player-info? grid-posn? player-info?)]
  ; Move a player's goal to the given gridposn
  [player-info-change-goal (-> player-info? grid-posn? player-info?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "board.rkt")
(require rackunit)

;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;;A AvatarColor is one of:
;;  - a String that matches the regular expression:
;;      "^[A-F|\d][A-F|\d][A-F|\d][A-F|\d][A-F|\d][A-F|\d]$"
;;  - "purple",
;;  - "orange",
;;  - "pink",
;;  - "red",
;;  - "blue",
;;  - "green",
;;  - "yellow",
;;  - "white",
;;  - "black".
;; interpretation: The color of a player's avatar
;; SOURCED FROM SPEC: https://course.ccs.neu.edu/cs4500f22/4.html#%28tech._color%29
(define avatar-colors (list "red" "green" "yellow" "blue" "purple" "orange" "pink" "white" "black"))

;; String -> Boolean
;; Check whether the given hex matches the hex regex
(define (hex-color-code? hex)
  (list? (regexp-match-positions #px"^[A-F|\\d][A-F|\\d][A-F|\\d][A-F|\\d][A-F|\\d][A-F|\\d]$" hex)))

(define avatar-color? (apply or/c (cons hex-color-code? avatar-colors)))

;; A PlayerInfo is a structure:
;;    (struct GridPosn GridPosn GridPosn Boolean AvatarColor)
;; interpretation: The referee knows a player's current position, home position, goal position,
;;                 whether or not they2 3))'ve visited their goal position, and avatar color
(struct player-info [curr-pos home-pos goal-pos visited-goal? color] #:transparent)

;; GridPosn GridPosn [Listof Gem] Boolean AvatarColor -> PlayerInfo
;; Create a new player
(define (player-info-new curr-pos home-pos goal-pos visited-goal? color)
  (player-info curr-pos home-pos goal-pos visited-goal? color))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Player GridPosn -> Player
;; Move a player to the given gridposn
(define (player-info-move-to p pos)
  (struct-copy player-info p [curr-pos pos]))

;; Player GridPosn -> Plyaer
;; Move a player's goal to the given gridposn
(define (player-info-change-goal p new-goal)
  (struct-copy player-info p [goal-pos new-goal]))

;; Player GridPosn -> Boolean
;; Returns True if the player is on the given position
(define (player-info-on-pos? p pos)
  (equal? (player-info-curr-pos p) pos))


(module+ examples
  (provide (all-defined-out))
  (define player-info0
    (player-info
     (cons 0 0)
     (cons 6 6)
     (cons 5 1)
     #f
     "blue"))
  (define player-info1
    (player-info
     (cons 1 1)
     (cons 5 5)
     (cons 1 1)
     #f
     "red"))
  (define player-info2
    (player-info
     (cons 2 2)
     (cons 4 4)
     (cons 3 3)
     #f
     "green"))
  (define player-info3
    (player-info
     (cons 3 3)
     (cons 3 3)
     (cons 1 3)
     #f
     "yellow"))
  (define player-info4
    (player-info
     (cons 4 4)
     (cons 2 2)
     (cons 5 5)
     #f
     "blue"))
  (define player-info5
    (player-info
     (cons 0 6)
     (cons 5 5)
     (cons 1 5)
     #f
     "red"))
  (define player-info6
    (player-info
     (cons 6 0)
     (cons 4 4)
     (cons 3 1)
     #f
     "green"))
  (define player-info7
    (player-info
     (cons 6 6)
     (cons 3 3)
     (cons 5 3)
     #f
     "yellow"))
  (define player-info8
    (player-info
     (cons 5 5)
     (cons 3 3)
     (cons 5 1)
     #f
     "yellow"))
  (define player-info9
    (player-info
     (cons 5 5)
     (cons 3 3)
     (cons 5 1)
     #t
     "yellow")))

;; test hex-color-code?
(module+ test
  (check-true (hex-color-code? "A5B4C1"))
  (check-false (hex-color-code? "G5B4C1"))
  (check-false (hex-color-code? "A5B4C"))
  (check-false (hex-color-code? "a5B4C1"))
  (check-true (hex-color-code? "000000"))
  (check-true (hex-color-code? "B49E23")))