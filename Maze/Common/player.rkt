#lang racket

;;; This module provides a data definition and logic for a player


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  [player? contract?]
  ; Create a new player
  [player-new (-> grid-posn? grid-posn? grid-posn? boolean? avatar-color? player?)]
  ; Get a player's goal position
  [player-get-goal-pos (-> player? grid-posn?)]
  ; Get a player's home position
  [player-get-home-pos (-> player? grid-posn?)]
  ; Get a player's current position
  [player-get-curr-pos (-> player? grid-posn?)]
  ; True if a player has already visited their goal
  [player-visited-goal? (-> player? boolean?)]
  ; Move a player to the given gridposn
  [player-move-to (-> player? grid-posn? player?)]
  ; Move a player's goal to the given gridposn
  [player-change-goal (-> player? grid-posn? player?)]))

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
  

;; Player Player -> Boolean
;; Are the two players the same?
(define (player=? p1 p2 rec)
  (and (rec (player-curr-pos p1) (player-curr-pos p2))
       (rec (player-home-pos p1) (player-home-pos p2))
       (rec (player-goal-pos p1) (player-goal-pos p2))
       (rec (player-visited-goal p1) (player-visited-goal p2))
       (rec (player-color p1) (player-color p2))))

(define (player-hash-code pl rec)
  (+ (* 10000 (rec (player-curr-pos pl)))
     (* 1000  (rec (player-home-pos pl)))
     (* 100   (rec (player-goal-pos pl)))
     (* 10    (rec (player-visited-goal pl)))
     (* 1     (rec (player-color pl)))))

(define (player-secondary-hash-code pl rec)
  (+ (* 10000 (rec (player-color pl)))
     (* 1000  (rec (player-visited-goal pl)))
     (* 100   (rec (player-goal-pos pl)))
     (* 10    (rec (player-home-pos pl)))
     (* 1     (rec (player-curr-pos pl)))))

;; A Player is a structure:
;;    (struct GridPosn GridPosn GridPosn Boolean AvatarColor)
;; interpretation: A player has a current position, home position, goal position,
;;                 whether or not they've visited their goal position, and avatar color
(struct player [curr-pos home-pos goal-pos visited-goal color]
  #:methods gen:equal+hash
  [(define equal-proc player=?)
   (define hash-proc  player-hash-code)
   (define hash2-proc player-secondary-hash-code)])

;; GridPosn GridPosn [Listof Gem] Boolean AvatarColor -> Player
;; Create a new player
(define (player-new curr-pos home-pos goal-pos visited-goal color)
  (player curr-pos home-pos goal-pos visited-goal color))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Player GridPosn -> Player
;; Move a player to the given gridposn
(define (player-move-to p pos)
  (struct-copy player p [curr-pos pos]))

;; Player GridPosn -> Plyaer
;; Move a player's goal to the given gridposn
(define (player-change-goal p new-goal)
  (struct-copy player p [goal-pos new-goal]))

;; Player GridPosn -> Boolean
;; Returns True if the player is on the given position
(define (player-on-pos? p pos)
  (equal? (player-get-curr-pos p) pos))

;; Player -> GridPosn
;; Get a player's current position
(define (player-get-curr-pos plyr)
  (player-curr-pos plyr))

;; Player -> GridPosn
;; Get a player's goal treasures
(define (player-get-goal-pos plyr)
  (player-goal-pos plyr))

;; Player -> GridPosn
;; Get a player's goal location
(define (player-get-home-pos plyr)
  (player-home-pos plyr))

;; Player -> Boolean
;; True if a player has already visited their goal
(define (player-visited-goal? plyr)
  (player-visited-goal plyr))


(module+ examples
  (provide (all-defined-out))
  (define player0
    (player
     (cons 0 0)
     (cons 6 6)
     (cons 5 1)
     #f
     "blue"))
  (define player1
    (player
     (cons 1 1)
     (cons 5 5)
     (cons 1 1)
     #f
     "red"))
  (define player2
    (player
     (cons 2 2)
     (cons 4 4)
     (cons 3 3)
     #f
     "green"))
  (define player3
    (player
     (cons 3 3)
     (cons 3 3)
     (cons 1 3)
     #f
     "yellow"))
  (define player4
    (player
     (cons 4 4)
     (cons 2 2)
     (cons 5 5)
     #f
     "blue"))
  (define player5
    (player
     (cons 0 6)
     (cons 5 5)
     (cons 1 5)
     #f
     "red"))
  (define player6
    (player
     (cons 6 0)
     (cons 4 4)
     (cons 3 1)
     #f
     "green"))
  (define player7
    (player
     (cons 6 6)
     (cons 3 3)
     (cons 5 3)
     #f
     "yellow"))
  (define player8
    (player
     (cons 5 5)
     (cons 3 3)
     (cons 5 1)
     #f
     "yellow"))
  (define player9
    (player
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