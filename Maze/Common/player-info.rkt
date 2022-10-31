#lang racket

;;; This module provides a data definition and logic for a player


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  [player-info?     contract?]
  [pub-player-info? contract?]
  [ref-player-info? contract?]
  [avatar-color?    contract?]
  ; Create a new public player info
  [pub-player-info-new (-> grid-posn? grid-posn? 'hidden 'hidden avatar-color? player-info?)]
  ; Create a new referee player info
  [ref-player-info-new (-> grid-posn? grid-posn? grid-posn? boolean? avatar-color? player-info?)]
  ; Convert a referee player info into a public player info
  [ref-player-info->pub-player-info (-> ref-player-info? pub-player-info?)]
  ; Get a player's treasure position
  [player-info-treasure-pos (-> ref-player-info? grid-posn?)]
  ; Get a player's home position
  [player-info-home-pos (-> player-info? grid-posn?)]
  ; Get a player's current position
  [player-info-curr-pos (-> player-info? grid-posn?)]
  ; Check if a player is on a position
  [player-info-on-pos? (-> player-info? grid-posn? boolean?)]
  ; True if a player has already visited their treasure
  [player-info-visited-treasure? (-> ref-player-info? boolean?)]
  ; Move a player to the given gridposn
  [player-info-move-to (-> player-info? grid-posn? player-info?)]
  ; Move a player's treasure to the given gridposn
  [change-treasure (-> ref-player-info? grid-posn? ref-player-info?)]
  ; Get a player's color
  [player-info-color (-> player-info? avatar-color?)]
  ; Is the player currently on their treasure?
  [on-treasure? (-> ref-player-info? boolean?)]
  ; Is the player both at home and has visited their treasure?
  [visited-treasure-and-on-home? (-> ref-player-info? boolean?)]))

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

;; A PlayerInfo is one of:
;;     - PubPlayerInfo
;;     - RefPlayerInfo
;; interpretation: The information about a player is either public, or contains
;;                 details that only the referee and the player itself should know.

;; (struct GridPosn GridPosn (U GridPosn 'hidden) Boolean AvatarColor)
;; interpretation: The referee knows a player's current position, home position, treasure position,
;;                 whether or not they've visited their treasure position, and avatar color
(struct player-info [curr-pos home-pos treasure-pos visited-treasure? color] #:transparent)

;; PlayerInfo -> Boolean
;; Is this PlayerInfo a public player info?
(define (pub-player-info? plyr)
  (and (player-info? plyr)
       (equal? 'hidden (player-info-treasure-pos plyr))
       (equal? 'hidden (player-info-visited-treasure? plyr))))

;; PlayerInfo -> Boolean
;; Is this PlayerInfo a referee player info?
(define (ref-player-info? plyr)
  (and (player-info? plyr)
       (grid-posn? (player-info-treasure-pos plyr))))


;; GridPosn GridPosn 'hidden 'hidden AvatarColor -> PubPlayerInfo
;; Create a new pub-player-info
(define (pub-player-info-new curr-pos home-pos treasure-pos visited-treasure? color)
  (player-info curr-pos home-pos treasure-pos visited-treasure? color))


;; GridPosn GridPosn GridPosn Boolean AvatarColor -> RefPlayerInfo
;; Create a new ref-player-info
(define (ref-player-info-new curr-pos home-pos treasure-pos visited-treasure? color)
  (player-info curr-pos home-pos treasure-pos visited-treasure? color))

;; RefPlayerInfo -> PubPlayerInfo
;; Convert a referee payer info into a public player info
(define (ref-player-info->pub-player-info plyr)
  (player-info (player-info-curr-pos plyr)
               (player-info-home-pos plyr)
               'hidden
               'hidden
               (player-info-color plyr)))


;; RefPlayerInfo -> Boolean
;; Has this player visited their treasure and is currently on their home position?
(define (visited-treasure-and-on-home? plyr)
  (and (player-info-visited-treasure? plyr)
       (equal? (player-info-curr-pos plyr) (player-info-home-pos plyr))))


;; RefPlayerInfo -> Boolean
;; Returns true if the player is currently on their treasure
(define (on-treasure? plyr)
  (equal? (player-info-curr-pos plyr) (player-info-treasure-pos plyr)))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Player GridPosn -> Player
;; Move a player to the given gridposn
(define (player-info-move-to p pos)
  (struct-copy player-info p
               [curr-pos pos]
               [visited-treasure? (or
                                   (player-info-visited-treasure? p)
                                   (equal? pos (player-info-treasure-pos p)))]))
               

;; RefPlayerInfo GridPosn -> RefPlayerInfo
;; Move a player's treasure to the given gridposn
(define (change-treasure p new-treasure)
  (struct-copy player-info p [treasure-pos new-treasure]))


;; PlayerInfo GridPosn -> Boolean
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
     "purple"))
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

(module+ test
  (require (submod ".." examples)))

;; test hex-color-code?
(module+ test
  (check-true (hex-color-code? "A5B4C1"))
  (check-false (hex-color-code? "G5B4C1"))
  (check-false (hex-color-code? "A5B4C"))
  (check-false (hex-color-code? "a5B4C1"))
  (check-true (hex-color-code? "000000"))
  (check-true (hex-color-code? "B49E23")))


;; test ref-player-info->pub-player-info
(module+ test
  (check-equal? (ref-player-info->pub-player-info player-info9)
                (player-info (cons 5 5)
                             (cons 3 3)
                             'hidden
                             'hidden
                             "yellow")))