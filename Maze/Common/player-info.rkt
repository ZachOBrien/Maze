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
  [hex-color-code?  contract?]
  ; Create a new public player info
  [pub-player-info-new (-> grid-posn? grid-posn? avatar-color? player-info?)]
  ; Create a new referee player info
  [ref-player-info-new (-> grid-posn? grid-posn? grid-posn? (listof grid-posn?) boolean? avatar-color? player-info?)]
  ; Convert a referee player info into a public player info
  [ref-player-info->pub-player-info (-> ref-player-info? pub-player-info?)]
  ; Convert a public player info into a referee player info
  [pub-player-info->ref-player-info (-> pub-player-info? grid-posn? (listof grid-posn?) boolean? ref-player-info?)]
  ; Get a player's goal position
  [player-info-goal-pos (-> ref-player-info? grid-posn?)]
  ; Get a player's home position
  [player-info-home-pos (-> player-info? grid-posn?)]
  ; Get a player's current position
  [player-info-curr-pos (-> player-info? grid-posn?)]
  ; Get the list of goals a player visited
  [player-info-goals-visited (-> ref-player-info? (listof grid-posn?))]
  ; Get the number of goals this player has visited
  [num-goals-visited (-> ref-player-info? natural-number/c)]
  ; Check if a player is on a position
  [player-info-on-pos? (-> player-info? grid-posn? boolean?)]
  ; Is this player finished chasing goals and working back home?
  [player-info-going-home? (-> ref-player-info? boolean?)]
  ; Set whether this player has finished chasing goals and should work toward home
  [set-going-home (-> ref-player-info? boolean? ref-player-info?)]
  ; Check if a player is on their home position
  [player-info-on-home? (-> player-info? boolean?)]
  ; Move a player to the given gridposn
  [player-info-move-to (-> player-info? grid-posn? player-info?)]
  ; Move a player's goal to the given gridposn
  [replace-dummy-goal (-> ref-player-info? grid-posn? ref-player-info?)]
  ; Inform the player of whether they should pursue the next goal or go home
  [receive-next-goal (-> ref-player-info? (or/c #f grid-posn?) ref-player-info?)]
  ; Get a player's color
  [player-info-color (-> player-info? avatar-color?)]
  ; Is the player currently on their treasure?
  [on-treasure? (-> ref-player-info? boolean?)]
  ; Determine the distance of a player from their objective. If they have not found their treasure,
  ; that is their objective. If they have found their treasure, getting home is their objective.
  [distance-from-objective (-> ref-player-info? (-> grid-posn? grid-posn? (not/c negative?)) (not/c negative?))]
  ; Get all players which have visited the maximum number of goals
  [all-with-max-num-goals-visited (-> (listof ref-player-info?) (listof ref-player-info?))]
  ; Get all players which are minimum distance from their objective
  [players-min-distance-from-objective (-> (listof ref-player-info?) (listof ref-player-info?))]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require rackunit)

(require "board.rkt")
(require "math.rkt")

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

;; (struct GridPosn GridPosn (U GridPosn 'hidden) (U [Listof GridPosn] 'hidden) (U Boolean 'hidden) AvatarColor)
;; interpretation: The referee knows a player's current position, home position, position they're working toward,
;;                 whether or not they've visited their treasure position, and avatar color
(struct player-info [curr-pos home-pos goal-pos goals-visited going-home? color] #:transparent)

;; PlayerInfo -> Boolean
;; Is this PlayerInfo a public player info?
(define (pub-player-info? plyr)
  (and (player-info? plyr)
       (equal? 'hidden (player-info-goal-pos plyr))
       (equal? 'hidden (player-info-goals-visited plyr))
       (equal? 'hidden (player-info-going-home? plyr))))

;; PlayerInfo -> Boolean
;; Is this PlayerInfo a referee player info?
(define (ref-player-info? plyr)
  (and (player-info? plyr)
       (grid-posn? (player-info-goal-pos plyr))
       ((listof grid-posn?) (player-info-goals-visited plyr))
       (boolean? (player-info-going-home? plyr))))


;; GridPosn GridPosn AvatarColor -> PubPlayerInfo
;; Create a new pub-player-info
(define (pub-player-info-new curr-pos home-pos color)
  (player-info curr-pos home-pos 'hidden 'hidden 'hidden color))

;; GridPosn GridPosn GridPosn [Listof GridPosn] Boolean AvatarColor -> RefPlayerInfo
;; Create a new ref-player-info
(define (ref-player-info-new curr-pos home-pos goal-pos goals-visited going-home? color)
  (player-info curr-pos home-pos goal-pos goals-visited going-home? color))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Player GridPosn -> Player
;; Move a player to the given gridposn
(define (player-info-move-to p pos)
  (struct-copy player-info p
               [curr-pos pos]))
               

;; RefPlayerInfo GridPosn -> RefPlayerInfo
;; Move a player's goal to the given gridposn
(define (replace-dummy-goal p new-goal-pos)
  (struct-copy player-info p [goal-pos new-goal-pos]))

;; RefPlayerInfo (U #f GridPosn) -> RefPlayerInfo
;; Inform the player of whether they should pursue the next goal or go home
;; If goal is false, the state has run out of goals and the player should go home
(define (receive-next-goal plyr goal)
  (if goal
      (struct-copy player-info plyr
                   [goal-pos goal]
                   [goals-visited (cons (player-info-goal-pos plyr) (player-info-goals-visited plyr))])
      (struct-copy player-info plyr
                   [goal-pos (player-info-home-pos plyr)]
                   [goals-visited (cons (player-info-goal-pos plyr) (player-info-goals-visited plyr))]
                   [going-home? #t])))
                   

;; PlayerInfo GridPosn -> Boolean
;; Returns True if the player is on the given position
(define (player-info-on-pos? p pos)
  (equal? (player-info-curr-pos p) pos))

;; PlayerInfo -> Boolean
;; Returns True if the player is on the their home position
(define (player-info-on-home? plyr-info)
  (equal? (player-info-curr-pos plyr-info) (player-info-home-pos plyr-info)))

;; RefPlayerInfo -> PubPlayerInfo
;; Convert a referee payer info into a public player info
(define (ref-player-info->pub-player-info plyr-info)
  (struct-copy player-info plyr-info
               [goal-pos 'hidden]
               [goals-visited 'hidden]
               [going-home? 'hidden]))

;; PubPlayerInfo GridPosn Boolean -> RefPlayerInfo
;; Convert a public player info into a referee player info
(define (pub-player-info->ref-player-info plyr-info goal-pos goals-visited going-home?)
  (struct-copy player-info plyr-info
               [goal-pos goal-pos]
               [goals-visited goals-visited]
               [going-home? going-home?]))

;; RefPlayerInfo -> Boolean
;; Returns true if the player is currently on their treasure
(define (on-treasure? plyr)
  (equal? (player-info-curr-pos plyr) (player-info-goal-pos plyr)))

;; RefPlayerInfo Boolean -> RefPlayerInfo
;; Sets the value of whether this player should be working toward home
(define (set-going-home plyr-info going-home?)
  (struct-copy player-info plyr-info
               [going-home? going-home?]))

;; RefPlayerInfo (-> GridPosn GridPosn PositiveReal) -> PositiveReal
;; Determine the distance of a player from the goal it is working toward
(define (distance-from-objective plyr-info dist-func)
  (dist-func (player-info-curr-pos plyr-info) (player-info-goal-pos plyr-info)))

;; RefPlayerInfo -> Natural
;; How many goals has this player already visited?
(define (num-goals-visited plyr-info)
  (length (player-info-goals-visited plyr-info)))

;; [NonEmptyListof PlayerInfo] -> Natural
;; Find the maximum number of goals any player visited
(define (max-num-goals-visited plyr-infos)
  (apply max (map num-goals-visited plyr-infos)))

;; [Listof PlayerInfo] -> [Listof PlayerInfo]
;; Get all players which have visited the maximum number of goals
(define (all-with-max-num-goals-visited plyr-infos)
  (cond
    [(empty? plyr-infos) empty]
    [else (let ([max-goals-visited (max-num-goals-visited plyr-infos)])
            (filter (λ (plyr-info) (= (num-goals-visited plyr-info) max-goals-visited)) plyr-infos))]))

;; [Listof PlayerInfo] -> [Listof Player]
;; Get all players which are minimum distance from their objective
(define (players-min-distance-from-objective plyr-infos)
  (cond
    [(empty? plyr-infos) empty]
    [(let* ([distances (map (curryr distance-from-objective euclidean-dist) plyr-infos)]
            [min-dist (apply min distances)])
       (filter (λ (plyr) (= (distance-from-objective plyr euclidean-dist) min-dist)) plyr-infos))]))

(module+ serialize
  (require json)
  (require (submod "board.rkt" serialize))
  (provide
   (contract-out
    [json-public-player-info? contract?]
    [json-referee-player-info? contract?]
    ; Make a referee player into a JsonRefPlayerInfo
    [referee-player-info->json-referee-player-info (-> ref-player-info? json-referee-player-info?)]
    ; Make a public player into a JsonPubPlayerInfo
    [player-info->json-public-player-info (-> player-info? json-public-player-info?)]
    ; Create a RefPlayerInfo from a JsonRefPlayerInfo
    [json-referee-player-info->referee-player-info (-> json-referee-player-info? ref-player-info?)]
    ; Create a PubPlayerInfo from a JsonPubPlayerInfo
    [json-public-player-info->public-player-info (-> json-public-player-info? pub-player-info?)]))

  ;; Any -> Boolean
  ;; Is this object a hashtable JSON representation of a PublicPlayerInfo
  (define (json-public-player-info? ht)
    (and (hash? ht)
         (hash-has-key? ht 'current)
         (hash-has-key? ht 'home)
         (hash-has-key? ht 'color)
         (json-coordinate? (hash-ref ht 'current))
         (json-coordinate? (hash-ref ht 'home))
         (avatar-color?    (hash-ref ht 'color))))

  (module+ test
    (check-true (json-public-player-info? (hash 'current (hash 'row# 1 'column# 1)
                                                'home (hash 'row# 3 'column# 3)
                                                'color "purple")))
    (check-false (json-public-player-info? (hash 'current (hash 'row# 1 'column# 1)
                                                 'home (hash 'row# 3 'column# 3)
                                                 'color "purpl"))))

  ;; Any -> Boolean
  ;; Is this object a hashtable JSON representation of a RefPlayerInfo
  (define (json-referee-player-info? ht)
    (and (json-public-player-info? ht)
         (hash-has-key? ht 'goto)
         (json-coordinate? (hash-ref ht 'goto))))

  (module+ test
    (check-true (json-referee-player-info? (hash 'current (hash 'row# 1 'column# 1)
                                                 'home (hash 'row# 3 'column# 3)
                                                 'goto (hash 'row# 5 'column# 7)
                                                 'color "purple")))
    (check-false (json-referee-player-info? (hash 'current (hash 'row# 1 'column# 1)
                                                  'home (hash 'row# 3 'column# 3)
                                                  'color "purple"))))

  ;; RefPlayerInfo -> JsonRefPlayerInfo
  ;; Make a referee player into a RefPlayerInfo
  (define (referee-player-info->json-referee-player-info ref-plyr)
    (hash 'current (gridposn->json-coordinate (player-info-curr-pos ref-plyr))
          'home    (gridposn->json-coordinate (player-info-home-pos ref-plyr))
          'goto    (gridposn->json-coordinate (player-info-goal-pos ref-plyr))
          'color (player-info-color ref-plyr)))


  (module+ test
    (check-equal? (referee-player-info->json-referee-player-info (ref-player-info-new (cons 0 0) (cons 2 2) (cons 1 1) empty #f "blue"))
                  (hash 'current (hash 'row# 0 'column# 0)
                        'home (hash 'row# 2 'column# 2)
                        'goto (hash 'row# 1 'column# 1)
                        'color "blue"))
                  
    (check-equal? (referee-player-info->json-referee-player-info (ref-player-info-new (cons 6 1) (cons 3 4) (cons 5 1) empty #f "red"))
                  (hash 'current (hash 'row# 6 'column# 1)
                        'home (hash 'row# 3 'column# 4)
                        'goto (hash 'row# 5 'column# 1)
                        'color "red")))
  

  ;; PubPlayerInfo -> JsonPubPlayerInfo
  ;; Make a public player into a PubPlayerInfo
  (define (player-info->json-public-player-info pub-plyr)
    (hash 'current (gridposn->json-coordinate (player-info-curr-pos pub-plyr))
          'home    (gridposn->json-coordinate (player-info-home-pos pub-plyr))
          'color (player-info-color pub-plyr)))

  (module+ test
    (check-equal? (player-info->json-public-player-info (pub-player-info-new (cons 0 0) (cons 2 2) "blue"))
                  (hash 'current (hash 'row# 0 'column# 0)
                        'home (hash 'row# 2 'column# 2)
                        'color "blue"))
                  
    (check-equal? (player-info->json-public-player-info (pub-player-info-new (cons 6 1) (cons 3 4) "red"))
                  (hash 'current (hash 'row# 6 'column# 1)
                        'home (hash 'row# 3 'column# 4)
                        'color "red")))
                  

  ;; JsonPubPlayerInfo -> PubPlayerInfo
  ;; Create a PubPlayerInfo from a JsonPubPlayerInfo
  (define (json-public-player-info->public-player-info ht)
    (pub-player-info-new (json-coordinate->gridposn (hash-ref ht 'current))
                         (json-coordinate->gridposn (hash-ref ht 'home))
                         (hash-ref ht 'color)))

  (module+ test
    (check-equal? (json-public-player-info->public-player-info (hash 'current (hash 'row# 0 'column# 0)
                                                                     'home (hash 'row# 2 'column# 2)
                                                                     'color "blue"))
                  (pub-player-info-new (cons 0 0) (cons 2 2) "blue"))
    (check-equal? (json-public-player-info->public-player-info (hash 'current (hash 'row# 6 'column# 1)
                                                                     'home (hash 'row# 3 'column# 4)
                                                                     'color "red"))
                  (pub-player-info-new (cons 6 1) (cons 3 4) "red")))

  ;; JsonRefPlayerInfo -> PlayerInfo
  ;; Create a RefPlayerInfo from a JsonRefPlayerInfo
  (define (json-referee-player-info->referee-player-info ht)
    (ref-player-info-new (json-coordinate->gridposn (hash-ref ht 'current))
                         (json-coordinate->gridposn (hash-ref ht 'home))
                         (json-coordinate->gridposn (hash-ref ht 'goto))
                         empty
                         #f
                         (hash-ref ht 'color)))

  (module+ examples
    (provide (all-defined-out))
    (define example-player-infos1
      (list (hash 'current (hash 'row# 0 'column# 0) 'home (hash 'row# 6 'column# 6) 'color "blue")
            (hash 'current (hash 'row# 1 'column# 1) 'home (hash 'row# 5 'column# 5) 'color "red")
            (hash 'current (hash 'row# 2 'column# 2) 'home (hash 'row# 4 'column# 4) 'color "green")
            (hash 'current (hash 'row# 3 'column# 3) 'home (hash 'row# 3 'column# 3) 'color "yellow")))
    (define expected-player-infos1
      (list (pub-player-info-new (cons 0 0) (cons 6 6) "blue")
            (pub-player-info-new (cons 1 1) (cons 5 5) "red")
            (pub-player-info-new (cons 2 2) (cons 4 4) "green")
            (pub-player-info-new (cons 3 3) (cons 3 3) "yellow"))))

  (module+ test
    (check-equal? (json-public-player-info->public-player-info
                   (hash 'current (hash 'row# 0 'column# 0) 'home (hash 'row# 6 'column# 6) 'color "blue"))
                  (pub-player-info-new (cons 0 0) (cons 6 6) "blue")))

  (module+ test
    (check-equal? (json-referee-player-info->referee-player-info (hash 'current (hash 'row# 0 'column# 0)
                                                                       'home (hash 'row# 2 'column# 2)
                                                                       'goto (hash 'row# 1 'column# 1)
                                                                       'color "blue"))
                  (ref-player-info-new (cons 0 0) (cons 2 2) (cons 1 1) empty #f "blue"))
    (check-equal? (json-referee-player-info->referee-player-info (hash 'current (hash 'row# 6 'column# 1)
                                                                       'home (hash 'row# 3 'column# 4)
                                                                       'goto (hash 'row# 5 'column# 1)
                                                                       'color "red"))
                  (ref-player-info-new (cons 6 1) (cons 3 4) (cons 5 1) empty #f "red"))))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (define player-info0
    (player-info
     (cons 0 0)
     (cons 6 6)
     (cons 5 1)
     empty
     #f
     "blue"))
  (define player-info1
    (player-info
     (cons 1 1)
     (cons 5 5)
     (cons 1 1)
     empty
     #f
     "purple"))
  (define player-info2
    (player-info
     (cons 2 2)
     (cons 4 4)
     (cons 3 3)
     empty
     #f
     "green"))
  (define player-info3
    (player-info
     (cons 3 3)
     (cons 3 3)
     (cons 1 3)
     empty
     #f
     "yellow"))
  (define player-info4
    (player-info
     (cons 4 4)
     (cons 2 2)
     (cons 5 5)
     empty
     #f
     "black"))
  (define player-info5
    (player-info
     (cons 0 6)
     (cons 5 5)
     (cons 1 5)
     empty
     #f
     "red"))
  (define player-info6
    (player-info
     (cons 6 0)
     (cons 4 4)
     (cons 3 1)
     empty
     #f
     "pink"))
  (define player-info7
    (player-info
     (cons 6 6)
     (cons 3 3)
     (cons 5 3)
     empty
     #f
     "white"))
  (define player-info8
    (player-info
     (cons 5 5)
     (cons 3 3)
     (cons 5 1)
     empty
     #f
     "orange"))
  (define player-info9
    (player-info
     (cons 5 5)
     (cons 3 3)
     (cons 5 5)
     (list (cons 5 1))
     #f
     "A5B4C1")) ; ice blue gray ish
  (define public-player-info0
    (player-info
     (cons 0 0)
     (cons 6 6)
     'hidden
     'hidden
     'hidden
     "blue"))
  (define public-player-info1
    (player-info
     (cons 1 1)
     (cons 5 5)
     'hidden
     'hidden
     'hidden
     "purple"))
  (define public-player-info2
    (player-info
     (cons 2 2)
     (cons 4 4)
     'hidden
     'hidden
     'hidden
     "green"))
  (define ref-player-info10
    (ref-player-info-new
     (cons 6 6)
     (cons 5 5)
     (cons 1 1)
     empty
     #f
     "A1A1A1"))
  (define ref-player-info11
    (ref-player-info-new
     (cons 1 1)
     (cons 1 1)
     (cons 5 5)
     empty
     #f
     "B2B2B2")) )

(module+ test
  (require (submod ".." examples))
  (require (submod ".." serialize))
  (require (submod ".." serialize test))
  (require "math.rkt"))

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
                             'hidden
                             "A5B4C1")))

;; test pub-player-info->ref-player-info
(module+ test
  (check-equal? (pub-player-info->ref-player-info (player-info (cons 5 5)
                                                               (cons 3 3)
                                                               'hidden
                                                               'hidden
                                                               'hidden
                                                               "A5B4C1")
                                                  (cons 5 5)
                                                  (list (cons 5 1))
                                                  #f)
                player-info9))

;; Test player-info-move-to
(module+ test
  (check-equal? (player-info-move-to player-info0 (cons 3 3))
                (ref-player-info-new
                 (cons 3 3)
                 (cons 6 6)
                 (cons 5 1)
                 empty
                 #f
                 "blue"))
  (check-equal? (player-info-move-to player-info0 (cons 6 6))
                (ref-player-info-new
                 (cons 6 6)
                 (cons 6 6)
                 (cons 5 1)
                 empty
                 #f
                 "blue")))

;; test referee-player-info->hash
(module+ test
  (check-equal? (referee-player-info->json-referee-player-info player-info0)
                (hash 'current (hash 'row# 0 'column# 0)
                      'goto (hash 'row# 5 'column# 1)
                      'home (hash 'row# 6 'column# 6)
                      'color "blue"))
  (check-equal? (referee-player-info->json-referee-player-info player-info1)
                (hash 'current (hash 'row# 1 'column# 1)
                      'goto (hash 'row# 1 'column# 1)
                      'home (hash 'row# 5 'column# 5)
                      'color "purple"))
  (check-equal? (referee-player-info->json-referee-player-info player-info2)
                (hash 'current (hash 'row# 2 'column# 2)
                      'goto (hash 'row# 3 'column# 3)
                      'home (hash 'row# 4 'column# 4)
                      'color "green")))

;; test public-player-info->hash
(module+ test
  (check-equal? (player-info->json-public-player-info public-player-info0)
                (hash 'current (hash 'row# 0 'column# 0)
                      'home (hash 'row# 6 'column# 6)
                      'color "blue"))
  (check-equal? (player-info->json-public-player-info public-player-info1)
                (hash 'current (hash 'row# 1 'column# 1)
                      'home (hash 'row# 5 'column# 5)
                      'color "purple"))
  (check-equal? (player-info->json-public-player-info public-player-info2)
                (hash 'current (hash 'row# 2 'column# 2)
                      'home (hash 'row# 4 'column# 4)
                      'color "green")))

;; test distance-from-objective
(module+ test
  (check-equal? (distance-from-objective (player-info (cons 2 1) (cons 2 2) (cons 5 1) empty #f "blue") euclidean-dist) 3)
  (check-equal? (distance-from-objective (player-info (cons 2 1) (cons 2 2) (cons 2 2) (list (cons 5 1)) #t "blue") euclidean-dist) 1))

;; test num-goals-visited
(module+ test
  (check-equal? 0 (num-goals-visited player-info1))
  (check-equal? 1 (num-goals-visited (receive-next-goal player-info1 (cons 3 3)))))

;; test receive-next-goal
(module+ test
  (check-equal? (receive-next-goal player-info1 (cons 3 3))
                (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1)) #f "purple"))
  (check-equal? (receive-next-goal player-info1 #f)
                (player-info (cons 1 1) (cons 5 5) (cons 5 5) (list (cons 1 1)) #t "purple")))

;; test max-num-goals-visited
(module+ test
  (check-equal? (max-num-goals-visited (list (player-info (cons 1 1) (cons 5 5) (cons 3 3) empty #f "purple"))) 0)
  (check-equal? (max-num-goals-visited (list (player-info (cons 1 3) (cons 1 5) (cons 5 2) empty #f "purple")
                                             (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1)) #f "purple")))
                1)
  (check-equal? (max-num-goals-visited (list (player-info (cons 1 3) (cons 1 5) (cons 5 2) empty #f "purple")
                                             (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1)) #f "purple")
                                             (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1) (cons 3 3)) #f "purple")
                                             (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1) (cons 5 5)) #f "purple")))
                2))

;; test all-with-shortest-distance-to-objective
(module+ test
  (check-equal? (all-with-max-num-goals-visited empty) empty)
  (check-equal? (all-with-max-num-goals-visited (list (player-info (cons 1 3) (cons 1 5) (cons 5 2) empty #f "purple")
                                                      (player-info (cons 1 1) (cons 5 5) (cons 2 2) (list (cons 1 1)) #f "purple")))
                (list (player-info (cons 1 1) (cons 5 5) (cons 2 2) (list (cons 1 1)) #f "purple")))
  (check-equal? (all-with-max-num-goals-visited (list (player-info (cons 1 3) (cons 1 5) (cons 5 2) empty #f "purple")
                                                      (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1)) #f "purple")
                                                      (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1) (cons 3 3)) #f "purple")
                                                      (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1) (cons 5 5)) #f "purple")))
                (list (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1) (cons 3 3)) #f "purple")
                      (player-info (cons 1 1) (cons 5 5) (cons 3 3) (list (cons 1 1) (cons 5 5)) #f "purple"))))

(module+ test
  (check-equal? (players-min-distance-from-objective empty) empty)
  (check-equal? (players-min-distance-from-objective (list (player-info (cons 1 3) (cons 1 5) (cons 5 7) empty #f "purple")
                                                           (player-info (cons 1 3) (cons 5 3) (cons 3 3) (list (cons 1 1)) #f "purple")
                                                           (player-info (cons 1 1) (cons 1 1) (cons 7 7) (list (cons 1 1) (cons 3 3)) #f "purple")
                                                           (player-info (cons 3 1) (cons 1 5) (cons 3 3) (list (cons 1 1) (cons 5 5)) #f "purple")))
                (list (player-info (cons 1 3) (cons 5 3) (cons 3 3) (list (cons 1 1)) #f "purple")
                      (player-info (cons 3 1) (cons 1 5) (cons 3 3) (list (cons 1 1) (cons 5 5)) #f "purple"))))
