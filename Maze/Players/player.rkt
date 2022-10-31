#lang racket

;;; This module provides data definitions and logic for a player

;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out
  [player? contract?]
  ; Create a new player
  [player-new (-> string? strategy? player?)]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "../Common/board.rkt")
(require "../Common/tile.rkt")
(require "../Common/state.rkt")
(require "strategy.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; String Strategy -> Player
;; Create a new player
(define (player-new name strat)
  (new player% [init-plyr-name name] [init-strategy strat]))



(define/contract player%
  (class/c
   [name (->m string?)]
   [propose-board (->m natural-number/c natural-number/c board?)]
   [setup (->m player-state? grid-posn? any)]
   [take-turn (->m player-state? action?)]
   [won (->m boolean? any)])
  (class object%
    (init init-plyr-name init-strategy)
    
    (define plyr-name init-plyr-name)
    (define strategy init-strategy)
    (define plyr-state0 #f)
    (define goal #f)
    (define won-game 'unknown)

    (super-new)

    ;; -> String
    ;; Get the player's name
    (define/public (name) plyr-name)

    ;; Natural Natural -> Board
    ;; Get a starting board
    (define/public (propose-board min-num-rows min-num-cols)
      (define board-size (max min-num-rows min-num-cols))
      (create-random-board (if (even? board-size)
                               (add1 board-size)
                               board-size)))
  
    ;; PlayerState GridPosn -> Any
    ;; Sets initial state and treasure position
    (define/public (setup plyr-state new-goal)
      (set! plyr-state0 plyr-state)
      (set! goal new-goal))

    ;; PlayerState -> Action
    ;; Chooses either to make a move or pass
    (define/public (take-turn plyr-state)
      (strategy plyr-state))

    ;; Boolean -> Any
    ;; Informs the player whether they won or lost
    (define/public (won status)
      (set! won-game status))
    
    (define/public (get-goal) goal)
    (define/public (get-plyr-state0) plyr-state0)
    (define/public (get-won-game) won-game)))

;; -> (Any -> Boolean)
;; Is an instance of player?
(define player?
  (is-a?/c player%))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (define player0 (new player% [init-plyr-name "bob"] [init-strategy riemann-strategy]))
  (define player1 (new player% [init-plyr-name "colin"] [init-strategy euclidean-strategy]))
  (define player2 (new player% [init-plyr-name "zach"] [init-strategy euclidean-strategy]))
  (define player3 (new player% [init-plyr-name "aoun"] [init-strategy riemann-strategy])))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "../Common/board.rkt" examples))
  (require (submod "../Common/state.rkt" examples))
  (require (submod "../Common/player-info.rkt" examples)))

;; test take-turn
(module+ test
  (check-equal? (send player0 take-turn player-state0) (move-new (cons 3 3) (shift-new 'right 2) 0))
  (check-equal? (send player0 take-turn player-state1) (move-new (cons 0 0) (shift-new 'right 6) 90))
  (check-equal? (send player0 take-turn player-state-nowhere-to-go) #f)
  (check-equal? (send player1 take-turn player-state0) (move-new (cons 3 3) (shift-new 'right 2) 0))
  (check-equal? (send player1 take-turn player-state1) (move-new (cons 5 2) (shift-new 'right 6) 90))
  (check-equal? (send player1 take-turn player-state-nowhere-to-go) #f)
  (check-equal? (send player2 take-turn player-state0) (move-new (cons 3 3) (shift-new 'right 2) 0))
  (check-equal? (send player2 take-turn player-state1) (move-new (cons 5 2) (shift-new 'right 6) 90))
  (check-equal? (send player2 take-turn player-state-nowhere-to-go) #f))

;; test name
(module+ test
  (check-equal? (send player0 name) "bob")
  (check-equal? (send player1 name) "colin")
  (check-equal? (send player2 name) "zach"))


;; test setup
(module+ test
  (check-equal? (send player0 get-goal) #f)
  (check-equal? (send player0 get-plyr-state0) #f)
  (send player0 setup player-state0 (cons 0 0))
  (check-equal? (send player0 get-goal) (cons 0 0))
  (check-equal? (send player0 get-plyr-state0) player-state0))

;; test won
(module+ test
  (check-equal? (send player0 get-won-game) 'unknown)
  (send player0 won #t)
  (check-true (send player0 get-won-game))
  (check-equal? (send player1 get-won-game) 'unknown)
  (send player1 won #f)
  (check-false (send player1 get-won-game))
  (check-equal? (send player2 get-won-game) 'unknown)
  (send player2 won #t)
  (check-true (send player2 get-won-game)))

;; test propose board
(module+ test
  (check-equal? (length (send player0 propose-board 7 7)) 7)
  (check-equal? (length (send player0 propose-board 6 7)) 7)
  (check-equal? (length (send player0 propose-board 6 6)) 7)
  (check-equal? (length (first (send player0 propose-board 7 7))) 7)
  (check-equal? (length (first (send player0 propose-board 6 7))) 7)
  (check-equal? (length (first (send player0 propose-board 6 6))) 7))