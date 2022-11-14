#lang racket

;;; This module provides data definitions and logic for a player

;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out
  [player? contract?]
  ; Create a new player
  [player-new (-> string? strategy? player?)]
  ; Create a new player which breaks on a call to name
  [player-bad-name-new (-> string? strategy? (is-a?/c player-bad-name%))]
  ; Create a new player which breaks on a call to setup
  [player-bad-setup-new (-> string? strategy? (is-a?/c player-bad-setup%))]
  ; Create a new player which breaks on a call to take-turn
  [player-bad-taketurn-new (-> string? strategy? (is-a?/c player-bad-taketurn%))]
  ; Create a new player which breaks on a call to won
  [player-bad-won-new (-> string? strategy? (is-a?/c player-bad-won%))]
  ; Create a new player which enters an infinite loop on the kth call to name
  [player-infloop-name-new (-> string? strategy? natural-number/c (is-a?/c player-infloop-name%))]
  ; Create a new player which enters an infinite loop on the kth call to setup
  [player-infloop-setup-new (-> string? strategy? natural-number/c (is-a?/c player-infloop-setup%))]))

#;(
  
  ; Create a new player which enters an infinite loop on the kth call to take-turn
  [player-infloop-taketurn-new (-> string? strategy? natural-number/c (is-a?/c player-infloop-taketurn%))]
  ; Create a new player which enters an infinite loop on the kth call to win
  [player-infloop-win-new (-> string? strategy? natural-number/c (is-a?/c player-infloop-win%))])
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/sandbox)

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

;; String Strategy -> PlayerBadName
;; Create a new player which breaks on a call to name
(define (player-bad-name-new name strat)
  (new player-bad-name% [init-plyr-name name] [init-strategy strat]))

;; String Strategy -> PlayerBadSetup
;; Create a new player which breaks on a call to setup
(define (player-bad-setup-new name strat)
  (new player-bad-setup% [init-plyr-name name] [init-strategy strat]))

;; String Strategy -> PlayerBadTaketurn
;; Create a new player which breaks on a call to take-turn
(define (player-bad-taketurn-new name strat)
  (new player-bad-taketurn% [init-plyr-name name] [init-strategy strat]))

;; String Strategy -> PlayerBadWon
;; Create a new player which breaks on a call to won
(define (player-bad-won-new name strat)
  (new player-bad-won% [init-plyr-name name] [init-strategy strat]))

;; String Strategy PositiveInteger -> PlayerBadWon
;; Create a new player which breaks on the `k`th call to `name`
(define (player-infloop-name-new name strat k)
  (new player-infloop-name% [init-plyr-name name] [init-strategy strat] [init-call-limit k]))

;; String Strategy PositiveInteger -> PlayerBadWon
;; Create a new player which breaks on the `k`th call to `setup`
(define (player-infloop-setup-new name strat k)
  (new player-infloop-setup% [init-plyr-name name] [init-strategy strat] [init-call-limit k]))

;; String Strategy PositiveInteger -> PlayerBadWon
;; Create a new player which breaks on the `k`th call to `take-turn`
(define (player-infloop-taketurn-new name strat k)
  (new player-infloop-taketurn% [init-plyr-name name] [init-strategy strat] [init-call-limit k]))

;; String Strategy PositiveInteger -> PlayerBadWon
;; Create a new player which breaks on the `k`th call to `won`
(define (player-infloop-won-new name strat k)
  (new player-infloop-won% [init-plyr-name name] [init-strategy strat] [init-call-limit k]))


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


;; A player which behaves almost normally, except raises an error
;; when `name` is called
(define player-bad-name%
  (class player%
    (super-new)
    (define (name)
      (/ 1 0))
    (override name)))


;; A player which behaves almost normally, except raises an error
;; when `setup` is called
(define player-bad-setup%
  (class player%
    (super-new)
    (define (setup plyr-state new-goal)
      (/ 1 0))
    (override setup)))


;; A player which behaves almost normally, except raises an error
;; when `take-turn` is called
(define player-bad-taketurn%
  (class player%
    (super-new)
    (define (take-turn plyr-state)
      (/ 1 0))
    (override take-turn)))


;; A player which behaves almost normally, except raises an error
;; when `won` is called
(define player-bad-won%
  (class player%
    (super-new)
    (define (won status)
      (/ 1 0))
    (override won)))

    
;; A player which behaves almost normally, except enters an infinite loop on the
;; kth call to `name`
(define player-infloop-name%
  (class player%
    (init init-call-limit)

    (define call-limit init-call-limit)
    (define call-count 0)
    
    (super-new)
    
    (define (name)
      (set! call-count (add1 call-count))
      (if (= call-count call-limit)
          (loop)
          (super name)))
    (override name)))


;; A player which behaves almost normally, except enters an infinite loop on the
;; kth call to `setup`
(define player-infloop-setup%
  (class player%
    (init init-call-limit)

    (define call-limit init-call-limit)
    (define call-count 0)
    
    (super-new)
    
    (define (setup plyr-state new-goal)
      (set! call-count (add1 call-count))
      (if (= call-count call-limit)
          (loop)
          (super setup plyr-state new-goal)))
    (override setup)))


;; A player which behaves almost normally, except enters an infinite loop on the
;; kth call to `take-turn`
(define player-infloop-taketurn%
  (class player%
    (init init-call-limit)

    (define call-limit init-call-limit)
    (define call-count 0)
    
    (super-new)
    
    (define (take-turn plyr-state)
      (set! call-count (add1 call-count))
      (if (= call-count call-limit)
          (loop)
          (super take-turn plyr-state)))
    (override take-turn)))


;; A player which behaves almost normally, except enters an infinite loop on the
;; kth call to `take-turn`
(define player-infloop-won%
  (class player%
    (init init-call-limit)

    (define call-limit init-call-limit)
    (define call-count 0)
    
    (super-new)
    
    (define (won status)
      (set! call-count (add1 call-count))
      (if (= call-count call-limit)
          (loop)
          (super won status)))
    (override won)))


; An infinite loop
(define (loop) (loop))

  
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
  (define player3 (new player% [init-plyr-name "aoun"] [init-strategy riemann-strategy]))
  (define player-bad-name (new player-bad-name% [init-plyr-name "bob"] [init-strategy riemann-strategy]))
  (define player-bad-setup (new player-bad-setup% [init-plyr-name "bob"] [init-strategy riemann-strategy]))
  (define player-bad-taketurn (new player-bad-taketurn% [init-plyr-name "bob"] [init-strategy riemann-strategy]))
  (define player-bad-won (new player-bad-won% [init-plyr-name "bob"] [init-strategy riemann-strategy])))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "../Common/board.rkt" examples))
  (require (submod "../Common/state.rkt" examples))
  (require (submod "../Common/player-info.rkt" examples)))

;; test player-new
(module+ test
  (check-equal? "bob" (send (player-new "bob" riemann-strategy) name)))

;; test take-turn
(module+ test
  (check-equal? (send player0 take-turn player-state0) (move-new (cons 3 3) (shift-new 'left 0) 0))
  (check-equal? (send player0 take-turn player-state1) (move-new (cons 0 4) (shift-new 'down 6) 90))
  (check-equal? (send player0 take-turn player-state-nowhere-to-go) #f)
  (check-equal? (send player1 take-turn player-state0) (move-new (cons 3 3) (shift-new 'left 0) 0))
  (check-equal? (send player1 take-turn player-state1) (move-new (cons 6 1) (shift-new 'right 6) 90))
  (check-equal? (send player1 take-turn player-state-nowhere-to-go) #f)
  (check-equal? (send player2 take-turn player-state0) (move-new (cons 3 3) (shift-new 'left 0) 0))
  (check-equal? (send player2 take-turn player-state1) (move-new (cons 6 1) (shift-new 'right 6) 90))
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


;; test player-bad-name
(module+ test
  (test-case
   "name call to bad player raises error"
   (define bad-player (player-bad-name-new "azamat" riemann-strategy))
   (check-exn exn:fail? (thunk (send bad-player name)))))

;; test player-bad-setup
(module+ test
  (test-case
   "setup call to bad player raises error"
   (define bad-player (player-bad-setup-new "azamat" riemann-strategy))
   (check-exn exn:fail? (thunk (send bad-player setup player-state0 (cons 0 0))))))

;; test player-bad-take-turn
(module+ test
  (test-case
   "take-turn call to bad player raises error"
   (define bad-player (player-bad-taketurn-new "azamat" riemann-strategy))
   (check-exn exn:fail? (thunk (send bad-player take-turn player-state0)))))

;; test player-bad-won
(module+ test
  (test-case
   "won call to bad player raises error"
   (define bad-player (player-bad-won-new "azamat" riemann-strategy))
   (check-exn exn:fail? (thunk (send bad-player won #f)))))


;; test player-infloop-name
(module+ test
  (test-case
   "3rd call to `name` sends bad player into an infinite loop"
   (define bad-player (player-infloop-name-new "borat" riemann-strategy 3))
   (check-equal? "borat" (call-with-limits 1 #f (thunk (send bad-player name))))
   (check-equal? "borat" (call-with-limits 1 #f (thunk (send bad-player name))))
   (check-exn exn:fail? (thunk (call-with-limits 1 #f (thunk (send bad-player name)))))
   (check-equal? "borat" (call-with-limits 1 #f (thunk (send bad-player name))))))

; test player-infloop-setup
(module+ test
  (test-case
   "2nd call to `setup` sends bad player into an infinite loop"
   (define bad-player (player-infloop-setup-new "borat" riemann-strategy 2))
   (call-with-limits 1 #f (thunk (send bad-player setup player-state0 (cons 0 0))))
   (check-exn exn:fail? (thunk (call-with-limits 1 #f (thunk (send bad-player setup player-state0 (cons 0 0))))))
   (call-with-limits 1 #f (thunk (send bad-player setup player-state0 (cons 0 0))))))


; test player-infloop-taketurn
(module+ test
  (test-case
   "2nd call to `take-turn` sends bad player into an infinite loop"
   (define bad-player (player-infloop-taketurn-new "borat" riemann-strategy 2))
   (define _1 (call-with-limits 1 #f (thunk (send bad-player take-turn player-state0))))
   (check-exn exn:fail? (thunk (call-with-limits 1 #f (thunk (send bad-player take-turn player-state0)))))))


; test player-infloop-won
(module+ test
  (test-case
   "3rd call to `won` sends bad player into an infinite loop"
   (define bad-player (player-infloop-won-new "borat" riemann-strategy 3))
   (define _1 (call-with-limits 1 #f (thunk (send bad-player won #f))))
   (define _2 (call-with-limits 1 #f (thunk (send bad-player won #f))))
   (check-exn exn:fail? (thunk (call-with-limits 1 #f (thunk (send bad-player won #f)))))))
    