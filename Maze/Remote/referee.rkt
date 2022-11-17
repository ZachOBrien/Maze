#lang racket

;;; This module implements a remote proxy referee

;; --------------------------------------------------------------------
;; MODULE INTERFACE

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require json)
(require racket/list)
(require racket/class)

(require "tcp-conn.rkt")
(require (only-in "../Players/player.rkt" player-interface))
(require (submod "../Common/state.rkt" serialize))
(require (submod "../Common/board.rkt" serialize))
(require (submod "../Players/strategy.rkt" serialize))


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; A JsonVoid is a string "void"
;; interpretation: Signals a remote procedure call completed
(define json-void? (or/c "void"))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; (U Player ProxyPlayer) TcpConn -> ProxyReferee
;; Create a new proxy referee, which enables communication with a single player
(define (proxy-referee/new player tcp-conn)
  (new proxy-referee% [init-player player] [init-tcp-conn tcp-conn]))

(define proxy-referee%
  (class object%
    (init init-player init-tcp-conn)

    (define player init-player)
    (define tcp-conn init-tcp-conn)

    (super-new)

    ;; Handle incoming remote procedure calls
    (define/public (msg-handling-loop)
      (define msg (send tcp-conn receive-json))
      (match msg
        ; TODO: this is looking like it could be a macro to me
        [`["propose-board", msg] (check-valid-message msg propose-board-request-msg? propose-board-handler)]
        [`["setup",         msg] (check-valid-message msg setup-request-msg?         setup-handler)]
        [`["win",           msg] (check-valid-message msg win-request-msg?           win-handler)]
        [`["take-turn",     msg] (check-valid-message msg take-turn-request-msg?     take-turn-handler)]
        [`["get-goal",      msg] (check-valid-message msg get-goal-request-msg?      get-goal-handler)]))

    ;; JsonString (-> JsonString Boolean) (->JsonString JsonPlayerResponse)
    ;; Checks if message is valid as determined by the validator, and if so executes it.
    (define (check-valid-message msg validator handler)
      (if (validator msg)
          (send tcp-conn send-json (handler msg player))
          (error (string-append "receieved bad call from ref: " msg)))
      (msg-handling-loop)) ))


;; ProposeBoardRequestMsg Player -> JsonBoard
;; Handle a propose-board request
(define (propose-board-handler propose-board-request-msg player)
  (define min-num-rows (first propose-board-request-msg))
  (define min-num-cols (second propose-board-request-msg))
  (define proposed-board (send player propose-board min-num-rows min-num-cols))
  (board->json-board proposed-board))

;; SetupRequestMsg Player -> JsonVoid
;; Handle a setup request
(define (setup-handler setup-request-msg player)
  (define new-goal (json-coordinate->gridposn (second setup-request-msg)))
  (define plyr-state (json-public-state-and-goal-gridposn->player-state (first setup-request-msg) new-goal))
  (send player setup plyr-state new-goal)
  "void")

;; WinRequestMsg Player -> JsonVoid
;; Handle a win request
(define (win-handler win-request-msg player)
  (define win-boolean (first win-request-msg))
  (send player win win-boolean)
  "void")

;; TakeTurnRequestMsg Player -> JsonChoce
;; Handle a take-turn request
(define (take-turn-handler take-turn-request-msg player)
  (define player-goal (send player get-goal))
  (define plyr-state (json-public-state-and-goal-gridposn->player-state (first take-turn-request-msg) player-goal))
  (define player-action (send player take-turn plyr-state))
  (action->json-choice player-action))

;; GetGoalRequestMsg Player -> JsonCoordinate
;; Handle a get-goal request
(define (get-goal-handler get-goal-request-msg player)
  (define player-goal (send player get-goal))
  (gridposn->json-coordinate player-goal))

(module+ test
  (require rackunit)
  (require (submod "../Players/player.rkt" examples)))

; test propose-board handler
(module+ test
  (define proposed-board1 (propose-board-handler (list 7 7) player0))
  (check-true (json-board? proposed-board1)))

; test setup handler
; TODO: test setup handler


(define proxy-referee? (is-a?/c proxy-referee%))

(define propose-board-request-msg? (list/c natural-number/c natural-number/c))
(define setup-request-msg? (list/c json-public-state? json-coordinate?))
(define take-turn-request-msg? (list/c json-public-state?))
(define win-request-msg? (list/c boolean?))
(define get-goal-request-msg? empty?)
