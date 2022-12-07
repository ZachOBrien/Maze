#lang racket/base

;;; This module implements a remote proxy referee

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  ; Create a new proxy referee
  [proxy-referee-new (-> (or/c player? proxy-player?) tcp-conn? proxy-referee?)]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require json)
(require racket/list)
(require racket/class)
(require racket/match)

(require "tcp-conn.rkt")
(require (only-in "../Players/player.rkt" player-interface player?))
(require (only-in "../Remote/player.rkt" proxy-player?))
(require (submod "../Common/state.rkt" serialize))
(require (submod "../Common/board.rkt" serialize))
(require (submod "../Players/strategy.rkt" serialize))

(module+ examples
  (provide (all-defined-out))
  (require (submod "../Players/player.rkt" examples))
  (require (submod "../Common/state.rkt" examples)))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "../Players/player.rkt" examples)))


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; A JsonVoid is a string "void"
;; interpretation: Signals a remote procedure call completed
(define json-void? (or/c "void"))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; (U Player ProxyPlayer) TcpConn -> ProxyReferee
;; Create a new proxy referee, which enables communication with a single player
(define (proxy-referee-new player tcp-conn)
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
        [`["setup",     args] (handle-rpc valid-setup-args? setup-handler msg)]
        [`["win",       args] (handle-rpc valid-win-args? win-handler msg)]
        [`["take-turn", args] (handle-rpc valid-take-turn-args? take-turn-handler msg)]
        [`["get-goal",  args] (handle-rpc valid-get-goal-args? get-goal-handler msg)]
        [#f "Server closed connection"]))

    ;; (-> [List String [Listof Jsexpr]] Boolean) (-> [Listof Jsexpr] JsonPlayerResponse) [List String [Listof Jsexpr]] -> Void
    ;; Checks if message is valid as determined by the validator, and if so executes it
    (define (handle-rpc validator handler msg)
      (define rpc-args (second msg))
      (if (validator rpc-args)
          (send tcp-conn send-json (handler rpc-args player))
          (error (string-append "receieved bad call from ref: " (jsexpr->string msg))))
      (msg-handling-loop)) ))


(define proxy-referee? (is-a?/c proxy-referee%))


;; SetupRequestMsg Player -> JsonVoid
;; Handle a setup request
(define (setup-handler setup-request-args player)
  (define new-goal (json-coordinate->gridposn (second setup-request-args)))
  (define plyr-state (if (first setup-request-args) (json-public-state-and-goal-gridposn->player-state (first setup-request-args) new-goal) #f))
  (send player setup plyr-state new-goal)
  "void")

;; WinRequestMsg Player -> JsonVoid
;; Handle a win request
(define (win-handler win-request-args player)
  (define win-boolean (first win-request-args))
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

;; [List Jsexpr Jsexpr] -> Boolean
;; Are these two JSON expressions valid arguments for the setup rpc?
(define valid-setup-args? (list/c (or/c #f json-public-state?) json-coordinate?))

(module+ test
  (check-true (valid-setup-args? (list #f (hash 'column# 3 'row# 2))))
  (check-false (valid-setup-args? empty))
  (check-false (valid-setup-args? (list #f))))

;; [List Jsexpr] -> Boolean
;; Is this single JSON expression a valid argument list for the win rpc?
(define valid-win-args? (list/c boolean?))

(module+ test
  (check-true (valid-win-args? (list #f)))
  (check-true (valid-win-args? (list #t)))
  (check-false (valid-win-args? #f))
  (check-false (valid-win-args? #t)))

;; [List Jsexpr] -> Boolean
;; Is this single JSON expression a valid argument list for the take-turn rpc?
(define valid-take-turn-args? (list/c json-public-state?))

;; EmptyList -> Boolean
;; Returns true if given an empty list, the only valid input for get-goal rpc
(define valid-get-goal-args? (and/c list? empty?))

(module+ test
  (check-true (valid-get-goal-args? '()))
  (check-false (valid-get-goal-args? (list 1))))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (define example-setup-rpc-arglist (jsexpr->string (list "setup"
                                                          (list (player-state->json-public-state player-state0)
                                                                (gridposn->json-coordinate (cons 3 3))))))
  (define example-take-turn-rpc-arglist (jsexpr->string (list "take-turn"
                                                              (list (player-state->json-public-state player-state0)))))
  (define example-win-rpc-arglist (jsexpr->string (list "win" (list #t)))))


; Test receiving a setup message
(module+ test
  (test-case
   "Test receiving a remote procedure call for setup(...)"
   (define output (open-output-string))
   (define input (open-input-string example-setup-rpc-arglist))
   (define conn (tcp-conn-new input output))
   (define proxy-referee (proxy-referee-new player0 conn))
   (send proxy-referee msg-handling-loop)
   (check-equal? (get-output-string output) "\"void\"")))


; Test receiving a take-turn message
(module+ test
  (test-case
   "Test receiving a remote procedure call for take-turn(...)"
   (define output (open-output-string))
   (define input (open-input-string example-take-turn-rpc-arglist))
   (define conn (tcp-conn-new input output))
   (define proxy-referee (proxy-referee-new player0 conn))
   (send proxy-referee msg-handling-loop)
   (check-equal? (get-output-string output) "[0,\"LEFT\",0,{\"column#\":3,\"row#\":3}]")))


; Test receiving a win message
(module+ test
  (test-case
   "Test receiving a remote procedure call for win(...)"
   (define output (open-output-string))
   (define input (open-input-string example-win-rpc-arglist))
   (define conn (tcp-conn-new input output))
   (define proxy-referee (proxy-referee-new player0 conn))
   (send proxy-referee msg-handling-loop)
   (check-equal? (get-output-string output) "\"void\"")))

