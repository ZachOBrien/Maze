#lang racket

;;; This module provides data definitions and logic for the gems that appear on tiles

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)
(require "../Common/state.rkt")
(require "../Common/player-info.rkt")

(provide
 (contract-out
  ; Connect remote players, and run a game of Maze
  [main (-> referee-state? (listof any/c) (and/c positive? integer?) (values (listof string?) (listof string?)))]))

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/class)
(require racket/tcp)
(require racket/function)
(require racket/bool)
(require racket/list)

(require json)

(require "../Remote/safety.rkt")
(require "../Remote/player.rkt")
(require "../Remote/tcp-conn.rkt")
(require "../Referee/referee.rkt")


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;;; This module implements a server for a game of Maze
(define PLAYER-NAME-TIME-LIMIT-SEC 2)
(define SIGNUP-ROUND-TIME-LIMIT-SEC 20)
(define MAX-PLAYERS 6)
(define DEFAULT-PORT 27015)
(define MAX-SIGNUP-ROUNDS 2)

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; RefereeState [Listof Observer] PositiveInteger -> (values [Listof String] [Listof String])
;; Runs a server which hosts a game of Maze
(define (main state0 observers port)
  (define server (tcp-listen port))
  (define proxy-players (signup server SIGNUP-ROUND-TIME-LIMIT-SEC MAX-PLAYERS MAX-SIGNUP-ROUNDS))
  (define-values (winners criminals color-to-name)
    (if ((length proxy-players) . < . 2)
        (values empty empty (hash))
        (run-game proxy-players state0 observers)))
  (tcp-close server)
  
  (define winner-names (map (λ (col) (hash-ref color-to-name col)) winners))
  (define criminal-names (map (λ (col) (hash-ref color-to-name col)) criminals))
  (values winner-names criminal-names))


;; String -> Boolean
;; Is the string a valid name for a player?
(define (valid-name? name)
  (and (regexp-match #rx"^[a-zA-Z0-9]+$" name)
       ((string-length name) . >= . 1)
       ((string-length name) . <= . 20)))

;; Listener Integer Integer Integer -> [Listof ProxyPlayer]
;; Sign up proxy players to play a game of Maze
(define (signup listener time-limit max-players periods-remaining [collected-players '()])
  (cond
    [(zero? periods-remaining) collected-players]
    [else (let* ([players (collect-players listener (current-seconds) time-limit (- max-players (length collected-players)) collected-players)])
            (if ((length players) . >= . 2)
                players
                (signup listener time-limit max-players (sub1 periods-remaining) players)))]))

  
;; Listener Integer PositiveInteger PositiveInteger -> [Listof ProxyPlayer]
;; Attempts to collect up to `max-players` proxy players over a maximum time span `time-limit-s` in seconds
;; INTEPRETATION: `start-time-s` is expressed in UNIX epoch seconds
;; TODO: Look into futures rather than manually counting time
(define (collect-players listener start-time-s time-limit-s max-players [players '()])
  (define listening-complete? (or (= (length players) max-players)
                                  ((- (current-seconds) start-time-s) . >= . time-limit-s)))
  (cond
    [listening-complete? players]
    [else (collect-players listener start-time-s time-limit-s max-players
                           (if (tcp-accept-ready? listener)
                               (let*-values ([(input-port output-port) (tcp-accept listener)]
                                             [(new-proxy-player) (new-connection->proxy-player input-port output-port PLAYER-NAME-TIME-LIMIT-SEC)])
                                 (if (not (false? new-proxy-player))
                                     (begin (cons new-proxy-player players))
                                     players))
                               players))]))

;; InputPort OutputPort PositiveInteger -> (U ProxyPlayer #f)
;; Given a connection, attempts to create a new ProxyPlayer by retrieving a name within some time limit
(define (new-connection->proxy-player input-port output-port time-limit-s)
  (define name (execute-safe (thunk (read-json input-port)) time-limit-s))
  (if (and (string? name) (valid-name? name))
      (proxy-player-new name (tcp-conn-new input-port output-port))
      #f))


;; --------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit))

; test valid-name?
(module+ test
  (check-true (valid-name? "f"))
  (check-false (valid-name? ""))
  (check-true (valid-name? "12345678"))
  (check-false (valid-name? "who-ey-lewis"))
  (check-false (valid-name? "12345fjas6as78hajfjfj"))
  (check-true (valid-name? "12345fjas6as78hajfjf"))
  (check-false (valid-name? "12345fja#jfjfj")))

; Test new-connection->proxy-player
(module+ test
  (test-case
   "Test invalid JSON sent for name"
   (define input (open-input-string "chucky"))
   (define output (open-output-string))
   (check-equal? #f (new-connection->proxy-player input output 2)))
  (test-case
   "Test non-string sent for name"
   (define input (open-input-string "1337"))
   (define output (open-output-string))
   (check-equal? #f (new-connection->proxy-player input output 2)))
  (test-case
   "Test valid name sent"
   (define input (open-input-string "\"chucky\""))
   (define output (open-output-string))
   (check-equal? "chucky" (send (new-connection->proxy-player input output 2) name))))
  
      
