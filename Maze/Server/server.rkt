#lang racket/base

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

;;; This script implements a server for a game of Maze


(define PLAYER-NAME-TIME-LIMIT-SEC 2)
(define SIGNUP-ROUND-TIME-LIMIT-SEC 20)
(define MAX-PLAYERS 6)
(define DEFAULT-PORT 27015)
(define MAX-SIGNUP-ROUNDS 2)

;; RefereeState Boolean -> [List [Listof String] [Listof String]]
;; Runs a server which hosts a game of Maze
(define (main state0 observer?)
  (define server (tcp-listen DEFAULT-PORT))
  (define proxy-players (signup server SIGNUP-ROUND-TIME-LIMIT-SEC MAX-PLAYERS MAX-SIGNUP-ROUNDS))
  (begin
    (cond
      [((length proxy-players) . < . 2) (write-json (list empty empty))]
      [(run-game proxy-players state0 observer?)])
    (tcp-close server)))


;; Listener Integer Integer Integer -> [Listof ProxyPlayer]
;; Sign up proxy players to play a game of Maze
(define (signup listener time-limit max-players periods-remaining [collected-players '()])
  (cond
    [(zero? periods-remaining) collected-players]
    [else (let* ([new-players (collect-players listener (current-seconds) time-limit (- max-players (length collected-players)))]
                 [total-players (cons new-players collected-players)])
            (if ((length total-players) . >= . 2)
                collected-players
                (signup listener time-limit max-players (sub1 periods-remaining) total-players)))]))

  
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
                                                (cons new-proxy-player players)
                                                players))
                               players))]))

;; InputPort OutputPort PositiveInteger -> (U ProxyPlayer #f)
;; Given a connection, attempts to create a new ProxyPlayer by retrieving a name within some time limit
(define (new-connection->proxy-player input-port output-port time-limit-s)
  (define name (execute-safe (thunk (read-json input-port)) time-limit-s))
  (cond
    [(or (not (string? name)) (equal? name 'misbehaved)) #f]
    [else (proxy-player-new name (tcp-conn-new input-port output-port))]))


;; --------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit))


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
  
      
