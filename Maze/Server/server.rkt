#lang racket/base

(require racket/tcp)
(require racket/function)
(require json)

(require "../Remote/safety.rkt")

;;; This script implements a server for a game of Maze


;; Listener -> [Listof ProxyPlayer]
;; Sign up proxy players to play a game of Maze
(define (signup listener time-limit max-players)
  (define first-phase-conns (collect-connections listener (current-seconds) time-limit max-players))
  (define final-connections
    (cond
      [((length first-phase-conns) . >= . 2) first-phase-conns]
      [else (collect-connections listener (current-seconds) time-limit max-players first-phase-conns)]))
  1)
  
  
;; Listener Integer PositiveInteger PositiveInteger -> [Listof (cons InputPort OutputPort)]
;; Attempts to establish up to `max-conns` connections over a maximum time span `time-limit`
;; INTEPRETATION: `start-time` is expressed in UNIX epoch seconds
(define (collect-connections listener start-time time-limit max-conns [connections '()])
  (define listening-complete? (or (= (length connections) max-conns)
                                  ((- (current-seconds) start-time) . >= . time-limit)))
  (cond
    [listening-complete? connections]
    [else (collect-connections listener start-time time-limit max-conns
                               (if (tcp-accept-ready? listener)
                                   (let-values ([(input-port output-port) (tcp-accept listener)])
                                     (cons (cons input-port output-port) connections))
                                   connections))]))


;; InputPort OutputPort -> (U ProxyPlayer #f)
;; Attempts to create a proxy player from a connection. Returns false
;; if the player does not provide a name within `time-limit-s` seconds
(define (connect-player input-port output-port time-limit-s)
  (define name (execute-safe (thunk (read-json input-port))))
  (cond
    [(or (eq? name 'misbehaved) (not (string? name))) #f]
    [else (proxy-player-new name input-port output-port)]))
     

#;
(define (main)
  (define server (tcp-listener 27015))
  (define proxy-players (setup))
  (cond
    [((length proxy-players) . < . 2) (write-json (list empty empty))]
    [...]))


(module+ main
  1)


;; --------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit))

; Test connect-player
(module+ test
  (test-case
   "Test creating a well-behaved player"
   (define input (open-input-string "\"aoun\""))
   (define output (open-output-string))
   (define new-player (connect-player input output 2))
   (check-equal? "aoun" (send name new-player)))
  (test-case
   "Test creating a player that sends invalid JSON for a name"
   (define input (open-input-string "aoun"))
   (define output (open-output-string))
   (check-equal? #f (connect-player input output 2)))
  (test-case
   "Test creating a player that sends non-string JSON for their name"
   (define input (open-input-string "27"))
   (define output (open-output-string))
   (check-equal? #f (connect-player input output 2))))
  
      
      