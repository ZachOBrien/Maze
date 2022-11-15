#lang racket/base

;;; This script implements a server for a game of Maze


;; Listener -> [Listof ProxyPlayer]
;; Sign up proxy players to play a game of Maze
(define (signup listener time-limit max-players)
  (define first-phase-conns (collect-connections listener (current-seconds) time-limit max-players))
  (define final-connections
    (cond
      [((length players1) . >= . 2) first-phase-conns]
      [else (collect-connections listener (current-seconds) time-limit max-players first-phase-conns)]))
  ; Get player names
  ; Turn connections into proxy players
  ; return proxy players
  )
  
  
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



#;
(define (main)
  (define server (tcp-listener 27015))
  (define proxy-players (setup))
  (cond
    [((length proxy-players) . < . 2) (write-json (list empty empty))]
    [...]))
      
      