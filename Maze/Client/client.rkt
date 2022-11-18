#lang racket/base

;;; Implements a client for the game of Maze

(require racket/tcp)
(require racket/class)
(require json)

(require "../Remote/tcp-conn.rkt")
(require "../Remote/referee.rkt")
(require "../Players/player.rkt")
(require "../Players/strategy.rkt")

(define DEFAULT-IP-ADDR "localhost")
(define DEFAULT-PORT 27015)


;; IpAddress PortNo String Strategy
;; Run a client for the Maze game
(define (run-client ip-addr port player-name strategy)         
  (define-values (c-in c-out) (tcp-connect ip-addr port))
  (define conn (tcp-conn-new c-in c-out))
  (define ref-proxy (proxy-referee-new (player-new player-name strategy) conn))
  (send ref-proxy msg-handling-loop))
