#lang racket/base

;;; Implements a client for the game of Maze

(require racket/tcp)
(require racket/class)
(require racket/sandbox)
(require racket/function)
(require json)

(require "../Remote/tcp-conn.rkt")
(require "../Remote/referee.rkt")
(require "../Players/player.rkt")
(require "../Players/strategy.rkt")

(define DEFAULT-IP-ADDR "localhost")
(define DEFAULT-PORT 27015)
(define CONNECTION-ATTEMPT-TIME-LIMIT-SEC 2)


(require racket/contract)

(provide
 (contract-out
  ; Run a client for a player of Maze
  [run-client (-> string? (and/c integer? positive?) player? any)]))


;; IpAddress PortNo Player
;; Run a client for the Maze game
(define (run-client ip-addr port player)         
  (define-values (c-in c-out) (try-connect ip-addr port))
  (define conn (tcp-conn-new c-in c-out))
  (write-json (send player name) c-out)
  (flush-output c-out)
  (define ref-proxy (proxy-referee-new player conn))
  (send ref-proxy msg-handling-loop))


(define (try-connect ip-addr port)
  (define (loop)
    (with-handlers ([exn:fail? (λ (exn) (loop))])
      (tcp-connect ip-addr port)))
  (call-with-limits CONNECTION-ATTEMPT-TIME-LIMIT-SEC #f (thunk (loop))))
