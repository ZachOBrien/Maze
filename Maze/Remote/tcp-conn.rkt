#lang racket/base


;;; This module implements a JSON wrapper around a TCP connection.
;;; More accurately, it is a JSON wrapper around any pair of input and output ports.


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [tcp-conn? contract?]
  ; Create a new TcpConn
  [tcp-conn-new (-> input-port? output-port? tcp-conn?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES
 
(require json)

(require racket/class)


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; InputPort OutputPort -> TcpConn
;; Creates a new instance of TcpConn, a wrapper for a TCP connection
(define (tcp-conn-new input-port output-port)
  (new tcp-conn% [init-input-port input-port]
       [init-output-port output-port]))

                   
(define/contract tcp-conn%
  (class/c [send-json (->m jsexpr? void?)]
           [receive-json (->m (or/c jsexpr? #f))])
  (class object%
    (init init-input-port init-output-port)

    (define input init-input-port)
    (define output init-output-port)

    (super-new)

    ;; Send a single JSON value
    (define/public (send-json jsexpr)
      (write-json jsexpr output)
      (flush-output output))

    ;; Read a single JSON value
    ;; If the connection has been closed, returns #f
    (define/public (receive-json)
      (define val (read-json input))
      (if (eof-object? val) #f val))))


;; Any -> Boolean
;; Is this value an instance of tcp-conn%?
(define tcp-conn? (is-a?/c tcp-conn%))


;; --------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit))

; Test reading from tcp conn
(module+ test
  (test-case
   "Test reading until empty"
   (define conn (tcp-conn-new (open-input-string "\"hello, world\"")
                              (open-output-string "")))
   (check-equal? (send conn receive-json) "hello, world")
   (check-equal? (send conn receive-json) #f)))


; Test writing to tcp conn
(module+ test
  (test-case
   "Test writing two values"
   (define output (open-output-string ""))
   (define conn (tcp-conn-new (open-input-string "\"hello, world\"")
                              output))
   (send conn send-json 2)
   (send conn send-json #t)
   (check-equal? (get-output-string output) "2true")))
