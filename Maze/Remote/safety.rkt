#lang racket/base


;;; This module provides logic for handling remote communication safely

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  ; Execute a thunk within some time limit constraints
  [execute-safe [->* [(-> any)] [natural-number/c] (or/c any/c 'misbehaved)]]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/sandbox)
(require racket/function)


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(define DEFAULT-TIME-LIMIT-SECONDS 4)


;; Thunk Natural -> (U Any 'misbehaved)
;; Evaluate a Thunk safely
(define (execute-safe thnk [time-limit-sec DEFAULT-TIME-LIMIT-SECONDS])
  (with-handlers ([exn:fail? (λ (exn) 'misbehaved)])
    (call-with-limits time-limit-sec #f thnk)))


;; --------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit))


;; test execute-safe
(module+ test
  (check-equal? (execute-safe (thunk (error 'hi))) 'misbehaved)
  (check-equal? (execute-safe (thunk (sleep 2)) 1) 'misbehaved)
  (check-equal? (execute-safe (thunk 2)) 2))
