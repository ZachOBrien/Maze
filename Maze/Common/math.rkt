#lang racket

;;; This module provides logic for mathematical operations

;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out
  ; Euclidean distance between two points
  [euclidean-dist (-> grid-posn? grid-posn? number?)]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "../Common/board.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; GridPosn GridPosn -> Number
;; Computes the euclidean distance between two gridposns
(define (euclidean-dist pos1 pos2)
  (sqrt (+ (expt (- (car pos2) (car pos1)) 2) (expt (- (cdr pos2) (cdr pos1)) 2))))

;; --------------------------------------------------------------------
;; TESTS

(module+ test
  (require rackunit))


(define EPSILON .00001)

; test euclidean-dist
(module+ test
  (check-= (euclidean-dist (cons 1 1) (cons 1 1)) 0 EPSILON)
  (check-= (euclidean-dist (cons 1 2) (cons 3 4)) 2.828427 EPSILON)
  (check-= (euclidean-dist (cons 3 4) (cons 1 2)) 2.828427 EPSILON))
