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

