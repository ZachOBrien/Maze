#lang racket

;;; This module provides utilities for working with lists


;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out
  ; Do two lists have the same elements?
  [same-elements? (-> list? list? boolean?)]
  ; Divide a list into chunks
  [chunk-list (-> list? (and/c integer? positive?) (listof list?))]))


;; --------------------------------------------------------------------
;; DEPENDENCIES



;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; List List -> Boolean
;; Do two lists have the same elements?
(define (same-elements? lst1 lst2)
  (and (= (length lst1) (length lst2))
       (andmap (λ (elem) (and (member elem lst2) #t)) lst1)))


;; List PositiveInteger -> [Listof Set]
;; Divide a list into chunks. If the list cannot be evenly divided into such chunks,
;; the last chunk will be smaller.
(define (chunk-list lst chunk-size [acc '()])
  (cond
    [(empty? lst) (reverse acc)]
    [(< (length lst) chunk-size) (reverse (cons lst acc))]
    [else (chunk-list (list-tail lst chunk-size) chunk-size (cons (take lst chunk-size) acc))]))


;; --------------------------------------------------------------------
;; TEST

(module+ test
  (require rackunit))


; Test same-elements?
(module+ test
  (check-true (same-elements? empty empty))
  (check-false (same-elements? empty '(1)))
  (check-false (same-elements? '(1) empty))
  (check-true (same-elements? '(1) '(1)))
  (check-false (same-elements? '(1) '(1 1)))
  (check-true (same-elements? '(1 2) '(2 1)))
  (check-false (same-elements? '(1 3 2 4) '(4 3 2 1 4)))
  (check-true (same-elements? '(1 3 2 4) '(4 3 2 1))))


; Test chunk-list
(module+ test
  (check-equal? (chunk-list '() 1) '())
  (check-equal? (chunk-list '(1) 1) (list (list 1)))
  (check-equal? (chunk-list '(1 2) 1) (list (list 1) (list 2)))
  (check-equal? (chunk-list '(1 2) 2) (list (list 1 2)))
  (check-equal? (chunk-list '(1 2 3) 2) (list (list 1 2) (list 3)))
  (check-equal? (chunk-list '(1 2 3 4 5 6 7) 2) (list (list 1 2) (list 3 4) (list 5 6) (list 7)))
  (check-equal? (chunk-list '(1 2 3 4 5 6 7) 3) (list (list 1 2 3) (list 4 5 6) (list 7))))