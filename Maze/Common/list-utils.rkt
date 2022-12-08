#lang racket

;;; This module provides utilities for working with lists


;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out
  ; Do two lists have the same elements?
  [same-elements? (-> list? list? boolean?)]
  ; Divide a list into chunks
  [chunk-list (-> list? (and/c integer? positive?) (listof list?))]
  ; Replace the first item in the list that passes the predicate with the given item
  [replacef (-> list? (-> any/c boolean?) any/c list?)]
  ; Convert a pair of numbers to a string representation
  [pair->string (-> (cons/c number? number?) string?)]
  ; Keep all elements in list1 which do not appear in list2
  ; Items must be comparable with equal?
  [list-difference (-> (listof any/c) (listof any/c) (listof any/c))]
  ; Keep all elements in list1 which alsoappear in list2
  ; Items must be comparable with equal?
  [list-intersection (-> (listof any/c) (listof any/c) (listof any/c))]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/function)

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

;; [Listof T] (-> T Boolean) T -> [Listof T]
;; Replace the first item in the list that passes the predicate with the given item
(define (replacef lst pred item)
  (cond
    [(empty? lst) empty]
    [(pred (first lst)) (cons item (rest lst))]
    [else (cons (first lst) (replacef (rest lst) pred item))]))


;; Pair -> String
;; Represent a cons pair of numbers as a string
(define (pair->string pair)
  (string-append "(" (number->string (car pair)) ", " (number->string (cdr pair)) ")"))


;; [Listof Any] [Listof Any] -> [Listof Any]
;; Keep all elements in list1 which do not appear in list2
;; Items must be comparable with equal?
(define (list-difference list1 list2)
  (filter (λ (x) (not (member x list2))) list1))


;; [Listof Any] [Listof Any] -> [Listof Any]
;; Keep all elements in list1 which alsoappear in list2
;; Items must be comparable with equal?
(define (list-intersection list1 list2)
  (filter (λ (x) (member x list2)) list1))


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

(module+ test
  (check-equal? (replacef (list 1 2 3 4) even? 9) (list 1 9 3 4))
  (check-equal? (replacef empty even? 0) empty)
  (check-equal? (replacef (list 1 2 3 4) (curry equal? 5) 9) (list 1 2 3 4)))


; test pair->string
(module+ test
  (check-equal? (pair->string (cons 9 1)) "(9, 1)"))


; test list-difference
(module+ test
  (check-equal? (list-difference empty empty) empty)
  (check-equal? (list-difference empty '(1)) empty)
  (check-equal? (list-difference '(1) empty) '(1))
  (check-equal? (list-difference '(1 2 3) '(2)) '(1 3))
  (check-equal? (list-difference '(1 2 3) '(2 3 1)) empty))

; test list-intersection
(module+ test
  (check-equal? (list-intersection empty empty) empty)
  (check-equal? (list-intersection empty '(1)) empty)
  (check-equal? (list-intersection '(1) empty) empty)
  (check-equal? (list-intersection '(1 2 3) '(2)) '(2))
  (check-equal? (list-intersection '(1 2 3) '(2 3 1)) '(1 2 3)))
