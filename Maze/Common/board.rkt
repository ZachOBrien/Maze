#lang racket/base

(require racket/contract)


(provide
 (contract-out
  ; Creates a new tile
  [make-tile (-> connector? orientation? (listof gem?) tile?)]))


;; Interpretation: Represents a tile in the game of labyrinth
(struct tile (connector orientation gems))

(define (make-tile connector orientation gems)
  (tile connector orientation gems))



;; A Connector is one of:
;  - 'straight
;  - 'elbow
;  - 'tri
;  - 'cross

(define (connector? symbol)
  (ormap (lambda (x) (eq? x symbol)) (list 'straight 'elbow 'tri 'cross)))


;; An Orientation is one of:
;  - 'up
;  - 'down
;  - 'left
;  - 'right

(define (orientation? symbol)
  (ormap (lambda (x) (eq? x symbol)) (list 'up 'down 'left 'right)))

