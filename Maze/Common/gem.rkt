#lang racket

;;; This module provides data definitions and logic for the gems that appear on tiles

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [gem? contract?]
  [gems (listof gem?)]))


;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; A Gem is a symbol, enumerated in gems
;; interpretation: The name of a precious gem
(define gems (list
              'alexandrite-pear-shape
              'alexandrite
              'almandine-garnet))
(define gem? (apply or/c gems))