#lang racket

;; This module provides data definitions and logic for the gems that appear on tiles

(provide
 gems
 gem/c)


;; interpretation: A precious gem
(define gems (list
              'alexandrite-pear-shape
              'alexandrite
              'almandine-garnet))
(define gem/c (apply or/c gems))