#lang racket

;;; This module provides data definitions and logic for the gems that appear on tiles

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [gem? contract?]
  [gems (listof gem?)]
  [gem->image (-> gem? image?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require 2htdp/image)

;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; A Gem is a symbol, enumerated in gems
;; interpretation: The name of a precious gem
(define gems (list
              'alexandrite-pear-shape
              'alexandrite
              'almandine-garnet
              'amethyst
              'ametrine
              'ammolite
              'apatite
              'aplite
              'apricot-square-radiant
              'aquamarine
              'australian-marquise
              'aventurine
              'azurite
              'beryl
              'black-obsidian
              'black-onyx
              'black-spinel-cushion
              'blue-ceylon-sapphire
              'blue-cushion
              'blue-pear-shape
              'blue-spinel-heart
              'bulls-eye
              'carnelian
              'chrome-diopside
              'chrysoberyl-cushion
              'chrysolite
              'citrine-checkerboard
              'citrine
              'clinohumite
              'color-change-oval
              'cordierite
              'diamond
              'dumortierite
              'emerald
              'fancy-spinel-marquise
              'garnet
              'golden-diamond-cut
              'goldstone
              'grandidierite
              'gray-agate
              'green-aventurine
              'green-beryl-antique
              'green-beryl
              'green-princess-cut
              'grossular-garnet
              'hackmanite
              'heliotrope
              'hematite
              'iolite-emerald-cut
              'jasper
              'jaspilite
              'kunzite-oval
              'kunzite
              'labradorite
              'lapis-lazuli
              'lemon-quartz-briolette
              'magnesite
              'mexican-opal
              'moonstone
              'morganite-oval
              'moss-agate
              'orange-radiant
              'padparadscha-oval
              'padparadscha-sapphire
              'peridot
              'pink-emerald-cut
              'pink-opal
              'pink-round
              'pink-spinel-cushion
              'prasiolite
              'prehnite
              'purple-cabochon
              'purple-oval
              'purple-spinel-trillion
              'purple-square-cushion
              'raw-beryl
              'raw-citrine
              'red-diamond
              'red-spinel-square-emerald-cut
              'rhodonite
              'rock-quartz
              'rose-quartz
              'ruby-diamond-profile
              'ruby
              'sphalerite
              'spinel
              'star-cabochon
              'stilbite
              'sunstone
              'super-seven
              'tanzanite-trillion
              'tigers-eye
              'tourmaline-laser-cut
              'tourmaline
              'unakite
              'white-square
              'yellow-baguette
              'yellow-beryl-oval
              'yellow-heart
              'yellow-jasper
              'zircon
              'zoisite))

(define gem? (apply or/c gems))


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;(define IMAGES-PATH "../Assets/gems/")
(define IMAGES-EXTENSION ".png")

(define-values (up-path cur-path bool) (split-path (current-directory)))

(define (get-project-root [path (current-directory)])
  (define path-string (path->string path))
  (if (string-contains? path-string "Maze")
      (let-values ([(up-path cur-dir root?) (split-path path)])
        (get-project-root up-path))
      path))
  

(define IMAGES-PATH (build-path (get-project-root up-path) "Maze/Assets/gems/"))

;; Gem -> Image
;; Render a gem as an image
(define (gem->image gem)
  (bitmap/file (string-append (path->string IMAGES-PATH) (symbol->string gem) IMAGES-EXTENSION)))


(module+ serialize
  (require json)

  (provide
   (contract-out
    ; Does this JSON expression represent a treasure?
    [json-treasure? contract?]))

  (module+ test
    (require rackunit))

  ;; Any -> Boolean
  ;; Does this JSON expression represent a treasure?
  (define (json-treasure? lst)
    (and (list? lst)
         ((list/c gem? gem?) (map string->symbol lst))))

  (module+ test
    (check-true (json-treasure? (list "alexandrite" "aplite")))
    (check-false (json-treasure? (list "Alexandrite" "Aplite")))
    (check-false (json-treasure? (list "alexandrite")))))


(module+ test
  (require rackunit)
  (require (submod ".." serialize test)))

