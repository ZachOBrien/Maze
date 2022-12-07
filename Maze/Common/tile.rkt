#lang racket/base

;;; This module provides the data definition and functionality for a Maze game Tile


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)


(provide
 (contract-out
  [tile?        contract?]
  [connector?   contract?]
  [orientation? contract?]
  [orientations (listof orientation?)]
  ; Constructs a new tile
  [tile-new (-> connector? orientation? (list/c gem? gem?) tile?)]
  ; Rotates a tile
  [tile-rotate (-> tile? orientation? tile?)]
  ; Check if a tile holds exactly some gems
  [tile-has-gems? (-> tile? (list/c gem? gem?) boolean?)]
  ; Returns true if you can travel from one tile to its adjacent neighbor vertically
  [tile-connected-vertical?   (-> tile? tile? boolean?)]
  ; Returns true if you can travel from one tile to its adjacent neighbor horizontally
  [tile-connected-horizontal? (-> tile? tile? boolean?)]
  ; Create a tile with a random connector and orientation
  [create-random-tile (-> (list/c gem? gem?) tile?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/match)
(require racket/list)
(require racket/function)

(require "gem.rkt")
(require "list-utils.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS

;; Tile Tile (-> Tile Tile Boolean) -> Boolean
(define (tile=? tile1 tile2 recursive-equal?)
  (and (equal? (tile-connector tile1)
               (tile-connector tile2))
       (equal? (tile-orientation tile1)
               (tile-orientation tile2))
       (same-elements? (tile-gems tile1)
                       (tile-gems tile2))))
 
(define (tile-hash-code tile recursive-equal-hash)
  (+ (* 10000 (tile-connector tile))
     (* 100 (tile-orientation tile))
     (* 1 (tile-gems tile))))
 
(define (tile-secondary-hash-code tile recursive-equal-hash)
  (+ (* 10000 (tile-gems tile))
     (* 100 (tile-orientation tile))
     (* 1 (tile-connector tile))))

;; A Tile is a structure:
;;    (tile Connector Orientation [Listof Gem])
;; interpretation: Represents a tile in the game of labyrinth
(struct tile [connector orientation gems] #:transparent
  #:methods gen:equal+hash
  [(define equal-proc tile=?)
   (define hash-proc  tile-hash-code)
   (define hash2-proc tile-secondary-hash-code)])

;; Connector Orientation [Listof Gem] -> Tile
;; Create a new tile
(define (tile-new connector orientation gems)
  (tile connector orientation gems))

;; A Connector is one of:
;;   - 'straight
;;   - 'elbow
;;   - 'tri
;;   - 'cross
;; interpretation: A pathway along a tile which determines whether a tile
;;                 is "connected" to its neighbor
(define connectors (list 'straight 'elbow 'tri 'cross))
(define connector? (apply or/c connectors))


;; An Orientation is one of:
;;   - 0
;;   - 90
;;   - 180
;;   - 270
;; interpretation: A direction a tile could be facing. Connector shapes
;;                 have the following orientations:
;; "│" 0
;; "─" 270
;; "│" 180
;; "─" 90
;;
;; "└" 0
;; "┘" 90
;; "┐" 180
;; "┌" 270
;;
;; "┬" 0
;; "├" 90
;; "┴" 180
;; "┤" 270
;;
;; "┼" 0
(define orientations (list 0 90 180 270))
(define orientation? (apply or/c orientations))

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

;; Tile [Listof Gem] -> Boolean
;; Check if a tile holds specific gems
(define (tile-has-gems? tile gems)
  (same-elements? (tile-gems tile) gems))

;; Tile Orientation -> Tile
(define (tile-rotate t rotation)
  (tile-new (tile-connector t)
            (modulo (+ (tile-orientation t) rotation) 360)
            (tile-gems t)))

;; Tile Tile -> Boolean
(define (tile-connected-horizontal? left right)
  (and (open-on-right? (tile-connector left) (tile-orientation left))
       (open-on-left? (tile-connector right) (tile-orientation right))))


;; Tile Tile -> Boolean
(define (tile-connected-vertical? top bottom)
  (and (open-on-bottom? (tile-connector top) (tile-orientation top))
       (open-on-top? (tile-connector bottom) (tile-orientation bottom))))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its top edge
(define (open-on-top? connector orientation)
  (match* (connector orientation)
    [('cross _)     #t]
    [('tri o)      (not (= 0 o))]
    [('elbow o)    (or (= 0 o) (= 90 o))]
    [('straight o) (or (= 0 o) (= 180 o))]))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its bottom edge
(define (open-on-bottom? connector orientation)
  (match* (connector orientation)
    [('cross _)     #t]
    [('tri o)      (not (= 180 o))]
    [('elbow o)    (or (= 270 o) (= 180 o))]
    [('straight o) (or (= 0 o) (= 180 o))]))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its left edge
(define (open-on-left? connector orientation)
  (match* (connector orientation)
    [('cross _)     #t]
    [('tri o)      (not (= 90 o))]
    [('elbow o)    (or (= 180 o) (= 90 o))]
    [('straight o) (or (= 270 o) (= 90 o))]))


;; Connector Orientation -> Boolean
;; Returns true if a tile with this connector and orientation is open on its right edge
(define (open-on-right? connector orientation)
  (match* (connector orientation)
    [('cross _)     #t]
    [('tri o)      (not (= 270 o))]
    [('elbow o)    (or (= 0 o) (= 270 o))]
    [('straight o) (or (= 270 o) (= 90 o))]))


;; [Listof Gem] -> Tile
;; Create a tile with a random connector and orientation
(define (create-random-tile gems)
  (define conn (first (shuffle connectors)))
  (define ornt (first (shuffle orientations)))
  (tile conn ornt gems))


(module+ draw
  (require 2htdp/image)

  (provide
   (contract-out
    [tile->image (-> tile? natural-number/c image?)]))
  
  ;; Orientation -> Image
  ;; Draw one arm of a connector
  (define (arm-image orientation arm-length)
    (define arm-width (/ arm-length 3))
    (define vert0degree (put-pinhole (/ arm-width 2) arm-length
                                     (rectangle arm-width arm-length "solid" "sienna")))
    (rotate orientation vert0degree))

  ;; Orientation -> Image
  ;; Draw an elbow connector
  (define (elbow-image orientation arm-length)
    (define elbow0degree (overlay/pinhole (arm-image 0 arm-length)
                                          (arm-image 270 arm-length)))
    (rotate orientation elbow0degree))

  ;; Orientation -> Image
  ;; Draw a straight connector
  (define (straight-image orientation arm-length)
    (define straight0degree (overlay/pinhole (arm-image 0 arm-length)
                                             (arm-image 180 arm-length)))
    (rotate orientation straight0degree))

  ;; Orientation -> Image
  ;; Draw a tri connector
  (define (tri-image orientation arm-length)
    (define tri0degree (overlay/pinhole (arm-image 180 arm-length)
                                        (arm-image 90 arm-length)
                                        (arm-image 270 arm-length)))
    (rotate orientation tri0degree))

  ;; Orientation -> Image
  ;; Draw a cross connector
  (define (cross-image orientation arm-length)
    (define cross0degree (overlay/pinhole (arm-image 0 arm-length)
                                          (arm-image 90 arm-length)
                                          (arm-image 180 arm-length)
                                          (arm-image 270 arm-length)))
    (rotate orientation cross0degree))


  ;; Connector Orientation Natural -> Image
  ;; Draw a connector with some orientation and with arms of length `arm-length`
  (define (draw-connector connector orientation arm-length)
    (match connector
      ['straight (straight-image orientation arm-length)]
      ['elbow (elbow-image orientation arm-length)]
      ['tri (tri-image orientation arm-length)]
      ['cross (cross-image orientation arm-length)]))


  ;; Tile [MultipleOf 10] -> Image
  ;; Draw a tile, a square with side lengths `size`
  (define (tile->image t size)
    (overlay (square size "outline" "black")
             (clear-pinhole (overlay/pinhole (draw-connector (tile-connector t)
                                                             (tile-orientation t)
                                                             (/ size 2))
                                             (square size "solid" "navajowhite"))))))

(module+ serialize
  (require json)

  (require (submod "gem.rkt" serialize))

  (provide
   (contract-out
    [json-connector? contract?]
    [json-tile? contract?]
    ; Converts a tile into a string connector according to spec
    [get-json-connector (-> tile? json-connector?)]
    ; Converts a tile into a list of gems according to spec
    [get-json-treasure (-> tile? json-treasure?)]
    ; Convert a json connector and json treasure into a tile
    [json-connector-and-json-treasure->tile (-> json-connector? json-treasure? tile?)]
    ; Convert a tile to a json representation of a tile
    [tile->json-tile (-> tile? json-tile?)]
    ; Create a tile from a json representation of a tile
    [json-tile->tile (-> json-tile? tile?)]))

  (module+ test
    (require rackunit))

  ;; Any -> Boolean
  ;; Is a Json object a Connector?
  (define json-connector? (or/c "│" "─" "┐" "└" "┌" "┘" "┬" "├" "┴" "┤" "┼"))

  ;; Tile -> JsonConnector
  ;; Converts a tile into a json representation of a connector
  (define (get-json-connector t)
    (match (cons (tile-connector t) (tile-orientation t))
      [(cons 'straight 0)   "│"]
      [(cons 'straight 180) "│"]
      [(cons 'straight 90)  "─"]
      [(cons 'straight 270) "─"]
      [(cons 'elbow 180)    "┐"]
      [(cons 'elbow 0)      "└"]
      [(cons 'elbow 270)    "┌"]
      [(cons 'elbow 90)     "┘"]
      [(cons 'tri 0)        "┬"]
      [(cons 'tri 90)       "├"]
      [(cons 'tri 180)      "┴"]
      [(cons 'tri 270)      "┤"]
      [(cons 'cross _)      "┼"]))

  ;; JsonConnector -> (values Connector Orientation)
  ;; Converts a json representation of a connector into a canonical string and its orientation
  (define string-connector-conversion
    (hash "│" (cons 'straight 0)
          "─" (cons 'straight 90)
          "┐" (cons 'elbow 180)
          "└" (cons 'elbow 0)
          "┌" (cons 'elbow 270)
          "┘" (cons 'elbow 90)
          "┬" (cons 'tri 0)
          "├" (cons 'tri 90)
          "┴" (cons 'tri 180)
          "┤" (cons 'tri 270)
          "┼" (cons 'cross 0)))  ; TODO: Make these both a function

  ;; Tile -> [Listof Gems]
  ;; Converts a tile into a list of gems according to spec
  (define (get-json-treasure t)
    (map symbol->string (tile-gems t)))

  (module+ test
    (check-equal? (get-json-treasure (tile-new 'elbow 180 (list 'amethyst 'beryl))) (list "amethyst" "beryl")))

  ;; JsonConnector JsonTreasure -> Tile
  ;; Converts json representations of connector and teasure into a tile
  (define (json-connector-and-json-treasure->tile conn gems)
    (match-define (cons connector orientation) (hash-ref string-connector-conversion conn))
    (tile-new connector orientation (map string->symbol gems)))
  
  (module+ test
    (check-equal?
     (json-connector-and-json-treasure->tile "│" (list "aplite" "beryl"))
     (tile-new 'straight 0 (list 'aplite 'beryl)))
    (check-equal?
     (json-connector-and-json-treasure->tile "┐" (list "amethyst" "beryl"))
     (tile-new 'elbow 180 (list 'amethyst 'beryl)))
    (check-equal?
     (json-connector-and-json-treasure->tile "┴" (list "aplite" "beryl"))
     (tile-new 'tri 180 (list 'aplite 'beryl))))


  ;; Any -> Boolean
  ;; Is this object a hashtable JSON representation of a tile?
  (define (json-tile? ht)
    (and (hash? ht)
         (hash-has-key? ht 'tilekey)
         (hash-has-key? ht '1-image)
         (hash-has-key? ht '2-image)
         (json-connector? (hash-ref ht 'tilekey))
         (gem? (string->symbol (hash-ref ht '1-image)))
         (gem? (string->symbol (hash-ref ht '2-image)))))

  (module+ test
    (check-true (json-tile? (hash 'tilekey "│" '1-image "alexandrite" '2-image "beryl")))
    (check-false (json-tile? (hash 'tilkey "│" '1-image "alexandrite" '2-image "beryl"))))
  
  ;; Tile -> JsonTile
  ;; Convert a tile into a json representation of a tile
  (define (tile->json-tile t)
    (define gem-list (tile-gems t))
    (hash 'tilekey (get-json-connector t)
          '1-image (symbol->string (first gem-list))
          '2-image (symbol->string (first (rest gem-list)))))

  (module+ test
    (check-equal? (tile->json-tile (tile-new 'straight 90 (list 'alexandrite-pear-shape 'alexandrite)))
                  (hash 'tilekey "─"
                        '1-image "alexandrite-pear-shape"
                        '2-image "alexandrite"))
    (check-equal? (tile->json-tile (tile-new 'elbow 270 (list 'unakite 'white-square)))
                  (hash 'tilekey "┌"
                        '1-image "unakite"
                        '2-image "white-square")))


  ;; JsonTile -> Tile
  ;; Create a tile from a JSON representation of a tile
  (define (json-tile->tile ht)
    (define conn (hash-ref ht 'tilekey))
    (define treasures (list (string->symbol (hash-ref ht '1-image))
                            (string->symbol (hash-ref ht '2-image))))
    (match-define (cons connector orientation) (hash-ref string-connector-conversion conn))
    (tile-new connector orientation treasures))

  (module+ test
    (check-equal? (json-tile->tile (hash 'tilekey "┌"
                                         '1-image "goldstone"
                                         '2-image "heliotrope"))
                  (tile-new 'elbow 270 (list 'goldstone 'heliotrope)))
    (check-equal? (json-tile->tile (hash 'tilekey "┼"
                                         '1-image "diamond"
                                         '2-image "unakite"))
                  (tile-new 'cross 0 (list 'diamond 'unakite)))
  
    (check-equal? (json-tile->tile (hash 'tilekey "─"
                                         '1-image "raw-beryl"
                                         '2-image "pink-opal"))
                  (tile-new 'straight 90 (list 'raw-beryl 'pink-opal)))
  
    (check-equal? (json-tile->tile (hash 'tilekey "┴"
                                         '1-image "hematite"
                                         '2-image "jasper"))
                  (tile-new 'tri 180 (list 'hematite 'jasper)))))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out))
  (define tile00 (tile 'straight 90 (list 'alexandrite-pear-shape 'alexandrite)))
  (define tile01 (tile 'elbow 180 (list 'almandine-garnet 'amethyst)))
  (define tile02 (tile 'elbow 0 (list 'amethyst 'ametrine)))
  (define tile03 (tile 'elbow 90 (list 'ammolite 'apatite)))
  (define tile04 (tile 'elbow 270 (list 'aplite 'apricot-square-radiant)))
  (define tile05 (tile 'tri 0 (list 'aquamarine 'australian-marquise)))
  (define tile06 (tile 'tri 270 (list 'aventurine 'azurite)))

  (define tile10 (tile 'tri 180 (list 'beryl 'black-obsidian)))
  (define tile11 (tile 'tri 90 (list 'black-obsidian 'black-onyx)))
  (define tile12 (tile 'cross 0 (list 'black-spinel-cushion 'blue-ceylon-sapphire)))
  (define tile13 (tile 'straight 0 (list 'blue-cushion 'blue-pear-shape)))
  (define tile14 (tile 'straight 270 (list 'blue-spinel-heart 'bulls-eye)))
  (define tile15 (tile 'elbow 180 (list 'carnelian 'chrome-diopside)))
  (define tile16 (tile 'elbow 0 (list 'chrysoberyl-cushion 'chrysolite)))

  (define tile20 (tile 'elbow 90 (list 'citrine-checkerboard 'citrine)))
  (define tile21 (tile 'elbow 270 (list 'clinohumite 'color-change-oval)))
  (define tile22 (tile 'tri 0 (list 'cordierite 'diamond)))
  (define tile23 (tile 'tri 270 (list 'dumortierite 'emerald)))
  (define tile24 (tile 'tri 180 (list 'fancy-spinel-marquise 'garnet)))
  (define tile25 (tile 'tri 90 (list 'golden-diamond-cut 'goldstone)))
  (define tile26 (tile 'cross 270 (list 'grandidierite 'gray-agate)))

  (define tile30 (tile 'straight 180 (list 'green-aventurine 'green-beryl-antique)))
  (define tile31 (tile 'straight 270 (list 'green-beryl 'green-princess-cut)))
  (define tile32 (tile 'elbow 180 (list 'grossular-garnet 'hackmanite)))
  (define tile33 (tile 'elbow 0 (list 'heliotrope 'hematite)))
  (define tile34 (tile 'elbow 90 (list 'iolite-emerald-cut 'jasper)))
  (define tile35 (tile 'elbow 270 (list 'jaspilite 'kunzite-oval)))
  (define tile36 (tile 'tri 0 (list 'kunzite 'labradorite)))

  (define tile40 (tile 'tri 270 (list 'lapis-lazuli 'lemon-quartz-briolette)))
  (define tile41 (tile 'tri 180 (list 'magnesite 'mexican-opal)))
  (define tile42 (tile 'tri 90 (list 'moonstone 'morganite-oval)))
  (define tile43 (tile 'cross 0 (list 'moss-agate 'orange-radiant)))
  (define tile44 (tile 'straight 0 (list 'padparadscha-oval 'padparadscha-sapphire)))
  (define tile45 (tile 'straight 90 (list 'peridot 'pink-emerald-cut)))
  (define tile46 (tile 'elbow 180 (list 'pink-opal 'pink-round)))
  
  (define tile50 (tile 'elbow 0 (list 'pink-spinel-cushion 'prasiolite)))
  (define tile51 (tile 'elbow 90 (list 'prehnite 'purple-cabochon)))
  (define tile52 (tile 'elbow 270 (list 'purple-oval 'purple-spinel-trillion)))
  (define tile53 (tile 'tri 0 (list 'purple-square-cushion 'raw-beryl)))
  (define tile54 (tile 'tri 270 (list 'raw-citrine 'red-diamond)))
  (define tile55 (tile 'tri 180 (list 'red-spinel-square-emerald-cut 'rhodonite)))
  (define tile56 (tile 'tri 90 (list 'rock-quartz 'rose-quartz)))
  
  (define tile60 (tile 'cross    0 (list 'ruby-diamond-profile 'ruby)))
  (define tile61 (tile 'straight 0 (list 'sphalerite 'spinel)))
  (define tile62 (tile 'straight 90 (list 'star-cabochon 'stilbite)))
  (define tile63 (tile 'elbow 180 (list 'sunstone 'super-seven)))
  (define tile64 (tile 'elbow 0   (list 'tanzanite-trillion 'tigers-eye)))
  (define tile65 (tile 'elbow 90 (list 'tourmaline-laser-cut 'tourmaline)))
  (define tile66 (tile 'elbow 270 (list 'unakite 'white-square)))

  (define tile-extra (tile 'straight 180 (list 'yellow-baguette 'yellow-beryl-oval)))

  (define tile-horiz (tile 'straight 90 (list 'bulls-eye 'blue-ceylon-sapphire)))
  (define tile-vert (tile 'straight 0 (list 'alexandrite 'blue-ceylon-sapphire))))


(module+ test
  (require rackunit)
  (require (submod ".." serialize test))
  (require (submod ".." examples))
  (require (submod ".." serialize)))

;; test tile-has-gems?
(module+ test
  (check-true (tile-has-gems? tile11 (list 'black-obsidian 'black-onyx)))
  (check-true (tile-has-gems? tile11 (list 'black-onyx 'black-obsidian)))
  (check-false (tile-has-gems? tile11 (list 'alexandrite 'blue-ceylon-sapphire))))

;; test tile-rotate
(module+ test
  (check-equal? (tile-rotate tile00 90) (tile 'straight 180 (list 'alexandrite-pear-shape 'alexandrite)))
  (check-equal? (tile-rotate tile00 180) (tile 'straight 270 (list 'alexandrite-pear-shape 'alexandrite)))
  (check-equal? (tile-rotate tile00 270) (tile 'straight 0 (list 'alexandrite-pear-shape 'alexandrite)))
  (check-equal? (tile-rotate tile00 0) (tile 'straight 90 (list 'alexandrite-pear-shape 'alexandrite)))
  (check-equal? (tile-rotate tile66 270) (tile 'elbow 180 (list 'white-square 'unakite))))

;; test tile-connected-horizontal
(module+ test
  (check-true (tile-connected-horizontal? tile00 tile01))
  (check-false (tile-connected-horizontal? tile01 tile02))
  (check-false (tile-connected-horizontal? tile55 tile56))
  (check-false (tile-connected-horizontal? tile60 tile61)))

;; test tile-connected-vertical
(module+ test
  (check-true (tile-connected-vertical? tile01 tile11))
  (check-false (tile-connected-vertical? tile00 tile10))
  (check-false (tile-connected-vertical? tile02 tile12)))

;; test open-on-top?
(module+ test
  (check-true (open-on-top? 'straight 0))
  (check-false (open-on-top? 'straight 90))
  (check-true (open-on-top? 'straight 180))
  (check-false (open-on-top? 'straight 270))
  
  (check-true (open-on-top? 'elbow 0))
  (check-true (open-on-top? 'elbow 90))
  (check-false (open-on-top? 'elbow 180))
  (check-false (open-on-top? 'elbow 270))
  
  (check-false (open-on-top? 'tri 0))
  (check-true (open-on-top? 'tri 90))
  (check-true (open-on-top? 'tri 180))
  (check-true (open-on-top? 'tri 270))
  
  (check-true (open-on-top? 'cross 0))
  (check-true (open-on-top? 'cross 90))
  (check-true (open-on-top? 'cross 180))
  (check-true (open-on-top? 'cross 270)))

;; test open-on-right
(module+ test
  (check-false (open-on-right? 'straight 0))
  (check-true (open-on-right? 'straight 90))
  (check-false (open-on-right? 'straight 180))
  (check-true (open-on-right? 'straight 270))
  
  (check-true (open-on-right? 'elbow 0))
  (check-false (open-on-right? 'elbow 90))
  (check-false (open-on-right? 'elbow 180))
  (check-true (open-on-right? 'elbow 270))
  
  (check-true (open-on-right? 'tri 0))
  (check-true (open-on-right? 'tri 90))
  (check-true (open-on-right? 'tri 180))
  (check-false (open-on-right? 'tri 270))
  
  (check-true (open-on-right? 'cross 0))
  (check-true (open-on-right? 'cross 90))
  (check-true (open-on-right? 'cross 180))
  (check-true (open-on-right? 'cross 270)))

;; test open-on-bottom
(module+ test
  (check-true (open-on-bottom? 'straight 0))
  (check-false (open-on-bottom? 'straight 90))
  (check-true (open-on-bottom? 'straight 180))
  (check-false (open-on-bottom? 'straight 270))
  
  (check-false (open-on-bottom? 'elbow 0))
  (check-false (open-on-bottom? 'elbow 90))
  (check-true (open-on-bottom? 'elbow 180))
  (check-true (open-on-bottom? 'elbow 270))
  
  (check-true (open-on-bottom? 'tri 0))
  (check-true (open-on-bottom? 'tri 90))
  (check-false (open-on-bottom? 'tri 180))
  (check-true (open-on-bottom? 'tri 270))

  (check-true (open-on-bottom? 'cross 0))
  (check-true (open-on-bottom? 'cross 90))
  (check-true (open-on-bottom? 'cross 180))
  (check-true (open-on-bottom? 'cross 270)))

;; test open-on-left
(module+ test
  (check-false (open-on-left? 'straight 0))
  (check-true (open-on-left? 'straight 90))
  (check-false (open-on-left? 'straight 180))
  (check-true (open-on-left? 'straight 270))
  
  (check-false (open-on-left? 'elbow 0))
  (check-true (open-on-left? 'elbow 90))
  (check-true (open-on-left? 'elbow 180))
  (check-false (open-on-left? 'elbow 270))
  
  (check-true (open-on-left? 'tri 0))
  (check-false (open-on-left? 'tri 90))
  (check-true (open-on-left? 'tri 180))
  (check-true (open-on-left? 'tri 270))

  (check-true (open-on-left? 'cross 0))
  (check-true (open-on-left? 'cross 90))
  (check-true (open-on-left? 'cross 180))
  (check-true (open-on-left? 'cross 270)))

;; test tile=?
(module+ test
  (check-equal? (tile-new 'straight 0 empty) (tile-new 'straight 0 empty))
  (check-not-equal? (tile-new 'straight 0 empty) (tile-new 'straight 90 empty))
  (check-not-equal? (tile-new 'elbow 0 empty) (tile-new 'straight 0 empty))
  (check-not-equal? 
   (tile-new 'straight 0 (list 'aplite 'beryl))
   (tile-new 'straight 0 (list 'aplite 'aplite)))
  (check-equal?
   (tile-new 'straight 0 (list 'aplite 'beryl))
   (tile-new 'straight 0 (list 'aplite 'beryl)))
  (check-equal?
   (tile-new 'straight 0 (list 'beryl 'aplite))
   (tile-new 'straight 0 (list 'aplite 'beryl))))

; test get-json-connector
(module+ test
  (check-equal? (get-json-connector (tile-new 'straight 90 empty)) "─")
  (check-equal? (get-json-connector (tile-new 'straight 270 empty)) "─")
  (check-equal? (get-json-connector (tile-new 'straight 0 empty)) "│")
  (check-equal? (get-json-connector (tile-new 'straight 180 empty)) "│")

  (check-equal? (get-json-connector (tile-new 'elbow 90 empty)) "┘")
  (check-equal? (get-json-connector (tile-new 'elbow 270 empty)) "┌")
  (check-equal? (get-json-connector (tile-new 'elbow 0 empty)) "└")
  (check-equal? (get-json-connector (tile-new 'elbow 180 empty)) "┐")

  (check-equal? (get-json-connector (tile-new 'tri 90 empty)) "├")
  (check-equal? (get-json-connector (tile-new 'tri 270 empty)) "┤")
  (check-equal? (get-json-connector (tile-new 'tri 0 empty)) "┬")
  (check-equal? (get-json-connector (tile-new 'tri 180 empty)) "┴")

  (check-equal? (get-json-connector (tile-new 'cross 90 empty)) "┼")
  (check-equal? (get-json-connector (tile-new 'cross 270 empty)) "┼")
  (check-equal? (get-json-connector (tile-new 'cross 0 empty)) "┼")
  (check-equal? (get-json-connector (tile-new 'cross 180 empty)) "┼"))
                   
