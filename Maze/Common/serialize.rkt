#lang racket

;;; This module provides logic for serializing and deserializing JSON representations
;;; of the Maze game's data definitions

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  ;; Convert a hashtable to a Board
  [hash->board (-> hash? board?)]
  ;; Convert a GridPosn to a hashtable
  [gridposn->hash (-> grid-posn? hash?)]
  ;; Convert a hashtable to a Gamestate
  [hash->gamestate (-> hash? gamestate?)]
  ;; Convert a hashtable to a GridPosn
  [hash->gridposn (-> hash? grid-posn?)]
  ;; Convert a hashtable to a PlayerInfo
  [hash->player-info (-> hash? player-info?)]
  ;; Convert a json action to a Move
  [json-action->last-action (-> (or/c (listof any/c) 'null) shift?)]
  ;; Convert a string direction to a symbol
  [string-direction->symbol (-> string? symbol?)]
  ;; Convert a hashtable to a referee state
  [hash->referee-state (-> hash? referee-state?)]
  ; Convert an Action to json
  [action->json (-> action? (or/c string? (list/c natural-number/c string? orientation? hash?)))]
  ; Convert a list of values to a Player
  [list->player (-> (or/c (list/c string? string?)
                          (list/c string? string? string?)
                          (list/c string? string? string? (and/c integer? positive?)))
                    player?)]))


;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "tile.rkt")
(require "board.rkt")
(require "state.rkt")
(require "player-info.rkt")
(require "../Players/strategy.rkt")
(require "../Players/player.rkt")

;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(module+ examples
  (provide (all-defined-out)))

(module+ test
  (require rackunit))


;; HashTable -> PlayerInfo
;; Create a player-info from a HashTable
(define (hash->player-info ht)
  (ref-player-info-new (hash->gridposn (hash-ref ht 'current))
                       (hash->gridposn (hash-ref ht 'home))
                       (cons 1 1)
                       #f
                       (hash-ref ht 'color)))

;; HashTable -> PlayerInfo
;; Create a player-info from a HashTable
(define (hash->referee-player-info ht)
  (ref-player-info-new (hash->gridposn (hash-ref ht 'current))
                   (hash->gridposn (hash-ref ht 'home))
                   (hash->gridposn (hash-ref ht 'goto))
                   #f
                   (hash-ref ht 'color)))

(module+ test
  (check-equal? (hash->player-info (hash 'current (hash 'row# 0 'column# 0)
                                         'home (hash 'row# 2 'column# 2)
                                         'color "blue"))
                (ref-player-info-new (cons 0 0) (cons 2 2) (cons 1 1) #f "blue"))
  (check-equal? (hash->player-info (hash 'current (hash 'row# 6 'column# 1)
                                         'home (hash 'row# 3 'column# 4)
                                         'color "red"))
                (ref-player-info-new (cons 6 1) (cons 3 4) (cons 1 1) #f "red")))

;; List -> Player
;; Make a player from the json array
(define (list->player inp)
  (define strat (if (equal? (first (rest inp)) "Riemann")
                    riemann-strategy
                    euclidean-strategy))
  (cond
    [(= (length inp) 2) (player-new (first inp) strat)]
    [(= (length inp) 3) (cond
                          [(equal? (list-ref inp 2) "win") (player-bad-win-new (first inp) strat)]
                          [(equal? (list-ref inp 2) "takeTurn") (player-bad-taketurn-new (first inp) strat)]
                          [(equal? (list-ref inp 2) "setUp") (player-bad-setup-new (first inp) strat)])]
    [(= (length inp) 4) (cond
                          [(equal? (list-ref inp 2) "win") (player-infloop-win-new (first inp) strat (list-ref inp 3))]
                          [(equal? (list-ref inp 2) "takeTurn") (player-infloop-taketurn-new (first inp) strat (list-ref inp 3))]
                          [(equal? (list-ref inp 2) "setUp") (player-infloop-setup-new (first inp) strat (list-ref inp 3))])]))


;; HashTable -> RefereeState
;; Makes a RefereeState from a hashtable
(define (hash->referee-state ht)
  (referee-state-new
   (hash->board (hash-ref ht 'board))
   (hash->spare-tile (hash-ref ht 'spare))
   (map hash->referee-player-info (hash-ref ht 'plmt))
   (json-action->last-action (hash-ref ht 'last))))

;; HashTable -> Gamestate
;; Makes a gamestate from a hashtable
(define (hash->gamestate ht)
  (referee-state-new
   (hash->board (hash-ref ht 'board))
   (hash->spare-tile (hash-ref ht 'spare))
   (map hash->player-info (hash-ref ht 'plmt))
   (json-action->last-action (hash-ref ht 'last))))
   

(module+ examples
  (define example-board
    (list
     (list
      (tile-new 'straight 0 (list 'stilbite 'zircon))
      (tile-new 'straight 90 (list 'stilbite 'zircon))
      (tile-new 'elbow 180 (list 'stilbite 'zircon))
      (tile-new 'elbow 0 (list 'stilbite 'zircon))
      (tile-new 'elbow 270 (list 'stilbite 'zircon))
      (tile-new 'elbow 90 (list 'stilbite 'zircon))
      (tile-new 'tri 0 (list 'stilbite 'zircon)))
     (list
      (tile-new 'straight 0 (list 'prasiolite 'carnelian))
      (tile-new 'straight 90 (list 'prasiolite 'carnelian))
      (tile-new 'elbow 180 (list 'prasiolite 'carnelian))
      (tile-new 'elbow 0 (list 'prasiolite 'carnelian))
      (tile-new 'elbow 270 (list 'prasiolite 'carnelian))
      (tile-new 'elbow 90 (list 'prasiolite 'carnelian))
      (tile-new 'tri 0 (list 'prasiolite 'carnelian)))
     (list
      (tile-new 'straight 0 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'straight 90 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 180 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 0 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 270 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'elbow 90 (list 'fancy-spinel-marquise 'jasper))
      (tile-new 'tri 0 (list 'fancy-spinel-marquise 'jasper)))
     (list
      (tile-new 'straight 0 (list 'peridot 'purple-cabochon))
      (tile-new 'straight 90 (list 'peridot 'purple-cabochon))
      (tile-new 'elbow 180 (list 'peridot 'purple-cabochon))
      (tile-new 'elbow 0 (list 'peridot 'purple-cabochon))
      (tile-new 'elbow 270 (list 'peridot 'purple-cabochon))
      (tile-new 'elbow 90 (list 'peridot 'purple-cabochon))
      (tile-new 'tri 0 (list 'peridot 'purple-cabochon)))
     (list
      (tile-new 'straight 0 (list 'diamond 'lapis-lazuli))
      (tile-new 'straight 90 (list 'diamond 'lapis-lazuli))
      (tile-new 'elbow 180 (list 'diamond 'lapis-lazuli))
      (tile-new 'elbow 0 (list 'diamond 'lapis-lazuli))
      (tile-new 'elbow 270 (list 'diamond 'lapis-lazuli))
      (tile-new 'elbow 90 (list 'diamond 'lapis-lazuli))
      (tile-new 'tri 0 (list 'diamond 'lapis-lazuli)))
     (list
      (tile-new 'straight 0 (list 'cordierite 'mexican-opal))
      (tile-new 'straight 90 (list 'cordierite 'mexican-opal))
      (tile-new 'elbow 180 (list 'cordierite 'mexican-opal))
      (tile-new 'elbow 0 (list 'cordierite 'mexican-opal))
      (tile-new 'elbow 270 (list 'cordierite 'mexican-opal))
      (tile-new 'elbow 90 (list 'cordierite 'mexican-opal))
      (tile-new 'tri 0 (list 'cordierite 'mexican-opal)))
     (list
      (tile-new 'straight 0 (list 'pink-opal 'red-diamond))
      (tile-new 'straight 90 (list 'pink-opal 'red-diamond))
      (tile-new 'elbow 180 (list 'pink-opal 'red-diamond))
      (tile-new 'elbow 0 (list 'pink-opal 'red-diamond))
      (tile-new 'elbow 270 (list 'pink-opal 'red-diamond))
      (tile-new 'elbow 90 (list 'pink-opal 'red-diamond))
      (tile-new 'tri 0 (list 'pink-opal 'red-diamond)))))

  (define spare-tile (tile-new 'elbow 90 (list 'lapis-lazuli 'pink-opal)))

  (define example-treasures
    (list
     (list (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon")
           (list "stilbite" "zircon"))
     (list (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian")
           (list "prasiolite" "carnelian"))
     (list (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper")
           (list "fancy-spinel-marquise" "jasper"))
     (list (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon")
           (list "peridot" "purple-cabochon"))
     (list (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli")
           (list "diamond" "lapis-lazuli"))
     (list (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal")
           (list "cordierite" "mexican-opal"))
     (list (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond")
           (list "pink-opal" "red-diamond"))))


  (define example-connectors
    (list '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")
          '("│" "─" "┐" "└" "┌" "┘" "┬")))

  (define example-player-infos1
    (list (hash 'current (hash 'row# 0 'column# 0) 'home (hash 'row# 6 'column# 6) 'color "blue")
          (hash 'current (hash 'row# 1 'column# 1) 'home (hash 'row# 5 'column# 5) 'color "red")
          (hash 'current (hash 'row# 2 'column# 2) 'home (hash 'row# 4 'column# 4) 'color "green")
          (hash 'current (hash 'row# 3 'column# 3) 'home (hash 'row# 3 'column# 3) 'color "yellow")))

  (define expected-player-infos1
    (list (ref-player-info-new (cons 0 0) (cons 6 6) (cons 1 1) #f "blue")
          (ref-player-info-new (cons 1 1) (cons 5 5) (cons 1 1) #f "red")
          (ref-player-info-new (cons 2 2) (cons 4 4) (cons 1 1) #f "green")
          (ref-player-info-new (cons 3 3) (cons 3 3) (cons 1 1) #f "yellow")))

  (define example-board-hash
    (hash 'connectors example-connectors
          'treasures example-treasures)))

(module+ test
  (require (submod ".." examples))
  (check-equal? (hash->board example-board-hash) example-board))

(module+ test
  (require (submod ".." examples))
  (check-equal? (hash->gamestate (hash 'board example-board-hash
                                       'spare (hash 'tilekey "┘"
                                                    '1-image "lapis-lazuli"
                                                    '2-image "pink-opal")
                                       'plmt example-player-infos1
                                       'last (list 0 "LEFT")))
                (referee-state-new example-board
                                   spare-tile
                                   expected-player-infos1
                                   (shift-new 'left 0))))
