#lang racket

;;; This module provides logic for serializing and deserializing JSON representations
;;; of the Maze game's data definitions

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  ;; Convert a hashtable to a Boards
  [hash->board (-> hash? board?)]
  ;; Convert a GridPosn to a hashtable
  [gridposn->hash (-> grid-posn? hash?)]
  ;; Convert a hashtable to a Tile
  [hash->spare-tile (-> hash? tile?)]
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












