#lang racket/base

;;; This module implements a remote proxy player


;; --------------------------------------------------------------------
;; MODULE INTERFACE

(require racket/contract)

(provide
 (contract-out
  [proxy-player? contract?]
  ; Create a new proxy player
  [proxy-player-new (-> string? tcp-conn? proxy-player?)]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/class)

(require (only-in "../Players/player.rkt" player-interface))
(require "tcp-conn.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION


;; String TcpConn -> ProxyPlayer
;; Create a new proxy player
(define (proxy-player-new name tcp-conn)
  (new proxy-player% [init-name name] [init-tcp-conn tcp-conn]))


(define/contract proxy-player% player-interface
  (class object%
    (init init-name init-tcp-conn)

    (define plyr-name init-name)
    (define tcp-conn init-tcp-conn)

    (super-new)

    ;; -> String
    ;; Get the player's name
    (define/public (name) plyr-name)

    ;; Natural Natural -> Board
    ;; Get a starting board
    (define/public (propose-board min-num-rows min-num-cols)
      (send tcp-conn send-json (list "propose-board" (list min-num-rows min-num-cols))))
  
    ;; PlayerState GridPosn -> Any
    ;; Sets initial state and treasure position
    (define/public (setup plyr-state new-goal)
      (send tcp-conn send-json (list "setup" (list plyr-state new-goal))))

    ;; PlayerState -> Action
    ;; Chooses either to make a move or pass
    (define/public (take-turn plyr-state)
      (send tcp-conn send-json (list "take-turn" (list plyr-state))))

    ;; Boolean -> Any
    ;; Informs the player whether they won or lost
    (define/public (win status)
      (send tcp-conn send-json (list "win" (list status))))))

(define proxy-player? (is-a?/c proxy-player%))


;; --------------------------------------------------------------------
;; TESTS


