#lang racket

;;; This module provides data definitions and logic for a player

;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require "../Common/board.rkt")
(require "../Common/tile.rkt")
(require "../Common/state.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(define DEFAULT-BOARD-SIZE 7)

(define/contract referee%
  (class/c)
  (class object%
    (init init-player-list init-gamestate)
    
    (define gstate init-gamestate)
    (define active-players (make-hash
                            (for/list ([p init-player-list]
                                       [c (get-player-color-list gstate)])
                              (cons c p))))
    (define kicked-players '())

    (super-new)

    (define/public (run-game)
      (begin
        (send-setup)
        ; runs the game
        (execute-game)
        )
        #f)

    (define (send-setup)
      (hash-for-each active-players
                     (lambda (color plyr)
                       (send
                        plyr
                        setup
                        (gamestate->playerstate gstate color)
                        (player-info-goal-pos (gamestate-get-by-color gstate color))))))

    (define (execute-game)
      (for 1000 times
        (run-round)))

    ;; [Listof AvatarColor] [Listof AvatarColor] [Listof AvatarColor] ->
    ;;                                           (values [Listof AvatarColor] [Listof AvatarColor])
    ;; Run a round
    ;; ADD IN A NUM PASS ACCULUMATOR
    (define (run-round gs queue [finished-plyrs '()] [kicked-plyrs '()] )
      (cond [(empty? queue) (values gs finished-plyrs kicked-plyrs)]
            [else (let-values [(next-gs plyr-legal) (execute-turn gs (first queue))]
                    (if plyr-legal
                        (run-round next-gs (rest queue) (cons (first queue) finished-plyrs) kicked-plyrs)
                        (run-round next-gs (rest queue) finished-plyrs (cons (first queue) (kicked-plyrs)))))]))

    ;; Gamestate AvatarColor -> Gamestate
    ;; Execute a turn for the player
    (define (execute-turn gstate color)
      (define mv (send (hash-ref color) take-turn (gamestate->player-state gstate color)))
      (define valid-mv? (valid-move? (gamestate-board gstate)
                                     (gamestate-extra-tile gstate)
                                     (gamestate-prev-shift gstate)
                                     (gamestate-get-by-color color)
                                     mv))

      (if valid-mv?
          (end-current-turn (gamestate-move-player
                             (gamestate-shift-and-insert gstate
                                                         (move-shift mv)
                                                         (move-orientation mv))
                             (move-pos mv)))
          (remove-player gstate)))


;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out)))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "../Common/board.rkt" examples))
  (require (submod "../Common/state.rkt" examples))
  (require (submod "../Players/player-state.rkt" examples))
  (require (submod "../Common/player-info.rkt" examples)))

