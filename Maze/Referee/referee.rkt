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
(require "../Common/player-info.rkt")
(require "../Players/strategy.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(define DEFAULT-BOARD-SIZE 7)
(define MAX-ROUNDS 1000)


(define referee%
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
        (define final-state (play-until-completion gstate))
        (define winners (determine-winners final-state))
        (define criminals (filter (λ (plyr) (not (member plyr (get-player-color-list final-state))))
                                  (get-player-color-list gstate)))
        (values winners criminals)))

    ;; Void -> Void
    ;; Update each player with the initial board and their goal tile
    (define (send-setup)
      (hash-for-each active-players
                     (lambda (color plyr)
                       (send plyr setup
                        (gamestate->player-state gstate color)
                        (player-info-goal-pos (gamestate-get-by-color gstate color))))))

    ;; Gamestate Natural -> Gamestate
    ;; Plays at most `rounds-remaining` rounds of Maze, and returns the
    ;; gamestate when the game has ended
    (define (play-until-completion state [rounds-remaining MAX-ROUNDS])
      (cond
        [(< 0 rounds-remaining) state]
        [else (play-until-completion-help state rounds-remaining)]))
         
    ;; Gamestate Natural -> Gamestate
    ;; Plays at most `rounds-remaining` rounds of Maze, and returns the
    ;; gamestate when the game has ended
    (define (play-until-completion-help state rounds-remaining)
      (define players (get-player-color-list state))
      (define-values (next-state plyrs-passed-turn) (run-round state players))
      (define all-players-passed (equal? players plyrs-passed-turn))
      (cond
        [(or (game-over? next-state) all-players-passed) next-state]
        [else (play-until-completion next-state (sub1 rounds-remaining))]))

    ;; [Listof AvatarColor] [Listof AvatarColor] [Listof AvatarColor] ->
    ;;                                           (values Gamestate [Listof AvatarColor])
    ;; Run a round
    (define (run-round state queue [passed-plyrs '()])
      (cond [(empty? queue) (values state passed-plyrs)]
            [else (let-values ([(passed-turn next-state) (execute-turn state (first queue))])
                    (cond
                      [(game-over? next-state) (values next-state passed-plyrs)]
                      [else (if passed-turn
                                (run-round next-state (rest queue) (cons (first queue) passed-plyrs))
                                (run-round next-state (rest queue) passed-plyrs))]))]))

    
    ;; Gamestate AvatarColor -> Boolean Gamestate
    ;; Execute a turn for the player. The boolean flag is true if they chose to pass turn
    (define (execute-turn gstate color)
      (define mv (send (hash-ref color) take-turn (gamestate->player-state gstate color)))
      (define move-is-valid (valid-move? (gamestate-board gstate)
                                         (gamestate-extra-tile gstate)
                                         (gamestate-prev-shift gstate)
                                         (gamestate-get-by-color color)
                                         (move-orientation mv)
                                         (move-shift mv)
                                         (move-pos mv)))
      (cond
        [(false? mv) (values #t (end-current-turn gstate))]
        [move-is-valid (values #f (end-current-turn (gamestate-move-player
                                                     (gamestate-shift-and-insert gstate
                                                                                 (move-shift mv)
                                                                                 (move-orientation mv))
                                                     (move-pos mv))))]
        [(not move-is-valid) (values #f (remove-player gstate))]))

    ;; Gamestate -> [Listof AvatarColor]
    ;; Determine which players (if any) won the game
    (define (determine-winners state)
      (define players (get-player-color-list state))
      (define plyrs-visited-goal
        (filter (λ (color) (player-info-visited-goal? (gamestate-get-by-color state color)))
                players))
      (cond
        [(empty? plyrs-visited-goal)
         (let* ([distances (map (curry euclidean-distance-from-objective state) players)]
                [min-dist (apply min distances)])
           (filter (λ (plyr) (= (euclidean-distance-from-objective state plyr) min-dist)) players))]
        [else
         (let* ([distances (map (curry euclidean-distance-from-objective state) plyrs-visited-goal)]
                [min-dist (apply min distances)])
           (filter (λ (plyr) (= (euclidean-distance-from-objective state plyr) min-dist)) plyrs-visited-goal))]))))


;; TODO: REFACTOR THIS INTO RULEBOOK ***********************************************************************************************************************************
;; Board Tile Shift PlayerInfo Move -> Boolean
;; Returns true if the move is valid
;; PlayerState Move -> Boolean
;; Returns True if the move is valid in the state
(define (valid-move? board extra-tile prev-shift plyr-info orientation shift pos)
  (define-values
    (new-board new-extra-tile)
    (board-shift-and-insert board shift (tile-rotate extra-tile orientation)))
  (define new-player (first (shift-players (list plyr-info) board shift)))
  (and (not (equal? pos (player-info-curr-pos new-player)))
       (not (shift-undoes-shift? shift prev-shift))
       (member pos (board-all-reachable-from new-board (player-info-curr-pos new-player)))))

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

