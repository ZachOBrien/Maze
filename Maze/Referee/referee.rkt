#lang racket

;;; This module provides data definitions and logic for a player

;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/sandbox)

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
    
    (define state0 init-gamestate)
    (define active-players (make-hash
                            (for/list ([p init-player-list]
                                       [c (get-player-color-list state0)])
                              (cons c p))))
    (define kicked-players '())

    (super-new)

    ;; Void -> [Listof AvatarColor] [Listof AvatarColor]
    ;; Runs a game from start to finish, 
    (define/public (run-game)
      (begin
        (send-setup)
        (define final-state (play-until-completion state0))
        (define winners (determine-winners final-state))
        (define criminals (filter (λ (plyr) (not (member plyr (get-player-color-list final-state))))
                                  (get-player-color-list state0)))
        (values winners criminals)))

    ;; Void -> Void
    ;; Update each player with the initial board and their goal tile
    (define (send-setup)
      (hash-for-each active-players
                     (lambda (color plyr)
                       (send plyr setup
                        (gamestate->player-state state0 color)
                        (player-info-goal-pos (gamestate-get-by-color state0 color))))))

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
    (define (execute-turn state color)
      (define mv (safe-get-action (hash-ref color) (gamestate->player-state state color)))
      (define move-is-valid (valid-move? (gamestate-board state)
                                         (gamestate-extra-tile state)
                                         (gamestate-prev-shift state)
                                         (gamestate-get-by-color state color)
                                         (move-orientation mv)
                                         (move-shift mv)
                                         (move-pos mv)))
      (cond
        [(false? mv) (values #t (end-current-turn state))]
        [move-is-valid (values #f (end-current-turn (gamestate-move-player
                                                     (gamestate-shift-and-insert state
                                                                                 (move-shift mv)
                                                                                 (move-orientation mv))
                                                     (move-pos mv))))]
        [(not move-is-valid) (values #f (remove-player state))]))

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


;; Player -> (U Action 'misbehaved)
;; Get a player's action. The Player may misbehave, and the following behaviors are handled:
;;    1. The call to the Player's take-turn method raises an exception
;;    2. The call to the Player's take-turn method exceeds a time limit
;; If the player misbehaves in any of these ways, 'misbehaved is returned.
(define (safe-get-action plyr plyr-state [time-limit-sec 4])
  (with-handlers ([exn:fail? 'misbehaved])
    (with-limits time-limit-sec #f (send plyr take-turn plyr-state))))


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
  (require (submod "../Common/tile.rkt" examples))
  (require (submod "../Common/player-info.rkt" examples)))


(module+ examples
  (require (submod "../Common/state.rkt" examples))
  (require (submod "../Players/player.rkt" examples))
  (define example-referee0 (new referee%
                                [init-player-list (list player0 player1 player2)]
                                [init-gamestate gamestate5]))
  (define example-referee1 (new referee%
                                [init-player-list (list player0 player1 player2 player3)]
                                [init-gamestate gamestate4]))
  (define example-referee2 (new referee%
                                [init-player-list (list player0 player1)]
                                [init-gamestate gamestate1])))

(module+ test
  (test-case
   "Run a game of Maze"
   (let-values
     ([(winners criminals)
      (send example-referee0 run-game)])
     (check-equal? empty criminals)
     (check-equal? (list "red") winners)))
  (test-case
   "Run a game of Maze gs4"
   (let-values
       ([(winners criminals)
         (send example-referee1 run-game)])
     (check-equal? empty criminals)
     (check-equal? (list "purple") winners)))
  (test-case
   "Run a game of Maze gs5"
   (let-values
       ([(winners criminals)
         (send example-referee2 run-game)])
     (check-equal? empty criminals)
     (check-equal? (list "blue") winners))))

; test valid-move? board extra-tile prev-shift plyr-info orientation shift pos)
(module+ test
  (check-false (valid-move? board1 tile-extra (shift-new 'down 2) player-info1 0 (shift-new 'down 2) (cons 3 1)))
  (check-false (valid-move? board1 tile-extra (shift-new 'up 2) player-info1 90 (shift-new 'right 2) (cons 5 1)))
  (check-false (valid-move? board1 tile-extra (shift-new 'right 2) player-info1 180 (shift-new 'down 2) (cons 3 3)))
  (check-false (valid-move? board1 tile-extra (shift-new 'left 2) player-info1 270 (shift-new 'left 2) (cons 3 1)))
  (check-not-false (valid-move? board1 tile-extra (shift-new 'down 2) player-info1 0 (shift-new 'down 2) (cons 2 1))))