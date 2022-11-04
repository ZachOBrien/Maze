#lang racket

;;; This module provides data definitions and logic for a player

;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out
  [referee? contract?]
  ; Create a new Referee
  [referee-new (-> (listof player?) referee-state? referee?)]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/sandbox)

(require "../Common/state.rkt")
(require "../Common/player-info.rkt")
(require "../Common/rulebook.rkt")
(require "../Common/math.rkt")
(require "../Players/player.rkt")
(require "observer.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(define DEFAULT-BOARD-SIZE 7)
(define MAX-ROUNDS 1000)


;; [Listof Player] RefereeState -> Referee
;; Create a new Referee
(define (referee-new player-list state)
  (new referee% [init-player-list player-list] [init-state state]))


(define referee%
  (class object%
    (init init-player-list init-state)
    
    (define state0 init-state)
    (define players (make-hash
                     (for/list ([p init-player-list]
                                [c (get-player-color-list state0)])
                       (cons c p))))

    (super-new)

    ;; AvatarColor -> String
    ;; Get a players name by their avatar color
    (define/public (get-player-name-by-color color)
      (execute-safe (thunk (send (hash-ref players color) name))))

    ;; Void -> [Listof AvatarColor] [Listof AvatarColor]
    ;; Runs a game from start to finish, 
    (define/public (run-game)
      (begin
        (send-setup)
        (define intermediate-states (play-until-completion state0 players MAX-ROUNDS (list state0)))
        (define final-state (first intermediate-states))
        (define winners (determine-winners final-state))
        (define criminals (filter (λ (plyr) (not (member plyr (get-player-color-list final-state))))
                                  (get-player-color-list state0)))
        (run-observer (reverse intermediate-states))
        (values winners criminals)))

   
    ;; Void -> Void
    ;; Update each player with the initial board and their treasure position
    (define (send-setup)
      (hash-for-each players
                     (lambda (color plyr)
                       (execute-safe (thunk (send plyr setup
                                                  (referee-state->player-state state0 color)
                                                  (player-info-treasure-pos (gamestate-get-by-color state0 color))))))))))

;; RefereeState HashTable Natural -> [Listof RefereeState]
;; Plays at most `rounds-remaining` rounds of Maze, and returns the
;; gamestate when the game has ended. Accumulates the states after each player move
;; in reverse order
(define (play-until-completion state players rounds-remaining prev-states)
  (cond
    [(<= rounds-remaining 0) prev-states]
    [else (play-until-completion-help state players rounds-remaining prev-states)]))
         
;; RefereeState HashTable Natural -> [Listof RefereeState]
;; Plays at most `rounds-remaining` rounds of Maze, and returns the
;; gamestate when the game has ended
(define (play-until-completion-help state players rounds-remaining prev-states)
  (define player-colors (get-player-color-list state))
  (define-values (states-after-round plyrs-passed-turn) (run-round state players player-colors '() '()))
  (define all-players-passed (equal? player-colors plyrs-passed-turn))
  (cond
    [(or (game-over? (first states-after-round)) all-players-passed) (append states-after-round prev-states)]
    [else (play-until-completion (first states-after-round) players (sub1 rounds-remaining) (append states-after-round prev-states))]))


;; [Listof AvatarColor] HashTable [Listof AvatarColor] [Listof AvatarColor] ->
;;                                           (values [Listof RefereeState] [Listof AvatarColor])
;; Run a round of the game, end the round early if the game is over. Accumulates states after
;; each move in reverse order.
(define (run-round state players player-colors passed-plyrs intermediate-states)
  (cond [(empty? player-colors) (values intermediate-states passed-plyrs)]
        [else (let*-values ([(passed-turn next-state)
                            (execute-turn state
                                          (hash-ref players (first player-colors))
                                          (first player-colors))]
                           [(new-passed-plyrs) (if passed-turn (cons (first player-colors) passed-plyrs) passed-plyrs)])
                (cond
                  [(game-over? next-state) (values (cons next-state intermediate-states) passed-plyrs)]
                  [else (run-round next-state
                                   players
                                   (rest player-colors)
                                   new-passed-plyrs
                                   (cons next-state intermediate-states))]))]))


;; RefereeState HashTable AvatarColor -> Boolean RefereeState
;; Execute a turn for the player. The boolean flag is true if they chose to pass turn
(define (execute-turn state player color)
  (define mv (safe-get-action player (referee-state->player-state state color)))
  (cond
    [(false? mv) (values #t (end-current-turn state))]
    [(or (equal? 'misbehaved mv) (not (valid-move? state mv))) (values #f (remove-player state))]
    [else 
     (values #f (end-current-turn (gamestate-move-player
                                   (gamestate-shift-and-insert state
                                                               (move-shift mv)
                                                               (move-orientation mv))
                                   (move-pos mv))))]))


;; RefereeState -> [Listof AvatarColor]
;; Determine which players (if any) won the game
(define (determine-winners state)
  (define players-that-visited-treasure
    (filter (λ (plyr-info) (player-info-visited-treasure? plyr-info))
            (gamestate-players state)))
  (map player-info-color (if (empty? players-that-visited-treasure)
                             (all-min-distance (gamestate-players state))
                             (all-min-distance players-that-visited-treasure))))


;; -> (Any -> Boolean)
;; Creates a contract for instances of referee
(define referee?
  (is-a?/c referee%))


;; [Listof Player] -> [Listof Player]
;; Get all players which are minimum distance from their objective
(define (all-min-distance players)
  (define distances (map (curryr distance-from-objective euclidean-dist) players))
  (define min-dist (apply min distances))
  (filter (λ (plyr) (= (distance-from-objective plyr euclidean-dist) min-dist)) players))


;; Player -> (U Action 'misbehaved)
;; Get a player's action. The Player may misbehave, and the following behaviors are handled:
;;    1. The call to the Player's take-turn method raises an exception
;;    2. The call to the Player's take-turn method exceeds a time limit
;; If the player misbehaves in any of these ways, 'misbehaved is returned.
(define (safe-get-action plyr plyr-state [time-limit-sec 4])
  (execute-safe (thunk (send plyr take-turn plyr-state)) time-limit-sec))

;; Thunk Natural -> (U Any 'misbehaved)
;; Evaluate a Thunk safely
(define (execute-safe thnk [time-limit-sec 4])
  (with-handlers ([exn:fail? (λ (exn) 'misbehaved)])
    (call-with-limits time-limit-sec #f thnk)))

;; --------------------------------------------------------------------
;; TESTS

(module+ examples
  (provide (all-defined-out)))

(module+ test
  (require rackunit)
  (require (submod ".." examples))
  (require (submod "../Common/board.rkt" examples))
  (require (submod "../Common/tile.rkt" examples))
  (require (submod "../Common/player-info.rkt" examples))
  (require (submod "../Players/player.rkt" examples))
  (require (submod "../Common/state.rkt" examples)))


(module+ examples
  (require (submod "../Common/state.rkt" examples))
  (require (submod "../Players/player.rkt" examples))
  (require (submod "../Common/player-info.rkt" examples))
  (define example-referee0 (new referee%
                                [init-player-list (list player0 player1 player2)]
                                [init-state gamestate5]))
  (define example-referee1 (new referee%
                                [init-player-list (list player0 player1 player2 player3)]
                                [init-state gamestate4]))
  (define example-referee2 (new referee%
                                [init-player-list (list player0 player1)]
                                [init-state gamestate1])))

(module+ test
  (test-case
   "Run a game of Maze"
   #;(let-values
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
     (check-equal? (list "green") winners)))
  #;(test-case
   "Run a game of Maze gs5"
   (let-values
       ([(winners criminals)
         (send example-referee2 run-game)])
     (check-equal? empty criminals)
     (check-equal? (list "yellow") winners))))


(module+ test
  (check-equal? (determine-winners gamestate5) '("red"))
  (check-equal? (determine-winners gamestate4) '("purple"))
  (check-equal? (determine-winners gamestate1) '("black")))

;; test execute-safe
(module+ test
  (check-equal? (execute-safe (thunk (error 'hi))) 'misbehaved)
  (check-equal? (execute-safe (thunk (sleep 2)) 1) 'misbehaved)
  (check-equal? (execute-safe (thunk 2)) 2))
