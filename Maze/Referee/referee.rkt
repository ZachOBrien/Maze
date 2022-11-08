#lang racket

;;; This module provides data definitions and logic for a player

;; --------------------------------------------------------------------
;; MODULE INTERFACE


(provide
 (contract-out
  [run-game (-> (listof player?) referee-state? boolean? (values (listof avatar-color?) (listof avatar-color?)))]))
     

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

(define DEFAULT-BOARD-SIZE 7)  ; Default number of tiles in a row and in a column
(define MAX-ROUNDS 1000)  ; Maximum number of rounds the game may be played for


;; [Listof Player] RefereState Boolean -> [Listof AvatarColor] [Listof AvatarColor]
;; Runs a game of Labrynth, finding winners and cheaters
(define (run-game init-players state0 observer?)
  (begin
    (define players (make-hash (for/list ([p init-players]
                                          [c (get-player-color-list state0)])
                                 (cons c p))))
    (define state-after-setup (setup-all-players players state0))
    (define intermediate-states (play-until-completion state-after-setup players MAX-ROUNDS))
    (define final-state (first intermediate-states))
    (define winners (determine-winners final-state))
    (define criminals (filter (λ (plyr) (not (member plyr (get-player-color-list final-state))))
                              (get-player-color-list state0)))
    (if observer? (run-observer (reverse intermediate-states)) #f)
    (values winners criminals)))

;; [HashTable Color : Player] Gamestate -> Gamestate
;; Update each player with the initial board and their treasure position. The gamestate returned
;; is the same as the original gamestate, but with any misbehaving players kicked
(define (setup-all-players players state0)
  (for/fold ([state state0])
            ([color (hash-keys players)])
    (send-setup-to-player state (hash-ref players color) color)))


;; Gamestate Player AvatarColor -> Gamestate
;; Sends a gamestate to the player, and returns the same gamestate either with that player
;; or, if they don't behave properly, without the player
(define (send-setup-to-player state plyr color)
  (define result (execute-safe (thunk (send plyr setup
                                            (referee-state->player-state state color)
                                            (player-info-treasure-pos (gamestate-get-by-color state color))))))
  (if (equal? result 'misbehaved)
      (remove-player-by-color state color)
      state))

;; RefereeState HashTable Natural -> [Listof RefereeState]
;; Plays at most `rounds-remaining` rounds of Maze, and returns the
;; gamestate when the game has ended. Accumulates the states after each player move
;; in reverse order
(define (play-until-completion state players rounds-remaining)
  (play-until-completion-help (list state) players rounds-remaining))
         
;; [NonEmptyListof RefereeState] HashTable Natural -> [Listof RefereeState]
;; Plays at most `rounds-remaining` rounds of Maze, and returns a list of
;; all intermediate gamestates once the game is over
(define (play-until-completion-help prev-states players rounds-remaining)
  (cond
    [(<= rounds-remaining 0) prev-states]
    [else (let*-values ([(curr-state) (first prev-states)]
                        [(player-colors) (get-player-color-list curr-state)]
                        [(states-after-round plyrs-passed-turn) (run-round curr-state players player-colors)]
                        [(new-states) (append states-after-round prev-states)]
                        [(new-player-colors) (get-player-color-list (first new-states))]
                        [(all-players-passed) (equal? new-player-colors plyrs-passed-turn)])
            (cond
              [(or (game-over? (first states-after-round)) all-players-passed) new-states]
              [else (play-until-completion-help new-states players (sub1 rounds-remaining))]))]))


;; RefereeState HashTable [Listof AvatarColor] [Listof AvatarColor] [Listof RefereeState] -> (values [Listof RefereeState] [Listof AvatarColor])
;; Run a round of the game, end the round early if the game is over. Accumulates states after
;; each move in reverse order.
(define (run-round state players player-colors [passed-plyrs '()] [intermediate-states '()])
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
  (require (submod "../Common/player-info.rkt" examples)))

(module+ test
  (test-case
   "Run a game of Maze"
   (let-values
       ([(winners criminals)
         (run-game (list player0 player1 player2) gamestate5 #f)])
     (check-equal? empty criminals)
     (check-equal? (list "red") winners)))
  (test-case
   "Run a game of Maze gs4"
   (let-values
       ([(winners criminals)
         (run-game (list player0 player1 player2 player3) gamestate4 #f)])
     (check-equal? empty criminals)
     (check-equal? (list "green") winners)))
  (test-case
   "Run a game of Maze gs5"
   (let-values
       ([(winners criminals)
         (run-game (list player0 player1) gamestate1 #f)])
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
