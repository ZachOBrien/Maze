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
      (execute-safe (send (hash-ref players color) name)))

    ;; Void -> [Listof AvatarColor] [Listof AvatarColor]
    ;; Runs a game from start to finish, 
    (define/public (run-game)
      (begin
        (send-setup)
        (define final-state (play-until-completion state0 players))
        (define winners (determine-winners final-state))
        (define criminals (filter (λ (plyr) (not (member plyr (get-player-color-list final-state))))
                                  (get-player-color-list state0)))
        (values winners criminals)))

    ;; Void -> Void
    ;; Update each player with the initial board and their treasure position
    (define (send-setup)
      (hash-for-each players
                     (lambda (color plyr)
                       (execute-safe (send plyr setup
                                           (referee-state->player-state state0 color)
                                           (player-info-treasure-pos (gamestate-get-by-color state0 color)))))))))

;; RefereeState HashTable Natural -> RefereeState
;; Plays at most `rounds-remaining` rounds of Maze, and returns the
;; gamestate when the game has ended
(define (play-until-completion state players [rounds-remaining MAX-ROUNDS])
  (cond
    [(<= rounds-remaining 0) state]
    [else (play-until-completion-help state players rounds-remaining)]))
         
;; RefereeState HashTable Natural -> RefereeState
;; Plays at most `rounds-remaining` rounds of Maze, and returns the
;; gamestate when the game has ended
(define (play-until-completion-help state players rounds-remaining)
  (define player-colors (get-player-color-list state))
  (define-values (next-state plyrs-passed-turn) (run-round state players player-colors))
  (define all-players-passed (equal? player-colors plyrs-passed-turn))
  (cond
    [(or (game-over? next-state) all-players-passed) next-state]
    [else (play-until-completion next-state players (sub1 rounds-remaining))]))

;; [Listof AvatarColor] HashTable [Listof AvatarColor] [Listof AvatarColor] ->
;;                                           (values RefereeState [Listof AvatarColor])
;; Run a round of the game, end the round early if the game is over
(define (run-round state players player-colors [passed-plyrs '()])
  (cond [(empty? player-colors) (values state passed-plyrs)]
        [else (let-values ([(passed-turn next-state)
                            (execute-turn state
                                          (hash-ref players (first player-colors))
                                          (first player-colors))])
                (cond
                  [(game-over? next-state) (values next-state passed-plyrs)]
                  [else (if passed-turn
                            (run-round next-state (rest player-colors)
                                       (cons (first player-colors) passed-plyrs))
                            (run-round next-state (rest player-colors) passed-plyrs))]))]))

    
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
  (execute-safe (send plyr take-turn plyr-state) time-limit-sec))

;; Thunk Number -> (U Any 'misbehaved)
;; Evaluate a thunk safely
(define (execute-safe request [time-limit-sec 4])
  (with-handlers ([exn:fail? (λ (exn) 'misbehaved)])
    (with-limits time-limit-sec #f request)))

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
     (check-equal? (list "green") winners)))
  (test-case
   "Run a game of Maze gs5"
   (let-values
       ([(winners criminals)
         (send example-referee2 run-game)])
     (check-equal? empty criminals)
     (check-equal? (list "yellow") winners))))


(module+ test
  (check-equal? (determine-winners gamestate5) '("red"))
  (check-equal? (determine-winners gamestate4) '("purple"))
  (check-equal? (determine-winners gamestate1) '("blue")))
