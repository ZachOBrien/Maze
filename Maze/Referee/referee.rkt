#lang racket

;;; This module provides data definitions and logic for a player

;; --------------------------------------------------------------------
;; MODULE INTERFACE

(provide
 (contract-out
  ; Run a game of Maze
  [run-game (-> (listof (or/c player? proxy-player?)) referee-state? (listof any/c) (values (listof avatar-color?) (listof avatar-color?) hash?))]))
     

;; --------------------------------------------------------------------
;; DEPENDENCIES

(require racket/sandbox)

(require "../Common/state.rkt")
(require "../Common/player-info.rkt")
(require "../Common/rulebook.rkt")
(require "../Common/math.rkt")
(require "../Players/player.rkt")
(require "../Remote/player.rkt")
(require "../Remote/safety.rkt")

;; --------------------------------------------------------------------
;; DATA DEFINITIONS


;; --------------------------------------------------------------------
;; FUNCTIONALITY IMPLEMENTATION

(define DEFAULT-BOARD-SIZE 7)  ; Default number of tiles in a row and in a column
(define MAX-ROUNDS 1000)  ; Maximum number of rounds the game may be played for

;; [Listof Player] RefereeState [Listof Observer] -> [Listof AvatarColor] [Listof AvatarColor] HashTable
;; Runs a game of Labrynth, finding winners and cheaters
(define (run-game init-players state0 observers)
  (begin
    (notify-observers state0 observers)
    (define players (make-hash (for/list ([p init-players]
                                          [c (get-player-color-list state0)])
                                 (cons c p))))
    (define-values (state-after-getting-names color-names) (get-color-names players state0))
    (define state-after-setup (setup-all-players players state-after-getting-names))
    (notify-observers state-after-setup observers)
    (define game-over-state (play-until-completion state-after-setup players MAX-ROUNDS observers))
    (define winners (determine-winners game-over-state))
    (define final-state (notify-winners-and-losers winners game-over-state players))
    (define winners-that-didnt-get-kicked
      (filter (λ (plyr) (member plyr (get-player-color-list final-state))) winners))
    (define criminals (filter (λ (plyr) (not (member plyr (get-player-color-list final-state))))
                              (get-player-color-list state0)))
    (for ([observer observers])
      (send observer run))
    (values winners-that-didnt-get-kicked criminals color-names)))


;; RefereeState HashTable Natural [Listof Observer] -> [Listof RefereeState]
;; Plays at most `rounds-remaining` rounds of Maze, and returns the
;; gamestate when the game has ended. Accumulates the states after each player move
;; in reverse order
(define (play-until-completion state players rounds-remaining observers)
  (play-until-completion-help state players rounds-remaining observers))
         
;; RefereeState [HashTable AvatarColor : Player] Natural [Listof Observer] -> RefereeState
;; Plays at most `rounds-remaining` rounds of Maze, and returns final state
(define (play-until-completion-help curr-state players rounds-remaining observers)
  (cond
    [(<= rounds-remaining 0) curr-state]
    [else (let*-values ([(player-colors) (get-player-color-list curr-state)]
                        [(game-over? state-after-round plyrs-passed-turn) (run-round curr-state players player-colors observers)]
                        [(new-player-colors) (get-player-color-list state-after-round)]
                        [(all-players-passed) (equal? new-player-colors plyrs-passed-turn)])
            (cond
              [(or game-over? all-players-passed) state-after-round]
              [else (play-until-completion-help state-after-round players (sub1 rounds-remaining) observers)]))]))


;; RefereeState [HashTable AvatarColor : Player] [Listof AvatarColor] [Listof Observer] [Listof AvatarColor] -> (values Boolean RefereeState [Listof AvatarColor])
;; Run a round of the game, end the round early if the game is over.
;; Returned boolean flag indicates whether a player won the game this round
(define (run-round state players player-colors observers [passed-plyrs '()])
  (cond [(empty? player-colors) (values #f state passed-plyrs)]
        [else (let*-values ([(passed-turn? player-won? next-state) (execute-turn state
                                                                                 (hash-ref players (first player-colors))
                                                                                 (first player-colors))]
                            [(new-passed-plyrs) (if passed-turn? (cons (first player-colors) passed-plyrs) passed-plyrs)])
                (notify-observers next-state observers)
                (cond
                  [player-won? (values #t next-state passed-plyrs)]
                  [else (run-round next-state
                                   players
                                   (rest player-colors)
                                   observers
                                   new-passed-plyrs)]))]))


;; RefereeState [Hash Color:Player] AvatarColor -> (values Boolean Boolean RefereeState)
;; Execute a turn for the player.
;; The first boolean flag is true if they chose to pass turn
;; The second boolean flag is true if they won on this turn
(define (execute-turn state player color)
  (define mv (safe-get-action player (referee-state->player-state state color)))
  (cond
    [(false? mv) (values #t #f (end-current-turn state))]
    [(or (equal? 'misbehaved mv) (not (valid-move? state mv))) (values #f #f (remove-player state))]
    [else (begin (define gamestate-after-move (gamestate-execute-move state mv))
                 (cond
                   [(and (player-on-treasure? gamestate-after-move) (false? (player-info-going-home? (gamestate-current-player state))))
                    (values #f #f (assign-next-goal-and-send-setup gamestate-after-move player color))]
                   [(and (player-on-home? gamestate-after-move) (player-info-going-home? (gamestate-current-player state)))
                    (values #f #t (end-current-turn gamestate-after-move))]
                   [else
                    (values #f #f (end-current-turn gamestate-after-move))]))]))


;; Gamestate Player AvatarColor -> RefereeState
;; Assign a player their next goal, and return the Gamestate after notifying them
(define (assign-next-goal-and-send-setup state player color)
  (define state-after-assigning-next-goal (assign-next-goal state color))
  (let ([state-after-notify (send-setup-to-player state-after-assigning-next-goal player color)])
    (if (equal? (gamestate-current-player state-after-notify) (gamestate-current-player state-after-assigning-next-goal))
        (end-current-turn state-after-notify)
        state-after-notify)))


;; RefereeState -> [Listof AvatarColor]
;; Determine which players (if any) won the game
(define (determine-winners state)
  (cond
    [(empty? (gamestate-players state)) empty]
    [else (define max-goals-visited (apply max (map num-goals-visited (gamestate-players state))))
          (define players-that-visited-max-num-goals
            (filter (λ (plyr-info) (= (num-goals-visited plyr-info) max-goals-visited))
                    (gamestate-players state)))
          (map player-info-color (all-min-distance players-that-visited-max-num-goals))]))


;; [Listof Player] -> [Listof Player]
;; Get all players which are minimum distance from their objective
(define (all-min-distance players)
  (cond
    [(empty? players) empty]
    [(let* ([distances (map (curryr distance-from-objective euclidean-dist) players)]
            [min-dist (apply min distances)])
       (filter (λ (plyr) (= (distance-from-objective plyr euclidean-dist) min-dist)) players))]))


;; RefereeState [Listof Observer] -> [Listof Void]
;; Updates the observers with the state
(define (notify-observers state observers)
  (for ([observer observers])
    (send observer add-state state)))

;; ===== SAFELY GETTING NAMES =====

;; [HashTable AvatarColor : Player] RefereeState -> (values RefereeState [HashTable AvatarColor : String])
;; Asks each player for their name to construct a hash table mapping colors to player names
;; TODO: Use the state's color list to send setups in the correct order, rather than using
;;       a list of colors from the hash table keys which *does not preserve order*
(define (get-color-names players start-state)
  (define-values (final-state final-color-names)
    (for/fold ([state start-state]
               [color-names '()])
              ([color (hash-keys players)])
      (let-values ([(new-state name) (send-get-name-to-player state (hash-ref players color) color)])
        (values new-state (cons (cons color name) color-names)))))
  (values final-state (make-hash final-color-names)))

;; RefereeState Player AvatarColor -> Gamestate String
;; Get a player name
(define (send-get-name-to-player state plyr color)
  (define result (execute-safe (thunk (send plyr name))))
  (match result
    ['misbehaved (values (remove-player-by-color state color) "")]
    [_ (values state result)]))
  
;; ==================================


;; ===== SAFELEY SENDING SETUP =====

;; [HashTable AvatarColor : Player] Gamestate -> Gamestate
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
  (match (execute-safe (thunk (send plyr setup
                                    (referee-state->player-state state color)
                                    (player-info-goal-pos (gamestate-get-by-color state color)))))
    ['misbehaved (remove-player-by-color state color)]
    [_ state]))

;; ==================================

;; ===== SAFELY NOTIFYING WINNERS AND LOSERS =====

;; [Listof AvatarColor] RefereeState [Hash AvatarColor : Player] -> RefereeState
;; Notify players that they either won or lost
(define (notify-winners-and-losers winners final-state players)
  (define losers (filter (λ (color) (not (member color winners))) (get-player-color-list final-state)))
  (define state-after-notifying-winners (notify-outcome #t winners players final-state))
  (define state-after-notifying-losers (notify-outcome #f losers players state-after-notifying-winners))
  state-after-notifying-losers)

;; Boolean [Listof AvatarColor] [Hash Color:Player] RefereeState -> RefereeState
;; Notify a set of players whether they have won or lost the game
(define (notify-outcome win? colors players final-state)
  (for/fold ([state final-state])
            ([color colors])
    (safe-send-outcome state (hash-ref players color) color win?)))


;; Gamestate Player AvatarColor Boolean -> Gamestate
;; Notifies a player whether they won or lost, and returns the same gamestate either with that player
;; or, if they don't behave properly, without the player
(define (safe-send-outcome state plyr color win?)
  (match (execute-safe (thunk (send plyr win win?)))
    ['misbehaved (remove-player-by-color state color)]
    [_ state]))

;; ==================================


;; Player -> (U Action 'misbehaved)
;; Get a player's action. The Player may misbehave, and the following behaviors are handled:
;;    1. The call to the Player's take-turn method raises an exception
;;    2. The call to the Player's take-turn method exceeds a time limit
;; If the player misbehaves in any of these ways, 'misbehaved is returned.
(define (safe-get-action plyr plyr-state [time-limit-sec 4])
  (execute-safe (thunk (send plyr take-turn plyr-state)) time-limit-sec))


;; [Listof Any] [Listof Any] -> Boolean
;; Returns true if no elements from the first list are present in the second list,
;; and vice versa
(define (disjoint? l1 l2)
  (set-empty? (set-intersect (list->set l1) (list->set l2))))

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

;; ==========================================
;; TOP LEVEL TESTS - FOR RUNNING ENTIRE GAMES

(module+ test
  (test-case
   "Run a game of Maze gs5"
   (let-values
       ([(winners criminals color-names)
         (run-game (list player0 player1 player2) gamestate5 empty)])
     (check-equal? empty criminals)
     (check-equal? (list "red") winners)))
  (test-case
   "Run a game of Maze gs4"
   (let-values
       ([(winners criminals color-names)
         (run-game (list player0 player1 player2 player3) gamestate4 empty)])
     (check-equal? empty criminals)
     (check-equal? (list "green") winners)))
  (test-case
   "Run a game of Maze gs1"
   (let-values
       ([(winners criminals color-names)
         (run-game (list player0 player1) gamestate1 empty)])
     (check-equal? empty criminals)
     (check-equal? (list "yellow") winners)))
  (test-case
   "Run a game of Maze gs6: Multiple Goals"
   (let-values
       ([(winners criminals color-names)
         (run-game (list player0 player1 player2 player3 player-bad-win) gamestate6 empty)])
     (check-equal? (list "black") criminals)
     (check-equal? (list "yellow") winners)))
  (test-case
   "Run a game of Maze gs7: Multiple Goals"
   (let-values
       ([(winners criminals color-names)
         (run-game (list player0 player1) gamestate7 empty)])
     (check-equal? empty criminals)
     (check-equal? (list "A1A1A1") winners)))
  (test-case
   "Run a game of Maze gs8: Multiple Goals"
   (let-values
       ([(winners criminals color-names)
         (run-game (list player0 player1) gamestate8 empty)])
     (check-equal? empty criminals)
     (check-equal? (list "B2B2B2") winners))))


;; ==========================================
;; Tests for helpers

(module+ test
  (check-equal? (determine-winners gamestate5) '("red"))
  (check-equal? (determine-winners gamestate4) '("purple"))
  (check-equal? (determine-winners gamestate1) '("black")))


;; ==========================================
;; Tests for properly handling bad players

;; Test getting players' names
(module+ test
  (test-case
   "A well-behaved player gives their name"
   (let-values
       ([(state-after-getting-name name)
         (send-get-name-to-player gamestate0 player0 "blue")])
     (check-equal? state-after-getting-name gamestate0)
     (check-equal? name "bob")))
  (test-case
   "A misbehaving player fails to give their name"
      (let-values
       ([(state-after-getting-name name)
         (send-get-name-to-player gamestate0 player-bad-name "blue")])
     (check-equal? state-after-getting-name (remove-player gamestate0))
     (check-equal? name ""))))


;; Test sending players the initial state (calling `setup`)
(module+ test
  (test-case
   "A well-behaved player handles setup"
   (let ([state-after-setup (send-setup-to-player gamestate0 player0 "blue")])
     (check-equal? state-after-setup gamestate0)))
  (test-case
   "A misbehaved player fails to handle setup"
   (let ([state-after-setup (send-setup-to-player gamestate0 player-bad-setup "blue")])
     (check-equal? state-after-setup (remove-player gamestate0)))))

;; Test having players take their turn
(module+ test
  (test-case
   "A well-behaved player chooses an action"
   (let-values
       ([(passed-turn? player-won? state-after-turn)
         (execute-turn gamestate0 player0 "blue")])
     (check-equal? passed-turn? #f)))
  (test-case
   "A misbehaved player chooses an action"
   (let-values
       ([(passed-turn? player-won? state-after-turn)
         (execute-turn gamestate0 player-bad-taketurn "blue")])
     (check-equal? passed-turn? #f)
     (check-equal? state-after-turn (remove-player gamestate0)))))

;; Test informing players they won
(module+ test
  (test-case
   "A well-behaved player handles learning they won"
   (let ([state-after-inform-outcome (safe-send-outcome gamestate0 player0 "blue" #t)])
     (check-equal? state-after-inform-outcome gamestate0)))
  (test-case
   "A misbehaved player fails to handle setup"
   (let ([state-after-inform-outcome (safe-send-outcome gamestate0 player-bad-win "blue" #t)])
     (check-equal? state-after-inform-outcome (remove-player gamestate0)))))